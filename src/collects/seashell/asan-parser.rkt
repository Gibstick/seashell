#lang racket/base

;; Seashell
;; Copyright (C) 2012-2014 The Seashell Maintainers
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/contract 
         racket/list 
         racket/string
         json
         )

(provide parse-asan-output)

;; convert a bytestring to a number
;; produces false if the conversion fails.
(define/contract (bytes->number n)
  (-> bytes? (or/c number? #f))
  (string->number (bytes->string/utf-8 n)))

;; a list of functions to ignore when tracing
(define ignored-functions '("_start" "__libc_start_main"))

;; parse-asan-output(contents) parses AddressSanitizer output
;; requires: contents is solely ASan output
(define/contract (parse-asan-output contents)
  (-> any/c any/c)
  ;; Note: special chars need to be escaped like \\t,
  ;;   due to ugliness in Racket strings
  (define file-pattern
    (byte-regexp #"\\/([^\\/]+(:[0-9]+|[^\\)]+))\\)?$"))
  (define address-pattern
    (byte-regexp #"0x[0-9a-f]+"))
  (define supp-alloc-addr-pattern
    (byte-regexp #"^(0x[0-9a-f]+)(?:[^0-9])*([0-9]+).*(left|right|inside)(?:[^0-9])*([0-9]+)-byte"))
  (define supp-stack-addr-pattern
    (byte-regexp #"(0x[0-9a-f]+).*located in stack.*offset ([0-9]+)"))
  (define supp-global-addr-pattern
    (byte-regexp #"^(0x[0-9a-f]+)(?:[^0-9])*([0-9]+).*(left|right|inside).*global variable '([^']+)'.*'([^':]+):([0-9]+):([0-9]+)' \\((0x[0-9a-f]+)\\).*size ([0-9]+)$"))
  (define frame-info-pattern
    (byte-regexp #"frame(?:[^0-9])*([0-9]+) object"))
  (define frame-var-pattern
    (byte-regexp #"\\[([0-9]+), ([0-9]+)\\) '([^']+)'"))
  
  ;; Trace an individual line of input
  (define (stack-trace-line line)
    (cond
      [(regexp-match #rx#"^{" line)
       ;; Parse the pretend-JSON

       ;; Convert single quotes to double quotes, efficiently
       ;; This is more efficient than convering to a string 
       ;; and using string-replace
       (define json (list->bytes
                     (map
                      (lambda (b) (if (= b 39) 34 b))
                      (bytes->list line))))
       (define frame (bytes->jsexpr json))
       (define short-module
         (and (hash-has-key? frame 'module)
              (drop-right (string-split (hash-ref frame 'module) "/") 1)))
       (define short-file
         (and (hash-has-key? frame 'file)
              (drop-right (string-split (hash-ref frame 'file) "/") 1)))
       (define function
         (hash-ref frame 'function #f))
       (define ignore? (member function ignored-functions))
       (cond
         [(and (not (equal? (hash-ref frame 'file 'nope) "<null>"))
               (not (equal? function "<null>"))
               (and (not (equal? function "main")) (not ignore?)))
          (format "frame ~a: ~a, ~a:~a:~a\n"
                  (hash-ref frame 'frame)
                  function
                  short-file
                  (hash-ref frame 'line)
                  (hash-ref frame 'column))]
         [(and (not (equal? function "<null>"))
               (not (equal? function "main"))
               (not ignore?))
          (format "frame ~a: ~a, from module ~a (+~a)\n"
                  (hash-ref frame 'frame)
                  function
                  short-module
                  (hash-ref frame 'offset))]
         [else
           (format "  frame ~a: module ~a (+~a)\n"
                   (hash-ref frame 'frame)
                   short-module
                   (hash-ref frame 'offset))])]

      ;; Parse other misc. output`
      [(regexp-match? #rx#"^(Direct|Indirect)" line)
       (format "\n~a byte(s) allocated, never freed.\n"
               (first (regexp-match #rx#"[0-9]+" line)))]
      [(or (regexp-match? #rx#"^WRITE" line) (regexp-match? #rx#"^READ" line))
       (define size (second (regexp-match #rx#"size ([0-9]+)" line)))
       (define address (first (regexp-match address-pattern line)))
       (format "  Error caused by ~a of size ~a  byte(s) to ~a:\n"
               (if (regexp-match? #rx#"^WRITE" line)
                   "write"
                   "read")
               size
               address)]
      [(regexp-match? supp-alloc-addr-pattern line)
       (apply (lambda (a b c d) (format "\n  ~a is ~a bytes ~a of ~a-byte region "))
              (rest (regexp-match supp-alloc-addr-pattern line)))]
      [(regexp-match? #rx#"^allocated by:\n" line)
       "allocated by:\n"]
      [(regexp-match? supp-stack-addr-pattern line)
       (apply (lambda (a b) (format "\n  ~a is contained ~a bytes into stack frame:\n" a b))
              (rest (regexp-match supp-stack-addr-pattern line)))]
      [(regexp-match? frame-info-pattern line)
       (define num-frame-objects (second (regexp-match frame-info-pattern line)))
       (format "\n  This frame has ~a object(s):\n" num-frame-objects)]
      [(regexp-match? frame-var-pattern line)
       (define p (open-output-string)) ; build up the string in a port
       (define frame-var-info (regexp-match frame-var-pattern line))
       (define object-size (- (bytes->number (third frame-var-info))
                              (bytes->number (second frame-var-info))))
       (fprintf p
                "  ~a byte object ~a located ~a bytes into frame."
                object-size
                (fourth frame-var-info)
                (second frame-var-info))
       (cond [(regexp-match? #rx#"overflow" line) (fprintf p " Access overflowed this variable.\n")]
             [(regexp-match? #rx#"underflow" line) (fprintf p"  Access underflowed this variable.\n")]
             [else (fprintf p "\n")])
       (get-output-string p)]
      [(regexp-match? #rx#"^previously allocated" line)
       "\n  Allocated by:\n"]
      [(regexp-match? #rx#"^freed by" line)
       "freed already by:\n"]
      [(regexp-match? supp-global-addr-pattern line)
       (define global-addr-info (regexp-match supp-global-addr-pattern line))
       (define short-addr-file-info (drop-right (string-split (sixth global-addr-info) "/") 1))
       (apply (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9) ; This way we don't have to repeatedly traverse the list
         (format "\n  ~a is ~a bytes ~a of ~a byte(s) global variable ~a (located ~a) defined at ~a:~a:~a.\n"
                 x1
                 x2
                 x3
                 x9
                 x4
                 x8
                 short-addr-file-info
                 x6
                 x7))
              (rest global-addr-info))]
      [else #f]
      ))

  (define (stack-trace contents)
    (filter (lambda (x) x) (map stack-trace-line contents)))

  
  (define to-check (second contents))
  ;; extract the address from the stack trace, if any
  (define address 
    (regexp-match address-pattern to-check))

  ;; determine if we have a buffer overflow somewhere
  (define buffer-overflow-match
    (regexp-match #rx#"(global|stack|heap)-buffer-(over|under)flow"
                  (second contents)))

  (cond
    [(and (regexp-match? #rx#" SEGV " to-check)
          (regexp-match? file-pattern (third contents)))
     (cons (format "Attempted to access invalid address ~a.\n"
                   address) 
           (stack-trace contents))]
    [(regexp-match? #rx#" SEGV " to-check)
     (list (format "~a\n" 
                   (second (regexp-match #rx#"^[^\\(]*" to-check))))]
    [(regexp-match? #rx#"stack-overflow " to-check)
     (cons (format "Stack overflow on address a. Check call stack.\n"
                   address)
           (stack-trace contents))]
    [buffer-overflow-match
     (cons (format "Buffer ~aflow on address ~a (~a). Check array indices.\n"
                   (third buffer-overflow-match)
                   address
                   (second buffer-overflow-match))
           (stack-trace contents))]
    [(regexp-match? #rx#"LeakSanitizer:" to-check)
     (cons "Memory leaks occurred:"
           (stack-trace contents))]
    [(regexp-match? #rx#"heap-use-after-free" to-check)
     (cons (format "Using address ~a after it has been freed.\n"
                   address)
           (stack-trace contents))]
    [(regexp-match? #rx#"double-free " to-check)
     (cons (format "Attempting to free address ~a, which has already been freed.\n"
                   address)
           (stack-trace contents))]
    [(regexp-match? #rx#"attempting free on" to-check)
     (cons (format "Attemtping to free address ~a, which has not been malloc'd.\n"
                   address)
           (stack-trace contents))]
    [else contents]))

