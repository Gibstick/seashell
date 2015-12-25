#lang racket

(require rackunit
         seashell/backend/project
         seashell/backend/runner
         seashell/backend/files
         seashell/seashell-config)

(define/provide-test-suite compiler-suite
  (test-suite "Compiler Tests"

    (test-case "ok-print-Hello."
      (new-project "foo1")
      (with-output-to-file (check-and-build-path (build-project-path "foo1") "test.c")
        (thunk (display "#include <stdio.h>\nint main() {\nprintf(\"Hello.\");\n}\n")))
      (define-values (success hsh) (compile-and-run-project "foo1" "test.c" '() #f))
      (check-true success)
      (define pid (hash-ref hsh 'pid))
      (sync (program-wait-evt pid))
      (check string=? (port->string (program-stdout pid)) "Hello.")
      (delete-project "foo1"))

    (test-case "fail-invalid-C"
      (new-project "foo2")
      (with-output-to-file (check-and-build-path (build-project-path "foo2") "error.c")
        (thunk (display "great code;")))
      (define-values (res hsh) (compile-and-run-project "foo2" "error.c" '() #f))
      (check-false res)
      (check string=? (hash-ref hsh 'status) "compile-failed")
      (delete-project "foo2"))

    (test-case "bad-werror-return-type-no-return"
      (new-project "foo4")
      (new-file "foo4" "1.c" (string->bytes/utf-8
#<<EOF
  int a() {
  }
  int main() {
  }
EOF
      ) 'raw #f)
      (define-values (res hsh) (compile-and-run-project "foo4" "1.c" '() #f))
      (check-false res)
      (delete-project "foo4"))
    
    (test-case "bad-werror-return-type-return-void"
      (new-project "foo5")
      (new-file "foo5" "1.c" (string->bytes/utf-8
#<<EOF
  void a() {
    return 5;
  }
  int main() {
  }
EOF
      ) 'raw #f)
      (define-values (res hsh) (compile-and-run-project "foo5" "1.c" '() #f))
      (check-false res)
      (delete-project "foo5"))
    
    (test-case "bad-werror-int-conversion"
      (new-project "foo6")
      (new-file "foo6" "1.c" (string->bytes/utf-8
#<<EOF
  int main() {
    int a = &a;
  }
EOF
      ) 'raw #f)
      (define-values (res hsh) (compile-and-run-project "foo6" "1.c" '() #f))
      (check-false res)
      (delete-project "foo6"))
    
    
    (test-case "bad-werror-int-to-pointer-cast"
      (new-project "foo7")
      (new-file "foo7" "1.c" (string->bytes/utf-8
#<<EOF
int x(long *f) {
}

int main() {
  int i = 0;
    x((void*) i);
    }
EOF
      ) 'raw #f)
      (define-values (res hsh) (compile-and-run-project "foo7" "1.c" '() #f))
      (check-false res)
      (delete-project "foo7"))

    ))
