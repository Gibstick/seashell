var editor;
var ss_console;
var currentFile;
var fileList = []; // array of ssFiles open in the current session
var numberOfFiles = 0;
var compiled = false;

// creates a new ssFile that fileList will be aware of
function ssFile(name, content) {
    this.name = name;
    this.index = numberOfFiles;
    this.content = content;
    this.tab = $('<li class="filename">' + name + '</li>');
    this.history = null;
    this.lastSaved = 'never';

    fileList[numberOfFiles] = this;
    numberOfFiles++;
}

// I'm not sure what this should look like. It should certainly be asynchronous.
// Maybe it should console_write(str) as it gets lines?
// ensure currentFile is synced before calling this.
function runProgram() {
  ss.runFile(
    function(res) {
      if(res) {
        window.ss_pipe_k = function() {
          ss.getProgramOutput(function(bytes) {
            if(bytes) {
              console_write_noeol(bytes);
              window.ss_pipe_k();
            } else {
              /* do not poll too quickly. */
              window.setTimeout(window.ss_pipe_k, 500);
            }
          });
        }
        window.ss_term_k = function() {
          ss.waitProgram(function(res) {
            if(res) {
              /* Program terminated. */
              window.ss_pipe_k = function(){};
            } else {
              window.ss_term_k();
            }
          });
        }
        window.ss_term_k();
        window.ss_pipe_k();
    }}, currentFile.content);
}

// eventually: parse clang output. Codemirror will let you jump around to arbitrary lines/positions
// and hilight bits of code. Should also probably be asynchronous.
function compileProgram() {
}
    
function mark_changed(instance, chobj) {
    compiled = false;
    $(".status_active").addClass("status_edited");
}

function mark_unchanged() {
    $(".status_active").removeClass("status_edited");
}

function console_write(str) {
    ss_console.setOption('readOnly', false);
    var newText = ss_console.getValue() + str + '\n';
    ss_console.setValue(newText);
    ss_console.setOption('readOnly', true);
}

function console_write_noeol(str) {
    ss_console.setOption('readOnly', false);
    var newText = ss_console.getValue() + str;
    ss_console.setValue(newText);
    ss_console.setOption('readOnly', true);
}

function makePrompt(str) {
    return str + ': <input type="text" style="width: 3em"/>';
}

function makeFilePrompt(str) {
    return str + ': <input type="text" style="width: 12em"/>';
}

/** as a general rule, the *Handler functions try to touch only 
 * currentFile and the UI. **/

/** handlers for buttons that only affect the client-side **/
function toggleCommentHandler(isC) {
    var from = editor.getCursor(true);
    var to = editor.getCursor(false);
    editor.commentRange(isC, from, to);
}
function autoIndentHandler() {
    var from = editor.getCursor(true);
    var to = editor.getCursor(false);
    editor.autoFormatRange(from, to);
    editor.autoIndentRange(from, to);
}

function gotoHandler() {
    editor.openDialog(makePrompt('Line'), function(query) {
            editor.setCursor(query-1, 0); });
}

/** handlers for buttons that need to interact with the back-end **/

function saveFile() {
    // editor.getValue() is a \n-delimited string containing the text currently in the editor
    currentFile.content = editor.getValue();
    currentFile.history = editor.getHistory();

    ss.saveFile(
      function(res) {
        if(!res) {
          alert("Could not save file. Please try again.");
        } else {
            mark_unchanged();
            currentFile.lastSaved = (new Date()).toLocaleTimeString();
            $('#time-saved').text(currentFile.lastSaved);
            //console_write('Your file has been saved as ' + currentFile.name + '.');
        }
      },
      currentFile.name,
      currentFile.content);
}

function saveHandler() {
    editor.openDialog(makeFilePrompt('Save as'), 
                        function(query) {
                            // TODO problem with nullstring checking...
                            if (query) {
                                currentFile.name = query;
                                currentFile.tab.text(query);
                            }
                            saveFile();
                        });
}

// applies k to the contents of currentFileName as a \n-delimited string 
function getFile(k) {
    ss.loadFile(k, currentFileName);
}

function openFileHandler() {
    editor.openDialog(makeFilePrompt('File name'), 
                        function(query) {
                            // skip if no filename is specified. TODO figure out how to handle nullstrings
                            if (!query) {
                                return;
                            }
                            // if file is already open, don't open it twice
                            for (var i=0; i<numberOfFiles; i++) {
                                if (fileList[i].name == query) {
                                    setTab(fileList[i]);
                                    return;
                                }
                            }

                            getFile(function(val) {
                              if(val) {
                                var file = new ssFile(name, val);
                                makeNewTab(file);
                                setTab(file);
                                console_write('Opened file ' + query + '.');
                              } else {
                                console_write('Failed to open the file ' + query + '.');
                              }});
                        });
}

function setTab(file) {
    // save previously open tab before opening new one
    saveFile(); 
    editor.clearHistory();
    
    // set active tab
    $(".status_active").removeClass("status_active");
    file.tab.addClass("status_active");
    editor.setValue(file.content);
    if (file.history != null) {
        editor.setHistory(file.history);
    }
    currentFile = file;
    $('#time-saved').text(currentFile.lastSaved);
    mark_unchanged();
}
function newFileHandler() {
    editor.openDialog(makeFilePrompt('Name of new file'), 
                        function(query) {
                            // skip if no filename is specified. TODO figure out how to handle nullstrings
                            if (!query) return;
// TODO
//                          if (successful) {
                                console_write('Creating file ' + query + '.');
                                var file = new ssFile(query, "");
                                makeNewTab(file);
                                setTab(file);
//                          else {
//                              console_write('Failed to create the file ' + query + '.');
//                          }
                        });
}

function submitHandler() {
    editor.openDialog(makePrompt('Assignment ID'),
                        function(query) {
                            // TODO
                            console_write('Submitted file ' + currentFileName + '.');
                        });
}

function compileHandler() {
    saveFile();
    /*if (!compiled) {
        // TODO compile file
        compiled = true;
        console_write('Done compiling.');
    } else {
        console_write('Already compiled.');
    }*/
}

function runHandler() {
    saveFile();
    runProgram();
}

function runInputHandler() {
    editor.openDialog(makeFilePrompt('Name of input file'), 
                        function(query) {
                            // TODO run
                        });
}

/** initialize api. **/
seashell_new(
  function(ss) {
    window.ss = ss;
    ss.authenticate(
      function(res) {
        if(!res) {
          alert("Couldn't authenticate as ctdalek!");
        }
      },
      "ctdalek", "exterminate");
  },
  function(err) {
    alert("Error initializing API: " + err);
  });

// reads off the form in div#config.
function configureEditor() {
    var editor_mode = $('#editor_mode input').filter(':checked').val();
    console_write("Setting editor mode to " + editor_mode);
    editor.setOption('keyMap', editor_mode);

    var tab_width = $('#tab-width option').filter(':selected').val();
    console_write("Tab-width changed to " + tab_width);
    editor.setOption('tabSize', tab_width);
    editor.setOption('indentUnit', tab_width);

    var use_tabs = $('#use-spaces').is(':checked');
    editor.setOption('indentWithTabs', use_tabs);
}

function printConfig() {
    console_write("Using editor " + editor.getOption('keyMap'));
    console_write("Using tab-width " + editor.getOption('tabSize'));
    if (editor.getOption('indentWithTabs')) {
        console_write("Indenting with tabs");
    } else {
        console_write("Indenting with spaces");
    }
}

function hoboFile(name) {
    var exampleCode = ['#include <stdio.h>',
    'int main() {',
    '    int i;',
    '    for (i = 0; i <3; i++) {',
    '        printf("She sells C shells by the sea shore");',
    '    }',
    '    return(0);',
    '}'].join('\n');
    return new ssFile(name, exampleCode);
}

function setUpUI() {
    /** create editor and console **/

    //$("#seashell").text(exampleCode);
    //makeNewTab(currentFileName);
    currentFile = hoboFile("foobar.c");

    editor = CodeMirror.fromTextArea($("#seashell")[0], 
                {//value: currentFile.content,
                lineNumbers: true,
                tabSize: 2});
    makeNewTab(currentFile);
    editor.setValue(currentFile.content); // hack
    mark_unchanged();

    var welcomeMessage = 'Welcome to Seashell! Messages and program output will appear here.\n';
    ss_console = CodeMirror($('#console')[0],
                                   {value: welcomeMessage, 
                                   readOnly: true, 
                                   theme: 'dark-on-light'});
    editor.on("change", mark_changed);

    /** attach actions to all the buttons. **/

    $("#undo").click(function() {editor.undo();});
    $("#redo").click(function() {editor.redo();});
            
    $("#comment").click(function() {toggleCommentHandler(true);});
    $("#uncomment").click(function() {toggleCommentHandler(false);});
    $("#autoindent").click(autoIndentHandler);
    $("#goto-line").click(gotoHandler);
    $("#submit-assignment").click(submitHandler);

    $("#clear-console").click(function() {ss_console.setValue('')});
    $("#compile").click(compileHandler);
    $("#run").click(runHandler);
    $("#run-input").click(runInputHandler);
    $("#save-file").click(saveHandler);
    $("#open-file").click(openFileHandler);
    $("#new-file").click(newFileHandler);

    $("#overlay").hide();
    $("#config").hide();
    $("#settings").click(showConfig);
    $("#config form").change(configureEditor);
    editor.focus();
}

function showConfig() {
    $("#overlay").show();
    $("#config").show();
    $("#overlay").click(hideConfig);
}

function hideConfig() {
    $("#overlay").hide()
    $("#config").hide();
    editor.focus();
}


function makeNewTab(file) {
    if (numberOfFiles == 1) {
        file.tab.addClass("status_active");
    }

    $("#filelist").append(file.tab);
    file.tab.click(function() { setTab(file); });
}

setUpUI();
makeNewTab(hoboFile("moocows.c"));
makeNewTab(hoboFile("bob.C"));