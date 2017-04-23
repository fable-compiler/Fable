(function(babel, $, ace, window) {
  'use strict';

  var fableWorker = new Worker("worker.js");

  function downloadFile(url, f) {
      var xhr = new XMLHttpRequest();
      xhr.open("GET", url);
      xhr.onload = function (oEvent) {
        f(xhr.responseText);
      };
      xhr.onerror = function (oEvent) {
        f('Error loading ' + url);
      };
      xhr.send();
  };

  var babelPlugins = (function () {
    var template = babel.template;
    /**
     * Removes unnecessary null statements (e.g. at the end of constructors)
     */
    var removeUnneededNulls = {
        visitor: {
            // Remove `null;` statements (e.g. at the end of constructors)
            ExpressionStatement: function (path) {
                if (path.node.expression.type === "NullLiteral") {
                    path.remove();
                }
            }
        }
    };
    /**
     * Custom plugin to simulate macro expressions.
     */
    var transformMacroExpressions = {
        visitor: {
            StringLiteral: function (path) {
                var node = path.node;
                if (!node.macro || !node.value) {
                    return;
                }
                var buildArgs = {}, macro = node.value;
                try {
                    var args = node.args;
                    for (var i = 0; i < args.length; i++) {
                        buildArgs["$" + i] = args[i];
                    }
                    macro = macro
                        .replace(/\$(\d+)\.\.\./, function (m, i) {
                        var rep = [], j = parseInt(i);
                        for (; j < args.length; j++) {
                            rep.push("$" + j);
                        }
                        return rep.join(",");
                    })
                        .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                        var i = parseInt(g1);
                        return i < args.length && args[i].value ? g2 : g3;
                    })
                        .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function (_, g1, g2) {
                        var i = parseInt(g2);
                        return i < args.length ? g1 : "";
                    });
                    var buildMacro = template(macro);
                    path.replaceWithMultiple(buildMacro(buildArgs));
                }
                catch (err) {
                  err.message =
                    "BABEL ERROR: Failed to parse macro: " + macro + "\n" +
                    "MACRO ARGUMENTS: " + Object.getOwnPropertyNames(buildArgs).join() + "\n" +
                    err.message;
                  throw err;
                }
            }
        }
    };

    return {
      removeUnneededNulls: removeUnneededNulls,
      transformMacroExpressions: transformMacroExpressions
    }
  })();

  // Utils for working with the browser's URI (e.g. the query params)
  var UriUtils = (function() {
    var UriUtils = {};

    UriUtils.encode = function (value) {
      return window.encodeURIComponent(value);
    };

    UriUtils.decode = function (value) {
      try {
        return window.decodeURIComponent('' + value);
      } catch (err) {
        return value;
      }
    };

    UriUtils.parseQuery = function () {
      var query = window.location.hash.replace(/^\#\?/, '');

      if (!query) {
        return null;
      }

      return query.split('&').map(function(param) {
        var splitPoint = param.indexOf('=');

        return {
          key : param.substring(0, splitPoint),
          value : param.substring(splitPoint + 1)
        };
      }).reduce(function(params, param){
        if (param.key && param.value) {
          params[param.key] = UriUtils.decode(param.value);
        }
        return params;
      }, {});
    };

    UriUtils.updateQuery = function (object) {
      var query = Object.keys(object).map(function(key){
        return key + '=' + UriUtils.encode(object[key]);
      }).join('&');

      window.location.hash = '?' + query;
    };

    return UriUtils;
  })();

  // Long term storage for persistence of state/etc
  var StorageService = (function() {
    function StorageService () {
      this.store = window.localStorage;
    }

    StorageService.prototype.get = function (key) {
      try {
        return JSON.parse(this.store.getItem(key));
      } catch(e) {}
    };

    StorageService.prototype.set = function (key, value) {
      try {
        this.store.setItem(key, JSON.stringify(value));
      } catch(e) {}
    };

    return StorageService;
  })();

  // Decorating the ACE editor
  function Editor(selector) {
    this.$el = $(selector);
    this.editor = ace.edit(this.$el[0]);
    this.session = this.editor.getSession();
    this.document = this.session.getDocument();

    this.editor.setTheme('ace/theme/tomorrow_night_bright');
    this.editor.setShowPrintMargin(false);
    this.editor.commands.removeCommands(['gotoline', 'find']);
    this.$el.css({
      fontFamily: '"Operator Mono", "Fira Code", "Ubuntu Mono", "Droid Sans Mono", "Liberation Mono", "Source Code Pro", Menlo, Monaco, Consolas, "Courier New", monospace',
      lineHeight: 'inherit'
    });

    if (selector.indexOf('-repl-input') >= 0) {
      this.session.setMode('ace/mode/ocaml'); //todo: mode/fsharp
    } else {
      this.session.setMode('ace/mode/javascript');
    }
    this.session.setUseSoftTabs(true);
    this.session.setTabSize(2);
    this.session.setUseWorker(false);

    this.editor.setOption('scrollPastEnd', 0.33);
  }

  var REPL = (function () {
    function REPL () {
      this.storage = new StorageService();
      var state = this.storage.get('replState') || { code: 'printfn "Hello World!"' };
      Object.assign(state, UriUtils.parseQuery());

      this.options = Object.assign({}, state);

      this.input = new Editor('.babel-repl-input .ace_editor').editor;
      this.input.setValue(UriUtils.decode(state.code || ''));

      this.output = new Editor('.babel-repl-output .ace_editor').editor;
      // this.output.setReadOnly(true);
      this.output.setHighlightActiveLine(false);
      this.output.setHighlightGutterLine(false);

      this.$errorReporter = $('.babel-repl-errors');
      this.$consoleReporter = $('.babel-repl-console');
      this.$toolBar = $('.babel-repl-toolbar');

      document.getElementById('babel-repl-version').innerHTML = babel.version;
    }

    REPL.prototype.clearOutput = function () {
      this.$errorReporter.text('');
      this.$consoleReporter.text('');
    };

    REPL.prototype.runOutput = function () {
      // Use AMD modules for evaluation
      var code = this.output.getValue();
      var transformed = babel.transform(code, { plugins: [ "transform-es2015-modules-amd" ]});

      // Make some changes to make the code immediately runnable
      this.evaluate(
        transformed.code
          .replace("define", "require")
          .replace('"use strict";', '"use strict"; try { exports = exports || {}; } catch (err) {}')
      );
    };

    REPL.prototype.setOutput = function (output) {
      this.output.setValue(output, -1);
    };

    REPL.prototype.printError = function (message) {
      this.$errorReporter.text(message);
    };

    REPL.prototype.getSource = function () {
      return this.input.getValue();
    };

    REPL.prototype.setSource = function (source) {
      this.input.setValue(source);
      this.onSourceChange();
    };

    REPL.prototype.onSourceChange = function () {
      this.compile();
      var code = this.getSource();
      var state = Object.assign(this.options, {
        code: code
      });
      this.persistState(state);
    }

    REPL.prototype.transformFromAst = function (data) {
      try {
        if (data.error) {
          throw data.error;
        }

        var options = {
          plugins: [
            babelPlugins.transformMacroExpressions,
            babelPlugins.removeUnneededNulls,
          ],
          filename: 'repl',
          babelrc: false,
        };

        if (!document.getElementById('option-es2015').checked) {
          options.presets = [
            ["es2015", {"modules": false }]
          ];
        }

        var startTime = performance.now();
        var ast = JSON.parse(data.jsonAst);
        var transformed = babel.transformFromAst(ast, null, options);
        var elapsed = performance.now() - startTime;

        var code = transformed.code;
        this.setOutput(code);
        this.runOutput();

        // print elapsed time
        var elapsedMessage = "Compile times - " +
            "FCS: " + Math.round(data.fsharpTime) + " ms, " +
            "Fable: " + Math.round(data.fableTime) + " ms, " +
            "Babel: " + Math.round(elapsed) + " ms";
        this.printError(elapsedMessage);
      } catch (err) {
        //this.printError(err.message);
        this.setOutput(err.message);
        console.error(err.message + "\n" + err.stack);
        // throw err;
      }
    }

    REPL.prototype.compile = function () {
      // this.output.session.setUseWrapMode(this.options.lineWrap);

      var transformed;
      this.clearOutput();

      var source = this.getSource();
      this.setOutput("Compiling, please wait...");
      fableWorker.postMessage(source);
    };

    var capturingConsole;
    REPL.prototype.evaluate = function(code) {
      capturingConsole = Object.create(console);
      var $consoleReporter = this.$consoleReporter;
      var buffer = [];
      var error;
      var done = false;

      function flush() {
        $consoleReporter.text(buffer.join('\n'));
      }

      function write(data) {
        buffer.push(data);
        if (done) flush();
      }

      function capture() {
        if (this !== capturingConsole) { return; }

        var logs = []
        for (var i=0; i<arguments.length; i++) {
          logs.push(window.prettyFormat(arguments[i]));
        }

        write(logs.join(' '));
      }

      capturingConsole.clear = function() {
        buffer = [];
        flush();
        console.clear();
      };

      ['error', 'log', 'info', 'debug'].forEach(function(key) {
        capturingConsole[key] = function() {
          Function.prototype.apply.call(console[key], console, arguments);
          capture.apply(this, arguments);
        };
      });

      try {
        new Function('console', code)(capturingConsole);
      } catch (err) {
        error = err;
        buffer.push(err.message);
      }

      done = true;
      flush();
    };

    REPL.prototype.persistState = function (state) {
      UriUtils.updateQuery(state);
      this.storage.set('replState', state);
    };

    return REPL;
  })();

  (function Initialize() {

    var repl = new REPL();

    fableWorker.onmessage = function (e) {
      repl.transformFromAst(e.data);
    };

    function initSamples(repl) {
      // Create the checkboxes for all available samples
      var $sampleContainer = document.getElementById('sample-dropdown');
      Object.keys(SAMPLES).forEach(function(sampleName) {
        var $label = document.createElement('a');
        $label.href = '#';
        $label.className = 'small';
        $label.tabIndex = -1;
        $label.addEventListener(
          'click',
          () => downloadFile("/repl_samples/" + sampleName + ".fs", repl.setSource.bind(repl)),
          false
        );

        $label.appendChild(document.createTextNode(SAMPLES[sampleName]));

        var $li = document.createElement('li');
        $li.appendChild($label);
        $sampleContainer.appendChild($li);
      });
    }

    // Init samples and trigger first compilation
    initSamples(repl);
    repl.onSourceChange();

    $('#fabel-compile').on('click', () => repl.onSourceChange());
    $('#fabel-run').on('click', () => repl.runOutput());
  })();

  // /*
  //  * Make REPL editors resizable by width
  //  * Returns a function to disable feature
  //  */
  // function initResizable(resizeSelector) {
  //   var $container = $('.babel-repl');
  //   var $leftPanel = $('.babel-repl-left-panel');
  //   var $rightPanel = $('.babel-repl-right-panel');
  //   var activeClass = 'babel-repl-resize-active';
  //   var offsetX;

  //   function onResize(e) {
  //     var curPos = e.pageX - offsetX;
  //     var leftWidth = curPos / $container.width() * 100;
  //     var rightWidth = 100 - leftWidth;
  //     if (leftWidth < 10 || leftWidth > 90) {
  //       return;
  //     }

  //     $leftPanel.outerWidth(leftWidth + '%');
  //     $rightPanel.outerWidth(rightWidth + '%');
  //   }

  //   function onResizeStart(e) {
  //     e.preventDefault();
  //     offsetX = e.offsetX;
  //     $(document).on('mousemove', onResize);
  //     $(document).on('mouseup', onResizeStop);
  //     $container.addClass(activeClass);
  //   }

  //   function onResizeStop(e) {
  //     $(document).off('mousemove', onResize);
  //     $(document).off('mouseup', onResizeStop);
  //     $container.removeClass(activeClass);
  //   }

  //   $(resizeSelector).on('mousedown', onResizeStart);

  //   return function() {
  //     $(resizeSelector).off('mousedown', onResizeStart);
  //   };
  // }

  // initResizable('.babel-repl-resize');

}(Babel, $, ace, window));
