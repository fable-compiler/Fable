(function(babel, $, _, ace, window) {
  'use strict';

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

  var presets = [
    'es2015',
    'es2015-loose',
    'es2016',
    'es2017',
    'latest',
    'react',
    'stage-0',
    'stage-1',
    'stage-2',
    'stage-3'
  ];

  /* Throw meaningful errors for getters of commonjs. */
  var enableCommonJSError = true;
  ["module", "exports", "require"].forEach(function(commonVar){
    Object.defineProperty(window, commonVar, {
      configurable: true,
      get: function () {
        if (enableCommonJSError) {
          throw new Error(commonVar + " is not supported in the browser, you need a commonjs environment such as node.js/io.js, browserify/webpack etc");
        }
      }
    });
  });

  /*
   * Utils for working with the browser's URI (e.g. the query params)
   */
  function UriUtils () {}

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

  /*
   * Long term storage for persistence of state/etc
   */
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

  /*
   * Decorating the ACE editor
   */
  function Editor(selector) {
    this.$el = $(selector);
    this.editor = ace.edit(this.$el[0]);
    this.session = this.editor.getSession();
    this.document = this.session.getDocument();

    this.editor.setTheme('ace/theme/tomorrow');
    this.editor.setShowPrintMargin(false);
    this.editor.commands.removeCommands(['gotoline', 'find']);
    this.$el.css({
      fontFamily: '"Operator Mono", "Fira Code", "Ubuntu Mono", "Droid Sans Mono", "Liberation Mono", "Source Code Pro", Menlo, Monaco, Consolas, "Courier New", monospace',
      lineHeight: 'inherit'
    });

    this.session.setMode('ace/mode/javascript');
    this.session.setUseSoftTabs(true);
    this.session.setTabSize(2);
    this.session.setUseWorker(false);

    this.editor.setOption('scrollPastEnd', 0.33);
  }

  /*
   * Options exposed for the REPL that will influence Babel's transpiling
   */
  function $checkbox($element){
    return {
      get: function () {
        return $element.is(":checked");
      } ,
      set: function (value) {
        var setting = value !== 'false' && value !== false;
        $element.prop('checked', setting);
      },
      enumerable: true,
      configurable: false
    };
  }

  /**
   * By default, Bootstrap closes dropdown menus whenever an item in them is
   * clicked. This function overrides that behaviour for the presets dropdown,
   * and ensures the checkbox is selected correctly.
   */
  function handlePresetClick($input, evt) {
    evt.preventDefault();
    evt.stopPropagation();

    // Needs to run in a timeout to properly handle clicks directly on the
    // checkbox.
    setTimeout(function() {
      $input.checked = !$input.checked;
      onPresetChange();
    }, 0);
  }

  /**
   * Options for selecting presets to use.
   */
  function getPresetOptions() {
    // Create the checkboxes for all available presets
    var $presetContainer = document.getElementById('babel-repl-preset-dropdown');
    var $presets = [];
    presets.forEach(function(presetName) {
      var $input = document.createElement('input');
      $input.type = 'checkbox';
      $input.name = 'preset';
      $input.value = presetName;
      $input.id = 'option-' + presetName;

      // This should really be a <label>, but it *needs* to be an <a> to get the
      // right styling. Thanks Bootstrap.
      var $label = document.createElement('a');
      $label.href = '#';
      $label.className = 'small';
      $label.tabIndex = -1;
      $label.addEventListener(
        'click',
        handlePresetClick.bind(null, $input),
        false
      );

      $label.appendChild($input);
      $label.appendChild(document.createTextNode(' ' + presetName));

      var $li = document.createElement('li');
      $li.appendChild($label);
      $presetContainer.appendChild($li);
      $presets.push($input);
    });

    return {
      get: function() {
        return $presets
          .filter(function($preset) { return $preset.checked; })
          .map(function($preset) { return $preset.value; })
          .join(',');
      },
      set: function(value) {
        value = value.split(',');
        $presets.forEach(function($preset) {
          $preset.checked = value.indexOf($preset.value) > -1;
        });
      },
      enumerable: true,
      configurable: true,
    };
  }

  var isBabiliLoading = false;
  /**
   * Checks if Babili has been loaded. If not, kicks off a load (if it hasn't
   * already started) and returns false. Returns true if Babili is ready to use.
   */
  function hasBabiliLoaded() {
    if (window.Babili) {
      return true;
    }
    if (isBabiliLoading) {
      return false;
    }
    // Babili-standalone is exported as a UMD script, and thus hits the CommonJS
    // error ("is not supported in the browser..."), temporarily disable it
    // while loading.
    enableCommonJSError = false;

    var script = document.createElement('script');
    script.async = true;
    script.src = 'https://unpkg.com/babili-standalone@0/babili.min.js';
    script.onload = function() {
      enableCommonJSError = true;
      //onSourceChange();
    };
    document.head.appendChild(script);
    isBabiliLoading = true;
    return false;
  }

  /*
   * Babel options for transpilation as used by the REPL
   */
  function Options () {
    var $astInput = $('#option-astInput');
    var $evaluate = $('#option-evaluate');
    var $lineWrap = $('#option-lineWrap');
    var $babili = $('#option-babili');

    var options = {};
    Object.defineProperties(options, {
      astInput: $checkbox($astInput),
      babili: $checkbox($babili),
      evaluate: $checkbox($evaluate),
      lineWrap: $checkbox($lineWrap),
      presets: getPresetOptions(),
    });

    // Merge in defaults
    var defaults = {
      astInput: false,
      babili: false,
      evaluate: false,
      lineWrap: false,
      presets: 'es2015,stage-2,react'
    };

    _.assign(options, defaults);

    return options;
  }

  /*
   * Babel Web REPL
   */
  function REPL () {
    this.storage = new StorageService();
    var state = this.storage.get('replState') || { code: 'printfn "Hello World!"' };
    _.assign(state, UriUtils.parseQuery());

    this.options = _.assign(new Options(), state);

    this.input = new Editor('.babel-repl-input .ace_editor').editor;
    this.input.setValue(UriUtils.decode(state.code || ''));

    this.output = new Editor('.babel-repl-output .ace_editor').editor;
    this.output.setReadOnly(true);
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

  REPL.prototype.setOutput = function (output) {
    this.output.setValue(output, -1);
  };

  REPL.prototype.printError = function (message) {
    this.$errorReporter.text(message);
  };

  REPL.prototype.getSource = function () {
    return this.input.getValue();
  };

  REPL.prototype.transformFromAst = function (ast) {
    var transformed;
    try {
      // if (this.options.babili && !hasBabiliLoaded()) {
      //   this.setOutput('// Babili is loading, please wait...');
      //   return;
      // }

      // var presets = this.options.presets.split(',');
      // if (this.options.babili) {
      //   presets.push('babili');
      // }

      if (ast.error) {
        throw ast.error;
      }

      var options = {
        // presets: presets.filter(Boolean),
        plugins: [
          transformMacroExpressions,
          removeUnneededNulls,
        ],
        filename: 'repl',
        babelrc: false,
      };
      transformed = babel.transformFromAst(ast, null, options);
    } catch (err) {
      //this.printError(err.message);
      this.setOutput(err.message + "\n" + err.stack);
      throw err;
    }

    this.setOutput(transformed.code);

    if (this.options.evaluate) {
      this.evaluate(transformed.code);
    }
  }

  REPL.prototype.compile = function () {
    this.output.session.setUseWrapMode(this.options.lineWrap);

    var transformed;
    this.clearOutput();

    var source = this.getSource();
    if (this.options.astInput) {
      this.transformFromAst(JSON.parse(source));
    }
    else {
      this.setOutput("Compiling, please wait...");
      myWorker.postMessage(source);
    }
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

      var logs = _.map(arguments, function(log) {
        return window.prettyFormat(log);
      });

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

  function onPresetChange() {
    // Update the list of presets that are displayed on the dropdown list anchor
    var presetList = repl.options.presets.replace(/,/g, ', ');
    document.getElementById('babel-repl-selected-presets').innerHTML = presetList;

    //onSourceChange();
  }

  function onSourceChange () {
    repl.compile();
    var code = repl.getSource();
    var state = _.assign(repl.options, {
      code: code
    });
    repl.persistState(state);
  }

  function onOutputClear() {
    repl.setOutput("");
  }

  /*
   * Initialize the REPL
   */
  var repl = new REPL();
  var myWorker = new Worker("worker.js");
  myWorker.onmessage = function (e) {
    repl.transformFromAst(JSON.parse(e.data));
  };

  // Trigger first compilation
  onSourceChange();

  //repl.input.on('change', _.debounce(onSourceChange, 500));
  //repl.$toolBar.on('change', onSourceChange);

  /*
   * Make REPL editors resizable by width
   * Returns a function to disable feature
   */
  function initResizable(resizeSelector) {
    var $container = $('.babel-repl');
    var $leftPanel = $('.babel-repl-left-panel');
    var $rightPanel = $('.babel-repl-right-panel');
    var activeClass = 'babel-repl-resize-active';
    var offsetX;

    function onResize(e) {
      var curPos = e.pageX - offsetX;
      var leftWidth = curPos / $container.width() * 100;
      var rightWidth = 100 - leftWidth;
      if (leftWidth < 10 || leftWidth > 90) {
        return;
      }

      $leftPanel.outerWidth(leftWidth + '%');
      $rightPanel.outerWidth(rightWidth + '%');
    }

    function onResizeStart(e) {
      e.preventDefault();
      offsetX = e.offsetX;
      $(document).on('mousemove', onResize);
      $(document).on('mouseup', onResizeStop);
      $container.addClass(activeClass);
    }

    function onResizeStop(e) {
      $(document).off('mousemove', onResize);
      $(document).off('mouseup', onResizeStop);
      $container.removeClass(activeClass);
    }

    $(resizeSelector).on('mousedown', onResizeStart);

    return function() {
      $(resizeSelector).off('mousedown', onResizeStart);
    };
  }

  $('#fabel-compile').on('click', onSourceChange);
  $('#fabel-clear').on('click', onOutputClear);

  //initResizable('.babel-repl-resize');
  //onPresetChange();
}(Babel, $, _, ace, window));
