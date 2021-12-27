function TutorialCompleter(tutorial) {
  this.$tutorial = tutorial;
  var self = this;

  this.$onChange = function(data) {
    clearTimeout(this.$autocompletionTimerId);
    data = data || {};

    // only perform live autocompletion while user
    // is typing (ie, single-character text insertions)
    if (data.action !== "insert")
      return;

    var lines = data.lines || [];
    if (lines.length !== 1)
      return;
    
    // NOTE: Ace has already updated the document line at this point
    // so we can just look at the state of that line
    var pos = this.getCursorPosition();
    var line = this.session.getLine(pos.row);

    // NOTE: we allow new autocompletion sessions following a
    // ':' insertion just to enable cases where the user is
    // typing e.g. 'stats::rnorm()' while the popup is visible
    var popup = (this.completer || {}).popup;
    if (popup && popup.isOpen && !/::$/.test(line))
      return;

    // figure out appropriate delay -- want to autocomplete
    // immediately after a '$' or '@' insertion, but need to
    // execute on timeout to allow Ace to finish processing
    // events (if any)
    var delayMs = 300;
    if (/[$@]$|::$/.test(line))
      delayMs = 10;

    this.$autocompletionTimerId = setTimeout(this.$liveAutocompleter, delayMs);
  };
 
  var MODIFIER_NONE  = 0;
  var MODIFIER_CTRL  = 1;
  var MODIFIER_ALT   = 2;
  var MODIFIER_SHIFT = 4;

  var KEYCODE_TAB   =  9;
  var KEYCODE_SPACE = 32;

  var KeyCombination = function(event) {
    this.keyCode = event.keyCode || event.which;
    this.modifier = MODIFIER_NONE;
    this.modifier |= event.ctrlKey  ? MODIFIER_CTRL  : 0;
    this.modifier |= event.altKey   ? MODIFIER_ALT   : 0;
    this.modifier |= event.shiftKey ? MODIFIER_SHIFT : 0;
  };

  function initializeAceEventListeners(editor) {

    // NOTE: each Ace instance gets its own handlers, so
    // we don't store these as instance variables on the
    // TutorialCompleter instance (as we have 1 TutorialCompleter
    // handling completions for all Ace instances)
    var handlers = {};

    handlers["change"]  = self.$onChange.bind(editor);
    handlers["destroy"] = function(event) {
      for (var key in handlers)
        this.off(key, handlers[key]);
    }.bind(editor);

    // register our handlers on the Ace editor
    for (var key in handlers)
      editor.on(key, handlers[key]);
  }

  function initializeCompletionEngine(editor) {
    
    editor.completers = editor.completers || [];
    editor.completers.push({

      getCompletions: function(editor, session, position, prefix, callback) {
        
        // send autocompletion request with document contents up to cursor
        // position (done to enable multi-line autocompletions)
        var contents = session.getTextRange({
          start: {row: 0, column: 0},
          end: position
        });

        var payload = {
          contents: contents,
          label: editor.tutorial.label
        };

        self.$tutorial.$serverRequest("completion", payload, function(data) {
          
          data = data || [];

          // define a custom completer -- used for e.g. automatic
          // parenthesis insertion, and so on
          var completer = {
            insertMatch: function(editor, data) {

              // remove prefix
              var ranges = editor.selection.getAllRanges();
              var completions = editor.completer.completions;
              var n = completions.filterText.length;
              for (var i = 0; i < ranges.length; i++) {
                ranges[i].start.column -= n;
                editor.session.remove(ranges[i]);
              }
              
              // insert completion term (add parentheses for functions)
              var term = data.value + (data.is_function ? "()" : "");
              editor.execCommand("insertstring", term);

              // move cursor backwards for functions
              if (data.is_function)
                editor.navigateLeft(1);
            }
          };

          var completions = data.map(function(el) {
            return {
              caption: el[0] + (el[1] ? "()" : ""),
              value: el[0],
              score: 0,
              meta: "R",
              is_function: el[1],
              completer: completer
            };
          });

          callback(null, completions);
        });
      }

    });

    editor.setOptions({
      enableBasicAutocompletion: true,
      enableLiveAutocompletion: false
    });
  }

  function initializeSetupChunk(editor) {
    var data = editor.tutorial;
    self.$tutorial.$serverRequest("initialize_chunk", data);
  }

  var ensureInitialized = function(editor) {
    
    // bail if completions are disabled for this editor
    if (!editor.tutorial.completion)
      return;
    
    if (editor.$autocompletionInitialized)
      return;

    initializeAceEventListeners(editor);
    initializeCompletionEngine(editor);
    initializeSetupChunk(editor);
    
    // generate a live autocompleter for this editor if
    // not yet available
    if (typeof editor.$liveAutocompleter === "undefined") {
      editor.$liveAutocompleter = function() {
        this.execCommand("startAutocomplete");
      }.bind(editor);
    }

    editor.$autocompletionInitialized = 1;
  };

  var findActiveAceInstance = function() {
    var el = document.activeElement;
    while (el != null) {
      if (el.env && el.env.editor)
        return el.env.editor;
      el = el.parentElement;
    }
    return null;
  };

  var autocomplete = function() {

    // find active Ace instance
    var editor = findActiveAceInstance();
    if (editor == null)
      return;
      
    // bail if completions are disabled for this editor
    if (!editor.tutorial.completion)
      return;
    
    // ensure completion engine initialized
    ensureInitialized(editor);

    // cancel any pending live autocompletion
    clearTimeout(editor.$autocompletionTimerId);

    // manually handle event
    event.stopPropagation();
    event.preventDefault();
    editor.execCommand("startAutocomplete");
  };

  document.addEventListener("keydown", function(event) {

    // TODO: find more appropriate place for one-time initialization
    var editor = findActiveAceInstance();
    if (editor !== null)
      ensureInitialized(editor);

    // bail if completions are disabled for this editor
    if (editor !== null && !editor.tutorial.completion)
      return;

    var keys = new KeyCombination(event);
    if (keys.keyCode == KEYCODE_TAB && keys.modifier == MODIFIER_NONE)
       return autocomplete();
    
    if (keys.keyCode == KEYCODE_SPACE && keys.modifier == MODIFIER_CTRL)
       return autocomplete();

  }, true);
  
}
