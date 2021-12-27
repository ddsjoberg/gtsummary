var TutorialDiagnostics = function(tutorial) {
  this.$tutorial = tutorial;
  var self = this;

  var unmatchedClosingBracket = function(token) {
    return {
      row: token.position.row,
      column: token.position.column,
      type: "error",
      text: "unmatched closing bracket '" + token.value + "'"
    };
  };

  var unmatchedOpeningBracket = function(token) {
    return {
      row: token.position.row,
      column: token.position.column,
      type: "error",
      text: "unmatched opening bracket '" + token.value + "'"
    };
  };

  var unexpected = function(symbol, token, type) {
    return {
      row: token.position.row,
      column: token.position.column,
      type: type || "error",
      text: "unexpected " + symbol + " '" + token.value + "'"
    };
  };

  var isSymbol = function(token) {
    
    // this is a cludge so that 'in' is treated as though it were an
    // operator by the diagnostics system
    var value = token.value || "";
    if (value == "in")
      return false;
      
    var type = token.type || "";
    return type == "string" ||
           type == "constant.numeric" ||
           type == "constant.language.boolean" ||
           type == "identifier" ||
           type == "keyword" ||
           type == "variable.language";
  };

  var isOperator = function(token) {
    var type = token.type || "";
    return type == "keyword.operator";
  };

  var isUnaryOperator = function(token) {
    var value = token.value || "";
    return value == "+" ||
           value == "-" ||
           value == "~" ||
           value == "!" ||
           value == "?";
  };

  var diagnose = function() {

    // alias editor
    var editor = this;

    // create tokenizer -- we do this manually as we do not
    // want Ace to merge brackets sitting together
    var Tokenizer = ace.require("ace/tokenizer").Tokenizer;
    var RHighlightRules = ace.require("ace/mode/r_highlight_rules").RHighlightRules;
    var rules = new RHighlightRules().getRules();
    for (var key in rules) {
      var rule = rules[key];
      for (var i = 0; i < rule.length; i++) {
        rule[i].merge = false;
      }
    }

    // fix up rules
    rules["start"].unshift({
      token : "string",
      regex : '"(?:(?:\\\\.)|(?:[^"\\\\]))*?"',
      merge : false,
      next  : "start"
    });
    
    rules["start"].unshift({
      token : "string",
      regex : "'(?:(?:\\\\.)|(?:[^'\\\\]))*?'",
      merge : false,
      next  : "start"
    });
    
    rules["start"].unshift({
      token : "keyword.operator",
      regex : ":::|::|:=|%%|>=|<=|==|!=|\\->|<\\-|<<\\-|\\|\\||&&|=|\\+|\\-|\\*\\*?|/|\\^|>|<|!|&|\\||~|\\$|:|@|\\?",
      merge : false,
      next  : "start"
    });

    rules["start"].unshift({
      token : "punctuation",
      regex : "[;,]",
      merge : false,
      next  : "start"
    });

    var tokenizer = new Tokenizer(rules);

    // clear old diagnostics
    editor.session.clearAnnotations();

    // retrieve contents and tokenize
    var lines = editor.session.doc.$lines;
    var tokens = [];
    var state = "start";
    for (var i = 0; i < lines.length; i++) {
      var tokenized = tokenizer.getLineTokens(lines[i], state);
      for (var j = 0; j < tokenized.tokens.length; j++)
        tokens.push(tokenized.tokens[j]);
      tokens.push({type: "text", value: "\n"});
      state = tokenized.state;
    }

    // add row, column to each token
    var doc = editor.session.doc;
    var docIndex = 0;
    for (var i = 0; i < tokens.length; i++) {
      tokens[i].position = doc.indexToPosition(docIndex);
      docIndex += tokens[i].value.length;
    }

    // remove whitespace, comments (not relevant for syntax diagnostics)
    tokens = tokens.filter(function(token) {
      return token.type !== "comment" && !/^\s+$/.test(token.value);
    });

    // state related to our simple diagnostics engine
    var diagnostics = [];
    var bracketStack = [];
    
    // iterate through tokens and look for invalid sequences
    for (var i = 0; i < tokens.length; i++) {

      // update local state
      var token = tokens[i];
      var type  = token.type;
      var value = token.value;

      // handle left brackets
      if (value === "(" || value === "{" || value === "[") {
        bracketStack.push(token);
        continue;
      }
      
      // handle right brackets
      if (value === ")" || value === "}" || value === "]") {

        // empty bracket stack: signal unmatched
        if (bracketStack.length === 0) {
          diagnostics.push(unmatchedClosingBracket(token));
          continue;
        }

        // pop off from bracket stack and verify
        var openBracket = bracketStack.pop();
        
        var ok =
          value === ")" && openBracket.value === "(" ||
          value === "]" && openBracket.value === "[" ||
          value === "}" && openBracket.value === "{";

        if (!ok) {
          diagnostics.push(unmatchedClosingBracket(token));
          diagnostics.push(unmatchedOpeningBracket(openBracket));
          continue;
        }
      }

      if (i > 0) {

        var lhs = tokens[i - 1];
        var rhs = tokens[i];
        var bracket = bracketStack[bracketStack.length - 1] || {};

        // if we have two symbols in a row with no binary operator inbetween, syntax error
        if (lhs.position.row == rhs.position.row && isSymbol(lhs) && isSymbol(rhs)) {
          diagnostics.push(unexpected("symbol", rhs));
          continue;
        }

        // if we have an operator followed by a binary-only operator, syntax error
        if (lhs.position.row == rhs.position.row && isOperator(lhs) && isOperator(rhs) && !isUnaryOperator(rhs)) {
          diagnostics.push(unexpected("operator", rhs));
          continue;
        }

        // if we have multiple commas in a row within a parenthetical context, warn
        if (lhs.value == "," && rhs.value == "," && bracket.value == "(") {
          diagnostics.push(unexpected("comma", rhs, "warning"));
          continue;
        }

        // if we have a comma preceding a closing bracket, warn
        if (lhs.value == "," && (rhs.value == "}" || rhs.value == ")" || rhs.value == "]")) {
          diagnostics.push(unexpected("comma", lhs, "warning"));
          continue;
        }
      }
    }

    // if we still have things on the bracket stack, they're unmatched
    for (var i = 0; i < bracketStack.length; i++) {
      diagnostics.push(unmatchedOpeningBracket(bracketStack[i]));
    }

    // signal diagnostics to Ace
    editor.session.setAnnotations(diagnostics);

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

  var ensureInitialized = function(editor) {

    if (editor.$diagnosticsInitialized)
      return;

    if (!editor.tutorial.diagnostics)
      return;

    // register handlers
    var handlers = {};
    handlers["change"] = self.$onChange.bind(editor);
    handlers["destroy"] = function(event) {
      for (var key in handlers)
        this.off(key, handlers[key]);
    }.bind(editor);

    for (var key in handlers)
      editor.on(key, handlers[key]);

    editor.$liveDiagnostics = diagnose.bind(editor);

    editor.$diagnosticsInitialized = 1;
  };

  this.$onChange = function(data) {
    if (!this.tutorial.diagnostics)
      return;

    clearTimeout(this.$diagnosticsTimerId);
    this.session.clearAnnotations();
    var delayMs = 1000;
    this.$diagnosticsTimerId = setTimeout(this.$liveDiagnostics, 1000);
  };

  this.$onKeyDown = function(event) {
    var editor = findActiveAceInstance();
    if (editor != null) {
      ensureInitialized(editor);
      document.removeEventListener("keydown", this.$onKeyDown);
    }
  };

  document.addEventListener("keydown", this.$onKeyDown);

};
