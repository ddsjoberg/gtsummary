// This must be executed with phantomjs
// Take a screenshot of a URL and saves it to a .png file
// phantomjs webshot.js <optsList>
//
// 'optsList' is a JSON array containing configurations for each screenshot that has
// to be taken. Each configuration object needs to contain at least properties
// "url" and "file". For instance:
// [{"url":"http://rstudio.github.io/leaflet/","file":"webshot.png"}]

var utils = require('./utils');
var system = require('system');

phantom.casperPath = phantom.libraryPath + '/casperjs';
phantom.injectJs(phantom.casperPath + '/bin/bootstrap.js');

var opt_defaults = {
  delay: 0.2,
  vwidth: 992,
  vheight: 744,
  zoom: 1
};


// =====================================================================
// Command line arguments
// =====================================================================
var args = system.args;

if (args.length < 2) {
  console.log(
    'Usage:\n' +
    '  phantomjs webshot.js <optsList>\n' +
    '\n' +
    'optsList is a JSON array containing configuration for each screenshot.\n' +
    'For instance:\n' +
    '\'[{"url":"url1.html","file":"file1.png"},{"url":"url2.html","file":"fil2.png","zoom":2}]\'');
  phantom.exit(1);
}

var optsList = JSON.parse(args[1]);

// Options passed to CasperJS
var casperOpts = {};
if (optsList[0].options) {
  casperOpts = JSON.parse(optsList[0].options);
}

// `debug` is a special option. The value from the first element in the
// optsList array is applied globally.
if (optsList[0].debug) {
  casperOpts.verbose = true;
  casperOpts.logLevel = 'debug';
}
delete optsList.debug;

var casper = require('casper').create(casperOpts);


// =====================================================================
// Screenshot
// =====================================================================

casper.start();
casper.options.onLoadError = function(c, url) {
  console.log("Could not load ", url);
  phantom.exit(1);
};

casper.eachThen(optsList, function(response) {
  var opts = response.data;

  // Prepare options
  opts = utils.fillMissing(opts, opt_defaults);

  // This should be four numbers separated by ","
  if (opts.cliprect) {
    opts.cliprect = opts.cliprect.split(",");
    opts.cliprect = opts.cliprect.map(function(x) { return +x; });
  }

  // Can be 1 or 4 numbers separated by ","
  if (opts.expand) {
    opts.expand = opts.expand.split(",");
    opts.expand = opts.expand.map(function(x) { return +x; });
    if (opts.expand.length !== 1 && opts.expand.length !== 4) {
      console.log("'expand' must have either 1 or 4 values.");
      phantom.exit(1);
    }
  }

  // Can have multiple selectors
  if (opts.selector) {
    opts.selector = opts.selector.split(",");
  }

  // Go to url and perform the desired screenshot
  this.zoom(opts.zoom)
    .viewport(opts.zoom * opts.vwidth, opts.zoom * opts.vheight)
    .thenOpen(opts.url)
    .wait(opts.delay * 1000)
    .then(function() {
      if (opts.eval) {
        eval(opts.eval);
      }
    })
    .then(function() {
      var cr = findClipRect(opts, this);
      this.capture(opts.file, cr);
    });
});

casper.run();


// =====================================================================
// Utility functions
// =====================================================================

// Given the options object, return an object representing the clipping
// rectangle. If opts.cliprect and opts.selector are both not present,
// return null.
function findClipRect(opts, casper) {
  // Convert top,right,bottom,left object to top,left,width,height
  function rel2abs(r) {
    return {
      top:    r.top,
      left:   r.left,
      bottom: r.top + r.height,
      right:  r.left + r.width
    };
  }
  // Convert top,left,width,height object to top,right,bottom,left
  function abs2rel(r) {
    return {
      top:    r.top,
      left:   r.left,
      width:  r.right - r.left,
      height: r.bottom - r.top
    };
  }

  var rect;

  if (opts.cliprect) {
    rect = {
      top:    opts.cliprect[0] * opts.zoom,
      left:   opts.cliprect[1] * opts.zoom,
      width:  opts.cliprect[2] * opts.zoom,
      height: opts.cliprect[3] * opts.zoom
    };

  } else if (opts.selector) {
    var selector = opts.selector;

    // Get bounds, in absolute coordinates so that we can find a bounding
    // rectangle around multiple items.
    var bounds = selector.map(function(s) {
      var b = casper.getElementBounds(s);
      return rel2abs(b);
    });

    // Get bounding rectangle around multiple
    bounds = bounds.reduce(function(a, b) {
      return {
        top:    Math.min(a.top, b.top),
        left:   Math.min(a.left, b.left),
        bottom: Math.max(a.bottom, b.bottom),
        right:  Math.max(a.right, b.right)
      };
    });

    // Convert back to width + height format
    rect = abs2rel(bounds);

  } else {
    return null;
  }

  // Expand clipping rectangle
  if (opts.expand) {
    var expand = opts.expand;
    if (expand.length === 1) {
      expand = [expand[0], expand[0], expand[0], expand[0]];
    }

    rect = rel2abs(rect);
    rect.top    -= expand[0] * opts.zoom;
    rect.right  += expand[1] * opts.zoom;
    rect.bottom += expand[2] * opts.zoom;
    rect.left   -= expand[3] * opts.zoom;
    rect = abs2rel(rect);
  }

  return rect;
}


// Exit on error, instead of just hanging
phantom.onError = function(msg, trace) {
  var msgStack = ['PHANTOM ERROR: ' + msg];
  if (trace && trace.length) {
    msgStack.push('TRACE:');
    trace.forEach(function(t) {
      msgStack.push(' -> ' + (t.file || t.sourceURL) + ': ' + t.line + (t.function ? ' (in function ' + t.function +')' : ''));
    });
  }
  console.log(msgStack.join('\n'));
  phantom.exit(1);
};
