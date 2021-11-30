HTMLWidgets.widget({

  name: 'formattable_widget',

  type: 'output',

  initialize: function(el, width, height) {

    return {  }

  },

  renderValue: function(el, x, instance) {
    el.innerHTML = x.html // to test js markdown conversion , markdown.toHTML( x.md )].join(' ')

    // apply the Bootstrap table class to our widget
    var tbl = el.getElementsByTagName("table")[0]
    tbl.className = tbl.className + " table table-condensed";

    // I believe that setting an explicit height doesn't make sense for a table
    // remove the assigned height
    el.style.height = null
  },

  resize: function(el, width, height, instance) {

  }

});
