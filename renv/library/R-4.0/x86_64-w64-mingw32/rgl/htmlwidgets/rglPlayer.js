/* el is the <span>, holding x as el.rglPlayer
   x is the JSON encoded playwidget.
   instance holds x as instance.rglPlayer
*/

HTMLWidgets.widget({

  name: 'rglPlayer',

  type: 'output',

  initialize: function(el, width, height) {
    return {
    };

  },

  renderValue: function(el, x, instance) {
    var ShowValue = function(value) {
        var rgldiv = document.getElementById(x.sceneId),
            rglinstance;
        x.value = value;
      /* We might be running before the scene exists.  If so, it
         will have to apply our initial value. */
        if (rgldiv && (rglinstance = rgldiv.rglinstance)) {
          rglinstance.Player(el, x);
          x.initialized = true;
        } else {
          if (x.initialized)
            rglwidgetClass.prototype.alertOnce("rgl widget '" + x.sceneId + "' not found.");
          x.initialized = false;
          instance.rglPlayer = x;
        }
      };

    el.rglPlayer = x;
    instance.rglPlayer = x;

    if (x.respondTo) {
      var control = window[x.respondTo];
      if (control) {
        var self = this, i,
            state = "idle";

        /* Store the previous handler on the control,
           so multiple calls here don't pile up a chain of
           old handlers */

        if (typeof control.rglOldhandler === "undefined")
          control.rglOldhandler = control.onchange;

        control.onchange = function() {
          var value;
          /* If we are called n>0 times while servicing a previous call, we want to finish
             the current call, then run again.  But the old handler might want to
             see every change. */
          if (state !== "idle") {
            state = "interrupted";
            if (control.rglOldhandler !== null)
              control.rglOldhandler.call(this);
          }
          do {
            state = "busy";
            if (control.rglOldhandler !== null)
              control.rglOldhandler.call(this);
            if (control.type == "checkbox")
              value = control.checked ? 0 : 1;
            else
              value = control.value;
            ShowValue(value);
            if (state === "busy")
              state = "idle";
          } while (state !== "idle");
        };
        control.onchange();
      }
    }
    ShowValue(x.value);
  },

  resize: function(el, width, height, instance) {
  }

});
