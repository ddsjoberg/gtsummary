HTMLWidgets.widget({

  name: 'htmlTableWidget',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
        $(el).empty();
        // Select number of rows to see:
        var select_entries_div = document.createElement('div');
        var select_entries_div_id = (el.id).concat('_entries');
        $(select_entries_div).attr('id', select_entries_div_id);
        $(el).append(select_entries_div);
        // Add the table:
        $(el).append(x.thetable);
        /// The navigation bar:
        var nav_obj = document.createElement('div');
        var nav_id = (el.id).concat('_nav');
        $(nav_obj).attr('id', nav_id);
        $(el).append(nav_obj);
        // Set instance variables:
        var thetable = $(el).find('table');
        $(el).css("position","relative");
        $(el).css("clear","both");
        $(thetable).css("width","100%");
        table_pagination(thetable, nav_id, select_entries_div_id, x.options, el);
      },

      resize: function(width, height) {

      }

    };
  }
});
