/**
 * Refreshes the table and the navigation bar
 * @param table the table to paginate
 * @param nav_id the div where the pagination menu will appear
 * @param currPage the page of the table to show
 * @param rowsShown the number of rows to show per page
 */
function refresh_table(table, nav_id, currPage, rowsShown) {
  "use strict";
  function append_link_to_page(pagenum, text, container) {
    var pagelink;
    pagelink = document.createElement("a");
    $(pagelink).attr('href','#').attr('data-page', pagenum).
      addClass('page_button').text(text);
    $(container).append(pagelink);
    return pagelink;
  }

  function showing_x_to_y_of_z_entries(startItem, endItem, rowsTotal) {
    var showing_entries_div = document.createElement('div');
    $(showing_entries_div).attr('id', 'showing_entries_div');
    if (+rowsTotal === 0) {
      $(showing_entries_div).append('<small>No entries.</small>');
    } else {
      $(showing_entries_div).append('<small>Showing ' +(+startItem+1) +
      ' to ' +endItem + ' of ' +rowsTotal + ' entries.</small>');
    }
    return showing_entries_div;
  }

  function first_previous_1_2_3_4_next_last(currPage, numPages, table, nav_id, rowsShown) {
    // First Previous 4 5 6 7 8 9 10 Next Last
    var page_numbers_div = document.createElement('div');
    if (numPages <= 1) {
      // Empty div if there are no pages to change
      return page_numbers_div;
    }
    $(page_numbers_div).attr('id', 'page_numbers_div');

    // Page: First and Previous
    var pagefirst = append_link_to_page(0, 'First', page_numbers_div);
    var pageprev = append_link_to_page(+currPage-1, 'Previous', page_numbers_div);
    if (+currPage === 0) {
      $(pagefirst).addClass('page_button_disabled');
      $(pageprev).addClass('page_button_disabled');
    }

    var spanpagenumber = document.createElement('span');
    $(page_numbers_div).append(spanpagenumber);
    var start_nearby_pages = Math.max(0, +currPage-3);
    var end_nearby_pages = Math.min(+numPages-1, +currPage+3);
    for (var i = start_nearby_pages; i <= end_nearby_pages; i++) {
      // Page: i
      var page_i = append_link_to_page(i, 1+i, spanpagenumber);
      if (+currPage === +i) {
        $(page_i).addClass('page_button_current');
      }
    }
    // Page: Next and Last
    var pagenext = append_link_to_page(+currPage+1, "Next", page_numbers_div);
    var pagelast = append_link_to_page(+numPages-1, "Last", page_numbers_div);
    if (+currPage === +numPages-1) {
      $(pagenext).addClass('page_button_disabled');
      $(pagelast).addClass('page_button_disabled');
    }

    $(page_numbers_div).find('a').bind('click', function() {
      var currPage = $(this).attr('data-page');
      refresh_table(table, nav_id, +currPage, +rowsShown);
    });
    return page_numbers_div;
  }

  var navobj = document.getElementById(nav_id);
  var rowsTotal = $(table).find('tbody').find('tr').length;
  var startItem = currPage * rowsShown;
  var endItem = Math.min(startItem + rowsShown, rowsTotal);
  var numPages;
  if (+rowsShown > 0) {
    numPages = Math.ceil(1.0*rowsTotal/rowsShown);
  } else {
    numPages = 0;
  }

  // Show the chosen rows:
  $(table).find('tbody').find('tr').css('opacity','0.0').hide().slice(startItem, endItem).
    css('display','table-row').animate({opacity:1}, 300);

  // Rewrite the navigation panel below the table on each page click
  $(navobj).empty();
  // Showing 31 to 40 entries out of 150 entries:
  $(navobj).append(showing_x_to_y_of_z_entries(startItem, endItem, rowsTotal));
  // First Previous 1 2 3 4 5 Next Last
  $(navobj).append(first_previous_1_2_3_4_next_last(currPage, numPages,
    table, nav_id, rowsShown));
}

/**
 * Adds pagination options to a table
 * @param table the table to be paginated
 * @param nav_id A string with the id of the div that will contain both the "Showing 11 to 20 of 100 entries" and the pagination buttons (First Previous 1 2 3 Next Last)
 * @param select_entries_div_id A string with the id of the div where "Show [10|25|100] entries" selection box will be placed
 * @param options Currently only one option accepted: options.number_of_entries = [10, 20, 30]. It controls the possible number of rows per page to show.
 */
function table_pagination(table, nav_id, select_entries_div_id, options) {
  "use strict";
  // <div><label> Show <select>[10|25|100]</select> entries</label></div>
  var select_entries_div = document.getElementById(select_entries_div_id);
  $(select_entries_div).empty();

  // Get the possible entries per page:
  var select_entries_allowed = options.number_of_entries;
  if (select_entries_allowed.length === 0) {
    select_entries_allowed = [10, 25, 100];
  }

  // If select_entries_allowed is a scalar, do not offer a select:
  if (!$.isArray(select_entries_allowed)) {
    refresh_table(table, nav_id, 0, +select_entries_allowed);
    return;
  }
  // Otherwise show the select menu:
  var label_entries = document.createElement('label');
  var select_entries = document.createElement('select');
  var select_entries_id = select_entries_div_id.concat('_select');
  $(label_entries).attr('for', select_entries_id);
  $(label_entries).append('Show ');
  $(select_entries).attr('id', select_entries_id);
  for (var i=0;i<select_entries_allowed.length;i++) {
    var option = document.createElement('option');
    $(option).attr('value', select_entries_allowed[i]);
    $(option).append(select_entries_allowed[i]);
    $(select_entries).append(option);
  }
  $(select_entries).bind('change', function() {
    var rowsShown = this.options[this.selectedIndex].value;
    refresh_table(table, nav_id, 0, +rowsShown);
  });
  $(label_entries).append(select_entries);
  $(label_entries).append(' entries');
  $(select_entries_div).append(label_entries);
  var rowsShown = select_entries.options[select_entries.selectedIndex].value;
  refresh_table(table, nav_id, 0, +rowsShown);
}
