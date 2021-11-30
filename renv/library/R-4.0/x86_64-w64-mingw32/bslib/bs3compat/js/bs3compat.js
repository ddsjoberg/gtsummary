// Inform the world that we have the ability to use BS3 nav/navbar markup in BS4
window.BS3_COMPAT = true;

(function($) {
  if (!$.fn.tab.Constructor.VERSION.match(/^3\./)) {
    (console.warn || console.error || console.log)("bs3compat.js couldn't find bs3 tab impl; bs3 tabs will not be properly supported");
    return;
  }
  var bs3TabPlugin = $.fn.tab.noConflict();

  if (!$.fn.tab.Constructor.VERSION.match(/^4\./)) {
    (console.warn || console.error || console.log)("bs3compat.js couldn't find bs4 tab impl; bs3 tabs will not be properly supported");
    return;
  }
  var bs4TabPlugin = $.fn.tab.noConflict();

  var EVENT_KEY = "click.bs.tab.data-api";
  var SELECTOR = '[data-toggle="tab"], [data-toggle="pill"]';

  $(document).off(EVENT_KEY);
  $(document).on(EVENT_KEY, SELECTOR, function(event) {
    event.preventDefault();
    $(this).tab("show");
  });

  function TabPlugin(config) {
    if ($(this).closest(".nav").find(".nav-item, .nav-link").length === 0) {
      // Bootstrap 3 tabs detected
      bs3TabPlugin.call($(this), config);
    } else {
      // Bootstrap 4 tabs detected
      bs4TabPlugin.call($(this), config);
    }
  }

  var noconflict = $.fn.tab;
  $.fn.tab = TabPlugin;
  $.fn.tab.Constructor = bs4TabPlugin.Constructor;
  $.fn.tab.noConflict = function() {
    $.fn.tab = noconflict;
    return TabPlugin;
  };

})(jQuery);

// bs3 navbar: li.active > a
// bs4 navbar: li > a.active
// bs3 tabset: li.active > a
// bs4 tabset: li > a.active


(function($) {
  /* 
   * Bootstrap 4 uses poppler.js to choose what direction to show dropdown
   * menus, except in the case of navbars; they assume that navbars are always
   * at the top of the page, so this isn't necessary. However, Bootstrap 3
   * explicitly supported bottom-positioned navbars via .navbar-fixed-bottom,
   * and .fixed-bottom works on Bootstrap 4 as well.
   * 
   * We monkeypatch the dropdown plugin's _detectNavbar method to return false
   * if we're in a bottom-positioned navbar.
   */
  if (!$.fn.dropdown.Constructor.prototype._detectNavbar) {
    // If we get here, the dropdown plugin's implementation must've changed.
    // Someone will need to go into Bootstrap's dropdown.js.
    (console.warn || console.error || console.log)("bs3compat.js couldn't detect the dropdown plugin's _detectNavbar method");
    return;
  }

  var oldDetectNavbar = $.fn.dropdown.Constructor.prototype._detectNavbar;
  $.fn.dropdown.Constructor.prototype._detectNavbar = function() {
    return oldDetectNavbar.apply(this, this.arguments) &&
      !($(this._element).closest('.navbar').filter('.navbar-fixed-bottom, .fixed-bottom').length > 0);
  };
})(jQuery);
