document.addEventListener("DOMContentLoaded", function(event) {
  var els = document.querySelectorAll(".tabwid");
  var tabwid_link = document.querySelector('link[href*="tabwid.css"]')
  if (tabwid_link === null) {
    const tabwid_styles = document.evaluate("//style[contains(., 'tabwid')]", document, null, XPathResult.ANY_TYPE, null );
    tabwid_link = tabwid_styles.iterateNext();
  }

  Array.prototype.forEach.call(els, function(template) {
      const dest = document.createElement("div");
      template.parentNode.insertBefore(dest, template.nextSibling)
      dest.setAttribute("class", "flextable-shadow-host");
      const fantome = dest.attachShadow({mode: 'open'});
      fantome.appendChild(template);
      if (tabwid_link !== null) {
        fantome.appendChild(tabwid_link.cloneNode(true));
      }
  });

  const shadowHosts = document.querySelectorAll('.flextable-shadow-host:not(:has(div > table.no-shadow-dom))');
  shadowHosts.forEach(host => {
    if (host.shadowRoot) {
      const spanElements = host.shadowRoot.querySelector('div > table > caption > span[id]');
      if (spanElements) {
        const id = spanElements.getAttribute("id");
        host.setAttribute("id", id);
      }
    }
  });

});
