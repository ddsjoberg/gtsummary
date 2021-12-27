$(document).ready(function(){
  $(".gmisc_table td .hidden").map(function(index, el){
     el.parentNode.style["original-color"] = el.parentNode.style["background-color"];
     el.parentNode.style["background-color"] = "#DDD";
  });

  getSelected = function(){
    var t = '';
    if(window.getSelection){
      t = window.getSelection();
    }else if(document.getSelection){
      t = document.getSelection();
    }else if(document.selection){
      t = document.selection.createRange().text;
    }
    return t.toString();
  };

  $(".gmisc_table td").map(function(index, el){
    this.style.cursor = "pointer";
    el.onmouseup =  function(e){
      if (getSelected().length > 0)
        return;

      var hidden = this.getElementsByClassName("hidden");
      if (hidden.length > 0){
        this.innerHTML = hidden[0].textContent;
        this.style["background-color"] = this.style["original-color"];

      }else{
        $(this).append("<span class='hidden' style='display: none'>" +
                       this.innerHTML + "</span>");

        this.childNodes[0].data = this.childNodes[0].data.substr(0, 20) + "... ";
        this.style["original-color"] = this.style["background-color"];
        this.style["background-color"] = "#DDD";
      }
    };
  });
});
