$(document).ready(function(){
  // Placeholder for button
  btn = "%btn%";

  // Ad the button to each element
  $(".gmisc_table td").map(function(index, el){
    if (el.innerHTML.length > %txt.maxlen% && el.getElementsByClassName("btn").length == 0)
      el.innerHTML += btn;
  })

  $(".gmisc_table td .btn").map(function(index, el){
    el.onclick =  function(e){
      var hidden = this.parentNode.getElementsByClassName("hidden");
      if (this.textContent === "+"){
        this.parentNode.childNodes[0].data = hidden[0].textContent;
        this.textContent = "-";
      }else{
        $(this.parentNode).append("<span class='hidden' style='display: none'>" + this.parentNode.childNodes[0].data + "</span>")
        this.parentNode.childNodes[0].data = this.parentNode.textContent.substr(0, %txt.maxlen%) + "... ";
        this.textContent = "+";
      }
    }
  })
})
