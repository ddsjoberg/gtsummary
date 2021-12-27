//initialize
var t;
var n = 1;
var i = 1;
var nmax = 1;
var tmp = 0;

function getElem(obj) {
	return(document.getElementById(obj));
}

function loading(obj) {
		getElem("loading"+obj).style.display="none";
		getElem("divPreload"+obj).style.height = getElem("height"+obj).value + "px";
		getElem("btnPlay"+obj).disabled = false;
		getElem("btnFirst"+obj).disabled = false;
		getElem("btnLast"+obj).disabled = false;
		getElem("btnMore"+obj).disabled = false;
}

// show the j-th frame
function showFrame(obj, num) {
	var j=1;
	nmax=parseInt(getElem("nmax"+obj).value);
	for(j = 1; j <= nmax; j++){
		getElem("divPreload" + obj + j).style.display = "none";
	}
	getElem("divPreload" + obj + num).style.display = "block";
}

// show more controls
function btnMore(obj) {
	var showMorePar=getElem("morePar"+obj).style.display;
	if(showMorePar=="") 
	{
		getElem("morePar"+obj).style.display="block" ;
		getElem("btnMore"+obj).value="  ><  ";
	}
	else
	{
		if(showMorePar=="none")
		{
			getElem("morePar"+obj).style.display = "block";
			getElem("btnMore"+obj).value="  ><  ";
		}
		else 
		{
			getElem("morePar"+obj).style.display = "none";
			getElem("btnMore"+obj).value="  <>  ";
		}
	}
}

// go to j-th frame
function txtFrame(num, obj) {
	num=parseInt(num);
	if (num>0 && num<=parseInt(getElem("nmax"+obj).value)) {
		showFrame(obj, num);
		eval("n"+obj+"="+num);
	}
	else {
		alert("You must enter a positive integer between 1 and "+parseInt(getElem("nmax"+obj).value)+"!");
		getElem("txtFrame"+obj).select();
	}
}


function displayImage(obj) {
	var txtInterval = getElem("txtInterval"+obj);
	eval("var tm"+obj+" = Math.round(Number(txtInterval.value)*1000)");
	eval("var tm=tm"+obj);
	eval("nmax=nmax"+obj);
	eval("n=n"+obj);	
	if (n > nmax) {
		if (getElem("checkLoop"+obj).checked) {
			n = 1;
			eval("n"+obj+"=1");
			displayImage(obj);
		}
		else {
			pauseAni(obj);	
		}
	}
	else {
		for(i = 1; i <= nmax; i++){
			getElem("divPreload" +obj+ i).style.display = "none";
		}
	
		getElem("divPreload" +obj+ n).style.display = "block";
		getElem("txtFrame"+obj).value = n ;
		eval("t"+obj+" = setTimeout(\"displayImage(\\\"\"+obj+\"\\\")\", tm)");
		eval("n"+obj+"++");
	}
}

function playAni(obj) {
	getElem("btnPlay"+obj).disabled = true;
	getElem("btnPause"+obj).disabled = false;
	getElem("btnFast"+obj).disabled = false;
	getElem("btnSlow"+obj).disabled = false;
	getElem("btnPrev"+obj).disabled = true;
	getElem("btnNext"+obj).disabled = true;
	getElem("btnFirst"+obj).disabled = true;
	getElem("btnLast"+obj).disabled = true;
	displayImage(obj);	
}

function pauseAni(obj) {
	eval("clearTimeout(t"+obj+")");
	getElem("btnPlay"+obj).disabled = false;
	getElem("btnPause"+obj).disabled = true;
	getElem("btnFast"+obj).disabled = true;
	getElem("btnSlow"+obj).disabled = true;
	getElem("btnPrev"+obj).disabled = false;
	getElem("btnNext"+obj).disabled = false;
	getElem("btnFirst"+obj).disabled = false;
	getElem("btnLast"+obj).disabled = false;
	eval("n"+obj+"--");
	eval("if (n"+obj+"<=0) n"+obj+"=1");
}
	
function fastAni(obj,step) {
	var txtInterval = getElem("txtInterval"+obj);
	txtInterval.value-=step*getElem("txtStep"+obj).value;
	if (Number(txtInterval.value) < 0) {
		txtInterval.value=0;
		getElem("btnFast"+obj).disabled=true;
	}
	if (step==-1) getElem("btnFast"+obj).disabled=false;
}

function prevAni(obj,step) {
	eval("n"+obj+"=n"+obj+"-1*"+step);
	eval("tmpnum=n"+obj);
	if (tmpnum<=0) {
		getElem("btnPrev"+obj).disabled = true;
		getElem("btnNext"+obj).disabled = false;
		eval("n"+obj+"=1");
	}
	else if (tmpnum>parseInt(getElem("nmax"+obj).value)) {
		getElem("btnPrev"+obj).disabled = false;
		getElem("btnNext"+obj).disabled = true;
		eval("n"+obj+"="+parseInt(getElem("nmax"+obj).value));
	}
	else {
		eval("showFrame(\""+obj+"\", "+tmpnum+")");
		getElem("txtFrame"+obj).value=tmpnum;
	}
	if (step==1) {
		getElem("btnNext"+obj).disabled = false	;
	}
	else {
		getElem("btnPrev"+obj).disabled = false	;
	}
}

function firstAni(obj,step) {
	if (step) {
		showFrame(obj, 1);
		getElem("txtFrame"+obj).value = 1;
		eval("n"+obj+"=1");
		getElem("btnNext"+obj).disabled = false	;
	}
	else {
		showFrame(obj, parseInt(getElem("nmax"+obj).value));
		getElem("txtFrame"+obj).value = parseInt(getElem("nmax"+obj).value);
		eval("n"+obj+"=nmax"+obj);
		getElem("btnPrev"+obj).disabled = false	;
	}
}

function txtInterval(Obj) {
	if (parseFloat(Obj.value)<0) {
		Obj.value=0;
	}
}
