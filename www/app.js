/*/ Hide options
$(document).on("ready change", function() {
  var e = document.getElementById("analysis");
  var v = e.options[e.selectedIndex].text;
  console.log(v)
  var percent = document.getElementById("uiPercent");
  if ( ['A03', 'A04', 'A05', 'A09', 'A10', 'A12', 'A18'].indexOf(v) < 0) {
    percent.style.display = "none"
  }
  
})


$(document).on("ready", function() {
  $("#cases").change(function(){
    $("#uiTabPanel>div>ul>li").removeClass()
    $("#uiTabPanel>div>ul>li")[0].className = "active"
    $("#uiTabPanel>div>div>div").removeClass()
    $("#uiTabPanel>div>div>div").addClass("tab-pane")
    $("#uiTabPanel>div>div>div")[0].className = "tab-pane active"
  })
  $("#pyr").change(function(){
    $("#uiTabPanel>div>ul>li").removeClass()
    $("#uiTabPanel>div>ul>li")[1].className = "active"
    $("#uiTabPanel>div>div>div").removeClass()
    $("#uiTabPanel>div>div>div").addClass("tab-pane")
    $("#uiTabPanel>div>div>div")[1].className = "tab-pane active"
  })
})
*/

// var x = $("#uiTabPanel>div>ul>li")[3]
// x.className = "active"

//http://manasbhardwaj.net/get-unique-values-from-a-javascript-array-using-jquery/
function GetUnique(inputArray){
  var outputArray = [];
  for (var i = 0; i < inputArray.length; i++){
  	if ((jQuery.inArray(inputArray[i], outputArray)) == -1){
			outputArray.push(inputArray[i]);
		}
	}
	return outputArray;
}


// Calculation in process...
$(document).on("ready change", function() {
  var tt = setInterval(function(){
    if ($('html').attr('class')=='shiny-busy') {$('.busy').show()
    } else {$('.busy').hide(); clearInterval(tt)}
  },50);
})

function showHide(ID) {
  if ($(ID + "+.sPanel").attr("style") == "height: 0px; visibility: hidden;" | $(ID + "+.sPanel").attr("style") == undefined) {
      $(ID + "+.sPanel").attr("style", "height: auto; visibility: visible;")
  } else {
    $(ID + "+.sPanel").attr("style", "height: 0px; visibility: hidden;")
  }
//  $(".wPanel:has(" + ID + "+.sPanel)").mouseleave(function(){
//    $(ID + "+.sPanel").attr("style", "height: 0px; visibility: hidden;")
//  });
}

function jsUpload() {showHide("#upload")}
function jsOption() {showHide("#option")}
function jsFigSetting() {showHide("#uiFigConfig")}
function jsFeedback()   {showHide("#uiFeedback")}
function jsExport()     {showHide("#export")
  // Select chart and get SVG data;
  // var Activ = $("div.output>div[style='display: block;']>div[style='display: block;']")
  var Activ = $("div.output>div[style='display: block;']")
  var chart = $(".shiny-html-output.rChart.highcharts.shiny-bound-output", Activ).highcharts()
  var svg = chart.getSVG();
  var svg = svg.replace(/<g class="highcharts-button".*?g>/, "")
  var svg = svg.replace(/<g class="highcharts-tooltip".*?g>/, "")
  // Empty target object and fill with new svg data;
  var target = $("#svg");
  target.val("");
  target.val(svg);
  target.trigger("change");
}


/*
function genWarnMsg() {
  if ($("#warnMsg").val() != "") {
    alert($("#warnMsg").val())
  }
}
*/

// Action tracking;
// IP info;
//$(document).ready(function(){
//  $.get("http://ipinfo.io", function(response) {
//    var target = $("#tracking");
//    target.val(JSON.stringify(response));
//    target.trigger("change");
//  }, 'json');
//});
// class is 'shiny-bound-input';
$(document).on("change", ".shiny-bound-input:not([id='tracking'])", function(evt) {
  var e = $(evt.target);
  var tagName = e.context.tagName;
  // SELECT
  if (tagName == "SELECT") {
    var A = {};
    A['id'] = e.context.id;
    A['type'] = e.context.type;
    var so = e.context.selectedOptions;
    var S = [];
    for (var i=0; i<so.length; i++) {
      var s = {};
      s[so[i].innerHTML] = so[i].value
      S.push(s)
    }
    A['selected'] = S;
    var target = $("#tracking");
    target.val(JSON.stringify(A));
    target.trigger("change");  		
  }
  // INPUT
  if (tagName == "INPUT") {	
    var A = {};
    var type = e.context.type;
    //radioButtons, numericInput, passwordInput;
    if (["radio", "numeric", "password"].indexOf(type) >= 0) {
      if (type == "radio") {A['name'] = e.context.name;}
      A['id'] = e.context.id;
      A['value'] = e.context.value;
      A['label'] = e.siblings()[0].innerHTML;
      var target = $("#tracking");
      target.val(JSON.stringify(A));
      target.trigger("change");	      
    }
    //fileInput;
    if (type == "file") {
      A['id'] = e.context.name;
      files = [];		
      for (var i=0; i<e.context.files.length; i++) {
        files.push(JSON.stringify(e.context.files[i]));
      }
      A['files'] = files;
      var target = $("#tracking");
      target.val(JSON.stringify(A));
      target.trigger("change");	      
    }			
    //checkboxInput, checkboxGroupInput;
    if (type == "checkbox") {
      A['id'] = e[0].id;
      A['checked'] = e[0].checked
      A['label'] = e.siblings()[0].innerHTML;
      var target = $("#tracking");
      target.val(JSON.stringify(A));
      target.trigger("change");	
    }
  }
});

// For type equal 'text' INPUT;
$(document).on("change", ".shiny-bound-input:not([id='tracking'])", function(evt) {
  var e = $(evt.target);
  var tagName = e.context.tagName;
  if (tagName == "INPUT") {
    var type = e.context.type;
    if (type == "text" && e.context.id != "svg") {
      var v = e.context.value
      setTimeout(function(){
        var e = $(evt.target);
        if (e.context.value == v) {
          var A = {};
          A['value'] = e.context.value;
          var id = e.context.id;
          var label = e.siblings()[0].innerHTML;
          if (id == "") {
            var id = e.parent()[0].id;
            if (id == "") {
              var id = e.parent().parent()[0].id;
              var label = e.parent().siblings()[0].innerHTML
              if (e.siblings()[1].tagName == "SPAN") {
                A['dateType'] = "end"
              } else {
                A['dateType'] = "start"
              }
            }
          }
          A['id'] = id;
          A['label'] = label;
          var target = $("#tracking");
          target.val(JSON.stringify(A));
          target.trigger("change");
        }
      }, 500);
    }			
  }
});
// actionButton;
$(document).on("click", function(evt) {
  var e = $(evt.target);
  var tagName = e.context.tagName;
  if (["A", "BUTTON"].indexOf(tagName) >= 0) {
    var A = {};
    A['id'] = e.context.id;
    A['label'] = e.context.innerText;
    var target = $("#tracking");
    target.val(JSON.stringify(A));
    target.trigger("change");		
  }
});
// submitButton;
$(':submit').on("click", function(evt) {
  var e = $(evt.target); 
  if (e.context.tagName == "BUTTON") {
    var A = {};
    A['label'] = e.context.innerText;
    var target = $("#tracking");
    target.val(JSON.stringify(A));
    target.trigger("change");
  }
});



