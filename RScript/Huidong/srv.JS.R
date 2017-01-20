#### Shared JavaScript ####
tooltip.formatter_p <- "#! 
  function() {
    return(this.x + '<br>' + '<b>' + this.series.name.replace(/ ?.N=.+/, '') + '</b>' + ': ' +  
    Highcharts.numberFormat(this.y * 100, 2)  + '%');
  } 
!#"

tooltip.formatter_n <- "#! 
  function() {
    return(this.x + '<br>' + '<b>' + this.series.name.replace(/ ?.N=.+/, '') + '</b>' + ': ' +  
    Highcharts.numberFormat(this.y, 0));
  } 
!#"

yAxis.labels.formatter_p <- "#! 
  function() {
    var X = this.axis.tickPositions;
    var Y = [];
    for(var i = 0; i < X.length; i++) Y.push(Highcharts.numberFormat(X[i], 2))
    if (X.length == GetUnique(Y).length) {
      return Highcharts.numberFormat(this.value *100, 0) + '%'; 
    } else {
      var Z = [];
      for(var i = 0; i < X.length; i++) Z.push(Highcharts.numberFormat(X[i], 3))
      if (X.length == GetUnique(Z).length) {
        return Highcharts.numberFormat(this.value *100, 1) + '%'; 
      } else {
        return Highcharts.numberFormat(this.value *100, 2) + '%'; 
      }
    }
  } 
!#"

legendItemClick <- "#! 
  function() {  
    var Series = this.chart.series;  
    if (!this.visible) {
      for (var i = 0; i < Series.length; i++) {
        if (Series[i].userOptions.name == this.userOptions.name) Series[i].show();    
      };  
    } else {    
      for (var i = 0; i < Series.length; i++) {      
        if (Series[i].index != this.index && Series[i].userOptions.name != this.userOptions.name) {
          Series[i].visible ? Series[i].hide() : Series[i].show();      
        };    
      };  
    };  
    return false; 
  } 
!#"

xAxis.events.setExtremes <- "#! 
  function(event) {  
    if (event.max) {    
      var frequency = Math.ceil((event.max - event.min)/10);    
      this.options.labels.step = frequency;  
    } else {    
      var frequency = Math.ceil((this.chart.xAxis[0].categories.length - 1)/10);    
      this.options.labels.step = frequency;  
    };
  } 
!#"

demoText <- "#!
  function() {
    if (window.location.hostname == 'kremt.kreftregisteret.no') {
      if ($('#lang1').attr('checked') == 'checked') {
        var demo = 'Demoversion'
      } else {
        var demo = 'Demoversjon'
      }
      this.renderer.text(demo, 10, 50).css({
        color: '#FAB9C1',
        fontSize: '50px'
      }).attr({
        zIndex: 9
      }).add();
    }
  } 
!#"

xAxis.extreme <- "#!
  function() {
    var chart = $('#fig').highcharts();
    chart.yAxis[1].setExtremes(chart.yAxis[0].min, chart.yAxis[0].max);
    chart.xAxis[0].setExtremes(chart.xAxis[0].min-.0001, chart.xAxis[0].max);
  } 
!#"
