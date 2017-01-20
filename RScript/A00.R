
##################
## Export panel ##
##################

## Export figure as PNG;
output$uiFigPNG <- renderUI({
  downloadLink(outputId = "figPNG", label = ("figPNG"))
})
output$figPNG <- downloadHandler(
  filename = function() {
    paste('Highcharts-', Sys.Date(), '.png', sep='')
  },
  content = function(file) {
    system(paste("inkscape -f", rDat$tempSVG, "-d 300 -e", rDat$tempPNG))
    file.copy(rDat$tempPNG, file)
  }
)

## Export figure as PDF;
output$uiFigPDF <- renderUI({
  downloadLink(outputId = "figPDF", label  = ("figPDF"))
})
output$figPDF <- downloadHandler(
  filename = function() {
    paste('Highcharts-', Sys.Date(), '.pdf', sep='')
  },
  content = function(file) {
    system(paste("inkscape -f", rDat$tempSVG, "-A", rDat$tempPDF))
    file.copy(rDat$tempPDF, file)
  }
)

## Export figure data;
output$uiFigDat <- renderUI({
  downloadLink(outputId = "figDat", label = ("figDat"))
})
output$figDat <- downloadHandler(
  filename = function() {
    paste('Highcharts data-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.table(rDat$dat, file, quote = FALSE, sep = ",", na = "")
  }
)

