# library(XLConnect)
source("RScript/Huidong/nordpred.R", local = TRUE)
source("RScript/Huidong/predict.nordpred.R", local = TRUE)

shinyServer(function(input, output, session) {
  source("RScript/A00.R", local = TRUE)
  source("RScript/srv.JS.R", local = TRUE)
  source("RScript/srv.Fun.R", local = TRUE)
  
  ## uiRecent;
  output$uiTrend <- renderUI({
    if (!is.null(input$linkfun)) {
      selectInput(inputId = "trend", 
                  label   = "Project trend:",
                  choices = c("Average trend" = "avg", "Slop for last 10 years" = "slop"),
                  selected = "avg", multiple = length(input$linkfun) < 2)
    }
  })
  
  
  ## reactiveValues;
  rDat <- reactiveValues()
  observe({
    if (!is.null(input$cases)) {
      fCases <- input$cases
      dCases <- read.csv(fCases$datapath)
      rDat$cases <- dCases
    }
    if (!is.null(input$pyr)) {
      fPyr <- input$pyr
      dPyr <- read.csv(fPyr$datapath)
      rDat$pyr <- dPyr
    }
  })
  
  ## Datatable: cases;
  output$cases <- renderDataTable({
    rDat$cases
  }, options = list(searching = 0, lengthChange = 0))
  
  ## Datatable: population;
  output$pyr <- renderDataTable({
    rDat$pyr 
  }, options = list(searching = 0, lengthChange = 0))
  
  ## Model fitting;
  rMod <- reactive({
    if(!is.null(rDat$cases) & !is.null(rDat$pyr)) {
      startAge <- which(sapply(1:18, function(x) {min(rDat$cases[x, -1])}) >= 10)[1]
      Mod <- Pred <- list()      
      for (m in 1:length(input$linkfun)) {
        Mod[[m]] <- nordpred(cas      = rDat$cases[, -1], 
                             pyr      = rDat$pyr  [, -1], 
                             periods  = input$noPeriod, 
                             startAge = startAge,
                             linkfunc = input$linkfun[m])
      }
      for (m in 1:length(Mod)) {
        for (n in 1:length(input$trend)) {
          pred <- predict(Mod[[m]], recent = (input$trend[n] == "slop"))
          row.names(pred)[1:18] <- c(paste(seq(0, 80, 5), seq(4, 84, 5), sep = "-"), "85+")
          Pred[[length(Pred)+1]] <- pred
        }
      }
      return(list(Mod = Mod, Pred = Pred))
    }
  })
  
  ## Figure rate;
  output$fig.rate.mult <- renderChart({
    H <- Highcharts$new()
    H$chart(zoomType = "xy")
    H$exporting(enabled = TRUE)
    H$addParams(dom = "fig.rate.mult")
    H$legend(align = "right", verticalAlign = "middle", layout = "vertical")
    if (length(rMod()$Pred) == 2) {
      P1 <- rMod()$Pred[[1]]
      P2 <- rMod()$Pred[[2]]
      Pred <- rbind(P1[which(row.names(P1) == "Standardized rate"), ], 
                    P2[which(row.names(P2) == "Standardized rate"), ])
      if (length(input$linkfun) > 1) row.names(Pred) <- input$linkfun
      if (length(input$trend)   > 1) row.names(Pred) <- input$trend
      H$xAxis(tickLength = 0, categories = List(names(Pred)))
      for (i in 1:2) {
        H$series(data = t(Pred[i, ]), 
                 name = row.names(Pred)[i],
                 marker = list(radius = 3),
                 zoneAxis = "x",
                 zones = list(list(value = length(rDat$cases)-2), list(dashStyle = "dot")))
      }
    }
    H
  })
  
  
  ## Figure rate;
  output$fig.rate <- renderChart({
    H <- Highcharts$new()
    H$chart(zoomType = "xy")
    H$exporting(enabled = TRUE)
    H$addParams(dom = "fig.rate")
    H$legend(align = "right", verticalAlign = "middle", layout = "vertical")
    if (length(rMod()$Pred) == 1) {
      Pred <- rMod()$Pred[[1]]
      H$xAxis(tickLength = 0, categories = List(names(Pred)))
      for (i in which(row.names(Pred) %in% c("Crude rate", "Standardized rate"))) {
        H$series(data = t(Pred[i, ]), 
                 name = row.names(Pred)[i],
                 marker = list(radius = 3),
                 zoneAxis = "x",
                 zones = list(list(value = length(rDat$cases)-2), list(dashStyle = "dot")))
      }
    }
    H
  })
  
  ## Figure: age-specified rate;
  output$fig.rate.age <- renderChart({
    H <- Highcharts$new()
    H$chart(zoomType = "xy")
    H$exporting(enabled = TRUE)
    H$addParams(dom = "fig.rate.age")
    H$legend(align = "right", verticalAlign = "middle", layout = "vertical")
    H$subtitle(text = " ", style = list(fontSize = "14px", color = "black"))
    if (length(rMod()$Pred) == 1) {
      Pred <- rMod()$Pred[[1]]
      H$xAxis(tickLength = 0, categories = List(names(Pred)))
      for (i in 1:18) {
        H$series(data = t(Pred[i, ]), 
                 name = row.names(Pred)[i], 
                 marker = list(radius = 3),
                 zoneAxis = "x",
                 zones = list(list(value = length(rDat$cases)-2), list(dashStyle = "dot")))
      }
    }
    H
  })
  
  ## Figure: age-specified rate;
  output$fig.changedueto <- renderChart({
    H <- Highcharts$new()
    H$chart(zoomType = "xy")
    H$exporting(enabled = TRUE)
    H$addParams(dom = "fig.changedueto")
    H$legend(align = "right", verticalAlign = "middle", layout = "vertical")
    H$subtitle(text = " ", style = list(fontSize = "14px", color = "black"))
    if (length(rMod()$Pred) == 1) {
      Pred <- rMod()$Pred[[1]]
      H$xAxis(tickLength = 0, categories = List(names(Pred)))
      for (i in grep("change.", row.names(Pred))) {
        H$series(data = t(Pred[i, ]), 
                 name = row.names(Pred)[i], 
                 marker = list(radius = 3),
                 zoneAxis = "x",
                 zones = list(list(value = length(rDat$cases)-2), list(dashStyle = "dot")))
      }
    }
    H
  })
  
  
  
  ## Datatable: figure data;
  output$dat <- renderDataTable({
    if (length(rMod()$Pred) == 1) {
      Pred <- rMod()$Pred[[1]]
      data.frame(X = row.names(Pred), Pred)
    }
  }, options = list(searching = 0, lengthChange = 0, pageLength = 30, autoWidth = FALSE))
  
  ## Datatable: figure data;
  output$dat1 <- renderDataTable({
    if (length(rMod()$Pred) == 2) {
      Pred <- rMod()$Pred[[1]]
      data.frame(X = row.names(Pred), Pred)
    }
  }, options = list(searching = 0, lengthChange = 0, pageLength = 30, autoWidth = FALSE))
  
  ## Datatable: figure data;
  output$dat2 <- renderDataTable({
    if (length(rMod()$Pred) == 2) {
      Pred <- rMod()$Pred[[2]]
      data.frame(X = row.names(Pred), Pred)
    }
  }, options = list(searching = 0, lengthChange = 0, pageLength = 30, autoWidth = FALSE))
  
  
#   <table style="width:100%">
#     <td>Jill</td>
#     <td>Smith</td>		
#     <td>50</td>
#     </table>
    
    
  output$uiTabPanel <- renderUI({
    if (length(rMod()$Pred) == 2) {
      tabsetPanel( 
        tabPanel("Cases",                          dataTableOutput("cases")),
        tabPanel("Person Years",                   dataTableOutput("pyr")),
        tabPanel("Prediction: rate", showOutput("fig.rate.mult", lib = "highcharts")),
        tabPanel("Prediction: data 1",             dataTableOutput("dat1")),
        tabPanel("Prediction: data 2",             dataTableOutput("dat2"))
#         ,
#         tabPanel("Model summary",                  h4(HTML(paste(capture.output(print(summary(rMod()$Mod[[1]]$mod))), collapse = "<br>"))))
      )
    } else {
      tabsetPanel( 
        tabPanel("Cases",                          dataTableOutput("cases")),
        tabPanel("Person Years",                   dataTableOutput("pyr")),
        tabPanel("Prediction: rate",               showOutput("fig.rate", lib = "highcharts")),
        tabPanel("Prediction: age-specified rate", showOutput("fig.rate.age", lib = "highcharts")),
        tabPanel("Prediction: change due to:",     showOutput("fig.changedueto", lib = "highcharts")),
        tabPanel("Prediction: data",               dataTableOutput("dat"))
#         ,
#         tabPanel("Model summary",                  h4(HTML(paste(capture.output(print(summary(rMod()$Mod[[1]]$mod))), collapse = "<br>"))))
      )
    }
  })
      
}) 
  
  
  