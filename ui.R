library(rCharts)
shinyUI(bootstrapPage(
  # Add custom CSS;
  tagList(
    tags$head(
      tags$style("hr{margin:5px;} "),
      tags$link(rel="stylesheet", type="text/css",href="style.css")
    ),
    tags$body(
      tags$div(class = "busy", p("Calculation in progress.."), img(src="busy.gif")),
      tags$script(type="text/javascript", src = "app.js")
    )
  ),
  
  
  # Database, Group, Subgroup and Year; 
  div(class="input",
      div(class = "wPanel", 
          fileInput(inputId = "cases", label = "Cancer cases:"),
          fileInput(inputId = "pyr",   label = "Person years:")
      ),
      div(class = "wPanel", 
          numericInput(inputId = "noPeriod", 
                       label = "Number of periods to use:", 
                       value = 5, min = 4, max = 8, step = 1),
          selectInput(inputId = "linkfun", 
                      label   = "Link function:",
                      choices = c("Power 5" = "power5", "Poisson" = "poisson"),
                      selected = "power5", multiple = TRUE),
          uiOutput("uiTrend") 
      ),      
      ## Export panel;
      div(class = "wPanel", downloadLink(outputId = "figDat", label = ("Export figure data"))),
      textInput(inputId = "svg", label = ""),
      textInput(inputId = "tracking", label = "")
  ),
  
  # Output: figures and table;
  div(class = "output",
      uiOutput("uiTabPanel")
  )
  
))
