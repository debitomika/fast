output$ui_init <- renderUI({
  list(
    includeCSS("www/style.css"),
    tags$head(
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/busy.js"),
      tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
                  type = 'text/javascript')
      #tags$style(type='text/css', '.well {background-color: rgB( 0, 132, 180);}')
    ),
    
    navbarPage("Fast", id = "nav_fast", collapsable = TRUE,inverse = T, 
               
               tabPanel("Data",progressInit(),uiOutput('data_ui_and_tabs')),
               
               navbarMenu("Forecasting",
                          tabPanel("ARIMA", uiOutput("autoarima"))),
               
               navbarMenu("Cluster",
                          tabPanel("Hierarchical", uiOutput("clusteringH")),
                          tabPanel("Partitional", uiOutput("clusteringP"))),
               
               navbarMenu("Regression",
                          tabPanel("Tobit Regression", uiOutput("tobit_reg"))),
               
               navbarMenu("R",
                          tabPanel("Report", uiOutput("report")),
                          tabPanel("code", uiOutput("rcode")))))
})