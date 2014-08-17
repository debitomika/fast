require(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='', dbname='fast', host='127.0.0.1')
shinyServer(function(input, output, session){
  
  # fungsi source untuk meload fungsi yang dipakai bersama
  source('fast.R', local = TRUE)
  setInitValues <- function() {
    # initialize state list and reactive values
    if(testingFast) {
      # load previous state for testing
      
    } else {
      
      state_list <<- list()
      values <<- reactiveValues()
      
      # initial plot height and width
      values$plotHeight <- 650
      values$plotWidth <- 650
      
      values$modellist <- list()
      
      nilaiTabel <<- reactiveValues()
      nilaiTabel[["tmp"]] <- data.frame()
      nilaiTabel[["flag"]] <- 0
      
      nilaiTabel[["frequency"]] <- 12
      nilaiTabel[["start"]] <- 2004
      
      # Datasets can change over time (i.e. the changedata function). Therefore,
      # the data need to be a reactive value so the other reactive functions
      # and outputs that depend on these datasets will know when they are changed.
      # robj <- load("../base/data/data_init/diamonds.rda") 
      
      #       robj <- load("data/data_init/diamonds.rda") 
      #       df <- get(robj)
      #       values[["diamonds"]] <- df
      #       values[["diamonds_descr"]] <- attr(df,'description')
      #       values$datasetlist <- c("diamonds")
      robj <- load("data/data_init/IHK_TahunDasar2012.rda") 
      df <- get(robj)
      values[["IHK_TahunDasar2012"]] <- df 
      values[["IHK_TahunDasar2012_descr"]] <- attr(df,'description')
      values$datasetlist <- c("IHK_TahunDasar2012")
    }
  }
  
  setInitValues()   # using a function here so it can also be called from state.R to reset the app
  
  
  # source dari data & alat analisis
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)
})