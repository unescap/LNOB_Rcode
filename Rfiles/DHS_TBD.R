
##### First appeared in DHS_main_functions.R
##### START OF TBD codes #####
  source(paste(source_folder,"http_request.R",sep=""))

  treeDataRequest <<- list()
  dIndexDataRequest <<- list()
  logitDataRequest <<- list()
  regionTreeDataRequest <<- list()
  regionDIndexRequest <<- list()
##### END OF TBD codes #####


#### previously defined in DHS_main_functions
#### seems not to be used any more
insert_indicator <- function(request_body){
  ## insert to drupal
  print(c('INSERT INDICATOR:'))
  endpoint <- "taxonomy/indicator/terms/upsert"
  result <- http_post(endpoint, request_body)
  print(c('RESULT', result))
}
  
  ##### START OF TBD codes #####
  assign("treeDataRequest", list(), envir = .GlobalEnv)
  assign("dIndexDataRequest", list(), envir = .GlobalEnv)
  assign("logitDataRequest", list(), envir = .GlobalEnv)
  assign("regionTreeDataRequest", list(), envir = .GlobalEnv)
  assign("regionDIndexRequest", list(), envir = .GlobalEnv)
  ##### END OF TBD codes #####
  
  
  indicator_list <- list()