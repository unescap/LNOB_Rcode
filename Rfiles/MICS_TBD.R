if(! exists(paste(r_folder,"MICS_http_request.R",sep=""))) {
  print("http_request.R not available, TBD version can not run,  please consult user manual, create a config file and define r_folder in it")
  stop()
} else source(paste(r_folder,"MICS_http_request.R",sep=""))

# original R code MICS_main_functions.R lines 76-89
treeDataRequest <<- list()
dIndexDataRequest <<- list()
logitDataRequest <<- list()
regionTreeDataRequest <<- list()

MICS_TBD_insert_indicator <- function(request_body){
  ## insert to drupal
  print(c('INSERT INDICATOR:'))
  endpoint <- "taxonomy/indicator/terms/upsert"
  result <- http_post(endpoint, request_body)
  print(c('RESULT', result))
}

# original R code MICS_main_functions.R lines 113
indicator_list <- list()


# original R code MICS_main_functions.R lines 186-192
MICS_TBD_app_indicator<-function(rv, it, indicator_list){
  indicator = list(
      name = rv,
      field_label = rv,
      field_title = rv,
      field_indicator_type = it
  )
  return(append(indicator_list, list(indicator)))
}





MICS_TBD_WriteTree<-function(data2, source_folder, country_code, version_code, Response_var){
  
  allTreeJson <- toJSON(data2, flatten = TRUE)
  #print(allTreeJson)
  
  # indicatorsRes <- GET(paste(base,endpoint, sep=""))
  # indicatorJsonText = content(indicatorsRes, "text")#'[{"tid":"1","name":"CleanWater"},{"tid":"4","name":"HouseholdTechNeed"},{"tid":"3","name":"NotCrowded"}]'
  # indicatorJson <- http_get("indicator_taxonomies") #fromJSON(txt=indicatorJsonText, flatten = TRUE)
  # print(indicatorJson)
  # indicatorDf <- as.data.frame(indicatorJson)
  
  # currentInd = filter(indicatorDf, Name == Response_var)
  # print("Ind Tid")
  # print(currentInd$tid)
  
  one_tree_data = list(
    type = "tree_data",
    field_survey_type = "MICS",
    field_dataset = basename(source_folder),
    title = paste(country_code,Response_var,version_code, sep = " "),
    field_geo = country_code,
    field_year = version_code,
    field_indicator = Response_var,
    field_data = toString(allTreeJson)
  )
# request_body = list(one_tree_data)
# print(treeDataRequest);
# treeDataRequest <- append(treeDataRequest, one_tree_data)
  assign("treeDataRequest", append(treeDataRequest, list(one_tree_data)), envir = .GlobalEnv)

}



MICS_TBD_WriteDindex<-function(output_folder, title_string, country_code, version_code, result)
{
##### START OF TBD codes #####
  one_d_data = list(
    type = "d_index",
    field_survey_type = "MICS",
    field_dataset = basename(output_folder),
    title = title_string,
    field_geo = country_code,
    field_year = version_code,
    field_data = toString(result)
  )
# request_body = list(one_d_data)
  assign("dIndexDataRequest", append(dIndexDataRequest, list(one_d_data)), envir = .GlobalEnv)
# print(request_body)
# endpoint <- "node-create"
# result <- http_post(endpoint,request_body)
# Output <- content(result)
# Output
##### END OF TBD codes #####

}

MICS_TBD_Writeglm(s.glm, output_folder, title_string, country_code, version_code)
{
logitList = list();
if(!is.null(s.glm)) {
  for (r in 1:nrow(s.glm)) {
    oneLogit = list()
    # print(row.names(s.glm)[r])
    oneLogit["indicator"] = row.names(s.glm)[r];
    for (c in 1:ncol(s.glm)){
      # print(colnames(s.glm)[c])
      # print(paste("Row", r, "and column",c, "have values of", s.glm[r,c]))
      oneLogit[[colnames(s.glm)[c]]] = s.glm[r,c]
    }
    logitList <- append(logitList, list(oneLogit))
  }
}

# print(toJSON(logitList, auto_unbox = TRUE))

one_logit_data = list(
  type = "logit",
  field_survey_type = "MICS",
  field_dataset = basename(output_folder),
  title = title_string,
  field_geo = country_code,
  field_year = version_code,
  field_data = toJSON(logitList, auto_unbox = TRUE)
)
assign("logitDataRequest", append(logitDataRequest, list(one_logit_data)), envir = .GlobalEnv)
##### END OF TBD codes #####
}


# original R code MICS_main_functions.R lines 258-404

#### this function was used towards the end of TBD version of run_together
#### results saved in global enviroment and pushed to somewhere

MICS_TBD_mainfunction<-function(indicator_list){
if (to_store_result_in_drupal) {
  MICS_TBD_insert_indicator(indicator_list)
  ########### Section to save tree data #####
  indicatorJson <- http_get("indicator_taxonomies")
  indicatorDf <- as.data.frame(indicatorJson)
  geoJson <- http_get("geo_taxonomies")
  geoDf <- as.data.frame(geoJson)
  treeDataJson <- http_get("tree_data")
  treeDataDf <- as.data.frame(treeDataJson)
  # print(treeDataDf)
  ### Covert from name to id for indicators and geos and then save to Drupal
  for (idx in seq_along(treeDataRequest)) {
    ### Covert indicator name to id
    # print(treeDataRequest[[idx]]$field_indicator)
    currentInd = filter(indicatorDf, Name == treeDataRequest[[idx]]$field_indicator)
    # print(currentInd$tid)
    if (nrow(currentInd) > 0) {
      treeDataRequest[[idx]]$field_indicator <- currentInd$tid
    }
    #####
    ### Convert geo name to id
    currentGeo = filter(geoDf, tolower(name) == tolower(treeDataRequest[[idx]]$field_geo))
    if (nrow(currentGeo) > 0) {
      treeDataRequest[[idx]]$field_geo <- currentGeo$tid
    }
    #####
    ### Check existing tree data
    
    if (nrow(treeDataDf) > 0) {
      currentTD = filter(treeDataDf, field_indicator == treeDataRequest[[idx]]$field_indicator, field_geo == treeDataRequest[[idx]]$field_geo, field_year == treeDataRequest[[idx]]$field_year)
      if (nrow(currentTD) > 0) {  
        print('current tree id')
        print(head(currentTD,1)$nid)
        treeDataRequest[[idx]]$nid <- head(currentTD,1)$nid
        treeDataRequest[[idx]]$moderation_state <- "draft"
        # treeDataRequest[[idx]]$title <- paste(treeDataRequest[[idx]]$title,' v2',sep = "")
      }
    }
    #####
  }
  # print(toJSON(treeDataRequest, auto_unbox = TRUE))
  endpoint <- "node-create"
  result <- http_post(endpoint,treeDataRequest)
  Sys.sleep(20)
  ##########
  ########### Section to save d-index data #####
  dIndexDataJson <- http_get("d_index_data")
  dIndexDataDf <- as.data.frame(dIndexDataJson)
  ### Covert from name to id for geos and then save to Drupal
  for (idx in seq_along(dIndexDataRequest)) {
    ### Convert geo name to id
    currentGeo = filter(geoDf, tolower(name) == tolower(dIndexDataRequest[[idx]]$field_geo))
    if (nrow(currentGeo) > 0) {
      dIndexDataRequest[[idx]]$field_geo <- currentGeo$tid
    }
    #####
    ### Check existing d-index data
    if (nrow(dIndexDataDf) != 0) {
      currentDD = filter(dIndexDataDf, title == dIndexDataRequest[[idx]]$title, field_geo == dIndexDataRequest[[idx]]$field_geo, field_year == dIndexDataRequest[[idx]]$field_year)
      if (nrow(currentDD) != 0) {
        print('current d-index id')
        print(head(currentDD,1)$nid)
        dIndexDataRequest[[idx]]$nid <- head(currentDD,1)$nid
        dIndexDataRequest[[idx]]$moderation_state <- "draft"
        # dIndexDataRequest[[idx]]$title <- paste(dIndexDataRequest[[idx]]$title,' v2',sep = "")
      }
    }
    #####
  }
  endpoint <- "node-create"
  result <- http_post(endpoint,dIndexDataRequest)
  Sys.sleep(20)
  #####
  ########### Section to save Logit data #####
  # print(toJSON(logitDataRequest, auto_unbox = TRUE))
  logitDataJson <- http_get("logit_data")
  logitDataDf <- as.data.frame(logitDataJson)
  ### Covert from name to id for geos and then save to Drupal
  for (idx in seq_along(logitDataRequest)) {
    ### Convert geo name to id
    currentGeo = filter(geoDf, tolower(name) == tolower(logitDataRequest[[idx]]$field_geo))
    if (nrow(currentGeo) > 0) {
      logitDataRequest[[idx]]$field_geo <- currentGeo$tid
    }
    #####
    ### Check existing logit data
    if (nrow(logitDataDf) != 0) {
      currentLD = filter(logitDataDf, title == logitDataRequest[[idx]]$title, field_geo == logitDataRequest[[idx]]$field_geo, field_year == logitDataRequest[[idx]]$field_year)
      if (nrow(currentLD) != 0) {
        print('current logit data id')
        print(head(currentLD,1)$nid)
        logitDataRequest[[idx]]$nid <- head(currentLD,1)$nid
        logitDataRequest[[idx]]$moderation_state <- "draft"
        # logitDataRequest[[idx]]$title <- paste(logitDataRequest[[idx]]$title,' v2',sep = "")
      }
    }
    #####
  }
  endpoint <- "node-create"
  result <- http_post(endpoint,logitDataRequest)
  Sys.sleep(20)
  #####
  ########### Section to save region tree data #####
  if(length(regionTreeDataRequest) > 0) {
    currentGeo = filter(geoDf, tolower(name) == tolower(regionTreeDataRequest[[idx]]$field_geo))
    print(currentGeo)
    # print(paste("region_tree_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
    regionTreeDataJson <- http_get(paste("region_tree_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
    # print(regionTreeDataJson)
    regionTreeDataDf <- as.data.frame(regionTreeDataJson)
    # print(nrow(regionTreeDataDf))
  }
  ### Covert from name to id for indicators and geos and then save to Drupal
  # print(toJSON(regionTreeDataRequest, auto_unbox = TRUE))
  for (idx in seq_along(regionTreeDataRequest)) {
    ### Covert indicator name to id
    # print(regionTreeDataRequest[[idx]]$field_indicator)
    currentInd = filter(indicatorDf, Name == regionTreeDataRequest[[idx]]$field_indicator)
    # print(currentInd$tid)
    if (nrow(currentInd) > 0) {
      regionTreeDataRequest[[idx]]$field_indicator <- currentInd$tid
    }
    #####
    ### Convert geo name to id
    currentGeo = filter(geoDf, tolower(name) == tolower(regionTreeDataRequest[[idx]]$field_geo))
    if (nrow(currentGeo) > 0) {
      regionTreeDataRequest[[idx]]$field_geo <- currentGeo$tid
    }
    #####
    ### Check existing tree data
    if (nrow(regionTreeDataDf) != 0) {
      currentTD = filter(regionTreeDataDf, field_indicator == regionTreeDataRequest[[idx]]$field_indicator, field_geo == regionTreeDataRequest[[idx]]$field_geo, field_year == regionTreeDataRequest[[idx]]$field_year)
      if (nrow(currentTD) > 0) {  
        # print('current tree id')
        # print(head(currentTD,1)$nid)
        regionTreeDataRequest[[idx]]$nid <- head(currentTD,1)$nid
        regionTreeDataRequest[[idx]]$moderation_state <- "draft"
        # regionTreeDataRequest[[idx]]$title <- paste(regionTreeDataRequest[[idx]]$title,' v2',sep = "")
      }
    }
    #####
  }
  print(toJSON(regionTreeDataRequest, auto_unbox = TRUE))
  endpoint <- "node-create"
  result <- http_post(endpoint,regionTreeDataRequest)
  ##########
}
}
 