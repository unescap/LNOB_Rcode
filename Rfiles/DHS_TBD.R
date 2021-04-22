
##### First appeared in DHS_main_functions.R
##### START OF TBD codes #####
if(! file.exists(paste(r_folder,"http_request.R",sep=""))) {
  print("http_request.R not available, TBD version can not run,  please consult user manual, create a config file and define r_folder in it")
  stop()
} else source(paste(r_folder,"http_request.R",sep=""))
##### END OF TBD codes #####

#### previously defined in DHS_main_functions
#### seems not to be used any more
insert_indicator <- function(request_body) {
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

DHS_TBD_app_indicator < -function(rv, it, indicator_list){
  indicator = list(
    name = rv,
    field_label = rv,
    field_title = rv,
    field_indicator_type = it
  )
  return(append(indicator_list, list(indicator)))
}

#### this function was used towards the end of TBD version of run_together
#### results saved in global enviroment and pushed to somewhere
DHS_TBD_mainfunction<-function(DHSKey){
  ####### last chunk of TBD codes in main functions r code
  ##### START OF TBD codes #####

  if (to_store_result_in_drupal) {
    # print(paste("Outside of loop: Tree Data for ", ds))
    # print(length(treeDataRequest))
    # print(toJSON(treeDataRequest, auto_unbox = TRUE))
    ########### Section to save tree data #####
    indicatorJson <- http_get("indicator_taxonomies")
    indicatorDf <- as.data.frame(indicatorJson)
    geoJson <- http_get("geo_taxonomies")
    geoDf <- as.data.frame(geoJson)
    treeDataJson <- http_get("tree_data")
    treeDataDf <- as.data.frame(treeDataJson)
    ### Covert from name to id for indicators and geos and then save to Drupal
    for (idx in seq_along(treeDataRequest)) {
      ### Covert indicator name to id
      currentInd = filter(indicatorDf, Name == treeDataRequest[[idx]]$field_indicator)
      if (nrow(currentInd) > 0) {
        treeDataRequest[[idx]]$field_indicator <- currentInd$tid
      }
      #####
      ### Convert actual year from version_code
      currentYear = filter(DHSKey, version_code == treeDataRequest[[idx]]$field_year, country_code == treeDataRequest[[idx]]$field_geo)
      if (nrow(currentYear) > 0) {
        treeDataRequest[[idx]]$field_year <- head(currentYear,1)$year
      }
      #####
      ### Convert geo name to id
      currentGeo = filter(geoDf, field_citf_iso2_code == treeDataRequest[[idx]]$field_geo | field_alternative_code == treeDataRequest[[idx]]$field_geo)
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
    if(length(treeDataRequest) > 0) {
      endpoint <- "node-create"
      result <- http_post(endpoint,treeDataRequest)
    }
    ##########
    ########### Section to save region tree data #####
    # print('regionTreeDataRequest')
    # print(toJSON(regionTreeDataRequest, auto_unbox = TRUE))
    if(length(regionTreeDataRequest) > 0) {
      currentGeo = filter(geoDf, field_citf_iso2_code == regionTreeDataRequest[[1]]$field_geo | field_alternative_code == regionTreeDataRequest[[idx]]$field_geo)
      print('CURRENT GEO------------------------------')
      print(currentGeo$tid)
      # print(paste("region_tree_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
      regionTreeDataJson <- http_get(paste("region_tree_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
      # print(regionTreeDataJson)
      regionTreeDataDf <- as.data.frame(regionTreeDataJson)
      # print(nrow(regionTreeDataDf))
    }
    ### Covert from name to id for indicators and geos and then save to Drupal
    for (idx in seq_along(regionTreeDataRequest)) {
      ### Covert indicator name to id
      currentInd = filter(indicatorDf, Name == regionTreeDataRequest[[idx]]$field_indicator)
      if (nrow(currentInd) > 0) {
        regionTreeDataRequest[[idx]]$field_indicator <- currentInd$tid
      }
      #####
      ### Convert actual year from version_code
      currentYear = filter(DHSKey, version_code == regionTreeDataRequest[[idx]]$field_year, country_code == regionTreeDataRequest[[idx]]$field_geo)
      if (nrow(currentYear) > 0) {
        regionTreeDataRequest[[idx]]$field_year <- head(currentYear,1)$year
      }
      #####
      ### Convert geo name to id
      currentGeo = filter(geoDf, field_citf_iso2_code == regionTreeDataRequest[[idx]]$field_geo | field_alternative_code == regionTreeDataRequest[[idx]]$field_geo)
      if (nrow(currentGeo) > 0) {
        regionTreeDataRequest[[idx]]$field_geo <- currentGeo$tid
      }
      #####
      ### Check existing tree data
      if (nrow(regionTreeDataDf) > 0) {
        currentTD = filter(regionTreeDataDf, field_region == regionTreeDataRequest[[idx]]$field_region, field_indicator == regionTreeDataRequest[[idx]]$field_indicator, field_geo == regionTreeDataRequest[[idx]]$field_geo, field_year == regionTreeDataRequest[[idx]]$field_year)
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
    # print(toJSON(regionTreeDataRequest, auto_unbox = TRUE))
    ### Not to have deadlock in Drupal Queue
    Sys.sleep(20)
    if(length(regionTreeDataRequest) > 0) {
      endpoint <- "node-create"
      result <- http_post(endpoint,regionTreeDataRequest)
    }
    ##########
    ########### Section to save d-index data #####
    dIndexDataJson <- http_get("d_index_data")
    dIndexDataDf <- as.data.frame(dIndexDataJson)
    ### Covert from name to id for geos and then save to Drupal
    for (idx in seq_along(dIndexDataRequest)) {
      ### Convert actual year from version_code
      currentYear = filter(DHSKey, version_code == dIndexDataRequest[[idx]]$field_year, country_code == dIndexDataRequest[[idx]]$field_geo)
      if (nrow(currentYear) > 0) {
        dIndexDataRequest[[idx]]$field_year <- head(currentYear,1)$year
      }
      #####
      ### Convert geo name to id
      currentGeo = filter(geoDf, field_citf_iso2_code == dIndexDataRequest[[idx]]$field_geo | field_alternative_code == dIndexDataRequest[[idx]]$field_geo)
      if (nrow(currentGeo) > 0) {
        dIndexDataRequest[[idx]]$field_geo <- currentGeo$tid
      }
      #####
      ### Covert indicator name to id
      currentInd = filter(indicatorDf, Name == dIndexDataRequest[[idx]]$field_indicator)
      if (nrow(currentInd) > 0) {
        dIndexDataRequest[[idx]]$field_indicator <- currentInd$tid
      }
      #####
      ### Check existing d-index data
      if (nrow(dIndexDataDf) != 0) {
        currentDD = filter(dIndexDataDf, field_indicator == dIndexDataRequest[[idx]]$field_indicator, field_geo == dIndexDataRequest[[idx]]$field_geo, field_year == dIndexDataRequest[[idx]]$field_year)
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
    ### Not to have deadlock in Drupal Queue
    Sys.sleep(20)
    if(length(dIndexDataRequest) > 0) {
      endpoint <- "node-create"
      result <- http_post(endpoint,dIndexDataRequest)
    }
    ########### Section to save Logit data #####
    # print(toJSON(logitDataRequest, auto_unbox = TRUE))
    logitDataJson <- http_get("logit_data")
    logitDataDf <- as.data.frame(logitDataJson)
    ### Covert from name to id for geos and then save to Drupal
    for (idx in seq_along(logitDataRequest)) {
      ### Convert actual year from version_code
      currentYear = filter(DHSKey, version_code == logitDataRequest[[idx]]$field_year, country_code == logitDataRequest[[idx]]$field_geo)
      if (nrow(currentYear) > 0) {
        logitDataRequest[[idx]]$field_year <- head(currentYear,1)$year
      }
      #####
      ### Convert geo name to id
      currentGeo = filter(geoDf, field_citf_iso2_code == logitDataRequest[[idx]]$field_geo | field_alternative_code == logitDataRequest[[idx]]$field_geo)
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
    ### Not to have deadlock in Drupal Queue
    Sys.sleep(20)
    if(length(logitDataRequest) > 0) {
      endpoint <- "node-create"
      result <- http_post(endpoint,logitDataRequest)
    }
    # #####
    ########### Section to save region dindex data #####
    if(length(regionDIndexRequest) > 0) {
      currentGeo = filter(geoDf, field_citf_iso2_code == regionDIndexRequest[[1]]$field_geo | field_alternative_code == regionDIndexRequest[[1]]$field_geo)
      # print(currentGeo)
      # print(paste("region_tree_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
      regionDIndexDataJson <- http_get(paste("region_d_index_data?field_geo_target_id=",as.integer(currentGeo$tid), sep=""))
      # print(regionDIndexDataJson)
      regionDIndexDataDf <- as.data.frame(regionDIndexDataJson)
      # print(nrow(regionDIndexDataDf))
    }
    ### Covert from name to id for indicators and geos and then save to Drupal
    # print(toJSON(regionDIndexRequest, auto_unbox = TRUE))
    for (idx in seq_along(regionDIndexRequest)) {
      ### Covert indicator name to id
      currentInd = filter(indicatorDf, Name == regionDIndexRequest[[idx]]$field_indicator)
      if (nrow(currentInd) > 0) {
        regionDIndexRequest[[idx]]$field_indicator <- currentInd$tid
      }
      #####
      ### Convert actual year from version_code
      currentYear = filter(DHSKey, version_code == regionDIndexRequest[[idx]]$field_year, country_code == regionDIndexRequest[[idx]]$field_geo)
      if (nrow(currentYear) > 0) {
        regionDIndexRequest[[idx]]$field_year <- head(currentYear,1)$year
      }
      #####
      ### Convert geo name to id
      currentGeo = filter(geoDf, field_citf_iso2_code == regionDIndexRequest[[idx]]$field_geo | field_alternative_code == regionDIndexRequest[[idx]]$field_geo)
      if (nrow(currentGeo) > 0) {
        regionDIndexRequest[[idx]]$field_geo <- currentGeo$tid
      }
      #####
      ### Check existing tree data
      if (nrow(regionDIndexDataDf) != 0) {
        currentDI = filter(regionDIndexDataDf, field_region == regionDIndexRequest[[idx]]$field_region, field_indicator == regionDIndexRequest[[idx]]$field_indicator, field_geo == regionDIndexRequest[[idx]]$field_geo, field_year == regionDIndexRequest[[idx]]$field_year)
        if (nrow(currentDI) > 0) {
          regionDIndexRequest[[idx]]$nid <- head(currentDI,1)$nid
          regionDIndexRequest[[idx]]$moderation_state <- "draft"
          # regionDIndexRequest[[idx]]$title <- paste(regionDIndexRequest[[idx]]$title,' v2',sep = "")
        }
      }
      #####
    }
    # print(toJSON(regionDIndexRequest, auto_unbox = TRUE))
    Sys.sleep(20)
    endpoint <- "node-create"
    result <- http_post(endpoint,regionDIndexRequest)
    ##########
  }
  ##### END OF TBD codes #####
}


##### following functions were originally in various places of the write/analysis functions
DHS_TBD_Writeglm<-function(s.glm, output_folder, title_string, country_code, version_code)
  
{
  
  ##### START OF TBD codes #####
  logitList = list();
  for (r in 1:nrow(s.glm)) {
    oneLogit = list()
    oneLogit["indicator"] = row.names(s.glm)[r];
    for (c in 1:ncol(s.glm)){
      oneLogit[[colnames(s.glm)[c]]] = s.glm[r,c]
    }
    logitList <- append(logitList, list(oneLogit))
  }
  # print(toJSON(logitList, auto_unbox = TRUE))
  
  one_logit_data = list(
    type = "logit",
    field_survey_type = "DHS",
    field_dataset = basename(output_folder),
    title = title_string,
    field_geo = country_code,
    field_year = version_code,
    field_data = toJSON(logitList, auto_unbox = TRUE)
  )
  assign("logitDataRequest", append(logitDataRequest, list(one_logit_data)), envir = .GlobalEnv)
}



### used in write_HOI_D function
DHS_TBD_WriteDindex<-function(output_folder, title_string, country_code, version_code, rv, result){
  one_d_data = list(
    type = "d_index",
    field_survey_type = "DHS",
    field_dataset = basename(output_folder),
    title = title_string,
    field_geo = country_code,
    field_year = version_code,
    field_indicator = rv,
    field_data = toString(result)
  )
  # request_body = list(one_d_data)
  assign("dIndexDataRequest", append(dIndexDataRequest, list(one_d_data)), envir = .GlobalEnv)
  
}


#### used in build_tree function
DHS_TBD_WriteTree<-function(data2, source_folder, country_code, version_code, Response_var){
  allTreeJson <- toJSON(data2, flatten = TRUE)
  one_tree_data = list(
    type = "tree_data",
    field_survey_type = "DHS",
    field_dataset = basename(source_folder),
    title = paste(country_code,Response_var,version_code, sep = " "),
    field_geo = country_code,
    field_year = version_code,
    field_indicator = Response_var,
    field_data = toString(allTreeJson)
  )
  # print(one_tree_data)
  assign("treeDataRequest", append(treeDataRequest, list(one_tree_data)), envir = .GlobalEnv)
}
