library(dplyr)


source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_uploaddrupal.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  




datafolder<-paste(source_folder,"drupalData/20210513230614/",sep="")
### it is by design that this data folder name change everytime
### you can have a fixed folder name and change it on your drive, or change the 
### name here accordingly


#### may not need this function
# collectData<-function(datafolder){
#   data_list<-list.files(datafolder, ".rds")
#   for(dn in data_list) {
#     print(paste(datafolder, dn, sep=""))
#     dt<-readRDS(paste(datafolder, dn, sep=""))
#     # print(dt)
#     print(dt[c(1:7)])
#   }
#   return(data_list)
# }
# 
# data_for_drupa<-collectData(datafolder)
#   
  
  
### plan: to create one .rds file for each result, with a data structure that describe it
###       save all of the r data in one folder (created each time we start running use_version 3 in run_together)
###       pick them up and push to drupal one by one by this script
###       resultIndex is created by run_together, its sole purpose is for this publication process. We just want a unique name 
###       for each of the R data so that we can pick them up from the folder

gettingDrupalFiles<-function(api_base, key){
  
  #### getting durpal server files
  indicatorJson <- http_get("indicator_taxonomies", api_base, key)
  indicatorDf <- as.data.frame(indicatorJson)
  
  geoJson <- http_get("geo_taxonomies", api_base, key)
  geoDf <- as.data.frame(geoJson)
  
  treeDataJson <- http_get("tree_data", api_base, key)
  treeDataDf <- as.data.frame(treeDataJson)

  dIndexDataJson <- http_get("d_index_data", api_base, key)
  dIndexDataDf <- as.data.frame(dIndexDataJson)
  
  logitDataJson <- http_get("logit_data", api_base, key)
  logitDataDf <- as.data.frame(logitDataJson)

  # #### organize regional taxonomy files, not ready yet   
  #### end getting durpal server files
 
  return(list( indicatorDf=indicatorDf,
               geoDf=geoDf, 
               treeDataDf=treeDataDf,
               dIndexDataDf=dIndexDataDf,
               logitDataDf=logitDataDf
               #### regional df not ready
         )) 
}
  #### getting the list of results for publication

  



drupalPush<-function(dt, drupalFiles, api_base, key){
  indicatorDf<-drupalFiles$indicatorDf
  geoDf<-drupalFiles$geoDf
  treeDataDf<-drupalFiles$treeDataDf
  dIndexDataDf<-drupalFiles$dIndexDataDf
  logitDataDf<-drupalFiles$logitDataDf
  
    ### Covert indicator name to id
    # print(colnames(indicatorDf))
    currentInd = filter(indicatorDf, Name == dt$field_indicator)
    if (nrow(currentInd) > 0) {
      dt$field_indicator <- currentInd$tid
    }
    else {
      result<-"indicatorNotFound"
      return(result)
    }
    #####
    ### Convert actual year from version_code
    # skip this because we use the year given in the run_together
    # currentYear = filter(DHSKey, version_code == treeDataRequest[[idx]]$field_year, country_code == treeDataRequest[[idx]]$field_geo)
    # if (nrow(currentYear) > 0) {
    #   treeDataRequest[[idx]]$field_year <- head(currentYear,1)$year
    # }
    #####
    ### Convert geo name to id
    currentGeo = filter(geoDf, field_citf_iso3_code == dt$field_geo | field_alternative_code == dt$field_geo)
    if (nrow(currentGeo) > 0) {
      dt$field_geo <- currentGeo$tid
    }
    else {
      result<-"countryNotFound"
      return(result)
    }
    #####
    ### Check if it is tree data
    if(dt$type=="tree_data") {
      if (nrow(treeDataDf) > 0) {
        currentTD = filter(treeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, field_year == dt$field_year)
        if (nrow(currentTD) > 0) {
          # print('current tree id')
          # print(head(currentTD,1)$nid)
          dt$nid <- head(currentTD,1)$nid
          dt$moderation_state <- "draft"
          Sys.sleep(20)
          endpoint <- "node-create"
          result <- http_post(endpoint,dt, api_base, key)
          # result<-endpoint
          # treeDataRequest[[idx]]$title <- paste(treeDataRequest[[idx]]$title,' v2',sep = "")
        } 
        else result<-"currentTD_empty"
      }
      else result<-"treeDataDf_empty"
    }
  ##########
  # check if it is region tree data (skip for now)
  # if(length(regionTreeDataRequest) > 0) 

  ########### Section to save d-index data #####
   else if(dt$type=="d_index") {
    if (nrow(dIndexDataDf) != 0) {
      currentDD = filter(dIndexDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, field_year == dt$field_year)
      if (nrow(currentDD) != 0) {
        # print('current d-index id')
        # print(head(currentDD,1)$nid)
        dt$nid <- head(currentDD,1)$nid
        dt$moderation_state <- "draft"
        # dIndexDataRequest[[idx]]$title <- paste(dIndexDataRequest[[idx]]$title,' v2',sep = "")
        # Sys.sleep(20)
        endpoint <- "node-create"
        result <- http_post(endpoint,dt, api_base, key)
        # result<-endpoint
      }
      else result<-"currentTD_empty"
    }
    else result<-"treeDataDf_empty"
    #####
  }
  ### Not to have deadlock in Drupal Queue
 
  ########### Section to save Logit data #####
  else if(dt$type=="logit") {
    if (nrow(logitDataDf) != 0) {
      currentLD = filter(logitDataDf, title == dt$title, field_geo == dt$field_geo, field_year == dt$field_year)
      if (nrow(currentLD) != 0) {
        # print('current logit data id')
        # print(head(currentLD,1)$nid)
        dt$nid <- head(currentLD,1)$nid
        dt$moderation_state <- "draft"
        # logitDataRequest[[idx]]$title <- paste(logitDataRequest[[idx]]$title,' v2',sep = "")
        Sys.sleep(20)
        endpoint <- "node-create"
        result <- http_post(endpoint,dt, api_base, key)
        # result<-endpoint
      }
      else result<-"currentTD_empty"
    }
      else result<-"treeDataDf_empty"
    #####
  }
    
  else result("typeNotFound")

  # #####
  ########### Section to save region dindex data #####
  ### not ready ###  
  return(result) # return the results of the push
}


push_together<-function(resultFolder, api_base, key){
  data_list<-list.files(datafolder, ".rds")

  # print(drupalFiles$indicatorDf)
  if(length(data_list)==0) {
    print("No Data Found")
      return()
  }

  logcsv<-paste(resultFolder, "validation/pushlogfile.csv", sep="")
  drupalFiles<-gettingDrupalFiles(api_base, key)
  for(dn in data_list){
    dt<-readRDS(paste(datafolder, dn, sep=""))
    pushresult<-drupalPush(dt, drupalFiles, api_base, key)
    #pushresult<-1
    pushresult<-data.frame(resultfile=dn, dt[(1:7)], PushResult=pushresult)
    if(file.exists(logcsv))
      write.table(pushresult, logcsv, sep=",", 
                  append = TRUE,   col.names = F, row.names = F)
    else write.table(pushresult, logcsv, sep=",", 
                     append = FALSE,   col.names = T, row.names = F)
  }
  
  
}
push_together(datafolder, api_base, key)














