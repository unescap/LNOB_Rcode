# this r code update drupal server 100 nodes at a time

library(dplyr)
library(fun)

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

# pubDatafolder<-paste(data_folder,"drupalData20210604version/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataLao20210707/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDatatesting/",sep="")

# pubDatafolder<-paste(data_folder,"drupalDataMongolia2018Secondary20210818/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataTongaUpdate20210819/",sep="") 
# pubDatafolder<-paste(data_folder,"drupalDataMongolia2013Secondary20210820/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataFinancialInclusion20210819/",sep="")

# pubDatafolder<-paste(data_folder,"drupalDataCovid20210915/",sep="") # need "covid added to indicator taxonomy"
# pubDatafolder<-paste(data_folder,"drupalDataECE20210916/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataInvestigated20210830/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataMongolia2018BasicWater20210831/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataNonBarrierDV/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataMICSEdufix20211022/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDataNoSexualVDHS20211022/",sep="")
# pubDatafolder<-paste(data_folder,"drupalData20211202TONreligionTrial/",sep="")
# pubDatafolder<-paste(data_folder,"drupalData20211202LoadtoDevSite/",sep="")
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData202201039newcases/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220304violencedatagap/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220321NEET/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220323MICSVAWgap/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220324Samoa2019/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220330ReloadAll/"
# pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220629India2020/"
pubDatafolder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20220718India2020Contraceptive/"



### plan: to create one .rds file for each result, with a data structure that describe it
###       save all of the r data in one folder (created each time we start running use_version 3 in run_together)
###       pick them up and push to drupal one by one by this script
###       resultIndex is created by run_together, its sole purpose is for this publication process. We just want a unique name 
###       for each of the R data so that we can pick them up from the folder

gettingDrupalFiles<-function(api_base, key){
  ### getting durpal server files
  indicatorJson <- http_get("indicator_taxonomies", api_base, key)
  indicatorDf <- as.data.frame(indicatorJson)
  
  geoJson <- http_get("geo_taxonomies", api_base, key)
  geoDf <- as.data.frame(geoJson)
  
  treeDataJson <- http_get("tree_data", api_base, key)
  treeDataDf <- as.data.frame(treeDataJson)
  
  dIndexDataJson <- http_get("d_index_data", api_base, key)
  dIndexDataDf <- as.data.frame(dIndexDataJson)
  
  
  geoid<-unique(treeDataDf$field_geo)
  
  geoid<-c('121')  # Afhan
  n<-0
  if(!is.null(geoid)){
    for(tid in geoid){
      print(tid)
      regionTreeDataJson <- http_get(paste("region_tree_data?geo_id=", as.integer(tid), sep=""), api_base, key)
      # print(regionTreeDataJson)
      ifelse (n==0,
              regionTreeDataDf<-as.data.frame(regionTreeDataJson),
              regionTreeDataDf <- rbind(regionTreeDataDf, as.data.frame(regionTreeDataJson)))
      
      regionDDataJson <- http_get(paste("region_d_index_data?geo_id=", as.integer(tid), sep=""), api_base, key)
      # print(regionDDataJson)
      ifelse(n==0,
             regionDDataDf <- as.data.frame(regionDDataJson),
             regionDDataDf <- rbind(regionDDataDf, as.data.frame(regionDDataJson)))
      n<-n+1
    }
  }
  else {
    regionTreeDataDf<-NULL
    regionDDataDf<-NULL
  }
  # #### organize regional taxonomy files, not ready yet   
  #### end getting durpal server files
  
  return(list( 
    indicatorDf=indicatorDf,
    geoDf=geoDf,
    treeDataDf=treeDataDf,
    dIndexDataDf=dIndexDataDf,
    regionTreeDataDf=regionTreeDataDf,
    regionDDataDf=regionDDataDf
  )) 
}
#### getting the list of results for publication





drupalPush<-function(dt, drupalFiles, api_base, key){
  ### data needed from web
  indicatorDf<-drupalFiles$indicatorDf
  geoDf<-drupalFiles$geoDf
  treeDataDf<-drupalFiles$treeDataDf
  dIndexDataDf<-drupalFiles$dIndexDataDf
  regionTreeDataDf=drupalFiles$regionTreeDataDf
  regionDDataDf=drupalFiles$regionDDataDf
  
  ### Covert indicator name to id
  # print(colnames(indicatorDf))
  currentInd = filter(indicatorDf, Name == dt$field_indicator)
  if (nrow(currentInd) > 0) {
    dt$field_indicator <- currentInd$tid
  }
  else {
    result<-"indicatorNotFound"
    return(list(result=result, drupalData=dt))
  }
  
  currentGeo = filter(geoDf, field_citf_iso3_code == dt$field_geo | field_alternative_code == dt$field_geo)
  if (nrow(currentGeo) > 0) {
    dt$field_geo <- currentGeo$tid
  }
  else {
    result<-"countryNotFound"
    return(list(result=result, drupalData=dt))
  }
  #####
  ### Check if it already on the server
  if(dt$type=="tree_data") {
    if (nrow(treeDataDf) > 0) {
      # dt$title<-htmlspecialchars(dt$title)
      currentDD = filter(treeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                         field_year == dt$field_year, field_is_experimental == dt$field_is_experimental, field_circumstances == toString(dt$field_circumstances))
      if (nrow(currentDD) != 0) {
        dt$nid <- head(currentDD,1)$nid
        dt$moderation_state<- head(currentDD,1)$moderation_state
      }
    }
  }
  ### check if 
  else if(dt$type=="d_index") {
    if (nrow(dIndexDataDf) != 0) {
      # dt$title<-htmlspecialchars(dt$title)
      currentDD = filter(dIndexDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                         field_year == dt$field_year, field_is_experimental == dt$field_is_experimental, field_circumstances == toString(dt$field_circumstances))
      if (nrow(currentDD) != 0) {
        dt$nid <- head(currentDD,1)$nid
        dt$moderation_state<- head(currentDD,1)$moderation_state
      }
    }
  }
  else if(dt$type=="region_d_index") {
    if (!is.null(regionDDataDf)) {
      if(nrow(regionDDataDf) != 0) {
        # dt$title<-htmlspecialchars(dt$title)
        # dt$field_region<-htmlspecialchars(dt$field_region)
        # print(paste(dt$field_region))
        currentDD = filter(regionDDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, field_region == dt$field_region, field_is_experimental == dt$field_is_experimental, field_circumstances == toString(dt$field_circumstances))
        if (nrow(currentDD) != 0) {
          dt$nid <- head(currentDD,1)$nid
          # print(dt$nid)
          dt$moderation_state<- head(currentDD,1)$moderation_state
        }
      }
    }
  }
  else if(dt$type=="region_tree_data") {
    if (!is.null(regionTreeDataDf)){
      if(nrow(regionTreeDataDf) != 0) {
        # dt$title<-htmlspecialchars(dt$title)
        # dt$field_region<-htmlspecialchars(dt$field_region)
        currentDD = filter(regionTreeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, field_region == dt$field_region, field_is_experimental == dt$field_is_experimental, field_circumstances == toString(dt$field_circumstances))
        if (nrow(currentDD) != 0) {
          dt$nid <- head(currentDD,1)$nid
          # print(dt$nid)
          dt$moderation_state<- head(currentDD,1)$moderation_state
        }
      }
    }
  }
  # dt$moderation_state <- "Published"
  
  
  return(dt)
}


push_together<-function(resultFolder, drupalFiles, api_base, key){
  data_list<-list.files(resultFolder, ".rds")
  if(length(data_list)==0) {
    print("No Data Found")
    return()
  }
  
  dt0<-list()
  ct<-30
  num<-0
  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    # print(dt)
    # if (dt$field_geo!="KHM") next;
    ### these indicators are in the validation data, but not in drupal indicator table
    
    dtDrupal<-drupalPush(dt, drupalFiles, api_base, key)
    dtDrupal$moderation_state <- "draft"
    dt0<-append(dt0, list(dtDrupal))
    
    num<-num+1
    if(!is.null(dt0[[num]]$result)) {
      print('Non-existing indicator or country: ')
      print(dt0[[num]]$drupalData$field_indicator)
      print(dt0[[num]]$drupalData$field_geo)
    }
    
    if(length(dt0)==30){
      
      print("posting now ----")
      Sys.sleep(1)
      print(ct)
      endpoint <- "node-create"
      result <- http_post(endpoint,dt0, api_base, key)
      print(result)
      dt0<-list()
      ct<-ct+30
      num<-0
      print(paste("------", ct, "--------posted" ))
    }
  }
  
  if(length(dt0)>0) {
    print("posting last batch ----")
    Sys.sleep(5)
    print(ct)
    endpoint <- "node-create"
    # print(dt0)
    result <- http_post(endpoint,dt0, api_base, key)
    print(result)
    
  }
}

checkingDrupalFiles<-function(drupalFiles, comm_vars){
  
  treeDataDf <- drupalFiles$treeDataDf
  treeDataDf$drupalTableName<-"tree_data"
  treeDataDf<-treeDataDf[, colnames(treeDataDf) %in% comm_vars]
  
  dIndexDataDf <- drupalFiles$dIndexDataDf
  dIndexDataDf$drupalTableName <-"d_index"
  dIndexDataDf<-dIndexDataDf[, colnames(dIndexDataDf) %in% comm_vars]
  
  regionTreeDataDf <- drupalFiles$regionTreeDataDf
  regionTreeDataDf$drupalTableName<-"region_tree_data"
  regionTreeDataDf<-regionTreeDataDf[, colnames(regionTreeDataDf) %in% comm_vars]
  
  regionDDataDf <- drupalFiles$regionDDataDf
  regionDDataDf$drupalTableName<-"region_d_index"
  regionDDataDf<-regionDDataDf[, colnames(regionDDataDf) %in% comm_vars]
  
  
  return(rbind(treeDataDf, dIndexDataDf, regionTreeDataDf, regionDDataDf) ) 
}

# 
drupalFilesPush<-gettingDrupalFiles(api_base, key)



for (type in c("d_index", "tree_data", "region_d_index", "region_tree_data")) {
# for (type in c("region_d_index", "region_tree_data")) {
# for (type in c("d_index", "tree_data")) {
    pubfolder<-paste(pubDatafolder, type, "/", sep="")
    print(pubfolder)
    push_together(pubfolder, drupalFilesPush, api_base, key)
}


# print(r)
# publishing lao data on July 7th
# http_publish("242", api_base, key)
# http_publish("346", api_base, key)

### publish all
# countryTID<-unique(drupalFilesPush$treeDataDf$field_geo)
# library(xml2)
# for (ci in countryTID2)
#   http_publish(ci, api_base, key)
