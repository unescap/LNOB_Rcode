library(dplyr)


source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

# pubDatafolder<-paste(data_folder,"drupalData20210604version/",sep="")
# pubDatafolder<-paste(data_folder,"drupalDatatesting/",sep="")
# runtime<-format(Sys.time(), "%Y%m%d%H%M%S")
### it is by design that this data folder name change every time
### you need to type in the correct folder name and then run "Source", all .rds files
### in this folder will be pushed to the drupal server


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
  
  # logitDataJson <- http_get("logit_data", api_base, key)
  # logitDataDf <- as.data.frame(logitDataJson)
  
  regionTreeDataJson <- http_get("region_tree_data", api_base, key)
  regionTreeDataDf <- as.data.frame(regionTreeDataJson)
  regionDDataJson <- http_get("region_d_index_data", api_base, key)
  regionDDataDf <- as.data.frame(regionDDataJson)
  
  # #### organize regional taxonomy files, not ready yet   
  #### end getting durpal server files
 
  return(list( indicatorDf=indicatorDf,
               geoDf=geoDf, 
               treeDataDf=treeDataDf,
               dIndexDataDf=dIndexDataDf,
               # logitDataDf=logitDataDf,
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
        currentTD = filter(treeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                           field_year == dt$field_year, title == dt$title)
        if (nrow(currentTD) > 0) 
          dt$nid <- head(currentTD,1)$nid
      }
    }
    ### check if 
    else if(dt$type=="d_index") {
      if (nrow(dIndexDataDf) != 0) {
        currentDD = filter(dIndexDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                           field_year == dt$field_year, title == dt$title)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
    else if(dt$type=="region_d_index") {
      if (nrow(regionDDataDf) != 0) {
        currentDD = filter(regionDDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, title == dt$title, field_region == dt$field_region)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
    else if(dt$type=="region_tree_data") {
      if (nrow(regionTreeDataDf) != 0) {
        currentDD = filter(regionTreeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, title == dt$title, field_region == dt$field_region)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
    dt$moderation_state <- "draft"
   return(dt)
}


push_together<-function(resultFolder, api_base, key){
  data_list<-list.files(resultFolder, ".rds")
  if(length(data_list)==0) {
    print("No Data Found")
      return()
  }

  logcsv<-paste(resultFolder, "validation/pushlogfile.csv", sep="")
  drupalFiles<-gettingDrupalFiles(api_base, key)
  dt0<-list()

  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    ### these indicators are in the validation data, but not in drupal indicator table
    if(! (dt$type %in% c("BasicWater", "NoViolenceJustifiedAgainstWomen", "MobilePhonePR")))
      {
      dt0<-append(dt0, list(drupalPush(dt, drupalFiles, api_base, key)))

    }
    if(length(dt0)==100){
      print("posting now ----")
      Sys.sleep(5)
      print(ct)
      endpoint <- "node-create"
      result <- http_post(endpoint,dt0, api_base, key)
      print(result)
      dt0<-list()
    }
  }

  if(length(dt0)>0) {
    print("posting last batch ----")
    #Sys.sleep(5)
    print(ct)
    endpoint <- "node-create"
    result <- http_post(endpoint,dt0, api_base, key)
    print(result)
    }
}
push_together(pubDatafolder, api_base, key)














