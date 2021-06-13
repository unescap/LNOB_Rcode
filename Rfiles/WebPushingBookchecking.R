library(dplyr)
library(fun)

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

pubDatafolder<-paste(data_folder,"drupalData20210604version/",sep="")
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
comm_vars<-c("title", "uuid", "nid", "moderation_state", "field_geo",        
             "field_indicator", "field_year", "field_survey_type", "field_geo_name", 
             "drupalTableName")

checkingDrupalFiles<-function(api_base, key, comm_vars){
  ### getting durpal server files
  # indicatorJson <- http_get("indicator_taxonomies", api_base, key)
  # indicatorDf <- as.data.frame(indicatorJson)
  # 
  # geoJson <- http_get("geo_taxonomies", api_base, key)
  # geoDf <- as.data.frame(geoJson)

  treeDataJson <- http_get("tree_data", api_base, key)
  treeDataDf <- as.data.frame(treeDataJson)
  treeDataDf$drupalTableName<-"tree_data"
  treeDataDf<-treeDataDf[, colnames(treeDataDf) %in% comm_vars]
  
  dIndexDataJson <- http_get("d_index_data", api_base, key)
  dIndexDataDf <- as.data.frame(dIndexDataJson)
  dIndexDataDf$drupalTableName <-"d_index"
  dIndexDataDf<-dIndexDataDf[, colnames(dIndexDataDf) %in% comm_vars]
  # logitDataJson <- http_get("logit_data", api_base, key)
  # logitDataDf <- as.data.frame(logitDataJson)
  
  geoid<-unique(treeDataDf$field_geo)
  # regionTreeDataJson <- http_get("region_tree_data", api_base, key)
  
  n<-0
  for(tid in geoid){
    regionTreeDataJson <- http_get(paste("region_tree_data?field_geo_target_id=", as.integer(tid), sep=""), api_base, key)
    ifelse (n==0, 
            regionTreeDataDf<-as.data.frame(regionTreeDataJson),
            regionTreeDataDf <- rbind(regionTreeDataDf, as.data.frame(regionTreeDataJson)))
    
    regionDDataJson <- http_get(paste("region_d_index_data?field_geo_target_id=", as.integer(tid), sep=""), api_base, key)
    ifelse(n==0, 
           regionDDataDf <- as.data.frame(regionDDataJson), 
           regionDDataDf <- rbind(regionDDataDf, as.data.frame(regionDDataJson)))
    n<-n+1
  }
  # #### organize regional taxonomy files, not ready yet   
  #### end getting durpal server files
  regionTreeDataDf$drupalTableName<-"region_tree_data"
  regionTreeDataDf<-regionTreeDataDf[, colnames(regionTreeDataDf) %in% comm_vars]
  
  regionDDataDf$drupalTableName<-"region_d_index"
  regionDDataDf<-regionDDataDf[, colnames(regionDDataDf) %in% comm_vars]
  
  return(rbind(treeDataDf, dIndexDataDf, regionTreeDataDf, regionDDataDf) ) 
}
#### getting the list of results for publication



drupalFiles<-checkingDrupalFiles(api_base, key, comm_vars)


checking<-function(resultFolder, drupalFiles){
  data_list<-list.files(resultFolder, ".rds")
  data_list<-c("R8669.rds")
  if(length(data_list)==0) {
    print("No Data Found")
    return()
  }
  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    dt$field_data<-NULL
    if(dt$type %in% c("tree_data", "d_index")) dt$field_region<-"National"
    dt$rdsName<-dn
    title2compare<-htmlspecialchars(dt$title)
    drupalLine<-drupalFiles[ drupalFiles$title == title2compare & dt$type==drupalFiles$drupalTableName, ]
    # print(drupalLine)
    # print(dt)
    k<-nrow(drupalLine)
    dt$FoundInDrupal<-k
    dt$nid<-NA
    if(k==1) dt$nid<-drupalLine$nid
    else if(k>1) dt$nid<-paste(drupalLine$nid, collapse=", ")

    logcsv<-paste(resultFolder, "/validation/checking6.csv", sep="") ### assuiming the validation subfolder is always there
    if(file.exists(logcsv))
      write.table(dt, logcsv, sep=",", 
                  append = TRUE,   col.names = F, row.names = F)
    else write.table(dt, logcsv, sep=",", 
                     append = FALSE,   col.names = T, row.names = F)
  }
  
}



checking(pubDatafolder, drupalFiles)


# checking the node id
print(drupalFiles[drupalFiles$nid==124402, ])







