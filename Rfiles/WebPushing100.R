# this r code update drupal server 100 nodes at a time

library(dplyr)
library(fun)

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

# pubDatafolder<-paste(data_folder,"drupalData20210615DHSviolence/",sep="")
# pubDatafolder<-paste(data_folder,"drupalData20210604version/",sep="")
pubDatafolder<-paste(data_folder,"drupalDatatesting/",sep="")
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
  ### getting durpal server files
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
 
  return(list( 
               indicatorDf=indicatorDf,
               geoDf=geoDf,
               treeDataDf=treeDataDf,
               dIndexDataDf=dIndexDataDf,
               # logitDataDf=logitDataDf,
               regionTreeDataDf=regionTreeDataDf,
               regionDDataDf=regionDDataDf
         )) 
}
  #### getting the list of results for publication

DrupalList<-function(TypeList){
  ### creating drupal list with 4 components
  emptyList<-vector(mode = "list", length = length(TypeList))
  names(emptyList)<-TypeList
  return(emptyList)
}

# PostDrupal<-function(DrupalList, api_base, key){
#     
#     
#     return(DrupalList)
#   }
# 


drupalNIDsearch<-function(dt, drupalFiles, nid=NULL){
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
        pushtitle<-htmlspecialchars(dt$title)
        currentTD = filter(treeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                           field_year == dt$field_year, title == pushtitle)
        if (nrow(currentTD) > 0) 
          dt$nid <- head(currentTD,1)$nid
      }
    }
    ### check if 
    else if(dt$type=="d_index") {
      if (nrow(dIndexDataDf) != 0) {
        pushtitle<-htmlspecialchars(dt$title)
        currentDD = filter(dIndexDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo, 
                           field_year == dt$field_year, title == pushtitle)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
    else if(dt$type=="region_d_index") {
      if (nrow(regionDDataDf) != 0) {
        pushtitle<-htmlspecialchars(dt$title)
        pushfield_region<-htmlspecialchars(dt$field_region)
        currentDD = filter(regionDDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, title == pushtitle, field_region == pushfield_region)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
    else if(dt$type=="region_tree_data") {
      if (nrow(regionTreeDataDf) != 0) {
        pushtitle<-htmlspecialchars(dt$title)
        pushfield_region<-htmlspecialchars(dt$field_region)
        currentDD = filter(regionTreeDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, title == pushtitle, field_region == pushfield_region)
        if (nrow(currentDD) != 0) dt$nid <- head(currentDD,1)$nid
      }
    }
   return(dt)
}


push_together<-function(resultFolder, drupalFiles, api_base, key, method=1,rdsList=NULL, indList=NULL, countryList=NULL){
### result_folder is where the r outputs in rds format are saved
### method =1, push everything in the folder
### method =2, push from a list of rds
### method =3, push by indicators
### method =4, push by countries, etc. Must use the iso-3 country code
### method =5, update the moderation_state on existing nodes on the server
  
  data_list<-list.files(resultFolder, ".rds")
  if(length(data_list)==0) {
    print("No Data Found")
      return()
  }


  drupalTpe<-c("tree_data", "d_index", "region_tree_data", "region_d_index")
  dt0<-DrupalList(drupalTpe) ### create an empty list of four components
  ct<-0
  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    ### these indicators are in the validation data, but not in drupal indicator table
    inlist<-FALSE
    nid<-NULL  # is nid is NULL, nid will be searched or new node will be pushed
               # if not, exisiting nid will be updated
    
    if(method==3){
      if(length(indList)==0) {
        print("No indicator list for method 3, program exits")
        return()
      }
      if(dt$field_indicator %in% indList) inlist<-TRUE
    }

    if(method==4){
      if(length(countryList)==0) {
        print("No country list for method 4, program exits")
        return()
      }
      countryIso3<-substr(dt$title, 1, 3)
      if(countryIso3 %in% countryList) inlist<-TRUE
    }
    

    if(method==2){
      if(length(rdsList)==0) {
        print("No nid list for method 2, program exits")
        return()
      }
      if(result$nid %in% rdsList) inlist<-TRUE
    }
    
   if(method %in% c(1, 5) | inlist) {
      result<-drupalNIDsearch(dt, drupalFiles, nid)
      result$moderation_state <- "draft"
      
      if(method==5){
        if(!is.null(result$nid)){
          result$moderation_state <- "published"
          dt0[[result$type]]<-append(dt0[[result$type]], list(result))
          ct<-ct+1
        }
      }
      else {
        dt0[[result$type]]<-append(dt0[[result$type]], list(result))
        ct<-ct+1
      }
    }
    

    for(t in drupalTpe){
      if(length(dt0[[t]])==10){
        print(paste(t, " posting now ----"))
        Sys.sleep(10)
        endpoint <- "node-create"
        # result <- http_post(endpoint,dt0[[t]], api_base, key)
        # print(result)
        dt0[[t]]<-list()
      }
    }
  }

  for(t in drupalTpe){
    if(length(dt0[[t]])>0){
      print(paste(t, " posting now ----"))
      Sys.sleep(10)
      endpoint <- "node-create"
      # result <- http_post(endpoint,dt0[[t]], api_base, key)
      # print(result)
      rtd<-dt0[[t]]
      for(i in c(1:length(dt0[[t]]))) print(rtd[[i]]$title)
    }
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


# push_together(pubDatafolder, drupalFiles, api_base, key, method = 1)

# push_together(pubDatafolder, drupalFiles, api_base, key, method = 3, 
#               rdsList = NULL, indList = c("BasicWater", "NoViolenceJustifiedAgainstWomen"))


comm_vars<-c("title", "uuid", "nid", "moderation_state", "field_geo",
             "field_indicator", "field_year", "field_survey_type", "field_geo_name",
             "drupalTableName")  # "field_data",

# drupalFiles<-gettingDrupalFiles(api_base, key)
# drupalRecords<-checkingDrupalFiles(drupalFiles, comm_vars)
push_together(pubDatafolder, drupalFiles, api_base, key, method = 5)

######### actual use
######### using the & to detect escape titles and reset the title on the server
# escapeTest<-grepl("&", drupalRecords$title)
# print(table(escapeTest, drupalRecords$field_indicator))
# print(table(escapeTest, drupalRecords$drupalTableName))
# 584 nids can't be found in June 04 version
# why VNM-2013-InternetUse-National-Religion---v1, 124279, 125917
#     KHM-2010-NotOverweight-National-NoReligion---v1, 124281, 125918
# why there is still duplicates? 



# escapeNid<-drupalRecords$nid[escapeTest]
# push_together(pubDatafolder, drupalFiles, api_base, key, method = 2, rdsList =escapeList )

# pushlogName<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20210604version/validation/checking20210612_2.csv"
# pushLog<-read.table(pushlogName, sep=",", header=T) #, colClasses="character")
# escapeList<-pushLog[pushLog$nid %in% escapeNid, c("rdsName", "nid")]
# pushLog<-scan(pushlogName, sep=",", skip = 1)


#### checking field_data format for "data2"
# pubDatafolder<-paste(data_folder, "drupalData20210615205629/", sep="")
# data_list<-list.files(pubDatafolder, ".rds")
# data2_flag<-c()
# for(dn in data_list){
#   dt<-readRDS(paste(pubDatafolder, dn, sep=""))
#   datacombined<-data.frame( data=grepl("data2", dt$field_data), type=dt$type)
#   data2_flag<-rbind(datacombined, data2_flag)
# }
# print(table(data2_flag$data, data2_flag$type))



