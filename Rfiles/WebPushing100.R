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

 pubDatafolder<-paste(data_folder,"drupalData20211202LoadtoDevSite/",sep="")


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
  

  geoid<-unique(treeDataDf$field_geo)


  n<-0
  if(!is.null(geoid)){
    for(tid in geoid){
      print(tid)
      regionTreeDataJson <- http_get(paste("region_tree_data?field_geo_target_id=", as.integer(tid), sep=""), api_base, key)
      print(regionTreeDataJson)
      ifelse (n==0, 
          regionTreeDataDf<-as.data.frame(regionTreeDataJson),
          regionTreeDataDf <- rbind(regionTreeDataDf, as.data.frame(regionTreeDataJson)))

      regionDDataJson <- http_get(paste("region_d_index_data?field_geo_target_id=", as.integer(tid), sep=""), api_base, key)
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
                           field_year == dt$field_year, title == dt$title)
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
                           field_year == dt$field_year, title == dt$title)
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
        currentDD = filter(regionDDataDf, field_indicator == dt$field_indicator, field_geo == dt$field_geo,
                           field_year == dt$field_year, title == dt$title, field_region == dt$field_region)
        if (nrow(currentDD) != 0) {
          dt$nid <- head(currentDD,1)$nid
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
                           field_year == dt$field_year, title == dt$title, field_region == dt$field_region)
        if (nrow(currentDD) != 0) {
          dt$nid <- head(currentDD,1)$nid
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
  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    # print(dt)
    ### these indicators are in the validation data, but not in drupal indicator table

    dtDrupal<-drupalPush(dt, drupalFiles, api_base, key)
    dtDrupal$moderation_state <- "draft"
    dt0<-append(dt0, list(dtDrupal))

    if(length(dt0)==30){
      print("posting now ----")
      Sys.sleep(1)
      print(ct)
      endpoint <- "node-create"
      result <- http_post(endpoint,dt0, api_base, key)
      print(result)
      dt0<-list()
      ct<-ct+30
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

# lao_drupal<-drupalRecords[drupalRecords$field_geo_name=="Lao", ]

# print(table(lao_drupal$field_indicator[lao_drupal$drupalTableName=="tree_data"]))
# 
# tree_lao<-lao_drupal[lao_drupal$drupalTableName=="tree_data", ]
# wrong_version<-grepl("v1.1", lao_drupal$title)
# print(table(wrong_version))
# wrong_lao<-lao_drupal[wrong_version, ]

# for (type in c("d_index", "tree_data", "region_d_index", "region_tree_data")) {
# # for (type in c("region_d_index", "region_tree_data")) {
# # for (type in c("d_index", "tree_data")) {
#     pubfolder<-paste(pubDatafolder, type, "/", sep="")
#     print(pubfolder)
#     push_together(pubfolder, drupalFilesPush, api_base, key)
# }

# push_together(pubfolder, drupalFilesPush, api_base, key) 


# print(r)
# publishing lao data on July 7th
# http_publish("242", api_base, key)
# http_publish("346", api_base, key)

####  Oct. 7th, publishing data on early childhood education and covid
# t1<-drupalFilesPush$treeDataDf
# t2<-drupalFilesPush$dIndexDataDf
# t3<-drupalFilesPush$regionTreeDataDf
# t4<-drupalFilesPush$regionDDataDf
# 
# table(t1$field_indicator_name[t1$moderation_state=="Draft"])
# table(t2$field_indicator_name[t2$moderation_state=="Draft"])
# table(t3$field_indicator_name[t3$moderation_state=="Draft"])
# table(t4$field_indicator_name[t4$moderation_state=="Draft"])
# 
# country<-unique(t1$field_geo)
# # country<-country[-c(1, 2)] # Afghanistan and Indian published already
# 
# for (ci in country){
#   r<-http_publish(ci, api_base, key)
#   print(r)
# }

#####  on oct 21st, collecting nids for the mics hh indicators and covid 
#####  finding the indicators
# indicatorD<-c("AccessElectricity", "BankCardHH", "BasicWater", "CleanFuel", "Covid", "MobilePhoneHH",
#               "SafeSanitation")
# indicatorD_id<-drupalFilesPush$indicatorDf$tid[drupalFilesPush$indicatorDf$Name %in% indicatorD]
# 
# nid_trees<-drupalFilesPush$treeDataDf$nid[drupalFilesPush$treeDataDf$field_indicator_name %in% indicatorD
#                                           & drupalFilesPush$treeDataDf$field_survey_type=="MICS"]
# 
# nid_ds<-drupalFilesPush$dIndexDataDf$nid[drupalFilesPush$dIndexDataDf$field_indicator %in% indicatorD_id
#                                           & drupalFilesPush$dIndexDataDf$field_survey_type=="MICS"]
# nid_regiontrees<-drupalFilesPush$regionDDataDf$nid[drupalFilesPush$regionDDataDf$field_indicator_name %in% indicatorD
#                                           & drupalFilesPush$regionDDataDf$field_survey_type=="MICS"]
# nid_regionds<-drupalFilesPush$regionTreeDataDf$nid[drupalFilesPush$regionTreeDataDf$field_indicator_name %in% indicatorD
#                                           & drupalFilesPush$regionTreeDataDf$field_survey_type=="MICS"]

# sink("/home/yw/Workspace/rstudio/LNOB_Rcode/output/deleted/nids.txt")
# for(i in c(1:length(nids)))
# print(as.numeric(nids[i]))
# sink()
# 2726 nids sent

# currently total nids on the website: region d 10188     -1253 =8935
#                                     region tree 9639    -1195 = 8444
#                                     d 926               -139 =787
#                                     tree 926            -139  =787



# dt<-readRDS(paste(pubDatafolder,"d_index/", "R39620.rds", sep=""))