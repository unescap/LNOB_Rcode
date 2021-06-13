# this r code update drupal server one node at a time

library(dplyr)
library(fun)

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  
source(paste(r_folder,"WebPushing100.R",sep=""))  

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


push_together_list<-function(resultFolder, drupalFiles, pushList, api_base, key){
  # data_list<-list.files(resultFolder, ".rds")
  if(length(pushList)==0) {
    print("No Data Found")
      return()
  }

  # no longer used. we send batch data to the server and download 
  # the drupal files later to check the publication results.
  # logcsv<-paste(resultFolder, "validation/pushlogfile.csv", sep="")

  
  # Error: parse error: premature EOF
  # (right here) ------^ 

  pushList<-c("R3819.rds")
  
  dt0<-list()
  ct<-50
  for(dn in pushList){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    ### these indicators are in the validation data, but not in drupal indicator table
    if(! (dt$field_indicator %in% c("BasicWater", "NoViolenceJustifiedAgainstWomen", "MobilePhonePR")))
    {
      dt0<-append(dt0, list(drupalPush(dt, drupalFiles, api_base, key)))
    }
    if(length(dt0)==50){
      print("posting now ----")
      Sys.sleep(5)
      print(ct)
      endpoint <- "node-create"
      result <- http_post(endpoint,dt0, api_base, key)
      print(result)
      dt0<-list()
      ct<-ct+50
    }
  }
  
  if(length(dt0)>0) {
    print("posting last batch ----")
    Sys.sleep(5)
    print(ct)
    endpoint <- "node-create"
    result <- http_post(endpoint,dt0, api_base, key)
    print(result)
  }
}

checklist<-read.table(paste(pubDatafolder, "validation/checking5.csv", sep=""), sep=",", header=T, colClasses="character")
dnList<-checklist$rdsName[checklist$FoundInDrupal==0]
# drupalFilesPush<-gettingDrupalFiles(api_base, key)
push_together_list(pubDatafolder, drupalFilesPush, dnList, api_base, key)














