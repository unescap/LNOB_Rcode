library(dplyr)
library(fun)

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

#### two folders that have the data for publication
# pubDatafolder<-paste(data_folder,"drupalData20210604version/",sep="")
# pubDatafolder<-paste(data_folder,"drupalData20210615DHSviolence/",sep="")

# pubDatafolder<-paste(data_folder,"drupalDatatesting/",sep="")
pubDatafolder<-paste(data_folder, "drupalDataLao20210705/",sep="")
# runtime<-format(Sys.time(), "%Y%m%d%H%M%S")
### it is by design that this data folder name change every time
### you need to type in the correct folder name and then run "Source", all .rds files
### in this folder will be pushed to the drupal server


### plan: to create one .rds file for each result, with a data structure that describe it
###       save all of the r data in one folder (created each time we start running use_version 3 in run_together)
###       pick them up and push to drupal one by one by this script
###       resultIndex is created by run_together, its sole purpose is for this publication process. We just want a unique name 
###       for each of the R data so that we can pick them up from the folder
#### getting the list of results for publication


checking<-function(resultFolder, drupalFiles){
  data_list<-list.files(resultFolder, ".rds")
  # data_list<-c("R8669.rds")
  if(length(data_list)==0) {
    print("No Data Found")
    return()
  }
  for(dn in data_list){
    dt<-readRDS(paste(resultFolder, dn, sep=""))
    dt$field_data<-NULL
    # data2compare<-dt$field_data
    if(dt$type %in% c("tree_data", "d_index")) dt$field_region<-"National"
    dt$rdsName<-dn
    title2compare<-paste(htmlspecialchars(dt$title), ".1", sep="")
    drupalLine<-drupalFiles[ drupalFiles$title == title2compare 
                             & dt$type==drupalFiles$drupalTableName, ]
    # print(drupalLine)
    # print(dt)
    k<-nrow(drupalLine)
    dt$FoundInDrupal<-k
    dt$nid<-NA
    if(k==1) dt$nid<-drupalLine$nid
    else if(k>1) dt$nid<-paste(drupalLine$nid, collapse=", ")
    dt$Religion_flag<-grepl("-Religion-", dt$title)
    logcsv<-paste(resultFolder, "/validation/checking20210705.csv", sep="") ### assuiming the validation subfolder is always there
    if(file.exists(logcsv))
      write.table(dt, logcsv, sep=",", 
                  append = TRUE,   col.names = F, row.names = F)
    else write.table(dt, logcsv, sep=",", 
                     append = FALSE,   col.names = T, row.names = F)
  }
  
}



checking(pubDatafolder, drupalRecords)
write.table(lao_drupal, paste(pubDatafolder, "/validation/lao_drupal2.csv", sep=""), sep=",",
            append = F,   col.names = T, row.names = F)

# checking the node id
# print(drupalFiles[drupalFiles$nid==124402, ])


# print(drupalFiles[drupalFiles$title=="KHM-2014-EmotionalViolence-Mondol Kiri &amp; Rattanak Kiri-NoReligion---v1", ])
# 
# print(drupalFiles[drupalFiles$nid==119521, 1]==
#         "KHM-2014-EmotionalViolence-Mondol Kiri &amp; Rattanak Kiri-NoReligion---v1")


#### getting rid of violence nodes from DHS
# violence<-grepl("Violence", drupalRecords$title)
# violence1<-grepl("TON-2019", drupalRecords$title)
# remove_nid2<-drupalRecords[violence & (!violence1), ]
# logcsv<-paste(pubDatafolder, "/validation/nid_remove.csv", sep="") ### assuiming the validation subfolder is always there
# write.table(remove_nid2, logcsv, sep=",", 
#               append = TRUE,   col.names = T, row.names = F)

# 

#### June 18th, assembles the nids with religion 
# religion<-grepl("-Religion-", drupalRecords$title)
# nid_relgion<-drupalRecords$nid[religion]
# logcsv<-paste("/home/yw/Workspace/rstudio/LNOB_Rcode/Doc/religionNids.csv", sep="") 
# write.table(nid_relgion, logcsv, sep=",",
#                           append = TRUE,   col.names = T, row.names = F)

# June 20th, checking all outputs-nids are ready and loaded properly.
# snapshot of the current drupal datasets
# logcsv<-paste("/home/yw/Workspace/rstudio/LNOB_Rcode/Doc/drupalRecords/drupalRecords20210620Afternoon.csv", sep="")
# write.table(drupalRecords, logcsv, sep=",",
#                           append = TRUE,   col.names = T, row.names = F)
