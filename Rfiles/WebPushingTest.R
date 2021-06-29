library(dplyr)


source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"Config_drupalkey.R",sep="")) ### obtain api_base, key
source(paste(r_folder,"http_request.R",sep=""))  

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


push_together(pubDatafolder, drupalFilesPush, api_base, key)


# dt<-list(type="d_index", field_survey_type="DHS", field_dataset="HR",
#          title="AFG-2015-AccessElectricity-National-NoReligion---v1.1",
#          field_geo="121", field_year="2015", field_indicator="391",
#          field_data= "{\"mean\":0.7145,\"HOI\":0.6506,\"D\":0.0895}",
#          nid=NULL, moderation_state="draft")
# 
# dt0<-list(dt)
# 
# endpoint <- "node-create"
# result <- http_post(endpoint,dt0, api_base, key)
# print(result)





