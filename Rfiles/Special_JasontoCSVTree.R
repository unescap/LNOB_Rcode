library(dplyr)
library(fun)
library(jsonlite)
library(data.table) # Data Manipulation 

source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
data_folder<-paste(source_folder, "output/",sep="") 
r_folder<-paste(source_folder, "Rfiles/", sep="")

tree_folder<-paste(data_folder, "OrlandoDataCovid20210917/tree_data/", sep = "")

data_list<-list.files(tree_folder, ".rds")

csvname<-paste(tree_folder, "Alltree.csv", sep = "")
for(dn in data_list){
  print(dn)
  dt<-readRDS(paste(tree_folder, dn, sep=""))
  datatowrite<-cbind(fromJSON(dt$field_data) %>% as.data.frame, dt$field_survey_type)
  write.table(datatowrite, 
            csvname, sep=",", append = TRUE, col.names = F, row.names = F)
}