library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)
library(httr)
library(jsonlite)

#### change to validated.csv: previously PR dataset was used for mobile phone in India survey.
#### we are manually changing it to IR dataset.
# validatedcsv<-"/home/yw/Workspace/rstudio/LNOB_Rcode/CSVdatabase/validated20210817.csv"
validatedcsv<-"/home/yw/Workspace/rstudio/LNOB_Rcode/CSVdatabase/validated_ECE.csv"

r_folder<-paste(source_folder, "Rfiles/", sep="")
# source(paste(r_folder,"ConfigDHS_ywvalidation.R",sep=""))

#### must have this file, only surveyXindicator on this file with validated results can have drupal data

### every time you run this program, we create an empty folder to store all drupal data in this run

output_folder<-paste(source_folder, "output/", sep="")
if(!dir.exists(output_folder)) dir.create(output_folder)
folder_index<-format(Sys.time(), "%Y%m%d%H%M%S")
drupal_folder<<-paste(output_folder, "drupalData", folder_index, sep="")
if(!dir.exists(drupal_folder)) dir.create(drupal_folder)

##### we will save different data types in four different subfolders
##### data_type are: "tree_data", "d_index",  "region_tree_data", "region_d_index"
wd_datatype<-function(drupal_folder, data_type){
  datatype_folder<-paste(drupal_folder, data_type, sep="/")
  if(!dir.exists(datatype_folder)) dir.create(datatype_folder)
  setwd(datatype_folder)
}


# reroute output files to the same folder
drupalI<- 1
# 

# no DHS data

source(paste(r_folder,"MICS_main_functions.R",sep=""))

#### running MICS:
data_folder<-mics_data_folder
print(csv_folder)
# 
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
                      NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
#                       religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
                      NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
#                       religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# # # bhutan religiondata not created, only one religion in the country
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Bhutan", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kiribati", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kiribati", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2011",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2011",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2017",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2017",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Nepal", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Nepal", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2012",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2012",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2015",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# 
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
                      NULL, FALSE, TRUE, use_version=3, validatedcsv, initialIndex=drupalI)
# 
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
#                       religioncsv, TRUE, TRUE, use_version=3, validatedcsv, initialIndex=drupalI)
# 
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
                       NULL, FALSE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# 
# 
# # # ###  no Uzbekistan 2006 csv file
# # # run_together(csv_folder, data_folder, drupal_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv)
# # # run_together(csv_folder, data_folder, drupal_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
# #
