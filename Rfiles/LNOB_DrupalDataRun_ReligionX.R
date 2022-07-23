library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)
library(httr)
library(jsonlite)

# Currently, the version number (a global variable):
V<<-"v2"

#### must have this file, only surveyXindicator on this file with validated results can have drupal data
religion_flag<- parameter_list_used$religion_flag
region_flag<- parameter_list_used$region_flag
use_version<- parameter_list_used$use_version
validatedcsv<- parameter_list_used$validatedcsv

### default value in the config file, which includes every indicator defined
### run every indicator
# indicator_selection=list(dataSet=NULL, IndList=NULL)

### every time you run this program, we create an empty folder to store all drupal data in this run
output_folder<-paste(source_folder, "output/", sep="")
if(!dir.exists(output_folder)) dir.create(output_folder)
folder_index<-format(Sys.time(), "%Y%m%d%H%M%S")

if (use_version==1) {
     drupal_folder<-paste(output_folder, "Dev", folder_index, sep="")
} else if (use_version==2) { drupal_folder<-paste(output_folder, "CodeValidationData", folder_index, sep="")
} else if (use_version==3) drupal_folder<-paste(output_folder, "drupalData", folder_index, sep="")


if(!dir.exists(drupal_folder)) dir.create(drupal_folder)

#sink(paste(drupal_folder, "/printout.txt", sep = ""))

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


source(paste(r_folder,"MICS_main_functions_ReligionX.R",sep=""))

#### running MICS:
data_folder<-mics_data_folder
print(csv_folder)
csvfile_name<-"MICS"
religioncsv<-"ReligionXMICS"
religion_flag<-TRUE
# 
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
                      religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
                      religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0.5, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kiribati", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2017",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2011",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Nepal", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2012",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2015",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0.5, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
                      religioncsv, TRUE, region_flag, use_version, validatedcsv, survey_vesion=0, initialIndex=drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tuvalu", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)


drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Samoa", "2019",  csvfile_name, edcationcsv,
                        religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)


# # 
# # # sink()
# # 
