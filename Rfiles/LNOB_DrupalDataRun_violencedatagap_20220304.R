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
V<<-"v1"

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


data_folder<-dhs_data_folder
source(paste(r_folder,"DHS_main_functions.R",sep=""))

drupalI<-run_together(csv_folder, data_folder, drupal_folder, "KY","61", "2012", NULL, NULL, csvfile_name2, TRUE, caste=religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PK","61", "2012", NULL, NULL, csvfile_name2, TRUE, caste=religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PH","61", "2013", NULL, "62", csvfile_name2, TRUE, caste=religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TJ","61", "2012", NULL, NULL, csvfile_name2, TRUE, caste=religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TL","61", "2009", NULL, NULL, csvfile_name7, TRUE, caste=religion_flag, region_flag, use_version, validatedcsv, survey_vesion=0, drupalI, indicator_selection)

source(paste(r_folder,"MICS_main_functions.R",sep=""))

#### running MICS:
data_folder<-mics_data_folder
print(csv_folder)
csvfile_name<-"MICS"

# no religion
religion_flag<-FALSE

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
                       NULL, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)


religion_flag<-TRUE

drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
                       religioncsv, religion_flag, region_flag, use_version, validatedcsv, survey_vesion=1, drupalI, indicator_selection)


# sink()

