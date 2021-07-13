source_folder <- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
mics_data_folder<-"/home/yw/Workspace/rstudio/SDD2017/sav_download/"



#### two versions of the catch_error treatment
catch_error<-function(code)
{
  return(catch_error_dev(code))
}

V<<-"dev"
# This is our version, used in drupal data
output_folder<-paste(source_folder, "output/education/", sep="")
ifelse(!dir.exists(output_folder), dir.create(output_folder), FALSE)


# Function to catch errors, XXX_prod allows the program to ignore error message and keep running 
catch_error_prod <- function(code) {
  out <- tryCatch(code,
                  error = function(c) {
                    error(logger, paste(c$message))
                  },
                  warning = function(c) { 
                    warn(logger, paste(c$message))
                  }
  )
  return(out)
}

# Function to catch errors, XXX_dev will stop the program to debug

catch_error_dev <- function(code) {
  return(code)
}

ds_output_dev <- function(output_folder, ds){
  ds_output_folder<-paste(output_folder, ds, "/",sep = "")
  ifelse(!dir.exists(ds_output_folder), dir.create(ds_output_folder), FALSE)
  return(ds_output_folder)
}

rv_Rdata_dev<-function(mics_Rdata_folder, rv){
  rv_Rdata_folder <- paste(mics_Rdata_folder, rv, sep = "/")
  ifelse(!dir.exists(rv_Rdata_folder), dir.create(rv_Rdata_folder), FALSE)
  setwd(rv_Rdata_folder)
  return(rv_Rdata_folder)
}


catch_error<-function(code)
{
  return(catch_error_dev(code))
}

ds_output <- function(output_folder, ds){
  # ds_output_folder<-ds_output_dev(output_folder, "validationm")
  return(paste(output_folder, "validation/",sep = "/"))
}

rv_Rdata<-function(mics_Rdata_folder, rv){
  rv_Rdata_folder <- rv_Rdata_dev(mics_Rdata_folder, rv)
  return(rv_Rdata_folder)
}

library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)
library(httr)
library(jsonlite)

r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"MICS_main_functions.R",sep=""))

#### running MICS:
data_folder<-mics_data_folder
print(csv_folder)

# drupalI<-run_together(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
#                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<-run_together(csv_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
                      NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name, edcationcsv,
                       NULL, FALSE, FALSE, use_version=1, validatedcsv, initialIndex=1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<-run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
#                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)


