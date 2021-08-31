library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)
library(httr)
library(jsonlite)

# for financial inclusiion
csvfile_name1 <-"DHSstandard_unmet"
csvfile_name2 <-"DHSstandard"
csvfile_name3 <- "DHSstandardKH71"
csvfile_name4 <- "DHSstandardKH61"
csvfile_name5 <- "DHSstandardIA52"
csvfile_name6 <- "DHSstandardIA71"
csvfile_name7 <- "DHSstandardTL61"
csvfile_name8 <- "DHSstandardAM61"   # education variable SH17A
csvfile_name9 <- "DHSstandardBD70"   # special questionaire on mobile phone for hh members over 13 yo


#### change to validated.csv: previously PR dataset was used for mobile phone in India survey.
#### we are manually changing it to IR dataset.
validatedcsv<-"/home/yw/Workspace/rstudio/LNOB_Rcode/CSVdatabase/validated.csv"

r_folder<-paste(source_folder, "Rfiles/", sep="")
# source(paste(r_folder,"ConfigDHS_ywvalidation.R",sep=""))

#### must have this file, only surveyXindicator on this file with validated results can have drupal data

### every time you run this program, we create an empty folder to store all drupal data in this run

output_folder<-paste(source_folder, "output/", sep="")
if(!dir.exists(output_folder)) dir.create(output_folder)
folder_index<-"Trial"   #format(Sys.time(), "%Y%m%d%H%M%S")
drupal_folder<-paste(output_folder, folder_index, sep="")
if(!dir.exists(drupal_folder)) dir.create(drupal_folder)
setwd(drupal_folder)

# reroute output files to the same folder


#### running DHS:
data_folder<-dhs_data_folder
source(paste(r_folder,"DHS_main_functions.R",sep=""))
uv<-1
regionFlag<-FALSE
drupalI<- 1
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "AF","70", "2015", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "AM","61", "2010", NULL, NULL, csvfile_name8, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "AM","71", "2016", "72", NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "BD","61", "2011", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "BD","70", "2014", NULL, NULL, csvfile_name9, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "KH","61", "2010", NULL, NULL, csvfile_name4, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "KH","72", "2014", NULL, NULL, csvfile_name3, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "KY","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "MV","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "MM","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "NP","61", "2011", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "NP","7H", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PK","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PK","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PG","70", "2018", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PH","61", "2013", NULL, "62", csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "PH","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TJ","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TJ","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TL","61", "2009", NULL, NULL, csvfile_name7, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TL","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
#drupalI<-run_together(csv_folder, data_folder, drupal_folder, "TR","61", "2013", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "ID","63", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "ID","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, FALSE, regionFlag, use_version=uv, validatedcsv, drupalI)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, TRUE, regionFlag, use_version=uv, validatedcsv, drupalI)


source(paste(r_folder,"MICS_main_functions.R",sep=""))

#### running MICS:
data_folder<-mics_data_folder
print(csv_folder)
# 
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
                      NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv,
#                       religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
                      NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv,
#                       religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# # # bhutan religiondata not created, only one religion in the country
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Bhutan", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Georgia", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv,
                       NULL, FALSE, FALSE, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv,
#                        NULL, FALSE, FALSE, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, FALSE, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kiribati", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, FALSE, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Kiribati", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2011",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
 drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2017",  csvfile_name, edcationcsv,
                       NULL, FALSE, FALSE, use_version=1, validatedcsv, initialIndex=1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Lao", "2017",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2013",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)

#### change upper secondary grade from 10 to 12. waiting for ermina approval.
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Mongolia", "2018",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Nepal", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Nepal", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2012",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2012",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2015",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Thailand", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Tonga", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# 
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
#                       NULL, FALSE, F, use_version=1, validatedcsv, 1)
# 
# drupalI<-run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv,
#                       religioncsv, TRUE, TRUE, use_version=3, validatedcsv, initialIndex=drupalI)
# 
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
#                        NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "Vietnam", "2010",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
drupalI<- run_together(csv_folder, data_folder, drupal_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
                      NULL, FALSE, F, use_version=1, validatedcsv, 1)
# drupalI<- run_together(csv_folder, data_folder, drupal_folder, "VietNam", "2013",  csvfile_name, edcationcsv,
#                        religioncsv, religion = TRUE, TRUE, use_version=3, validatedcsv, drupalI)
# 
# 
# # # ###  no Uzbekistan 2006 csv file
# # # run_together(csv_folder, data_folder, drupal_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv)
# # # run_together(csv_folder, data_folder, drupal_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
# #
