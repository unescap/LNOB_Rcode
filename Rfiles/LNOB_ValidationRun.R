source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"

library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)

# for financial inclusiion
csvfile_name1 <-"DHSstandard_unmet"
csvfile_name2 <-"DHSstandard"
csvfile_name3 <- "DHSstandardKH71"
csvfile_name4 <- "DHSstandardKH61"
csvfile_name5 <- "DHSstandardIA52"
csvfile_name6 <- "DHSstandardIA71"
csvfile_name7 <- "DHSstandardTL61"

r_folder<-paste(source_folder, "Rfiles/", sep="")
source(paste(r_folder,"ConfigDHS_ywvalidation.R",sep=""))
source(paste(r_folder,"DHS_main_functions.R",sep=""))

#### running DHS:

# run_together(csv_folder, data_folder, output_folder, "AF","70", "2015", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","61", "2010", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","71", "2016", "72", NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","61", "2011", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","70", "2014", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","61", "2010", NULL, NULL, csvfile_name4, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","72", "2014", NULL, NULL, csvfile_name3, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KY","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MV","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MM","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","61", "2011", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","7H", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","61", "2013", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","71", "2018", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PG","70", "2018", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","61", "2013", NULL, "62", csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","61", "2012", NULL, NULL, csvfile_name7, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TR","61", "2013", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","63", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, TRUE)


source(paste(r_folder,"ConfigMICS_ywvalidation.R",sep=""))
source(paste(r_folder,"MICS_main_functions.R",sep=""))

#### running MICS:

run_together(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
# bhutan religiondata not created, only one religion in the country
run_together(csv_folder, data_folder, output_folder, "Bhutan", "2010",  csvfile_name, edcationcsv)
run_together(csv_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2010",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Kiribati", "2019",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Kiribati", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Lao", "2011",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Lao", "2011",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Nepal", "2019",  csvfile_name, edcationcsv)
#run_together(csv_folder, data_folder, output_folder, "Nepal", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Thailand", "2012",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Thailand", "2012",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "Vietnam", "2010",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Vietnam", "2010",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)
run_together(csv_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)


# ###  no Uzbekistan 2006 csv file
# run_together(csv_folder, data_folder, output_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Uzbekistan", "2006",  csvfile_name, edcationcsv, religioncsv, religion = TRUE)

