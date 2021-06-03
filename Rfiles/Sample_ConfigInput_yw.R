### path where the project sits
### normally include sub folders
### 1) /Rfiles/ (where all r script files are saved)
### 2) /output/ (where all output files to be)
### 3) /DHScsv/ (where csv files needed for the r program are saved)
### 4) /MICScsv/ (where csv files needed for the r proragm are saved)
source_folder <- "/your/source/folder/path/"


#### where you saved the downloaded data files from DHS website
dhs_data_folder <-"/your/dhs/data/folder/"
####  you can specify your data folder structure here:
####  in DHS_main_functions file, this will be used in run_together()
#### original_data_folder is the dhs_data_folder you just give above
dhs_country_data<-function(original_data_folder, country_code, version_code){
    return(original_data_folder)
    
    # csvfile_name0 <- "DHSkey"
    # DHSKey <-read.table(paste(origin_folder, "DHScsv/", csvfile_name0, ".csv", sep=""), sep=",", header=T, colClasses="character")
    # as.data.frame(DHSKey)
    # 
    # country <- DHSKey$country[DHSKey$country_code == country_code & DHSKey$version_code == version_code]
    # 
    # version <- DHSKey$year[DHSKey$country_code == country_code & DHSKey$version_code == version_code]
    # 
    # return(paste(original_data_folder, "/",country, version, "/", sep= ""))

}

#### where you saved the downloaded data files from MICS website
mics_data_folder<-"/your/mics/data/folder/"



#### two versions of the catch_error treatment defined in XXX_get_data.R
catch_error<-function(code)
{
  return(catch_error_dev(code))
}
