######################################
# Master script 
######################################

if(! require("rpart")) install.packages("rpart")
if(! require("rpart.plot")) install.packages("rpart.plot")
if(! require("data.table")) install.packages("data.table")
if(! require("stringr")) install.packages("stringr")
if(! require("logging")) install.packages("logging")
if(! require("log4r")) install.packages("log4r")
if(! require("cgwtools")) install.packages("cgwtools")
# if(! require("rdhs")) install.packages("rdhs")
if(! require("dplyr")) install.packages("dplyr")
if(! require("ggplot2")) install.packages("ggplot2")
if(! require("ggparty")) install.packages("ggparty")
if(! require("dplyr")) install.packages("dplyr")
if(! require("dplyr")) install.packages("stringr")

library(rpart) # Decision Tree: for building
library(rpart.plot) # Decision Tree: for plotting 
library(data.table) # Data Manipulation 
library(stringr)
library(logging) # Logging: For logging info and errors 
library(log4r)
# library(re) # Regex library: For handling folder names 
library(cgwtools) # Library for resave .Rdata files
# library(rdhs) 
library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)

######################################
r_folder<-paste(source_folder, "Rfiles/", sep="")
csv_folder<-paste(source_folder, "DHScsv/", sep="")


if(! exists("r_folder")) {
  print("r_folder not defined, please consult user manual, create a config file and define r_folder in it")
  stop()
}
source(paste(r_folder,"DHSReadDatLib.R",sep=""))
source(paste(r_folder,"DHSShapleyValue.R",sep=""))
source(paste(r_folder,"DHS_get_data.R",sep=""))
source(paste(r_folder,"DHS_output.R",sep=""))
source(paste(r_folder,"DHSTreeAndLogistic.R",sep=""))

### Create logger
# eclac-logger.log: This tracks everything that goes right (message) and wrong (errors). 
logger <- create.logger(logfile = logger_location, level = 'DEBUG')

### Read DHSKey.csv: this contains the corresponding country_code and version_code for a
# country and year. Example: "Afghanisation 2015" has country_code "AF" and version_code "70".



### Main function: runs all the other functions 
# reading in a csv file with columnes: data type, variable names, variable type (categorical or numeric), 
# variable indicators (response/independent), recoding

# add use version for different purposes.
# 1 for development and debugging (default)
# 2 for validation on sharepoint, different input data structures
# 3 for TBD code (for now, better to be separated later)

run_together<-function(csv_folder, original_data_folder, output_folder, country_code, version_code, year_code, mrversion_code=NULL,
                       prversion_code=NULL, csvfile_name, Flag_New=TRUE, caste=FALSE, region=FALSE, use_version=1)
{
  svnm<-paste(country_code, version_code, sep="")
  # Reading DHSstandard.csv file. 
  print(paste(csv_folder, csvfile_name, ".csv", sep=""))
  meta_data<-read.table(paste(csv_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  info(logger, "Run_together function called.")
  # Type of Datasets: IR, HR, PR, MR.
  dataSet<-unique(meta_data$DataSet)
  dataSet<-dataSet[!dataSet=="MR"]
  
  # specify data set for debugginh
  # dataSet<-c("PR")
  # DataSet provides survey dataset shortname (HR, IR, or PR) and response/independent variables for each dataset
  # Iterate through each type of dataset. 
  
  # originally in lines 44-60, 80-86, 99-101 are moved to the DHS_TBD file and run here when use_version is 3
  if(use_version==3) {
    
    csvfile_name0 <- "DHSKey"
    
    if(! exists(paste(csv_folder, csvfile_name0,sep=""))) {
      print("DHSKey.csv not available, TBD version can not run,  please consult user manual, create a config file and define r_folder in it")
      stop()
    }
    else {
    
    DHSKey <-read.table(paste(csv_folder, csvfile_name0, ".csv", sep=""), sep=",", header=T, colClasses="character")
    as.data.frame(DHSKey)
    }
    
    if(! exists(paste(r_folder,"DHS_TBD.R",sep=""))) {
      print("DHS_TBD.R not available, TBD version can not run,  please consult user manual, create a config file and define r_folder in it")
      stop()
    }
    else source(paste(r_folder,"DHS_TBD.R",sep=""))
  }
  
  
  for(ds in dataSet) {
    
    # Creating output folder: Example ~ ./dat_download/Afghanistan 2015/HR 
    ########
    ds_output_folder <- ds_output(output_folder, ds)
    ifelse(!dir.exists(ds_output_folder), dir.create(ds_output_folder), FALSE)
    
    # Sample Weight transformation 
    dataList<-meta_data[meta_data$DataSet==ds, ]
    swV<-dataList$VarName[dataList$NickName=="SampleWeight"]
    
    # Example: ./dat_download/Afghanistan 2015/
    country_data_folder<-country_data(original_data_folder, country_code, version_code, use_version)
    print(country_data_folder)
    # Example: ./dat_download/Afghanistan 2015/AFIR70FL/
    # data_folder <- paste(country_data_folder, paste(filename, "/", sep = ""), sep="")
    data_folder<-country_data_folder
    
    
    # File name: Example ~ MVIR71FL.
    ### here line 119 for TDB code for picking up data file names
    if(use_version==1) filename<-paste(country_code, ds, version_code, "FL", sep="")
    else filename<-basename(dir(country_data_folder, pattern = paste(country_code, ds, "*", sep=""), full.names = TRUE, ignore.case = TRUE))

    
    # Printing current iteration of datatype.  
    message <- paste("## File name: ", filename, "  ############################")
    print(message)
    info(logger, message)
    message <- paste("## Current dataset: ", match(ds, dataSet), " of ", length(dataSet))
    print(message)
    info(logger, message) 
    
    ### Read the datafile downloaded from DHS into R with columns specified in DHSstandard.csv (in meta_data).
  
    if(ds=="PR") {
      if(!is.null(prversion_code)) {
        if(use_version==1) filename<-paste(country_code, ds, prversion_code, "FL", sep="")
        else filename<-basename(dir(country_data_folder, pattern = paste(country_code, ds, "*", sep=""), full.names = TRUE, ignore.case = TRUE))
      }
    }

    # Example: ./dat_download/Afghanistan 2015/AFIR70FL/AFIR70FL.DCF
    data_path = paste(data_folder, filename, sep="/")
    
    df<-importDHSDAT(data_path, Flag_New, dataList$VarName)
    
    # Scale the Sample Weight values by factor of 1000000.
    df<- scale_sample_weight(df, swV)
    
    # Preparing ethnicity dataset if it exists.
    eth<-NULL
    if(caste == TRUE) {
      eth<-paste(country_code, version_code, sep="")
      ### used for the caste() function, currently only defined for the IA71 & IA74 survey
    }
    
    # Prepare Response Variable List. 
    responseList<-dataList[dataList$IndicatorType=="ResponseV", ]

    # Iterate through each response variable for current dataset type. 
    unique_responseList <- unique(responseList$NickName)

    # DEBUG
    # debug <- unique_responseList[match("ChildMarriage18", unique_responseList)]
    # unique_responseList<-c("NoSexualViolence",  "AllViolence", "SexualPhysicalViolence", "PhysicalViolence", "SexualViolence", "EmotionalViolence")
    # unique_responseList<-c("Covid", "LearningPR", "WaterOnsitePR", "SafeSanitationPR", "HandWashPR", "NotCrowdedPR")
    # unique_responseList<-c("InternetUse")
    # unique_responseList<-c("CleanWater", "SafeSanitation")
    # unique_responseList<-c("PhysicalViolence")
    # unique_responseList<-c("Covid1")
    
    
    
    #Modified for YW
    for(rv in unique_responseList) {
      
      rtp<-unique(responseList$DataType[responseList$NickName==rv])

      if(use_version==3) indicator_list<- DHS_TBD_app_indicator(rv, responseList$IndicatorType[responseList$NickName==rv], indicator_list)
      
      # Printing current iteration of response variable. 
      message <- paste("Random variable: ", rv, "  ------------------")
      print(message)
      info(logger, message)
      message <- paste("Current Response Variable: ", match(rv, unique_responseList), " of ", length(unique_responseList))
      print(message)
      info(logger, message)
  
      ## need to define the function in configure file
      dhs_Rdata_folder<-data_folder
      rv_Rdata_folder<-rv_Rdata(dhs_Rdata_folder, rv)

      ifelse(!dir.exists(rv_Rdata_folder), dir.create(rv_Rdata_folder), FALSE)
      setwd(rv_Rdata_folder)
      
      # Retrieve Inpendent variable list. 
      indvar<- indList(rv, caste = caste )
      
      # Retrieve dataset for given Response/Independent variables. 
      pass_message <- "Successfuly retrieved dataset for given RV/IV [get_data]"
      datause <- catch_error(get_data(df, rv, dataList, indvar, svnm, eth)) #catch_error() 

      if(is.null(datause)) { 
        
        print("Data not generated") 
        error(logger, "Data not generated")
        
      }
      else { 
        
        info(logger, paste(pass_message))
        
        # MR files might be needed for response variables such as health insurance, internet use, child marriage, mobile phone for financial transaction. 
        print(rv)
        mr_ds<-unique(meta_data[meta_data$NickName==rv & meta_data$IndicatorType=="MresponseV", c("DataSet")])
        
        if(length(mr_ds)>0){
          n0<-nrow(datause)
          # Example: ./dat_download/Afghanisatan 2015/
          data_path <- country_data_folder 
          if(is.null(mrversion_code)) mrversion_code<-version_code
          datause <- merge_mr(mr_ds, meta_data, datause, dataList, country_code, mrversion_code, data_path, rv, indvar, svnm, eth, caste, Flag_New, use_version) 
          if(nrow(datause)>n0) indvar<-c(indvar, "Sex")
        }
      
        # for teenage pregnancy, we need pr file, if there are teenage girls not elegible for IR interview
        pr_ds<-meta_data[meta_data$NickName==rv & meta_data$IndicatorType=="PresponseV", c("DataSet")]  ### must include teenagers not in IR but in PR
      
        if(length(pr_ds)>0){
        
          # Example: ./dat_download/Afghanisatan 2015/
          data_path <- country_data_folder 
        
          if(is.null(prversion_code)) prversion_code<-version_code
          datause <- merge_pr(pr_ds, meta_data, datause, dataList, country_code, prversion_code, data_path, rv, indvar, svnm, eth, caste, Flag_New, use_version)
          print("data merge done")

        } 
        ## indList() is defined in DHS_get_data.R file, for every response variable, it specifies the independent variable
        ## for the Tree or GLM models.
        ## get_data() is defined in DHS_get_data.R file, it caculates the response variable and creat a 0-1 variable in 
        ## datause$var2tab

        country_ISO<-iso_code(country_code)
        if(country_ISO=="NotFound") {
          print("ISO code not found, using the DHS country code instead")
          country_ISO<-country_code
        }
        
        formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
        title_string<-paste(rv, paste(indvar, collapse=" + "), sep=" ~ ")
        
        print(formula_string)
        
        k<-match("Caste", colnames(datause))  
        
        if(caste==TRUE & length(k)==0) {
          
          print("Caste information not available")
          print("Please rerun the program without Caste")
          
        } 
        
        if(region) {
          regionList<-unique(datause$RegionName)
          for (rg in regionList){
            datause1<-datause[datause$RegionName==rg, ]
            country_code1<-paste(rg, country_ISO, sep = ", ")
            #### Construct and Write Decision Tree to output folder
            write_tree(datause1, country_code1, year_code, title_string, formula_string, sub_string, rv, rtp, filename, caste, ds_output_folder, use_version)
            
            #### Construct and Write HOI and dis-similarity index calculation to output folder
            write_HOI_D(datause1, country_code1, year_code, title_string, indvar, ds_output_folder, filename, use_version)
            
            #### Construct and Write Logistic Regression to output folder 
            write_glm(datause1, rtp,  country_code1, year_code, title_string, indvar, ds_output_folder, filename, use_version)
          }
        }
        else {
          
          # Constructing the formula string and title for the models 

          #### Construct and Write Decision Tree to output folder
          write_tree(datause, country_ISO, year_code, title_string, formula_string, sub_string, rv, rtp, filename, caste, ds_output_folder, use_version)

          #### Construct and Write HOI and dis-similarity index calculation to output folder
          write_HOI_D(datause, country_ISO, year_code, title_string, indvar, ds_output_folder, filename, use_version)
          
          #### Construct and Write Logistic Regression to output folder 
          write_glm(datause, rtp,  country_ISO, year_code, title_string, indvar, ds_output_folder, filename, use_version)

          #### Construct model for each region. 
          # region(output_folder, country_code, version_code,
          #        datause, rv,
          #        formula_string, title_string, sub_string,
          #        caste, filename, indvar)
          
        }
      } 
    }
    
    if(use_version==3)
      DHS_TBD_mainfunction(DHSKey)
    message <- paste("END OF SCRIPT.") 
    print(message)
    info(logger, message)
  }
}

#######################################################################################

# for financial inclusiion
csvfile_name1 <-"DHSstandard_unmet"
csvfile_name2 <-"DHSstandard"
csvfile_name3 <- "DHSstandardKH71"
csvfile_name4 <- "DHSstandardKH61"
csvfile_name5 <- "DHSstandardIA52"
csvfile_name6 <- "DHSstandardIA71"

# MV 71 
run_together(csv_folder, data_folder, output_folder, "MV","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, FALSE)

# run_together(csv_folder, data_folder, output_folder, "MV","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE, TRUE)


# checking professional help
# run_together(csv_folder, data_folder, output_folder, "BD","61", "2014", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","63", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)


# checking water and sanitation
# run_together(csv_folder, data_folder, output_folder, "AM","61", "2010", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","61", "2010", NULL, NULL, csvfile_name4, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","63", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","61", "2012", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)


# cambodia 2014 and india 2015 run tests 
# run_together(csv_folder, data_folder, output_folder, "IA","52", NULL, NULL, csvfile_name5, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","61", NULL, NULL, csvfile_name4, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", mrversion_code="74", NULL, csvfile_name6, TRUE, TRUE)

#covid runs
# run_together(csv_folder, data_folder, output_folder, "AF","70", "2015", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","71", "2016", "72", NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","70", "2014", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","72", "2014", NULL, NULL, csvfile_name3, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MV","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MM","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","7H", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","71", "2018", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PG","70", "2018", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","71", "2016", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", "2017", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", "2016", "74", NULL, csvfile_name6, TRUE, TRUE)

# run_together(csv_folder, data_folder, output_folder, "AF","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","71", "72", NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AZ","52", NULL, NULL, csvfile_name1, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","72", NULL, NULL, csvfile_name3, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KY","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MV","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MM","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","7H", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PG","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","61", NULL, "62", csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","61", NULL, NULL, csvfile_name1, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TR","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","63", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","61", NULL, NULL, csvfile_name4, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", NULL, NULL, csvfile_name2, TRUE, FALSE)


#######################################################################################
# run_together(csv_folder, data_folder, output_folder, "AF","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AM","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "AZ","52", NULL, NULL, csvfile_name1, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "BD","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","72", NULL, NULL, csvfile_name3, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KY","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MV","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "MM","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "NP","7H", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PK","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PG","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","61", NULL, "62", csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "PH","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TJ","70", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","61", NULL, NULL, csvfile_name1, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TL","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "TR","61", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","63", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "ID","71", NULL, NULL, csvfile_name2, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "KH","61", NULL, NULL, csvfile_name4, TRUE, FALSE)
# run_together(csv_folder, data_folder, output_folder, "IA","71", NULL, NULL, csvfile_name2, TRUE, FALSE)

#######################################################################################

# ### Run for all available data sets
# available_data_names <- list.files(path = paste(csv_folder,"dat_download",sep=""))
# 
# ### Only Run for One Data Set
# available_data_names <- "Cambodia 2014"
# 
# DHSstandard_type <- function(country_code, version_code) {
# 
#   if (country_code == "AZ" & version_code == 52) {
#     return (csvfile_name1)
#   }  
#   else if (country_code == "KH" & version_code == 71) {
#     return (csvfile_name3)
#   }
#   else if (country_code == "KH" & version_code == 61) {
#     return (csvfile_name4)
#   } else {
#     return (csvfile_name2)
#   }
#   
# }
# 
# for (name in available_data_names) {
# 
#   # Note: Must have folder structure as following ./dat_download/Afghanistan/AFBR70FL/AFBR70FL.DCF
#   info(logger, paste("##############  ", name, "  ##############", sep = ""))
#   message <- paste("## Current Country/Year: ", match(name, available_data_names), " of ", length(available_data_names))
#   info(logger, message)
#   
#   # Country and Version code
#   folder_name <- str_trim(DHSKey$folder, side = c("both", "left", "right"))
#   country_code <- DHSKey$country_code[folder_name == name]
#   version_code <- DHSKey$version_code[folder_name == name]
# 
#   # Set DHSstandard type:
#   temp_csvfile_name <- DHSstandard_type(country_code, version_code)
#   
#   # Data folder of Country Year location
#   data_folder_name <- paste(name,"/", sep = "")
#   temp_data_folder <- paste(data_folder, data_folder_name , sep = "")
# 
#   # Output folder of Country Year location
#   output_folder_name <- paste(name,"/", sep = "")
#   temp_output_folder <- paste(output_folder, output_folder_name, sep = "")
# 
#   # Creating Output folder of Country Year
#   ifelse(!dir.exists(temp_output_folder), dir.create(temp_output_folder), FALSE)
# 
#   # Call run_together function
#   run_together(csv_folder, temp_data_folder, temp_output_folder, 
#                country_code, version_code, 
#                NULL, NULL, temp_csvfile_name, TRUE, FALSE)
#   
# }


#######################################################################################

