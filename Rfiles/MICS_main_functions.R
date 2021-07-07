#####################################################################################
# for only the most recent version of MICS data (4 & 5). 6 is out now but need to checked for this code.
#
# This R-code file use package foreign to read in SPSS data files .sav, reorganize  #
# data and variable names 

#### mics version 4 and 5
#The fourth round of Multiple Indicator Cluster Surveys (MICS4) is scheduled for 2009-2011 and survey results are expected to be available from 2010 onwards.
# from http://microdata.worldbank.org/index.php/catalog/1795/study-description

#The results from the most recent MICS5 surveys, carried out in 2012-2015, are becoming progressively available.
# from https://www.unicef.org/statistics/index_24302.html

# SIERRA LEONE IS THE FIRST SURVEY IN THE 6TH ROUND OF MICS TO GO TO THE FIELD
# from http://mics.unicef.org/news_entries/80/Sierra-Leone-is-the-First-Survey-in-the-6th-Round-of-MICS-to-go-to-the-Field
#######################################################################################
#
# This is the main function that organizes all other functions
#
# reading in a csv file with columnes: data type, variable names, variable type (categorical or numeric), 
# variable indicators (response/independent), recoding
# 
# 
######################################


#source_folder<- "/Users/samuel/Desktop/ECLAC/standard-mics/"



######################################

if(! require("rpart")) install.packages("rpart")
if(! require("rpart.plot")) install.packages("rpart.plot")
if(! require("data.table")) install.packages("data.table")
if(! require("foreign")) install.packages("foreign")
if(! require("logging")) install.packages("logging")
if(! require("log4r")) install.packages("log4r")
if(! require("cgwtools")) install.packages("cgwtools")
# if(! require("rgdal")) install.packages("rgdal")
# if(! require("httr")) install.packages("httr")
# if(! require("openssl")) install.packages("openssl")   ### ERROR
# if(! require("rdhs")) install.packages("rdhs")   
if(! require("dplyr")) install.packages("dplyr")
if(! require("ggplot2")) install.packages("ggplot2")

library(rpart)
library(rpart.plot)
library(data.table)
library(foreign)
library(logging) # Logging: For logging info and errors 
library(log4r)
# library(re) # Regex library: For handling folder names 
library(cgwtools) # Library for resave .Rdata files
#library(rdhs)   ### RUNNING THROUGH WITHOUT IT
library(dplyr)
library(ggplot2)

r_folder<-paste(source_folder, "Rfiles/", sep="")
csv_folder<-paste(source_folder, "MICScsv/", sep="")

#  source(paste(r_folder,"MICS_education.R",sep="")) this file must be run to get informaiton on education for a new survey
source(paste(r_folder,"ShapleyValue.R",sep=""))
source(paste(r_folder,"MICS_get_data.R",sep=""))
source(paste(r_folder,"MICS_Unmet_Need.R",sep=""))
source(paste(r_folder,"TreeAndLogistic.R",sep=""))
source(paste(r_folder,"general_output.R",sep=""))

csvfile_name<-"MICS"
edcationcsv<-"EducationMICS"
religioncsv<-"ReligionMICS"

### Create logger
# eclac-logger.log: This tracks everything that goes right (message) and wrong (errors). 
logger_name <- "eclac-logger.log"
logger_location <- paste(source_folder, logger_name)
logger <- create.logger(logfile = logger_location, level = 'DEBUG')

######################################
# use_version 1 is the research/develope version
# use_version 3 is the drupal data generation version, only go to proceed
# the tree analysis etc if validated
# a validation file must be provided
#######################################
#### initialIndex is to create index for each analytic results
#### so we can save it under a unique name
#######################################

run_together<-function(csv_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, education_name, 
                       religion_name=NULL,  religion=FALSE, region_flag=FALSE, use_version=1, validationfile=NULL, initialIndex=0)
{
  drupalIndex<-initialIndex
  country_ISO<-country_ISO(country_code)
  svnm<-paste(country_code, version_code, sep="")
  
  csvfile_name<-paste(country_code, version_code, csvfile_name, sep="")
  if(!file.exists(paste(csv_folder, csvfile_name, ".csv", sep=""))) {
    print(paste(csvfile_name, " is required. R cannot run without it."))
    return(drupalIndex)
  }
  meta_data<-read.table(paste(csv_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character", nrows=300)
  
  # reading education data
  education_data<-read.table(paste(csv_folder, education_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  education_data<-education_data[education_data$SurveyName==paste(country_code, version_code, sep=""), ]
  
  if(nrow(education_data)==0) {
    print("education data missing, please consult user manual")
    return(drupalIndex)
  }
  
  # if needed, read the religion ethnicity and language data
  religion_data<-NULL
  if(!is.null(religion_name) & religion){
    if(!file.exists(paste(csv_folder, religion_name, ".csv", sep=""))) {
      print("No religion data found, can't analyze with religion")
      return(drupalIndex)
    }
    religion_data<-read.table(paste(csv_folder, religion_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
    religion_data<-religion_data[religion_data$SurveyName==paste(country_code, version_code, sep=""), ]
    if(nrow(religion_data)==0){
      print("religion/ethnicity metadata not ready, r-program stopped")
      return(drupalIndex)
    }
  }
  
  if(use_version>1) {
    if(is.null(validationfile)) {
      print("can't run this version when validation file not provided")
      return(drupalIndex)
    }
    else validationdata<-read.table(validationfile, sep=",", header=T, colClasses="character")
    Rlist<-unique(validationdata$IndicatorName)
  }
  
  info(logger, "Run_together function called.")
  
  # Type of Datasets: hh, hl, wm, ch, mn
  dataSet<-unique(meta_data$DataSet)
  dataSet<-dataSet[!(dataSet %in% c("DataSet", "mn"))]
  
  # dataSet<-c("wm")
  for(ds in dataSet){

    # Creating output folder: Example ~ ./dat_download/Afghanistan 2015/HR 
    ## this function ds_output() is defined in the config file
    ds_output_folder <-ds_output(output_folder, ds)
    ifelse(!dir.exists(ds_output_folder), dir.create(ds_output_folder), FALSE)
    
    dataList<-meta_data[meta_data$DataSet==ds, ]
    responseList<-dataList[dataList$IndicatorType=="ResponseV", ]
    
    responseList<-responseList[!(responseList$NickName %in% c("Covid", "Covid1", "Covid2", "LearningHL", "WaterOnstieHL", "HandwashHL", "SafeSanitationHL", "NotCrowdedHL")), ]
    
    
    if(ds=="hh") educationList<-education_data[education_data$DataSet=="hl", ]
    else educationList<-education_data[education_data$DataSet==ds, ]
    
    # File name: Example ~ BUCH2017.
    filename<-paste(country_code, toupper(ds), version_code, sep="")
    # Printing current iteration of datatype.  
    message <- paste("## File name: ", filename, "  ############################")
    print(message)
    info(logger, message)
    message <- paste("## Current dataset: ", match(ds, dataSet), " of ", length(dataSet))
    print(message)
    info(logger, message) 

    regionVar<-dataList$VarName[dataList$NickName=="Region"]
    provinceVar<-dataList$VarName[dataList$NickName=="Province"]
    # Import data 
    df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName), regionVar, provinceVar)
    colnames(df)<-toupper(colnames(df))

    if(ds=="hh"){
      ##### obtaining highest education level for household from hl data set
      df <- add_highestEducation(df, meta_data, data_folder, country_code, version_code,
                                dataList)
      
    }
    else if(ds %in% c("ch", "wm") ) {
      #### adding number of children under 5 for each household to ch and wm datasets.traceback
      df <- add_childrenU5(df, meta_data, data_folder, country_code, version_code,
                           dataList) 
    }

    if(ds=="hl") {
      df <- add_birthhistory(df, dataList) #not needed anymore?
      if("Covid1" %in% responseList$NickName | "Covid2" %in% responseList$NickName | "Covid" %in% responseList$NickName){
          Indicators <- c("ID", "Learning", "WallRoof", "Handwash", "NotCrowded", "ResponseV", "AuxResonpse") 
          dataList2<-meta_data[meta_data$DataSet=="hh" & meta_data$IndicatorType %in% Indicators, ]
          df <- add_hhdata(df, data_folder, country_code, version_code,
                       dataList, dataList2)
          dataList<-rbind(dataList, dataList2)
      }
    }  

    if(religion) {
      regList<-unique(religion_data$NickName)
      #### first, if religion information exists in hh dataset, as indicated in the relidionMICS.csv, not in other datasets, we have to add it
      religionVar<-dataList$VarName[dataList$NickName=="Religion"]
      if(length(religionVar)>0) k<-match(religionVar, colnames(df), nomatch = 0)
      else k<-0
      print(c("reglist", regList))
      k1<-length(intersect(c("Religion"), regList))
      if(k==0 & k1>0){
        df<-add_reglist(data_folder, country_code, version_code, df, meta_data, dataList, c("Religion"), religion_data)
      }
      
      #### second, if Ethnicity exists in hh dataset, not in other datasets, we need to add one of them, 
      ethnicityVar<-dataList$VarName[dataList$NickName=="Ethnicity"]
      k<-match(ethnicityVar, colnames(df), nomatch = 0)
      k1<-length(intersect(c("Ethnicity"), regList))
      if(k==0 & k1>0){
        df<-add_reglist(country_code, version_code, df, meta_data, dataList, c("Ethnicity"), religion_data)
      }
      
      #### Third, when ethnicity is not available, but Language exist in hh dataset, not in other datasets, we need to add one of them, 
      languageVar<-dataList$VarName[dataList$NickName=="Language"]
      k2<-match(languageVar, colnames(df), nomatch = 0)
      k3<-length(intersect(c("Language"), regList))
      if(k==0 & k1==0 & k2==0 & k3>0){
        df<-add_reglist(country_code, version_code, df, meta_data, dataList, c("Language"), religion_data)
      }
    }
              
      
    print("···debug; checking colmun names")
    print(colnames(df))
     
    # Sample Weight Pre-processing 
    swV<-toupper(dataList$VarName[dataList$NickName=="SampleWeight"])
    swK<-match(swV, colnames(df))

    if(is.na(swK) | sum(is.na(df[, swK]))==nrow(df))
        df$SampleWeight<-1
    else df$SampleWeight<-as.numeric(as.character(df[, swK]))

    df$SampleWeight[is.na(df$SampleWeight)] <- 0

    #print(sum(df$SampleWeight))
    
    # Response variables to model 
    # if(use_version>1){
    #   responseList<-responseList[responseList$NickName %in% Rlist ,]
    #   responseList<-responseList[!responseList$NickName %in% c("Covid", "LearningHL", 
    #                                                            "WaterOnstieHL", "HandwashHL", "SafeSanitationHL", "NotCrowdedHL") ,]
    #   
    # }
    # 

    # responseList<-dataList[dataList$NickName=="MobilePhone" & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("Covid", "LearningHL", "WaterOnstieHL", "HandwashHL", "SafeSanitationHL", "NotCrowdedHL") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("Covid") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("BasicWater") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("SecondaryEducation2035", "HigherEducation2535", "SecondaryEducation35plus") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("EarlyEducation25", "EarlyEducation35") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("CleanWater") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("CleanFuel") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("SafeSanitation") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("ContraceptiveMethod") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("MobilePhone") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("HealthInsurance") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("ProfessionalHelp") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName=="InternetUse" & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("Covid", "NotCrowdedHL") & dataList$IndicatorType=="ResponseV", ]
    
    

    
    print(c(ds, responseList))
    rn<-nrow(responseList)
    if(rn>0){
    for(i in c(1:rn)){

      # Printing current iteration of response variable. 
      rv<-responseList$NickName[i]
      rtp<-responseList$DataType[i]
      message <- paste("Random variable: ", rv, "  ------------------")
      print(message)
      info(logger, message)
      message <- paste("Current Response Variable: ", i, " of ", rn)
      print(message)
      info(logger, message)
      
      # Change Rdata output folder: Example ~ /mics_Rdata/HealthInsurance/mdAFHR70FL.Rdata 
      # Note: if you want to organise by Country --> Response Variable just need to add one level up. 
      # rv_Rdata_folder <- rv_Rdata(mics_Rdata_folder, rv)
      # Retrieve Independent variable list. 
      
      indvar<- indList(rv)

      if(religion) {
        indvar<-c(indvar, regList)
        # Retrieve dataset for given Response/Independent variables. 
      }
      pass_message <- "Successfuly retrieved dataset for given RV/IV [get_data]"

      datause <- catch_error(get_data(df, rv, dataList, indvar, svnm, educationList, religion_data))
      
      k<-match("Residence", colnames(datause), nomatch = 0)
      if(k>0) print((table(datause$Residence, datause$HH6)))
      else print("No residence var found")
        info(logger, paste(pass_message))
        mr_ds<-unique(meta_data[!is.na(meta_data$VarName) & meta_data$NickName==rv & meta_data$IndicatorType=="MresponseV", c("DataSet")])
        
        # if mn exists and response variable name in it has been found
        # we assume the mn and wm has the same education labels and religion ethnicity language labels
        if(length(mr_ds)>0){
          n0<-nrow(datause)
          # Example: ./dat_download/Afghanisatan 2015/
          datause <- merge_mr(mr_ds, meta_data, datause, dataList, country_code, version_code, data_folder, rv, indvar, svnm, educationList, religion_data, region_flag) 
          if(!is.null(datause))
          if(nrow(datause)>n0) indvar<-c(indvar, "Sex")
        }

        #### start of output
        ####  we create result log files at national level for every indicator we calculate
        result_log<-ResultList("MICS", country_ISO, version_code, country_ISO, version_code, ds, rv, religion, "National", !is.null(datause))

        if(is.null(datause)) {
          result_log$NationalValidated="NoData"
          print("Data not generated") 
          error(logger, "Data not generated")
          logcsv<-paste(ds_output_folder, "noDatalogfile.csv", sep="")
          result_log<-t(result_log)
          if(file.exists(logcsv))
            write.table(result_log, logcsv, sep=",", 
                        append = TRUE,   col.names = F, row.names = F)
          else write.table(result_log, logcsv, sep=",", 
                           append = FALSE,   col.names = T, row.names = F)
        } 
        else {
          drupalIndex<-output_data(datause, "MICS", country_code, version_code, country_ISO, version_code,  rv, rtp, indvar, ds, ds_output_folder, validationdata, 
                                   religion, region_flag=FALSE, use_version, drupalIndex)
          if(region_flag){
            drupalIndex<-output_data(datause, "MICS", country_code, version_code, country_ISO, version_code, rv, rtp, indvar, ds, ds_output_folder, validationdata, 
                                     religion, region_flag=TRUE, use_version, drupalIndex)
            
          }
        }
    }
    }
  }
  if(use_version==3) return(drupalIndex)
}

######################################
# (csv_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, education_name, 
#  religion_name=NULL,  religion=FALSE, region_flag=FALSE, use_version=1, validationfile=NULL, initialIndex=0)

# run_together(csv_folder, mics_data_folder, output_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name, edcationcsv)

# run_together(csv_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name, edcationcsv)
