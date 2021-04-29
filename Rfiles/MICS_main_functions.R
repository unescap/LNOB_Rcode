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
source(paste(r_folder,"MICSShapleyValue.R",sep=""))
source(paste(r_folder,"MICS_get_data.R",sep=""))
source(paste(r_folder,"MICS_Unmet_Need.R",sep=""))
source(paste(r_folder,"MICSTreeAndLogistic.R",sep=""))

csvfile_name<-"MICS"
edcationcsv<-"EducationMICS"
religioncsv<-"ReligionMICS"

### Create logger
# eclac-logger.log: This tracks everything that goes right (message) and wrong (errors). 
logger_name <- "eclac-logger.log"
logger_location <- paste(source_folder, logger_name)
logger <- create.logger(logfile = logger_location, level = 'DEBUG')

######################################


run_together<-function(csv_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, education_name, 
                       religion_name=NULL,  religion=FALSE, region_flag=FALSE, Flag_New=TRUE)
{

  # Reading MICSstandard.csv file. 

  csvfile_name<-paste(country_code, version_code, csvfile_name, sep="")

  meta_data<-read.table(paste(csv_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character", nrows=300)
  
  svnm<-paste(country_code, version_code, sep="")
  # reading education data
  education_data<-read.table(paste(csv_folder, education_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  education_data<-education_data[education_data$SurveyName==paste(country_code, version_code, sep=""), ]
  if(nrow(education_data)==0) {
    print("education data missing, please consult user manual")
    return()
  }
  
  # if needed, read the religion ethnicity and language data
  religion_data<-NULL
  if(!is.null(religion_name) & religion){
    religion_data<-read.table(paste(csv_folder, religion_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
    religion_data<-religion_data[religion_data$SurveyName==paste(country_code, version_code, sep=""), ]
    if(nrow(religion_data)==0){
      print("religion/ethnicity metadata not ready, r-program stopped")
      return()
    }
  }
  info(logger, "Run_together function called.")
  
  # Type of Datasets: hh, hl, wm, ch, mn
  dataSet<-unique(meta_data$DataSet)
  dataSet<-dataSet[!(dataSet %in% c("DataSet", "mn"))]
  
  
  
  dataSet<-c("hh")
  for(ds in dataSet){
    print(ds)
    # Creating output folder: Example ~ ./dat_download/Afghanistan 2015/HR 
    ds_output_folder <-ds_output(output_folder, ds)
    dataList<-meta_data[meta_data$DataSet==ds, ]
    responseList<-dataList[dataList$IndicatorType=="ResponseV", ]
    
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
        df<-add_reglist(country_code, version_code, df, meta_data, dataList, c("Religion"), religion_data)
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

    # Response variables to model 


    # responseList<-dataList[dataList$NickName=="MobilePhone" & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("Covid", "LearningHL", "WaterOnstieHL", "HandwashHL", "SafeSanitationHL", "NotCrowdedHL") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("Covid") & dataList$IndicatorType=="ResponseV", ]
    
    # responseList<-dataList[dataList$NickName %in% c("BasicWater") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("SecondaryEducation2035", "HigherEducation2535", "SecondaryEducation35plus") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("EarlyEducation25", "EarlyEducation35") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("CleanWater") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("CleanFuel") & dataList$IndicatorType=="ResponseV", ]
    responseList<-dataList[dataList$NickName %in% c("SafeSanitation") & dataList$IndicatorType=="ResponseV", ]
    # responseList<-dataList[dataList$NickName %in% c("ContraceptiveMethod") & dataList$IndicatorType=="ResponseV", ]
     
    rn<-nrow(responseList)
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
      rv_Rdata_folder <- rv_Rdata(mics_Rdata_folder, rv)
      
      # Retrieve Independent variable list. 
      indvar<- indList(rv)

      if(religion) {
        indvar<-c(indvar, regList)
        # Retrieve dataset for given Response/Independent variables. 
      }
      pass_message <- "Successfuly retrieved dataset for given RV/IV [get_data]"

      print(rv)

      datause <- catch_error(get_data(df, rv, dataList, indvar, svnm, educationList, religion_data))
      
      if(is.null(datause)) {
        print("Data not generated") 
        error(logger, "Data not generated")
      } 
      else {

        info(logger, paste(pass_message))
        mr_ds<-unique(meta_data[!is.na(meta_data$VarName) & meta_data$NickName==rv & meta_data$IndicatorType=="MresponseV", c("DataSet")])
        
        if(length(mr_ds)>0){
          n0<-nrow(datause)
          # Example: ./dat_download/Afghanisatan 2015/
          datause <- merge_mr(mr_ds, meta_data, datause, dataList, country_code, version_code, data_folder, rv, indvar, svnm, educationList, religion_data, region_flag) 
          
          if(nrow(datause)>n0) indvar<-c(indvar, "Sex")
        }

        
        # if mn exists and response variable name in it has been found
        # we assume the mn and wm has the same education labels and religion ethnicity language labels
        
        if(region_flag) indvar<-c(indvar, "REGION")

        formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
        title_string<-paste(rv, paste(indvar, collapse=" + "), sep=" ~ ")

        #### tree
        #### add a data type parameter, if numeric, we use a different criterion
        sub_string<-NULL
        country_ISO<-country_ISO(country_code)
        write_tree(datause, country_ISO, version_code,
                   title_string, formula_string, sub_string, rv, rtp, religion, ds_output_folder, ds, filename)

# ##### disable D and Logistic for validation
#         #### HOI and dis-similarity index calculation
#         #### not sure if this works for numeric
#         write_HOI_D(datause, country_ISO, version_code, title_string, indvar, ds_output_folder, filename)
# 
#         #### logistic regression
#         #### have to use lm for numeric here
#         write_glm(datause, rtp, country_ISO, version_code, title_string, indvar, ds_output_folder, filename)
# ##### disable D and Logistic for validation
        
        #### Construct model for each region.
        # region(output_folder, country_code, version_code,
        #        datause, rv,
        #        formula_string, title_string, sub_string,
        #        filename, indvar)
        
        # write_crosstab(datause, country_code, version_code, title_string, indvar, ds_output_folder, filename)

      }
     }
  }
}

######################################
# run_together(csv_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2014",  csvfile_name, edcationcsv)
# run_together(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name, edcationcsv)
run_together(csv_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name, edcationcsv)
