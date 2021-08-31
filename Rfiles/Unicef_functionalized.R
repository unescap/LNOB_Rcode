### R codes for Multidimensional Child Poverty for MICS surveys ### 

### Packages needed:
library(haven) # To read SPSS and Stata files
library(dplyr) # To arrange and clean our dataset and files
library(questionr)# To crosstabulate and generate estimates on weighted surveys
library(gmodels) # Optional: allow crosstabulation but not with weighting (in the CrossTable function)
library(epiDisplay) # Optional: allows to visualize Tab without weighting
### We use the example of Mongolia (MICS 6 2018) to generate the estimates ### 
## Following general recommendations and "code of conduct", we should: 

# 1. Ensure that we have downloaded the correct files for the country/year to estimate
# 2. Specify the researcher/author of the log file
# 3. Specifiy the name of the country, survey type and year of survey in the log file
# 4. Don't forget to set the right directory where your files are located to start the analysis. 

#### based on code originally made by
# Author: Mohamed 
# Country: Mongolia 
# Survey: MICS 6 
# Survey year: 2018 

##### we select related variables before merging
##### Merging #### 
#### 1st Merge: household listing + women ######

library(haven)

# location of the datafiles on my computer
data_folder<-"/home/yw/Workspace/rstudio/SDD2017/sav_download/Mongolia2018/"
csv_folder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/Unicef Files/"
# drceiption file --- rename the variables in a meaningful way (created for the LNOB project, modified)
meta_data<-read.table(paste(csv_folder, "Mongolia2018MICS.csv", sep=""), sep=",", header=T, colClasses="character", nrows=300)

#### 
Household <- importData(data_folder, "hh", meta_data)
HouseholdListing <- importData(data_folder,"hl", meta_data)
ChildrenUnder5 <-  importData(data_folder,"ch", meta_data)
Women <-  importData(data_folder,"wm", meta_data)


HouseholdListing$Age<-as.numeric(as.character(HouseholdListing$HL6.hl))
ALLChildren<-HouseholdListing[!is.na(HouseholdListing$Age), ]
ALLChildren<-ALLChildren[ALLChildren$Age>=0 & ALLChildren$Age<18, ]
ALLChildren<-mics_merge(ALLChildren, Household)
ALLChildren<-mics_merge(ALLChildren, Women)
ALLChildren<-mics_merge(ALLChildren, ChildrenUnder5)



