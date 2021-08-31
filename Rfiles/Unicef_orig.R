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

#### 
# Author: Mohamed 
# Country: Mongolia 
# Survey: MICS 6 
# Survey year: 2018 


##### Merging #### 

# The first step consist of dowloading the files and proceed to the right merging: 

# Importing MICS Data. We need 4 files: household, householdlisting, children and women files.

#### 1st Merge: household listing + women ######

library(haven)


data_folder<-"/home/yw/Workspace/rstudio/SDD2017/sav_download/Mongolia2018/"
Household <- read_sav(paste(data_folder, "hh.sav", sep=""), encoding = NULL, user_na = TRUE)
HouseholdListing <- read_sav(paste(data_folder,"hl.sav", sep=""), encoding = NULL, user_na = TRUE)


# Variables that uniquely identify the households are the cluster number (HH1) and the Household number 
#(HH2). We need to sort by these variables FIRST. Each household member also 
#has a line number (HL1) but this variable is named differently in some files, 
#like the children's file. We will save the sorted file using a new name. 

# We sort by key variables, sort HH1, HH2 and HL1: 
library(dplyr)
Household <- arrange(Household, HH1, HH2)
HouseholdListing <- arrange(HouseholdListing, HH1, HH2, HL1)

# We apply the same process to the children's file and rename LN as HL1:
library(dplyr)
Children <- read_sav(paste(data_folder, "ch.sav", sep=""), encoding = NULL, user_na = TRUE)
Children <- rename(Children, HL1 = LN)
Children <- arrange(Children, HH1, HH2, HL1)

# We also apply the same process to the women's file and rename LN as HL1:
women <- read_sav(paste(data_folder, "wm.sav", sep=""), encoding = 'latin1', user_na = TRUE)
women <- arrange(women, HH1, HH2, LN) 
women <- rename(women, HL1 = LN)

#### 1st Merge: household listing + women ######

# we first merge the household listing file with the women's file based on the arranged variables:

Mergedfile1 <- merge(HouseholdListing, women, all.x = TRUE) %>% group_by(HH1, HH2, HL1)


##### 2nd Merge: (HL + WM) with Children file ##### 

Mergedfile2 <- merge(Mergedfile1, Children, by = c("HH1", "HH2", "HL1"), all.x = TRUE) %>% group_by(HH1, HH2, HL1)


##### 3rd and Final Merge: [(HL+WM)+ Ch] with the household file #####

Mergedfile3 <- merge(Mergedfile2, Household, all = TRUE)
Mergedfile3 <- arrange(Mergedfile3, HH1, HH2, HL1) %>% group_by(HH1, HH2, HL1)


#### Important to note: the merge with the women and the children's files generate duplicate variables.
#### After long discussions with Ismael, enrique and Yadigar, I decided to go ahead with the merge and use the "x" variables for the estimates.

# We rename the newly merged file for clarity purpose:
Masterfile <- Mergedfile3 



# We can now start generating dimension by dimension before doing the overall aggregation. 
# We will also generate thresholds and ranges, and average number of deprivation per child. 



####### 1st Dimension: Housing/Shelter #######


### Notes and Definition of the Housing/Shelter Dimension ###

# Severe housing/shelter deprivation is defined as children in dwellings with 5+ people per room (i.e., severe overcrowding) 

# Moderate and severe deprivation  accounts for children in dwellings with 3 or more  people per room (i.e., moderate + severe overcrowding)

# VARIABLES LIKELY TO CHANGE

x <- wtd.table(Masterfile$HH48, weight = Masterfile$hhweight)
print(freq(x))

CrossTable(Masterfile$HH48)


# ···YW running error message: "Error in proxy[, ..., drop = FALSE] : incorrect number of dimensions"
# x <- wtd.table(Masterfile$HC3, weight = Masterfile$hhweight)
# print(freq(x))
# 
# CrossTable(Masterfile$HC3)

# Now we generate the person per room variable to estimate the housing dimension:

Masterfile1 <- Masterfile %>% mutate(personperroom = ifelse(HC3 != 99, HH48/HC3, NA))
x <- wtd.table(Masterfile1$personperroom, weight = Masterfile1$hhweight)
print(freq(x)) # might not need to weight here. So we can use "CrossTable" function.:

CrossTable(Masterfile1$personperroom)

##### Filtering the masterfile. Since the unit of analysis is the child  - defined by UNICEF as any person whose age is less than 18 - we drop the persons that are 18 years old and older. We must only keep people under 18:
Children_Masterfile1 <- filter(Masterfile1, HL6 < 18)

# To confirm: Age 
summary(Children_Masterfile1$HL6)
# or: 
glimpse(Children_Masterfile1$HL6)


### Having generated this new variable of personsperroom, we move on to 
#generate the indicator of severe overcrowding, which identifies those 
#children living in dwellings with more than five people per room as severely
#deprived: 

# Severe Overcrowding

Children_Masterfile1$severcrowdingdep <- ifelse(Children_Masterfile1$personperroom >= 0.25 & Children_Masterfile1$personperroom < 5, 0, ifelse(Children_Masterfile1$personperroom >=5 & Children_Masterfile1$personperroom <=10 , 1,NA))
x <- wtd.table(Children_Masterfile1$severcrowdingdep, weight = Children_Masterfile1$hhweight)
# YW note on code: why use household weight, not household member weight?
questionr::freq(x)

CrossTable(Children_Masterfile1$severcrowdingdep)

# Moderate and Severe Overcrowding

Children_Masterfile1$modercrowdingdep <- ifelse(Children_Masterfile1$personperroom >= 0.25 & Children_Masterfile1$personperroom < 3, 0, ifelse(Children_Masterfile1$personperroom >= 3 & Children_Masterfile1$personperroom <= 10, 1,NA))
x <- wtd.table(Children_Masterfile1$modercrowdingdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)


CrossTable(Children_Masterfile1$modercrowdingdep)

# Aggregate Measure to see the dimension on both thresholds:

CrossTable(Children_Masterfile1$severcrowdingdep,Children_Masterfile1$modercrowdingdep)
x <- wtd.table(Children_Masterfile1$severcrowdingdep, Children_Masterfile1$modercrowdingdep, weight = Children_Masterfile1$hhweight)
# CrossTable(x)



##### Dimension 2: Water ###### 


#### Moderate + Severe Water Deprivation for UNICEF ##

### Severe deprivation in the dimension of safe driking water is characterized 
###by children (under 18) who only have access to surface water (e.g., rivers) for 
###driking

###Let us start with severe water deprivation by source, which is characterized 
###by children (<18) who who only have access to surface water (e.g., rivers) for 
###driking 

### PLease make sure to confirm the variable in the data:
print(Children_Masterfile1$WS1)

## Severe Water deprivation:

Children_Masterfile1$severewaterdep <- ifelse(Children_Masterfile1$WS1 != 81 & Children_Masterfile1$WS1 != 99, 0, ifelse(Children_Masterfile1$WS1 == 81, 1, ifelse(Children_Masterfile1$WS1 == 99, NA, NA)) )
CrossTable(Children_Masterfile1$severewaterdep)
x <- wtd.table(Children_Masterfile1$severewaterdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

## Moderate Water Deprivation:
##Moderate deprivation in the water dimension is characterized by children 
##who live in housheolds without access to an improved water source; for this, 
##please see the Joint Monitoring Programme Ladder report, page 82.

Children_Masterfile1$moderatewaterdep <- ifelse(Children_Masterfile1$WS1 == 81|Children_Masterfile1$WS1 == 22|Children_Masterfile1$WS1 == 32|Children_Masterfile1$WS1 == 42|Children_Masterfile1$WS1 == 96 , 1, ifelse(Children_Masterfile1$WS1 == 99, NA, 0))
CrossTable(Children_Masterfile1$moderatewaterdep)
x <- wtd.table(Children_Masterfile1$moderatewaterdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

## Aggregate Measure: 

CrossTable(Children_Masterfile1$severewaterdep, Children_Masterfile1$moderatewaterdep)
x <- wtd.table(Children_Masterfile1$severewaterdep, Children_Masterfile1$moderatewaterdep, weight = Children_Masterfile1$hhweight)
# CrossTable(x) # This will always give you the weighted cross tabulation in constrast to "CrossTable" function. 



##### Dimension 3: Sanitation #########


##Severe deprivation in the dimension of sanitation is characterized by 
#children (under 18) who lack access to a toilet facility of any kind in the 
#vicinity of their dwelling; that is, no private, or communal, toilets or 
#latrines##

# Always verify that the variable is the accurate one:
print(Children_Masterfile1$WS11)

### Severe Sanitation Deprivation:
Children_Masterfile1$severesanitationdeprived <- ifelse(Children_Masterfile1$WS11 == 95 , 1, ifelse(Children_Masterfile1$WS11 == 99, NA, 0))
CrossTable(Children_Masterfile1$severesanitationdeprived)
x <- wtd.table(Children_Masterfile1$severesanitationdeprived, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

### Moderate and Severe Sanitation Deprivation for UNICEF
#For definition of moderate deprivation in this space, we count children who
#live in households without access to an improved sanitation facility; for this, 
#please see the Joint Monitoring Programme Ladder report, page 82.

Children_Masterfile1$moderatesanitationdeprived <- ifelse(Children_Masterfile1$WS11 == 95|Children_Masterfile1$WS11 == 23|Children_Masterfile1$WS11 == 41|Children_Masterfile1$WS11 == 51|Children_Masterfile1$WS11 == 96 , 1, ifelse(Children_Masterfile1$WS11 == 99, NA, 0))
CrossTable(Children_Masterfile1$moderatesanitationdeprived)
x <- wtd.table(Children_Masterfile1$moderatesanitationdeprived, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

### Aggregate Measure: 

CrossTable(Children_Masterfile1$severesanitationdeprived, Children_Masterfile1$moderatesanitationdeprived)
x <- wtd.table(Children_Masterfile1$severesanitationdeprived, Children_Masterfile1$moderatesanitationdeprived, weight = Children_Masterfile1$hhweight)
# questionr::freq(x)




##### Dimension 4: Nutrition/Stunting#####

##Severe deprivation in the dimension of nutrition is characterized by 
##children (under 5) who are severely stunted; that is, less than -3 standard
##deviations from the internatinal reference standard ##

##IMPORTANT NOTE: Prior to generating this indicator, one has to check 
##for flags. The best way to do this is to look for variables with the word 
##flag in the name or label ##

##For chronic/severe stunting, check variable HAZ2, which captures height for
##age z-scores by WHO standard ## 

print(Children_Masterfile1$HAZ2)

## Note that in this tabulation above, we are not controlling for possible 
## errors or values that are outside of the correct range. Thus, check for flags ## 

print(Children_Masterfile1$HAZFLAG)

## Severe Stunting: 

Children_Masterfile1$severestunting <- ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2 > -3.0 & Children_Masterfile1$HAZFLAG==0, 0, ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2<=-3.0 & Children_Masterfile1$HAZFLAG==0, 1, NA))
CrossTable(Children_Masterfile1$severestunting)
x <- wtd.table(Children_Masterfile1$severestunting, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#That is, the share of children whose height for age is less than -3 
#standard deviations from the WHO standard and as a result are chronically
#stunted

## Moderate Stunting: 

Children_Masterfile1$moderatestunting <- ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2 > -2.0 & Children_Masterfile1$HAZFLAG==0, 0, ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2<= -2.0 & Children_Masterfile1$HAZFLAG==0, 1, NA))
CrossTable(Children_Masterfile1$moderatestunting)
x <- wtd.table(Children_Masterfile1$moderatestunting, weight = Children_Masterfile1$hhweight)
questionr::freq(x)




####### Dimension 5: Education ####### (This dimension needs further revision and discussion with Stata programmers and education specialists)


#### Moderate + Severe Education Deprivation for UNICEF ##

CrossTable(Children_Masterfile1$ED4.x, Children_Masterfile1$ED9)

### Young cohort: 7 to 14 years old ### 

## Severe Threshold (if child has ever attended school, 1 = Yes; 2 = NO):

Children_Masterfile1$severeducdepbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4.x ==1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4.x ==2, 1, NA))

Children_Masterfile1$severeducdepbelow15a <- ifelse(Children_Masterfile1$ED4.x == 2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeducdepbelow15)

Children_Masterfile1$severeducdepbelow15b <- ifelse(Children_Masterfile1$ED4.x == 1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeducdepbelow15)


# Generate the indicator:

x <- wtd.table(Children_Masterfile1$severeducdepbelow15b, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

###  YW running error: "Error in dn[[2L]] : subscript out of bounds"
# CrossTable(x)
CrossTable(Children_Masterfile1$severeducdepbelow15b) # optional to check difference with unweighted

## Moderate Threshold:

Children_Masterfile1$moderateedudeprivbelow15 <- Children_Masterfile1$severeducdepbelow15b

Children_Masterfile1$moderateedudeprivbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 2, 1, Children_Masterfile1$severeducdepbelow15b))

Children_Masterfile1$moderateedudeprivbelow15a <- ifelse(Children_Masterfile1$ED4.x==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$moderateedudeprivbelow15)
# Generate the indicator:

x <- wtd.table(Children_Masterfile1$moderateedudeprivbelow15a, weight = Children_Masterfile1$hhweight)
questionr::freq(x)
#### YW running error: "Error in dn[[2L]] : subscript out of bounds"
# CrossTable(x)
CrossTable(Children_Masterfile1$moderateedudeprivbelow15a) # optional to check difference with unweighted

## Generate the dimension (aggregate mesure):

CrossTable(Children_Masterfile1$severeducdepbelow15, Children_Masterfile1$moderateedudeprivbelow15)
x <- wtd.table(Children_Masterfile1$severeducdepbelow15, Children_Masterfile1$moderateedudeprivbelow15, weight = Children_Masterfile1$hhweight)

### YW running error note: "Error in `.rowNamesDF<-`(x, value = value) : invalid 'row.names' length"
# questionr::freq(x)
CrossTable(x)


### Older Cohort: 15 to 17 years old: 

## Severe Threshold:

# Child attended school at some point during the year of the survey (0) or has never attended school (1):

Children_Masterfile1$severeedudeprived15older <- ifelse(((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4.x==1)) | ((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED9==1)), 0, ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4.x==2), 1, NA))

#Please keep in mind that there's no direct relationship between ED5A and ED6; 
#we use here simply to say that children who have not completed any grade within 
#primary or less are severely deprived

Children_Masterfile1$severeedudeprived15olderA <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED5A.x <= 1 & Children_Masterfile1$ED6.x==2 & Children_Masterfile1$ED9==2, 1, Children_Masterfile1$severeedudeprived15older)

Children_Masterfile1$severeedudeprived15olderB <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9 == 2 & (Children_Masterfile1$ED6.x==1|Children_Masterfile1$ED6.x==8) & Children_Masterfile1$ED5A.x==1 & Children_Masterfile1$ED5B.x<6, 1, Children_Masterfile1$severeedudeprived15olderA)

Children_Masterfile1$severeedudeprived15olderC <- ifelse(Children_Masterfile1$ED4.x==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeedudeprived15olderB)

Children_Masterfile1$severeedudeprived15olderD <- ifelse(Children_Masterfile1$ED4.x==1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeedudeprived15olderC)


# Tabulate the indicator:

x <- wtd.table(Children_Masterfile1$severeedudeprived15olderD, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

# YW running error note: "Error in dn[[2L]] : subscript out of bounds"
# CrossTable(x)
CrossTable(Children_Masterfile1$severeedudeprived15olderD)


## Moderate Threshold: 


Children_Masterfile1$moderateedudeprived15older <- Children_Masterfile1$severeedudeprived15olderD

# Child attended school the year of the survey but below secondary OR Child did not attend the year of survey OR Child did attend, but primary 

Children_Masterfile1$moderateedudeprived15olderA <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==1 & Children_Masterfile1$ED10A<2 & Children_Masterfile1$ED5A.x<2, 1, Children_Masterfile1$moderateedudeprived15older)

# Child did attended the year of the survey and did so in secondary or higher,
#OR child has completed all grades higher than secondary (irresp, OR child has completed
#the highest grade in senior secondary (irrespective of attendance)

Children_Masterfile1$moderateedudeprived15olderB <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==2, 1, Children_Masterfile1$moderateedudeprived15olderA)

Children_Masterfile1$moderateedudeprived15olderC <- ifelse((Children_Masterfile1$HL6 >=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==1 & Children_Masterfile1$ED10A==1, 1, Children_Masterfile1$moderateedudeprived15olderB)

##Line below sends the following children to non-deprived: 
##Child did attended the year of the survey and did so in secondary or higher,
##OR child has completed all grades higher than secondary (irresp, OR child has completed
##the highest grade in senior secondary (irrespective of attendance): 

Children_Masterfile1$moderateedudeprived15olderD <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17)&Children_Masterfile1$ED9==1 & (Children_Masterfile1$ED10A>=2 & Children_Masterfile1$ED10A<=4)|(Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED5A.x>=4 & Children_Masterfile1$ED5A.x<=5) & (Children_Masterfile1$ED6.x==1)|(Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17)&Children_Masterfile1$ED5A.x==3 &Children_Masterfile1$ED5B.x==4 & Children_Masterfile1$ED6.x==1, 0, Children_Masterfile1$moderateedudeprived15olderC)


#Send to missing inconsistencies between never attended and current attendance
#Send to missing inconsistencies between ever attended and no response in current attendance 

Children_Masterfile1$moderateedudeprived15olderE <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & ((Children_Masterfile1$ED4.x==2 & Children_Masterfile1$ED9==1)|(Children_Masterfile1$ED4.x==1 & Children_Masterfile1$ED9==9)), NA, Children_Masterfile1$moderateedudeprived15olderD)

# Tabulate the indicator:

x <- wtd.table(Children_Masterfile1$moderateedudeprived15olderE, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#  CrossTable(x)
CrossTable(Children_Masterfile1$moderateedudeprived15olderE)

###Aggregating the education by groups##

## Severe (both ages):

# To generate the group aggregate I operate a sum of the two severe deprivation indicators in education first:

Children_Masterfile1$SumSevedugroup <- rowSums(Children_Masterfile1[,c("severeducdepbelow15b", "severeedudeprived15olderD")], na.rm=TRUE)

# I then replace the 0 with NAs to avoid counting the NA as o and thus as non-deprived:
Children_Masterfile1$SumSevedugroup <- ifelse(is.na(Children_Masterfile1$severeducdepbelow15b) & is.na(Children_Masterfile1$severeedudeprived15olderD), NA, Children_Masterfile1$SumSevedugroup)

# I can now generate the group severe deprivation dimension in education
Children_Masterfile1$sevedugroup <- ifelse(Children_Masterfile1$SumSevedugroup>=1, 1, ifelse(Children_Masterfile1$SumSevedugroup==0, 0, NA))

x <- wtd.table(Children_Masterfile1$sevedugroup, weight = Children_Masterfile1$hhweight)
questionr::freq(x)
# CrossTable(x)

CrossTable(Children_Masterfile1$sevedugroup)

## Moderate (both ages):

# To generate the group aggregate I operate a sum of the two severe deprivation indicators in education first:

Children_Masterfile1$SumModeredugroup <- rowSums(Children_Masterfile1[,c("moderateedudeprivbelow15a", "moderateedudeprived15olderE")], na.rm=TRUE)

# I then replace the 0 with NAs to avoid counting the NA as o and thus as non-deprived:
Children_Masterfile1$SumModeredugroup <- ifelse(is.na(Children_Masterfile1$moderateedudeprivbelow15a) & is.na(Children_Masterfile1$moderateedudeprived15olderE), NA, Children_Masterfile1$SumModeredugroup)

# I can now generate the group severe deprivation dimension in education
Children_Masterfile1$Moderedugroup <- ifelse(Children_Masterfile1$SumModeredugroup>=1, 1, ifelse(Children_Masterfile1$SumModeredugroup==0, 0, NA))

x <- wtd.table(Children_Masterfile1$Moderedugroup, weight = Children_Masterfile1$hhweight)
questionr::freq(x)
# CrossTable(x)

CrossTable(Children_Masterfile1$Moderedugroup)



####### Dimension 6: Health ########

### This section is long and includes many indicators. 
### We start with imnumization first with the following definition:
### Here, we look at children between the ages of 12 to 35 months, and we 
### say that in order for a child to not be identified as deprived, they must have 
### received the following four immunizations: 
###1. Measles 
###2. DPT 1 (or Penta 1)
###3. DPT 2 (or Penta 2)
###4. DPT 3 (or Penta 3)
### A child can only be counted as non-deprived if they have received all four
### immunizations. 
### For a child to be severely deprived, they must be missing ALL four 
### vaccinations listed above. 
### For a child to be counted as moderately deprived, they must have anywhere 
### from 1 to 3 of the vaccinations listed above.

#### Measle:
## We start with those that could not have received the vaccine at all:

Children_Masterfile1$measlesdeprived <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1D==0|Children_Masterfile1$IM26==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4, 1, NA)

Children_Masterfile1$measlesdeprivedA <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE <= 35 & Children_Masterfile1$IM26==1, 0, Children_Masterfile1$measlesdeprived)

Children_Masterfile1$measlesdeprivedB <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1D>=1 & Children_Masterfile1$IM6M1D<= 66 , 0, Children_Masterfile1$measlesdeprivedA) 

Children_Masterfile1$measlesdeprivedC <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1M>=1 & Children_Masterfile1$IM6M1M<= 66 , 0, Children_Masterfile1$measlesdeprivedB) 

Children_Masterfile1$measlesdeprivedD <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1Y>=2012 & Children_Masterfile1$IM6M1Y<= 6666 , 0, Children_Masterfile1$measlesdeprivedC) 


CrossTable(Children_Masterfile1$measlesdeprivedD)

# tab1(Children_Masterfile1$measlesdeprivedD)

x <- wtd.table(Children_Masterfile1$measlesdeprivedD, weight = Children_Masterfile1$chweight)
questionr::freq(x)

#### We now move to DPT1 or Penta 1 ####

###/* NOTE: Very important point, codes below MUST account for children in IM2 and
###IM5 that never moved on to review the vaccination card because they don't 
###have one. This means that these children should be counted as deprived. 
###Otherwise we are discounting those children that are deprived because they 
###never received a vaccination.

## We start by identifying as deprived all the children who could not have received a DPT vaccine:

## Deprived based on day of vaccination and IM20 given that the latter is complementary. 

## The codes below must account for children in IM2 and IM5 that never moved on to review the vaccination
## card because they don't have one. This means that these children should be counted as deprived. 
## Otherwise we are discounting those children that are deprived because they never received a vaccination. 


Children_Masterfile1$dpt1deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA1D==0 | Children_Masterfile1$IM20==2 | Children_Masterfile1$IM2==4 | Children_Masterfile1$IM5==4,1, NA)

Children_Masterfile1$dpt1deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21>=1 & Children_Masterfile1$IM21<=7, 0, Children_Masterfile1$dpt1deprived)

# we then the folks below to missing:
Children_Masterfile1$dpt1deprivedB <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==8|Children_Masterfile1$IM21==9, NA, Children_Masterfile1$dpt1deprivedA)

# We then generate the vaccination that capture the day (no need for the year and month here, but it can bring changes):
Children_Masterfile1$dpt1deprivedC <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA1D>=1 & Children_Masterfile1$IM6PENTA1D<=66, 0, Children_Masterfile1$dpt1deprivedB)

# We now cross tabulate the penta 1 estimates:
CrossTable(Children_Masterfile1$dpt1deprivedC)

x <- wtd.table(Children_Masterfile1$dpt1deprivedC, weight = Children_Masterfile1$chweight)
questionr::freq(x)



#### DPT 2 or PENTA 2 #### 

## NOTE: One issue that I notice based on some tabulations is that some people may 
##have the date of their vaccination but not the month or the year. It seems 
##like the larger universe is capture by the date variable, but I am not sure. 
##All of this means that we may want to make sure to capture information from
##all three variables (re date, month, year), and four variables if we count 
##the additional question. */

Children_Masterfile1$dpt2deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==1 & Children_Masterfile1$IM6PENTA2D==0|Children_Masterfile1$IM20==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4, 1, NA)

Children_Masterfile1$dpt2deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21>=2 & Children_Masterfile1$IM21<=7, 0, Children_Masterfile1$dpt2deprived)

Children_Masterfile1$dpt2deprivedB <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==8 | Children_Masterfile1$IM21==9, NA, Children_Masterfile1$dpt2deprivedA)


# tab1(Children_Masterfile1$dpt2deprivedB)

# We now cross tabulate the penta 2 estimates:
CrossTable(Children_Masterfile1$dpt2deprivedB)

x <- wtd.table(Children_Masterfile1$dpt2deprivedB, weight = Children_Masterfile1$chweight)
questionr::freq(x)


#### We now move to DPT 3 or PENTA 3 ####


Children_Masterfile1$dpt3deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & (Children_Masterfile1$IM6PENTA3D==0|Children_Masterfile1$IM20==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4),1, NA)

Children_Masterfile1$dpt3deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA3D>=1 & Children_Masterfile1$IM6PENTA3D<=66, 0, Children_Masterfile1$dpt3deprived)


#I now generate the estimates for the DPT3 or Penta 3:

CrossTable(Children_Masterfile1$dpt3deprivedA)

x <- wtd.table(Children_Masterfile1$dpt3deprivedA, weight = Children_Masterfile1$chweight)
questionr::freq(x)




#### Now that we generated all indicators for vaccinations, we can aggregate them into 1:

Children_Masterfile1$hasmissvaccines <- rowSums(is.na(Children_Masterfile1[,c("measlesdeprivedD", "dpt1deprivedC", "dpt2deprivedB", "dpt3deprivedA")]), na.rm = TRUE)

CrossTable(Children_Masterfile1$hasmissvaccines)

# The tabulation above tells us the children that are missing the 4 vaccines. 
# We will discount them for the aggregation. 

Children_Masterfile1$sumvaccines <- rowSums(Children_Masterfile1[,c("measlesdeprivedD", "dpt1deprivedC", "dpt2deprivedB", "dpt3deprivedA")], na.rm = TRUE)

Children_Masterfile1$sumvaccines <- ifelse(is.na(Children_Masterfile1$measlesdeprivedD) & is.na(Children_Masterfile1$dpt1deprivedC) & is.na(Children_Masterfile1$dpt2deprivedB) & is.na(Children_Masterfile1$dpt3deprivedA), NA, Children_Masterfile1$sumvaccines)

CrossTable(Children_Masterfile1$sumvaccines)

# tab1(Children_Masterfile1$sumvaccines)

## We now generate the moderate deprivation: a child is moderately deprived if he/she is missing any of the 4:

Children_Masterfile1$moderatevaccinesdeprived <- ifelse(Children_Masterfile1$sumvaccines>=1, 1, ifelse(Children_Masterfile1$sumvaccines == 0, 0, NA))

CrossTable(Children_Masterfile1$moderatevaccinesdeprived)

x <- wtd.table(Children_Masterfile1$moderatevaccinesdeprived, weight = Children_Masterfile1$chweight)
questionr::freq(x)

## I now generate the severe deprivation: a child is deprived if he/she is mising the 4:

Children_Masterfile1$severevaccinesdeprived <- ifelse(Children_Masterfile1$sumvaccines == 4, 1, ifelse(Children_Masterfile1$sumvaccines == 0, 0, NA))

# YW error message:
# Error in chisq.test(t, correct = FALSE) : 
#   'x' must at least have 2 elements
# CrossTable(Children_Masterfile1$severevaccinesdeprived)


# tab1(Children_Masterfile1$severevaccinesdeprived)

x <- wtd.table(Children_Masterfile1$severevaccinesdeprived, weight = Children_Masterfile1$chweight)
questionr::freq(x)


########### Acute Respiratory Infection (ARI) and Treatment ##########

## Deprivation in this space is captured for children from 36 to 59 months old
# We first need to generate ARI symptoms
# UNICEF has a specific definition for ARI: 
# Children with symptoms of ARI are those who had an illness with (1) a cough
# accompanied by (2) a rapid or difficult breathing and whose symptoms were due to
# (3) a problem in the chest, OR (4) both a problem in the chest and a blcoked nose. 

CrossTable(Children_Masterfile1$CA16)
CrossTable(Children_Masterfile1$CA17)
CrossTable(Children_Masterfile1$CA18)


## ARI symptoms 

Children_Masterfile1$arisymptoms <- ifelse(Children_Masterfile1$CAGE>=36 & Children_Masterfile1$CAGE>=59 & Children_Masterfile1$CA16==1 & Children_Masterfile1$CA17==1 & Children_Masterfile1$CA18==1 | Children_Masterfile1$CA18==3, 1, NA)

# tab1(Children_Masterfile1$arisymptoms)

## As before, below we generate two thresholds of deprivation; one severe, and
#another that includes moderate and severe: 

# Severe: Child had ARI symptoms and no treatment, of any kind, was sought;
# Moderate: Child had ARI symptoms, and no treatment was sought at an appropriate medical facility; 

## We start with severe: 

Children_Masterfile1$ariseverlydeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==2, 1, ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==1, 0, NA))

CrossTable(Children_Masterfile1$ariseverlydeprived)
# tab1(Children_Masterfile1$ariseverlydeprived)
x <- wtd.table(Children_Masterfile1$ariseverlydeprived, weight = Children_Masterfile1$chweight)
questionr::freq(x)


## Moderate (+severe) threshold: 

Children_Masterfile1$arimoderatedeprived <- Children_Masterfile1$ariseverlydeprived

Children_Masterfile1$arimoderatedeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21A=="A"|Children_Masterfile1$CA21B=="B"|Children_Masterfile1$CA21D=="D"|Children_Masterfile1$CA21F=="F"|Children_Masterfile1$CA21I=="I"|Children_Masterfile1$CA21J=="J"|Children_Masterfile1$CA21K=="K"|Children_Masterfile1$CA21W=="W"), 0, ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21P=="P"|Children_Masterfile1$CA21Q=="Q"|Children_Masterfile1$CA21R=="R"|Children_Masterfile1$CA21X=="X"), 1, NA))

# YW running error:
# Error in chisq.test(t, correct = FALSE) : 
#   'x' must at least have 2 elements
# CrossTable(Children_Masterfile1$arimoderatedeprived)
# tab1(Children_Masterfile1$arimoderatedeprived)
x <- wtd.table(Children_Masterfile1$arimoderatedeprived, weight = Children_Masterfile1$chweight)
questionr::freq(x)



###### Access  to contraception by girls 15 and 17 yrs old #####
##This part is on progress and hasn't been finished yet. So it is not included in this syntax. Further discussion needed with R, Stata and contraception experts. 
## Thus we move to the aggregation part. 



##### I now generate the health dimension by aggregating immunization and ARI

### Severe: 

Children_Masterfile1$hasmissevhealth <- rowSums(is.na(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")]), na.rm = TRUE)

CrossTable(Children_Masterfile1$hasmissevhealth)


Children_Masterfile1$sumsevhealth <- rowSums(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")], na.rm = TRUE)

Children_Masterfile1$sumsevhealth <- ifelse(is.na(Children_Masterfile1$severevaccinesdeprived) & is.na(Children_Masterfile1$ariseverlydeprived), NA, Children_Masterfile1$sumsevhealth)

CrossTable(Children_Masterfile1$sumsevhealth)

## I now generate the severe health dimension: 

Children_Masterfile1$severehealth <- ifelse(Children_Masterfile1$sumsevhealth == 1, 1, ifelse(Children_Masterfile1$sumsevhealth == 0, 0, NA))

CrossTable(Children_Masterfile1$severehealth)
x <- wtd.table(Children_Masterfile1$severehealth, weight = Children_Masterfile1$chweight)
questionr::freq(x)

### Moderate:

Children_Masterfile1$hasmissmoderhealth <- rowSums(is.na(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")]), na.rm = TRUE)

# YW running error:
CrossTable(Children_Masterfile1$hasmissmoderhealth)


Children_Masterfile1$summoderhealth <- rowSums(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")], na.rm = TRUE)

#To not overcount the non-deprived "0" I make sure that the NAs remain NAs:

Children_Masterfile1$summoderhealth <- ifelse(is.na(Children_Masterfile1$moderatevaccinesdeprived) & is.na(Children_Masterfile1$arimoderatedeprived), NA, Children_Masterfile1$summoderhealth)

CrossTable(Children_Masterfile1$summoderhealth)

## I now generate the moderate health dimension: 

Children_Masterfile1$moderatehealth <- ifelse(Children_Masterfile1$summoderhealth >= 1, 1, ifelse(Children_Masterfile1$summoderhealth == 0, 0, NA))

CrossTable(Children_Masterfile1$moderatehealth)
x <- wtd.table(Children_Masterfile1$moderatehealth, weight = Children_Masterfile1$chweight)
questionr::freq(x)



###### Information ######### (This dimension is included in the syntax for reference to furter discussions as to whether we should include information or not. I do not include in the final aggregation of Multidimensional poverty)

## I will now add information as a dimension and compare how it impacts MDP. 
## If the child has no information medium at all, the child is severely derpived. 
## If the child has just 1, he/she is moderately deprived. If > 1, the child is not deprived. 
## Please note that these media are not substitute. 

#1). I start with Radio:

Children_Masterfile1$Radiodep <- ifelse(Children_Masterfile1$HC7B == 2, 1, ifelse(Children_Masterfile1$HC7B == 1, 0, NA))
CrossTable(Children_Masterfile1$Radiodep)
x <- wtd.table(Children_Masterfile1$Radiodep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#2). TV:

#### YW on code: There is no HC9A1 for mongolia 2018

# Children_Masterfile1$TVdep <- ifelse(Children_Masterfile1$HC9A == 1 | (Children_Masterfile1$HC9A == 2 & Children_Masterfile1$HC9A1 == 1), 0, 
#                                      ifelse(Children_Masterfile1$HC9A == 2 & Children_Masterfile1$HC9A1 == 2, 1, NA))



Children_Masterfile1$TVdep <- NA
Children_Masterfile1$TVdep[Children_Masterfile1$HC9A == 1]<-0
Children_Masterfile1$TVdep[Children_Masterfile1$HC9A == 2] <-1

CrossTable(Children_Masterfile1$TVdep)
x <- wtd.table(Children_Masterfile1$TVdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#3). Computer/Tablet

#  YW note on code: There is no HC11A1 in the MOngolia 2018 data
Children_Masterfile1$Compdep <- ifelse(Children_Masterfile1$HC11 == 1, 0, ifelse(Children_Masterfile1$HC11 == 2, 1, NA))
CrossTable(Children_Masterfile1$Compdep)
x <- wtd.table(Children_Masterfile1$Compdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#4). Mobile:
#### YW on code: There is no HC12A1 for mongolia 2018

Children_Masterfile1$Mobdep <- ifelse(Children_Masterfile1$HC12 == 1, 0, ifelse(Children_Masterfile1$HC12 == 2, 1, NA))
CrossTable(Children_Masterfile1$Mobdep)
x <- wtd.table(Children_Masterfile1$Mobdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

#5). Internet: 

Children_Masterfile1$Internetdep <- ifelse(Children_Masterfile1$HC13==1, 0, ifelse(Children_Masterfile1$HC13 == 2, 1, NA))
CrossTable(Children_Masterfile1$Internetdep)
x <- wtd.table(Children_Masterfile1$Internetdep, weight = Children_Masterfile1$hhweight)
questionr::freq(x)


### I now aggregate the 5 indicators to generate the information dimension. 
### I first check if any child has the 5 indicators missing at the same time:

Children_Masterfile1$hasmissinformation <- rowSums(is.na(Children_Masterfile1[,c("Radiodep", "TVdep", "Compdep", "Mobdep", "Internetdep")]), na.rm = TRUE)
CrossTable(Children_Masterfile1$hasmissinformation)

### No child is missing the 5 indicators, so no need to discount anybody. 
### I now proceed to the aggregation:

Children_Masterfile1$suminformation <- rowSums(Children_Masterfile1[,c("Radiodep", "TVdep", "Compdep", "Mobdep", "Internetdep")], na.rm = TRUE)
Children_Masterfile1$Suminformation <- ifelse(is.na(Children_Masterfile1$Radiodep) & is.na(Children_Masterfile1$TVdep) & is.na(Children_Masterfile1$Compdep) & is.na(Children_Masterfile1$Mobdep) & is.na(Children_Masterfile1$Internetdep), NA, Children_Masterfile1$suminformation)
CrossTable(Children_Masterfile1$suminformation)
CrossTable(Children_Masterfile1$Suminformation)


### According to Enrique a child is severely deprived if he/she is lacking all 5 media to access information
## Severe:

Children_Masterfile1$Severeinfo <- ifelse(Children_Masterfile1$Suminformation == 5, 1, ifelse(Children_Masterfile1$Suminformation <= 5, 0, NA))
CrossTable(Children_Masterfile1$Severeinfo)

# YW note on code: this definition seems to be experimenal and should not be here
# Children_Masterfile1$Severeinfo <- ifelse(Children_Masterfile1$Suminformation == 5, 1, ifelse(Children_Masterfile1$Suminformation == 0, 0, NA))
# CrossTable(Children_Masterfile1$Severeinfo)

# YW running error: "Error in plot.new() : figure margins too large"
# tab1(Children_Masterfile1$Severeinfo)

x <- wtd.table(Children_Masterfile1$Severeinfo, weight = Children_Masterfile1$hhweight)
questionr::freq(x)
## Moderate:

Children_Masterfile1$Moderateinfo <- ifelse(Children_Masterfile1$Suminformation == 4, 1, ifelse(Children_Masterfile1$Suminformation < 4, 0, NA))
CrossTable(Children_Masterfile1$Moderateinfo)
x <- wtd.table(Children_Masterfile1$Moderateinfo, weight = Children_Masterfile1$hhweight)
questionr::freq(x)







######## Agregation Exercise to Generate Multi-dimensional Child Poverty ####### (WITHOUT INFORMATION DIMENSION)



### We start with the aggregation of moderate deprivation:

# I first arrange my file with the 6 dimensions I generated. 
Children_Masterfile1 <- Children_Masterfile1 %>% arrange(modercrowdingdep, moderatewaterdep, moderatesanitationdeprived, moderatestunting, Moderedugroup, moderatehealth, Moderateinfo)

#I generate the variable that captures children that are missing all variables


Children_Masterfile1$hassmissmoderatepoor <-rowSums(is.na(Children_Masterfile1[,c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth")]), na.rm = TRUE)

CrossTable(Children_Masterfile1$hassmissmoderatepoor)
x <- wtd.table(Children_Masterfile1$hassmissmoderatepoor)
questionr::freq(x)
# CrossTable(x)



# This tabulation above gives us an idea of the children that are missing in 
#all dimensions. Rarely are children missing in all dimensions; and if so, 
#it is likely we made a mistake somewhere. Worth checking the codes again. 
#Nevertheless, we will generate a code to remove these individuals if they 
#show up. We will do so after the next line of codes

# None is missing the 6 dimensions
#Prior to discounting these children that are missing in all dimensions,  
#we have to aggregate across columns. See code below for this:

Children_Masterfile1$summoderpoor <- rowSums(Children_Masterfile1[, c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth")], na.rm = TRUE)

#Let us check if there were any children missing all

Children_Masterfile1$Summoderpoor <- ifelse(Children_Masterfile1$hassmissmoderatepoor == 6, ".", Children_Masterfile1$summoderpoor)

# We now generate the depth of poverty 

CrossTable(Children_Masterfile1$Summoderpoor)# Unweighted

x <- wtd.table(Children_Masterfile1$Summoderpoor, weight = Children_Masterfile1$hhweight)
questionr::freq(x)


# the ouput obtained gives an idea of how many children are deprived in 1, 2, 3, etc., dimensions - i.e the DEPTH of POVERTY. 

# Now we generate the final incidence/prevalence of multidimensional poverty child poverty.
# If a child has at least 1 deprivation in a dimension, we consider the child multi-dimensionally poor

Children_Masterfile1$moderatelydeprived <- ifelse(Children_Masterfile1$Summoderpoor == 0, 0, ifelse(Children_Masterfile1$Summoderpoor >= 1, 1, NA))
CrossTable(Children_Masterfile1$moderatelydeprived) # Unweighted

x <- wtd.table(Children_Masterfile1$moderatelydeprived, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

# The output represents the share of children moderately deprived.



####################### Aggregation of Severe Deprivation #########################



# I first arrange my file with the 6 dimensions I generated. 
Children_Masterfile1 <- Children_Masterfile1 %>% arrange(severcrowdingdep, severewaterdep, severesanitationdeprived, severestunting, sevedugroup, severehealth, Severeinfo)

#I generate the variable that captures children that are missing all the variables


Children_Masterfile1$hassmissmoderatepoor <-rowSums(is.na(Children_Masterfile1[,c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth")]), na.rm = TRUE)

CrossTable(Children_Masterfile1$hassmissmoderatepoor)
x <- wtd.table(Children_Masterfile1$hassmissmoderatepoor)
questionr::freq(x)
# CrossTable(x)



# This tabulation above gives us an idea of the children that are missing in 
#all dimensions. Rarely are children missing in all dimensions; and if so, 
#it is likely we made a mistake somewhere. Worth checking the codes again. 
#Nevertheless, we will generate a code to remove these individuals if they 
#show up. We will do so after the next line of codes

# None is missing the 6 dimensions
#Prior to discounting these children that are missing in all dimensions,  
#we have to aggregate across columns. See code below for this:

Children_Masterfile1$summoderpoor <- rowSums(Children_Masterfile1[, c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth")], na.rm = TRUE)

#Let us check if there were any children missing all

Children_Masterfile1$Summoderpoor <- ifelse(Children_Masterfile1$hassmissmoderatepoor == 6, ".", Children_Masterfile1$summoderpoor)

# We now generate the depth of poverty 

CrossTable(Children_Masterfile1$Summoderpoor)# Unweighted

x <- wtd.table(Children_Masterfile1$Summoderpoor, weight = Children_Masterfile1$hhweight)
questionr::freq(x)


# the ouput obtained gives an idea of how many children are deprived in 1, 2, 3, etc., dimensions - i.e the DEPTH of POVERTY. 

# Now we generate the final incidence/prevalence of multidimensional poverty child poverty.
# If a child has at least 1 deprivation in a dimension, we consider the child multi-dimensionally poor

Children_Masterfile1$severelydeprived <- ifelse(Children_Masterfile1$Summoderpoor == 0, 0, ifelse(Children_Masterfile1$Summoderpoor >= 1, 1, NA))
CrossTable(Children_Masterfile1$severelydeprived) # Unweighted

x <- wtd.table(Children_Masterfile1$severelydeprived, weight = Children_Masterfile1$hhweight)
questionr::freq(x)

# The output represents the share of children severely deprived.

print("All run through by now")

cn<-colnames(Children_Masterfile1)
ct<-as.data.frame(table(Children_Masterfile1$HL6, Children_Masterfile1$severelydeprived))
ct_age<-table(Children_Masterfile1$HL6)

for(i in c(1:18)){
  ct$pct[i]<-ct$Freq[i]/ct_age[i]
  ct$pct[i+18]<-ct$Freq[i+18]/ct_age[i]
}
