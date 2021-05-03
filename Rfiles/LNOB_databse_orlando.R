source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"

library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)



r_folder<-paste(source_folder, "Rfiles/", sep="")
csv_folder<-paste(source_folder, "DHScsv/", sep="")
csv_check_folder<-paste(source_folder, "CSVdatabase/", sep="")



country_ISO<-function(country_code){
  if(country_code %in% c("AF", "Afghanistan")) iso<-"AFG"
  else if(country_code %in% c("AM",  "Armenia")) iso<-"ARM"
  else if(country_code %in% c("BD", "Bangladesh")) iso<-"BGD"
  else if(country_code=="Bhutan") iso<-"BTN"
  else if(country_code=="Georgia") iso<-"GEO"
  else if(country_code %in% c("KH", "Cambodia")) iso<-"KHM"
  else if(country_code=="Kazakhstan") iso<-"KAZ"
  else if(country_code %in% c("Kyrgyzstan", "KY")) iso<-"KGZ"
  else if(country_code=="Kiribati") iso<-"KIR"
  else if(country_code %in% c("Lao PDR","Lao")) iso<-"LAO"
  else if(country_code=="Mongolia") iso<-"MNG"
  else if(country_code=="Nepal") iso<-"NPL"
  else if(country_code=="Thailand") iso<-"THA"
  else if(country_code %in% c("Tonga (5 pc)", "Tonga")) iso<-"TON"
  else if(country_code %in% c("VietNam", "Vietnam", "Viet Nam")) iso<-"VNM"
  else if(country_code=="Turkmenistan") iso<-"TKM"
  else if(country_code %in% c( "India", "IA")) iso<-"IND"
  else if(country_code %in% c("Maldives", "MV")) iso<-"MDV"
  else if(country_code %in% c("Myanmar", "MM")) iso<-"MMR"
  else if(country_code %in% c("Nepal", "NP")) iso<-"NPL"
  else if(country_code %in% c("Pakistan", "PK")) iso<-"PAK"
  else if(country_code %in% c("Papua New Guinea", "PG")) iso<-"PNG"
  else if(country_code %in% c("Philippines", "PH")) iso<-"PHL"
  else if(country_code %in% c("Tajikistan", "TJ")) iso<-"TJK"
  else if(country_code %in% c("Timor-Leste", "TL", "Timor-leste")) iso<-"TLS"
  else if(country_code %in% c("Indonesia", "ID")) iso<-"IDN"
  else if(country_code %in% c("Turkey", "TR")) iso<-"TUR"
  else if(country_code %in% c("Azerbaijan", "AZ")) iso<-"AZE"
  else if(country_code %in% c("Uzbekistan")) iso<-"UZB"
  else if(country_code %in% c("Vanuatu")) iso<-"VUT"
  else iso<-country_code  
  
  # print(c(country_code, iso))
  return(iso)
  
}

all_data<-read.table(paste(csv_check_folder, "all_check.csv", sep=""), sep=",", header=T, colClasses="character")
covid_data<-read.table(paste(csv_check_folder, "covid_check.csv", sep=""), sep=",", header=T, colClasses="character")
education_data<-read.table(paste(csv_check_folder, "education_check.csv", sep=""), sep=",", header=T, colClasses="character")
individual_data<-read.table(paste(csv_check_folder, "individual_check.csv", sep=""), sep=",", header=T, colClasses="character")
childhealth_data<-read.table(paste(csv_check_folder, "childhealth_check.csv", sep=""), sep=",", header=T, colClasses="character")

###### creating supporting files:
######  "Indicator.csv", "Circumstance.csv" "Survey.csv"
###### 
# 
# all_cn<-colnames(all_data)
# covid_cn<-colnames(covid_data)
# education_cn<-colnames(education_data)
# individual_cn<-colnames(individual_data)
# childhealth_cn<-colnames(childhealth_data)
# 
# 
# createIN<-function(Analysis_variable){
#   # analysis_variable example: Covid ~ PoorerHousehold + Residence + aGroupPR + EducationPR + Sex
# 
#   n<-length(Analysis_variable)
#   IndicatorName<-rep("n", n)
#   CircumstanceVarNames<-NULL
#   for(i in c(1:n)){
#     splitted<-strsplit(Analysis_variable[i], "~")
#     IndicatorName[i]<-trimws(splitted[[1]][1])
#     fs<-strsplit(splitted[[1]][2], "[+]")[[1]]
#     for(fsi in fs)
#       CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
#     
#   }
#   return(list(IndicatorName, CircumstanceVarNames))
# }
# 
# createCV<-function(Indicator_Name, Formula_variable){
#   # example from csv file
#   # Indicator Name	      Formula
#   # AccessElectricity 	 PoorerHousehold + Residence + HighestEducation
#   
#   n<-length(Formula_variable)
#   IndicatorName<-rep("n", n)
#   Formula.String<-NULL
#   for(i in c(1:n)){
#     IndicatorName[i]<-trimws(Indicator_Name[i])
#     fs<-strsplit(Formula_variable[i], "[+]")[[1]]
#     for(fsi in fs)
#       CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
#     
#   }
#   return(list(IndicatorName, CircumstanceVarNames))
# }
# 
# all_results<-createCV(all_data$Indicator.Name, all_data$Formula)
# covid_results<-createIN(covid_data$Analysis)
# education_results<-createIN(education_data$Analysis)
# individual_results<-createIN(individual_data$Analysis)
# childhealth_results<-createIN(childhealth_data$Analysis)
# 
# 
# IndicatorName<-unlist(c(all_results[1], covid_results[1], education_results[1], individual_results[1], childhealth_results[1]))
# CircumstanceVarNames<-unlist(c(all_results[2], covid_results[2], education_results[2], individual_results[2], childhealth_results[2]))
# 
# ### export the indicator names and circumstance variable names for database building
# Indicator<-unique(IndicatorName)
# Circumstance<-unique(CircumstanceVarNames)
# 
# ### indicator and circumstance variables lists are created here and worked manually later 
# # write.table(Indicator, file=paste(csv_check_folder, "Indicator.csv", sep=""),
# #             sep=",", append = TRUE,   col.names = F, row.names = F)
# # 
# # write.table(Circumstance, file=paste(csv_check_folder, "Circumstance.csv", sep=""),
# #             sep=",", append = TRUE,   col.names = F, row.names = F)
# 
# all_data$cy<-paste(trimws(all_data$Country), trimws(all_data$Year), sep=",")
# covid_data$cy<-paste(trimws(covid_data$Country), trimws(covid_data$Year), sep=",")
# education_data$cy<-paste(trimws(education_data$Country), trimws(education_data$Year), sep=",")
# individual_data$cy<-paste(trimws(individual_data$Country), trimws(individual_data$Year), sep=",")
# childhealth_data$cy<-paste(trimws(childhealth_data$Country), trimws(childhealth_data$Year), sep=",")
# 
# cy_list<-unique(c(unique(all_data$cy), unique(covid_data$cy), unique(education_data$cy), unique(individual_data$cy), unique(childhealth_data$cy)))
# country_list<-unique(c(trimws(all_data$Country), trimws(covid_data$Country), trimws(education_data$Country), 
#                        trimws(individual_data$Country), trimws(childhealth_data$Country)))
# n<-length(cy_list)
# ISO_list<-rep("n", n)
# year_list<-rep("n", n)
# country_list<-rep("n", n)
# 
# for(i in c(1:n)) {
#   seperated<-unlist(strsplit(cy_list[i], "[,]"))
#   country_list[i]<-trimws(seperated[1])
#   year_list[i]<-trimws(seperated[2])
#   ISO_list[i]<-country_ISO(country_list[i])
# }
# 
# survey_list<-data.frame(ISO_list, year_list, country_list)
# 
# # write.table(survey_list, file=paste(csv_check_folder, "Survey.csv", sep=""),
# #                  sep=",", append = TRUE,   col.names = F, row.names = F)


createTreeID1<-function(Analysis_variable, Country){
  # analysis_variable example: Covid ~ PoorerHousehold + Residence + aGroupPR + EducationPR + Sex
  
  n<-length(Analysis_variable)
  IndicatorName<-rep("n", n)
  Formula.String<-rep("n", n)
  Country_ISO<-rep("n", n)
  for(i in c(1:n)){
    Country_ISO[i]<-country_ISO(Country[i])
    splitted<-strsplit(Analysis_variable[i], "~")
    IndicatorName[i]<-trimws(splitted[[1]][1])
    fs<-strsplit(splitted[[1]][2], "[+]")[[1]]
    CircumstanceVarNames<-NULL
    for(fsi in fs)
      CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
    CircumstanceVarNames<-sort(CircumstanceVarNames)
    Formula.String[i]<-paste(CircumstanceVarNames, collapse="+")
  }
  return(data.frame(IndicatorName=IndicatorName, FormularString=Formula.String, Country_ISO=Country_ISO))
}

common_var<-c("Source","Type", "Country", "Year", "Latest...1", "Additional.variables",
              "Analysis", "Sample.Size", "Overall.Mean", "Max.Leaf.Size", "Max.Leaf.Access", "Min.Leaf.Size",
              "Min.Leaf.Access", "Max.Leaf.Characteristics",  "Min.Leaf.Characteristics", "IndicatorName") #, "FormularString")
newData<-function(dataSet, common_var){
  dataSet$Country<-trimws(dataSet$Country)
  dataSet$Year<-trimws(dataSet$Year)
  results<-createTreeID1(dataSet$Analysis, dataSet$Country)
  dataSet<-dataSet[, colnames(dataSet) %in% common_var]

  return(cbind(dataSet, results))
}

Newcovid_data<-newData(covid_data, common_var)
Newchildhealth_data<-newData(childhealth_data, common_var)
Neweducation_data<-newData(education_data, common_var)
Newindividual_data<-newData(individual_data, common_var)

newData2<-function(dataSet, common_var){
  dataSet$Country<-trimws(dataSet$Country)
  dataSet$Year<-trimws(dataSet$Year)
  dataSet$Analysis<-paste(dataSet$Indicator.Name, dataSet$Formula, sep="~")
  results<-createTreeID1(dataSet$Analysis, dataSet$Country)
  dataSet<-dataSet[, colnames(dataSet) %in% common_var]
  return(cbind(dataSet, results))
}
Newall_data<-newData2(all_data, common_var)


# check if all analysis in data2 are in data1 (data1 is all_data)
check_inclusion<-function(data1, data2){
  data1$data1<-1
  data2$data2<-1
  mergeddata<-merge(data1, data2, by=c("SurveyID", "IndicatorName", "FormularString"), all=T)
  mergeddata$data1[is.na(mergeddata$data1)]<-0
  mergeddata$data2[is.na(mergeddata$data2)]<-0
  return(mergeddata)
}

### no overlap
# check_inclusion(Newall_data, Newcovid_data)

survey<-read.table(paste(csv_check_folder, "Survey_info.csv", sep=""), sep=",", header=T, colClasses="character")
indicator<-read.table(paste(csv_check_folder, "Indicator_info.csv", sep=""), sep=",", header=T, colClasses="character")
#Circumstance<-read.table(paste(csv_check_folder, "Circumstance_info.csv", sep=""), sep=",", header=T, colClasses="character")

Newotherdata<-rbind(Newchildhealth_data, Neweducation_data, Newindividual_data)
Newotherdata<-merge(Newotherdata, survey, by=c("Country", "Year"), all.x = T)
Newalldata<-rbind( Newcovid_data, Newall_data)
Newalldata<-merge(Newalldata, survey, by=c("Country", "Year"),  all.x = T)


all<-rbind(Newalldata, Newotherdata)

all$SurveyIndicator<-paste(all$SurveyID, all$IndicatorName, sep = "+")
all$Overall.Mean<-as.numeric(as.character(all$Overall.Mean))
all$Overall.Mean[is.na(all$Overall.Mean)]<- 1000
indicatorData<-unique(all$SurveyIndicator)

n<-length(indicatorData)
mean_value<-rep(0, n)
max_value<-rep(0, n)
min_value<-rep(0,n)
indicatorName<-rep(NA, n)
SurveyID<-rep(NA, n)
for(si in c(1:n)){
  all_si<-all[all$SurveyIndicator==indicatorData[si], c("Overall.Mean")]
  indicatorName[si]<-unique(all$IndicatorName[all$SurveyIndicator==indicatorData[si]])
  SurveyID[si]<-unique(all$SurveyID[all$SurveyIndicator==indicatorData[si]])
  
  mean_value[si]<-mean(all_si)
  max_value[si]<-max(all_si)
  min_value[si]<-min(all_si)
}
validation_orlando<-data_frame(SurveyIndicator=indicatorData, IndicatorName=indicatorName,SurveyID=SurveyID, meanY=mean_value, maxY=max_value, minY=min_value)

write.table(validation_orlando, file=paste(csv_check_folder, "Orlando_mean.csv", sep=""),
                sep=",", append = F,   col.names = T, row.names = F)


# 
# 
# mergeddata<-check_inclusion(Newalldata, Newotherdata)
# mergeddata<-merge(mergeddata,  indicator, by=c("IndicatorName"), all.x = T)
# 
# mergeddata$InUse.x[is.na(mergeddata$InUse.x)]<-"NAN"
# mergeddata$InUse.y[is.na(mergeddata$InUse.y)]<-"NAN"
# 
# 
# mergeddata<-mergeddata[(mergeddata$InUse.y=="Y" | mergeddata$InUse.x=="Y") & mergeddata$InUse=="Y",] #### only indicators and surveys inuse==Y
# 
# # print(table(mergeddata$Country, mergeddata$InUse.x))
# # print(table(mergeddata$IndicatorName, mergeddata$InUse.y))
# # print(table(mergeddata$Country, mergeddata$data1))
# # print(table(mergeddata$IndicatorName, mergeddata$data1))
# # print(table(mergeddata$Country, mergeddata$data2))
# # print(table(mergeddata$IndicatorName, mergeddata$data2))
# 
# mergeddata$MergeResult<-"Both"
# mergeddata$MergeResult[mergeddata$data1==1 & mergeddata$data2==0]<-"ALL"
# mergeddata$MergeResult[mergeddata$data2==1 & mergeddata$data1==0]<-"Other" 
# 
# mergeddata$Overall.Mean.x<-as.numeric(as.character(mergeddata$Overall.Mean.x))
# mergeddata$Overall.Mean.y<-as.numeric(as.character(mergeddata$Overall.Mean.y))
# mergeddata$Max.Leaf.Access.y<-as.numeric(as.character(mergeddata$Max.Leaf.Access.y))
# mergeddata$Max.Leaf.Access.x<-as.numeric(as.character(mergeddata$Max.Leaf.Access.x))
# 
# mergeddata$AnaResult<-"Two"
# mergeddata$AnaResult[is.na(mergeddata$Overall.Mean.x) & is.na(mergeddata$Overall.Mean.y)]<-"None"
# mergeddata$AnaResult[is.na(mergeddata$Overall.Mean.x) & !is.na(mergeddata$Overall.Mean.y)]<-"Other"
# mergeddata$AnaResult[!is.na(mergeddata$Overall.Mean.x) & is.na(mergeddata$Overall.Mean.y)]<-"All"
# 
# print(table(mergeddata$MergeResult, mergeddata$AnaResult))
# print(table(mergeddata$AnaResult, mergeddata$InUse.x))
# print(table(mergeddata$AnaResult, mergeddata$InUse.y))
# print(table(mergeddata$AnaResult, mergeddata$InUse.x, mergeddata$InUse.y))
# 
# moveresult<- (mergeddata$AnaResult=="Other")
# mergeddata$Latest...1.x[moveresult]<-mergeddata$Latest...1.y[moveresult]
# mergeddata$Sample.Size.x[moveresult]<-mergeddata$Sample.Size.y[moveresult]
# mergeddata$Overall.Mean.x[moveresult]<-mergeddata$Overall.Mean.y[moveresult]
# mergeddata$Max.Leaf.Size.x[moveresult]<-mergeddata$Max.Leaf.Size.y[moveresult]
# mergeddata$Max.Leaf.Access.x[moveresult]<-mergeddata$Max.Leaf.Access.y[moveresult]
# mergeddata$Min.Leaf.Size.x[moveresult]<-mergeddata$Min.Leaf.Size.y[moveresult]
# mergeddata$Min.Leaf.Access.x[moveresult]<-mergeddata$Min.Leaf.Access.y[moveresult]
# mergeddata$Max.Leaf.Characteristics.x[moveresult]<-mergeddata$Max.Leaf.Characteristics.y[moveresult]
# mergeddata$Min.Leaf.Characteristics.x[moveresult]<-mergeddata$Min.Leaf.Characteristics.y[moveresult]
# mergeddata$Country_ISO<-mergeddata$Country_ISO.x
# mergeddata$Country_ISO[mergeddata$data2==1 & mergeddata$data1==0]<-mergeddata$Country_ISO.y[mergeddata$data2==1 & mergeddata$data1==0]
# mergeddata$Year<-mergeddata$YearUsed.x
# mergeddata$Year[mergeddata$data2==1 & mergeddata$data1==0]<-mergeddata$YearUsed.y[mergeddata$data2==1 & mergeddata$data1==0]
# 
# 
# # print(table(mergeddata$MergeResult, mergeddata$AnaResult))
# # print(table(mergeddata$Country_ISO, mergeddata$MergeResult))
# # print(table(mergeddata$IndicatorName, mergeddata$MergeResult))
# 
# 
# keep_var<-c( "IndicatorName", "Country_ISO", "Year", "FormularString", "Latest...1.x", 
#              "Sample.Size.x", "Overall.Mean.x", "Max.Leaf.Size.x",           
#              "Max.Leaf.Access.x", "Min.Leaf.Size.x", "Min.Leaf.Access.x", "Max.Leaf.Characteristics.x", 
#              "Min.Leaf.Characteristics.x", "Latest...1.y", "Sample.Size.y", "Overall.Mean.y", 
#              "Max.Leaf.Size.y", "Max.Leaf.Access.y", "Min.Leaf.Size.y", "Min.Leaf.Access.y", 
#              "Max.Leaf.Characteristics.y", "Min.Leaf.Characteristics.y", "SurveyID", "Country", 
#              "SurveySource", "CountryCode", "VersionCode", "InUse.x", "VariableID", "DatasetSource",
#              "InUse.y", "TreeID", "MergeResult", "AnaResult")
# 
# 
# mergeddata<-mergeddata[, colnames(mergeddata) %in% keep_var]
# 
# mergeddata$TreeID<-paste(mergeddata$Country_ISO, mergeddata$Year, mergeddata$IndicatorName, mergeddata$FormularString, sep="+")
# 
# write.table(mergeddata, file=paste(csv_check_folder, "MERGED.csv", sep=""),
#                 sep=",", append = F,   col.names = T, row.names = F)
# 
# 
# ##### checked out when both results (from all+covid tabs and all other tabs) exist, they agree
# 
# # mergeddataCompare<-mergeddata[mergeddata$AnaResult=="Two", ]
# # summary(mergeddataCompare$Overall.Mean.x - mergeddataCompare$Overall.Mean.y)
# # summary(mergeddataCompare$Max.Leaf.Access.x - mergeddataCompare$Max.Leaf.Access.y)
# # mergeddataCompare1<-mergeddataCompare[abs(mergeddataCompare$Max.Leaf.Access.x - mergeddataCompare$Max.Leaf.Access.y)>0.01, ]
# # # 
# # write.table(mergeddataCompare1, file=paste(csv_check_folder, "MERGED2.csv", sep=""),
# #             sep=",", append = F,   col.names = T, row.names = F)