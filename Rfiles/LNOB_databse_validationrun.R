source_folder<- "/home/yw/Workspace/rstudio/LNOB_Rcode/"

library(dplyr)
library(ggplot2)
library(ggparty)
library(dplyr)
library(stringr)
library(data.table)
library(foreign)



r_folder<-paste(source_folder, "Rfiles/", sep="")
csv_folder<-paste(source_folder, "output/validation/", sep="")
#csv_folderm<-paste(source_folder, "output/validationm/", sep="")
csv_check_folder<-paste(source_folder, "CSVdatabase/", sep="")



country_ISO<-function(country_code){
  if(country_code %in% c("AF", "Afghanistan")) iso<-"AFG"
  else if(country_code %in% c("AM",  "Armenia")) iso<-"ARM"
  else if(country_code %in% c("BD", "Bangladesh")) iso<-"BGD"
  else if(country_code=="Bhutan") iso<-"BTN"
  else if(country_code=="Georgia") iso<-"GEO"
  else if(country_code %in% c("KH", "Cambodia")) iso<-"KHM"
  else if(country_code=="Kazakhstan") iso<-"KAZ"
  else if(country_code=="Kyrgyzstan") iso<-"KGZ"
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

validationRun_data<-read.table(paste(csv_folder, "overallmean.csv", sep=""), sep=",", header=T, colClasses="character")
# mics_data<-read.table(paste(csv_folderm, "overallmean.csv", sep=""), sep=",", header=T, colClasses="character")
# mics_data<-mics_data[!mics_data$SurveyIndicator=="",]
# dhs_data$SurveySource<-"DHS"
# mics_data$SurveySource<-"MICS"
# validationRun_data<-rbind(dhs_data, mics_data)
validationRun_data<-validationRun_data[!validationRun_data$MeanY=="DataNotGenerated",]
validationRun_data$dataValidation<-1



orlando_data<-read.table(paste(csv_check_folder, "Orlando_mean.csv", sep=""), sep=",", header=T, colClasses="character")
orlando_data<-orlando_data[!is.na(orlando_data$IndicatorName), ]

orlando_data<-orlando_data[!orlando_data$SurveyID %in% c("AZE52", "UZB2006", "VUT2007", "MMR2000", "KAZ2006"), ]



n<-nrow(orlando_data)
orlando_data$Source<-"MICS"
for(i in c(1:n)){
  orlando_data$SurveyIndicator[i]<-gsub("35[+]", "35plus", orlando_data$SurveyIndicator[i])
  orlando_data$IndicatorName[i]<-gsub("35[+]", "35plus", orlando_data$IndicatorName[i])
  if(nchar(orlando_data$SurveyID[i])==5) orlando_data$Source[i]<-"DHS"
  # if( orlando_data$IndicatorName[i] =="MobilePhone") {
  #     orlando_data$IndicatorName[i] <- "MobilePhoneHH"
  #     orlando_data$SurveyIndicator[i]<-gsub("MobilePhone", "MobilePhoneHH", orlando_data$SurveyIndicator[i])
  # }
}

orlando_data$dataOrlando<-1


indicator_info<-read.table(paste(csv_check_folder, "Indicator_info.csv", sep=""), sep=",", header=T, colClasses="character")
indicatorUse<-unique(indicator_info$IndicatorName[indicator_info$InUse=="Y"])

validationRun_data<-validationRun_data[validationRun_data$IndicatorName %in% indicatorUse, ]
orlando_data<-orlando_data[orlando_data$IndicatorName %in% indicatorUse, ]

# 
# orland_indicator<-as.data.frame(table(orlando_data$Source, orlando_data$IndicatorName))
# 
# validationrun_indicator<-as.data.frame(table(validationRun_data$SurveySource, validationRun_data$IndicatorName))
# 
# all_indicator<-merge(orland_indicator, validationrun_indicator, by=c("Var1", "Var2"), all = T)
# write.table(all_indicator, file=paste(csv_check_folder, "all_indicator.csv", sep=""),
#             sep=",", append = F,   col.names = T, row.names = F)




comparison<-merge(orlando_data, validationRun_data, by=c("SurveyIndicator"), all=T)

comparison$IndicatorName<-comparison$IndicatorName.y
comparison$IndicatorName[is.na(comparison$dataValidation)] <-comparison$IndicatorName.x[is.na(comparison$dataValidation)]

# comparison<-merge(comparison, indicator_info, by=c("IndicatorName"), all.x=T)
# comparison<-comparison[!comparison$SurveyID %in% c("AZE52", "UZB2006", "VUT2007", "MMR2000", "KAZ2006"),]
# comparison<-comparison[comparison$InUse=="Y",]
# comparison<-comparison[!is.na(comparison$SurveyIndicator),]

base<-comparison[!is.na(comparison$dataOrlando), ]
b1<-base[base$meanY=="1000", ]
base<-base[!base$meanY=="1000", ]
b2<-base[is.na(base$dataValidation), ]
base<-base[!is.na(base$dataValidation), ]
base$diffY<-abs(as.numeric(as.character(base$meanY))-as.numeric(as.character(base$MeanY)))
investigate<-base[base$diffY>0.01, ]

additional<-comparison[is.na(comparison$dataOrlando), ]

write.table(investigate, file=paste(csv_check_folder, "investigate.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)


write.table(additional, file=paste(csv_check_folder, "additional.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)

write.table(b1, file=paste(csv_check_folder, "b1.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)

write.table(b2, file=paste(csv_check_folder, "b2.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)

write.table(base, file=paste(csv_check_folder, "validated.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)


missingRun<-comparison[is.na(comparison$dataValidation),]
missingRun<-missingRun[!missingRun$meanY=="1000", ]
print(table(missingRun$IndicatorName))

additional<-comparison[is.na(comparison$dataOrlando),]
additional<-additional[!additional$MeanY=="DataNotGenerated", ]
print(table(additional$IndicatorName))

both<-comparison[!is.na(comparison$dataOrlando),]
both<-both[!is.na(both$dataValidation), ]


print(nrow(comparison[comparison$meanY=="1000",]))
print(nrow(comparison[comparison$MeanY=="DataNotGenerated",]))
print(nrow(comparison[comparison$MeanY=="DataNotGenerated" & comparison$meanY=="1000",]))



cond0<- both$meanY=="1000" & both$MeanY=="DataNotGenerated"
both<-both[!cond0, ]
missingvalues<-both[cond0, ]
  
orlando_na<-both[both$meanY=="1000", ]
valrun_na<-both[both$MeanY=="DataNotGenerated", ]

cond0<- both$meanY=="1000" | both$MeanY=="DataNotGenerated"
both<-both[!cond0, ]




print(table(both$IndicatorName))

both<-both[as.numeric(as.character(both$meanY))<1000,]
both$diffY<-abs(as.numeric(as.character(both$meanY))-as.numeric(as.character(both$MeanY)))

print(summary(both$diffY))

investigate<-both[both$diffY>0.03, ]

write.table(investigate, file=paste(csv_check_folder, "investigate.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)


write.table(additional, file=paste(csv_check_folder, "additional.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)
write.table(missingRun, file=paste(csv_check_folder, "missingRun.csv", sep=""),
            sep=",", append = F,   col.names = T, row.names = F)

print(table(investigate$IndicatorName))
print(table(additional$IndicatorName))



print(comparison$SurveyIndicator[is.na(comparison$dataValidation) & comparison$InUse=="Y"])

print(table(comparison$SurveyID[is.na(comparison$dataValidation) & comparison$InUse=="Y"]))



createTreeID1<-function(Analysis_variable){
  # analysis_variable example: Covid ~ PoorerHousehold + Residence + aGroupPR + EducationPR + Sex
  
  n<-length(Analysis_variable)
  IndicatorName<-rep("n", n)
  Formula.String<-rep("n", n)
  
  for(i in c(1:n)){

    splitted<-strsplit(Analysis_variable[i], "~")
    IndicatorName[i]<-trimws(splitted[[1]][1])
    fs<-strsplit(splitted[[1]][2], "[+]")[[1]]
    CircumstanceVarNames<-NULL
    for(fsi in fs)
      CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
    CircumstanceVarNames<-sort(CircumstanceVarNames)
    Formula.String[i]<-paste(CircumstanceVarNames, collapse="+")
  }
  return(data.frame(IndicatorName=IndicatorName, FormularString=Formula.String))
}

treeid<-createTreeID1(validationRun_data$Analysis)
validationRun_data<-cbind(validationRun_data, treeid)
validationRun_data$TreeID<-paste(validationRun_data$Country_ISO, validationRun_data$Year, validationRun_data$IndicatorName, validationRun_data$FormularString, sep="+")

# common_var<-c("Country", "Year", 
#               "Analysis", "Sample.Size", "Overall.Mean", "Max.Leaf.Size", "Max.Leaf.Access", "Min.Leaf.Size",
#               "Min.Leaf.Access", "Max.Leaf.Characteristics",  "Min.Leaf.Characteristics") 


# check if all analysis in data2 are in data1 (data1 is all_data)
check_inclusion<-function(data1, data2){
  data1$data1<-1
  data2$data2<-1
  mergeddata<-merge(data1, data2, by=c("TreeID"), all=T)
  mergeddata$data1[is.na(mergeddata$data1)]<-0
  mergeddata$data2[is.na(mergeddata$data2)]<-0
  return(mergeddata)
}



### no overlap
# check_inclusion(Newall_data, Newcovid_data)

survey<-read.table(paste(csv_check_folder, "Survey_info.csv", sep=""), sep=",", header=T, colClasses="character")
indicator<-read.table(paste(csv_check_folder, "Indicator_info.csv", sep=""), sep=",", header=T, colClasses="character")
#Circumstance<-read.table(paste(csv_check_folder, "Circumstance_info.csv", sep=""), sep=",", header=T, colClasses="character")

validationRun_data<-merge(validationRun_data, survey, by.x=c("Country_ISO", "Year"), by.y=c("Country_ISO", "YearFromR"),  all.x = T)
validationRun_data<-merge(validationRun_data,  indicator, by=c("IndicatorName"), all.x = T)

validationRun_data$InUse.x[is.na(validationRun_data$InUse.x)]<-"NAN"
validationRun_data$InUse.y[is.na(validationRun_data$InUse.y)]<-"NAN"

validationRun_data<-validationRun_data[validationRun_data$InUse.y=="Y" & validationRun_data$InUse.x=="Y",] #### only indicators and surveys inuse==Y

mergeddata<-check_inclusion(orlando_data, validationRun_data)


# print(table(mergeddata$Country, mergeddata$InUse.x))
# print(table(mergeddata$IndicatorName, mergeddata$InUse.y))
# print(table(mergeddata$Country, mergeddata$data1))
# print(table(mergeddata$IndicatorName, mergeddata$data1))
# print(table(mergeddata$Country, mergeddata$data2))
# print(table(mergeddata$IndicatorName, mergeddata$data2))

mergeddata$MergeResult<-"Both"
mergeddata$MergeResult[mergeddata$data1==1 & mergeddata$data2==0]<-"ALL"
mergeddata$MergeResult[mergeddata$data2==1 & mergeddata$data1==0]<-"Other" 

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
# 

# print(table(mergeddata$MergeResult, mergeddata$AnaResult))
# print(table(mergeddata$Country_ISO, mergeddata$MergeResult))
# print(table(mergeddata$IndicatorName, mergeddata$MergeResult))


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

print( table(mergeddata$data1, mergeddata$data2))
mergeddata$Overall.Mean.x<-as.numeric(as.character(mergeddata$Overall.Mean.x))
mergeddata$Overall.Mean<-as.numeric(as.character(mergeddata$Overall.Mean))



comparison11<-mergeddata[mergeddata$data1==1 & mergeddata$data2==1,]
comparison11$mean.diff<-comparison11$Overall.Mean.x-comparison11$Overall.Mean
comparison11_diff<-comparison11[abs(comparison11$mean.diff)>0.01, ]
comparison11_nodiff<-comparison11[abs(comparison11$mean.diff)<=0.01, ]

comparison11_nodiff$Max.o<-as.numeric(as.character(comparison11_nodiff$Max.Leaf.Access.x))
comparison11_nodiff$Max.n<-as.numeric(as.character(comparison11_nodiff$Max.Leaf.Access))
comparison11_nodiff$maxleaf.diff<-comparison11_nodiff$Max.o-comparison11_nodiff$Max.n
comparison11_maxdiff<-comparison11_nodiff[abs(comparison11_nodiff$maxleaf.diff)>0.01, ]

# print(table(comparison11_diff$SurveySource))  
# print(table(comparison11_diff$Latest...1.x))  
print(table(comparison11_diff$IndicatorName.x, comparison11_diff$SurveySource))
print(table(comparison11_diff$SurveyID.x, comparison11_diff$IndicatorName.x))
# print(table(comparison11_diff$SurveyID.x, comparison11_diff$IndicatorName.x))


keepVar<- c("TreeID", "IndicatorName.x",  "SurveyID.x", "FormularString.x", "Latest...1.x", "Sample.Size.x", "Overall.Mean.x", "Max.Leaf.Size.x", 
            "Max.Leaf.Access.x", "Min.Leaf.Size.x", "Min.Leaf.Access.x", "Max.Leaf.Characteristics.x", "Min.Leaf.Characteristics.x", "InUse.x.x",
            "Country_ISO.x", "Year.x", "IndicatorName.y", "Country_ISO.y", "Year.y", "Analysis", "Sample.Size", "Overall.Mean", "Max.Leaf.Size",
            "Max.Leaf.Access", "Min.Leaf.Size", "Min.Leaf.Access", "Max.Leaf.Characteristics", "Min.Leaf.Characteristics", "Religion_data",              
            "Ethnicity_data", "Language_data", "mean.diff")
export_diff<-comparison11_diff[, colnames(comparison11_diff) %in% keepVar]
export_nodiff<-comparison11_nodiff[, colnames(comparison11_diff) %in% keepVar]

keepVar<- c("TreeID", "IndicatorName.x",  "SurveyID.x", "FormularString.x", "Latest...1.x", "Sample.Size.x", "Overall.Mean.x", "Max.Leaf.Size.x", 
            "Max.Leaf.Access.x", "Max.Leaf.Characteristics.x", "Max.Leaf.Size",
            "Max.Leaf.Access", "Max.Leaf.Characteristics")
export_maxdiff<-comparison11_maxdiff[, colnames(comparison11_maxdiff) %in% keepVar]


# write.table(mergeddata[mergeddata$data1==1 & mergeddata$data2==1,], file=paste(csv_check_folder, "comparison11.csv", sep=""),
#                 sep=",", append = F,   col.names = T, row.names = F)
# write.table(mergeddata[mergeddata$data1==0 & mergeddata$data2==1,], file=paste(csv_check_folder, "comparison01.csv", sep=""),
#             sep=",", append = F,   col.names = T, row.names = F)
# write.table(mergeddata[mergeddata$data1==1 & mergeddata$data2==0,], file=paste(csv_check_folder, "comparison10.csv", sep=""),
#             sep=",", append = F,   col.names = T, row.names = F)

# write.table(export_diff, file=paste(csv_check_folder, "comparison11_diff.csv", sep=""),
#             sep=",", append = F,   col.names = T, row.names = F)

write.table(export_maxdiff, file=paste(csv_check_folder, "comparison11_maxdiff.csv", sep=""),
                         sep=",", append = F,   col.names = T, row.names = F)
            
##### checked out when both results (from all+covid tabs and all other tabs) exist, they agree

# mergeddataCompare<-mergeddata[mergeddata$AnaResult=="Two", ]
# summary(mergeddataCompare$Overall.Mean.x - mergeddataCompare$Overall.Mean.y)
# summary(mergeddataCompare$Max.Leaf.Access.x - mergeddataCompare$Max.Leaf.Access.y)
# mergeddataCompare1<-mergeddataCompare[abs(mergeddataCompare$Max.Leaf.Access.x - mergeddataCompare$Max.Leaf.Access.y)>0.01, ]
# # 
# write.table(mergeddataCompare1, file=paste(csv_check_folder, "MERGED2.csv", sep=""),
#             sep=",", append = F,   col.names = T, row.names = F)