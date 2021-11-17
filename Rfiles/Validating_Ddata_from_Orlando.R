Orlando_data <- "/home/yw/Workspace/rstudio/SDD2021/from Orlando/"
Oct22_version_data<-"/home/yw/Workspace/rstudio/LNOB_Rcode/output/drupalData20211117115236/validation/"



  HealthCareD<-read.table(paste(Orlando_data, "HealthCareD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  # var2tab in formula
  CovidD<-read.table(paste(Orlando_data, "CovidD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  # vat2tab in variables
  EducationD<-read.table(paste(Orlando_data, "EducationD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  # okay
  ChildrenD<-read.table(paste(Orlando_data, "ChildrenD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  # okay
  WomenD<-read.table(paste(Orlando_data, "WomenD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  # okay
  HouseholdD<-read.table(paste(Orlando_data, "HouseholdD", ".csv", sep=""), sep=",", header=T, colClasses="character")
  colnames(HouseholdD)[7]<-"mean"
  # okay
  
  newData<-read.table(paste(Oct22_version_data, "NewLNOBdata", ".csv", sep=""), sep=",", header=T, colClasses="character")
  newData<-newData[newData$Region=="National", ]
  newData<-newData[, colnames(newData) %in% c("DataSource", "CountryCode", "Country", "VersionCode", "Year", "Additional_variable", 
                                              "Analysis_model", "D_Index", "Overall_Mean" )]
  
  
  n<-nrow(newData)
  for (i in c(1:n)){
    splitted<-strsplit(newData$Analysis_model[i], "~")
    newData$ResV[i]<-trimws(splitted[[1]][1])
    
    fs<-strsplit(splitted[[1]][2], "[+]")[[1]]
    CircumstanceVarNames<-NULL
    for(fsi in fs)
      CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
    CircumstanceVarNames<-sort(CircumstanceVarNames)
    newData$FormulaString[i]<-paste(CircumstanceVarNames, collapse="+")
  }
  newData<-newData[!(newData$ResV %in% c("MobilePhoneHH", "EarlyChildhoodEducation", 
                                         "MobilePhone", "ChildHealth", "CleanWater", "HealthcareNotUsed", 
                                         "HealthcareNotAffordable", "HealthcareNotAccessible", "HealthcareFar", 
                                         "HealthcareDiscouraged", "BankCardPR", "ChildMarriage15", 
                                         "ChildMarriage18", "HandWash", "Land", "EarlyChildBearing", 
                                         "EarlyEducation24", "EarlyEducation36", "HouseholdBasic", "HouseholdTechNeed",
                                         "MobilePhonePR", "NoChildMarriage15", "NoChildMarriage18", "NoEarlyChildbearing")), ]
  newData$Country<-trimws(newData$Country)
  newData$Year<-as.numeric(as.character(newData$Year))
  newData$Overall_Mean<-as.numeric(as.character(newData$Overall_Mean))
  newData$D_Index<-as.numeric(as.character(newData$D_Index))
  # newData<-newData[!(newData$Country=="India" & newData$Additional_variable==1), ]
  newData<-newData[!(newData$Country=="Tuvalu"), ]
  
  
  
  common_name<-c("Source", "Country", "Year", "Additional.variables", "Analysis", "Variables", "Formula",  "Overall.mean", "mean", "D")
  HealthCareD<-HealthCareD[, colnames(HealthCareD) %in% common_name]
  CovidD<-CovidD[, colnames(CovidD) %in% common_name]
  EducationD<-EducationD[, colnames(EducationD) %in% common_name]
  ChildrenD<-ChildrenD[, colnames(ChildrenD) %in% common_name]
  WomenD<-WomenD[, colnames(WomenD) %in% common_name]
  HouseholdD<-HouseholdD[, colnames(HouseholdD) %in% common_name]
  
  standardizeData<-function(datause, type){
    n<-nrow(datause)
   

      for (i in c(1:n)){
        if(type==1) datause$Analysis[i]<-gsub("var2tab", datause$Analysis[i], datause$Formula[i])    # healthcare
        if(type==2) {
          datause$Analysis[i]<-gsub("WaterOnstieHL", " WaterOnsiteHL", datause$Analysis[i])
          datause$Analysis[i]<-gsub("var2tab", datause$Analysis[i], datause$Variables[i])  # covid
        }
        if(type==3) { 
            datause$Analysis[i]<-gsub(" Under5", " NUnder5", datause$Analysis[i])  # women
            datause$Analysis[i]<-gsub("NoneViolence", "NoAllViolence", datause$Analysis[i])
            datause$Analysis[i]<-gsub("MarritalStatus", "MarriageStatus", datause$Analysis[i])
            datause$Analysis[i]<-gsub("NoBeating", "NoViolenceJustifiedAgainstWomen", datause$Analysis[i])
            
        }
        if(type==4) datause$Analysis[i]<-gsub("35+ ~", "35plus ~", datause$Analysis[i], fixed = TRUE)  # education
        splitted<-strsplit(datause$Analysis[i], "~")
        datause$ResV[i]<-trimws(splitted[[1]][1])

        fs<-strsplit(splitted[[1]][2], "[+]")[[1]]
        CircumstanceVarNames<-NULL
        for(fsi in fs)
          CircumstanceVarNames<-c(CircumstanceVarNames, trimws(fsi))
        CircumstanceVarNames<-sort(CircumstanceVarNames)
        datause$FormulaString[i]<-paste(CircumstanceVarNames, collapse="+")
      }

     datause<-datause[, colnames(datause) %in% c("Source", "Country", "Year", "Additional.variables", 
                                                 "Analysis", "mean", "D" , "ResV", "FormulaString")]   
     print(table(datause$ResV))
    return(datause)
  }
  
  print("Covid")
  datause<-standardizeData(CovidD, 2)
  print("HealthCareD")
  datause<-rbind(datause, standardizeData(HealthCareD, 1))
  print("EducationD")
  datause<-rbind(datause, standardizeData(EducationD, 4))
  print("HouseholdD")
  datause<-rbind(datause, standardizeData(HouseholdD, 0))
  print("ChildrenD")
  datause<-rbind(datause, standardizeData(ChildrenD, 0))
  print("WomenD")
  datause<-rbind(datause, standardizeData(WomenD, 3))
  
  datause$D<-as.numeric(as.character(datause$D))
  datause$mean<-as.numeric(as.character(datause$mean))
  datause<-datause[!is.na(datause$D) & !is.na(datause$mean), ]
  
  datause<-datause[!(datause$ResV %in% c("ChildHealth", "CleanWater", "HealthcareNotUsed", "HealthcareNotAffordable",
                                         "HealthcareNotAccessible", "HealthcareFar", "HealthcareDiscouraged", "BankAccountPR")), ]
  datause<-datause[!(datause$Year %in% c(2006, 2007)), ]
  datause<-datause[!(datause$Country %in% c("Tonga (5 pc_", "Tonga (5 pc)", "Tonga (5 pc) ")), ]
  datause$Country[datause$Country=="Lao PDR"]<-"Lao"
  datause$Country[datause$Country=="Viet Nam"]<-"VietNam"
  datause$Year[datause$Country=="Armenia" & datause$Year==2015]<-2016
  datause$Year[datause$Country=="Kazakhstan"  & datause$Year==2014]<-2015
  datause$Year[datause$Country=="Maldives"  & datause$Year==2016]<-2017
  datause$Year[datause$Country=="Myanmar"  & datause$Year==2015]<-2016
  datause$Year[datause$Country=="India" & datause$Year==2015]<-2016
  datause$Year[datause$Country=="Papua New Guinea"]<-2018
  datause$Year[datause$Country=="Timor-Leste" & datause$Year==2010]<-2009
  datause$Year[datause$Country=="Indonesia" & datause$Year==2015]<-2017
  datause$Year[datause$Country=="Kyrgyzstan" & datause$Year==2010]<-2012
  datause$Year[datause$Country=="Nepal" & datause$Year==2014]<-2011
  
  
  
  datause$Country<-trimws(datause$Country)
  datause$Year<-as.numeric(as.character(datause$Year))
  colnames(datause)[5]<-"Analysis_model"
  colnames(datause)[1]<-"DataSource"
  
  combined<-merge(datause, newData, by=c("DataSource", "Country", "Year",  "ResV", "FormulaString"), all=T)

  
  combined<-combined[!(combined$ResV %in% c("AllViolence", "Overweight", "PhysicalViolence", 
                                            "SexualPhysicalViolence", "SexualViolence", "Stunting", 
                                            "Wasting", "EmotionalViolence", "ReasonBeating", "InternetUse")), ]
  
  orlandoOnly<-combined[is.na(combined$Overall_Mean) & !is.na(combined$mean), ]
  newDataOnly<-combined[!is.na(combined$Overall_Mean) & is.na(combined$mean), ]
  bothdata<-combined[!is.na(combined$Overall_Mean) & !is.na(combined$mean), ]
  
  bothdata$absDdifference<-abs(bothdata$D-bothdata$D_Index)
  bothdata$bigDdifference<-0
  bothdata$bigDdifference[bothdata$absDdifference>0.01]<-1
  bothdata$bigDdifference[bothdata$absDdifference>0.02]<-2
  bothdata$bigDdifference[bothdata$absDdifference>0.04]<-4
  
  bothdata$absmeandifference<-abs(bothdata$mean-bothdata$Overall_Mean)
  bothdata$bigMEANdifference<-0
  bothdata$bigMEANdifference[bothdata$absmeandifference>0.01]<-1
  bothdata$bigMEANdifference[bothdata$absmeandifference>0.05]<-5
  bothdata$bigMEANdifference[bothdata$absmeandifference>0.10]<-10
  
  bothdata$count<-1
  # dhs vs mics
  # additional vs no
  # indicators (excluding low levels, stuning, violence, etc.)
  # change in values
  meanD<-aggregate(x = bothdata$absDdifference, 
                      by = list(bothdata$DataSource, bothdata$Additional.variables, bothdata$bigMEANdifference, bothdata$bigDdifference),
            FUN = "mean")
  colnames(meanD)[5]<-"meanDdiff"
  maxD<-aggregate(x = bothdata$absDdifference, 
                     by = list(bothdata$DataSource, bothdata$Additional.variables, bothdata$bigMEANdifference, bothdata$bigDdifference),
                      FUN = "max")
  colnames(maxD)[5]<-"maxDdiff"
  countD<-aggregate(x = bothdata$count, 
                    by = list(bothdata$DataSource, bothdata$Additional.variables, bothdata$bigMEANdifference, bothdata$bigDdifference),
                    FUN = "sum")
  colnames(countD)[5]<-"countDdiff"
  
  combinedD<-cbind(meanD, maxDdiff=maxD[,5], countDdiff=countD[,5])
  colnames(combinedD)[c(1:4)]<-c("DataSource", "AdditionalValue", "MeanChangeRange", "DChangeRange")
  
  
  inv<-bothdata[bothdata$bigDdifference>0 & bothdata$bigMEANdifference<5, ]
  # print("Only in orlando")
  # print(table(orlandoOnly$ResV))
  # print("only in new data")
  # print(table(newDataOnly$ResV))
  # 
  # 
  # print("In orlando")
  # print(table(datause$ResV))
  # 
  # print("In newdata")
  # print(table(newData$ResV))
  # 
  # 
  # print("only In orlando")
  # print(unique(orlandoOnly$FormulaString[orlandoOnly$ResV=="InternetUse"]))
  # print("only in new data")
  # print(unique(newDataOnly$FormulaString[newDataOnly$ResV=="InternetUse"]))
  # 
  write.table(orlandoOnly, paste(Oct22_version_data, "OnlyInOlando.csv", sep=""), sep=",",
              append = FALSE,   col.names = T, row.names = F)

  write.table(newDataOnly, paste(Oct22_version_data, "OnlyInNewData.csv", sep=""), sep=",",
              append = FALSE,   col.names = T, row.names = F)

  write.table(bothdata, paste(Oct22_version_data, "CleanedInBothData.csv", sep=""), sep=",",
              append = FALSE,   col.names = T, row.names = F)
  
  
  # newData<-newData[, colnames(newData) %in% c("DataSource", "CountryCode", "Country", "VersionCode", "Year", "Additional_variable", 
  #                                             "Analysis_model", "D_Index", "Overall_Mean" )]
  # "Source"               "Country"              "Year"                 "Additional.variables"
  # [5] "Analysis"             "mean"                 "D"  