

get_data<-function(df, rv, dataList, indvar, svnm, educationList,religion_data=NULL){

  
  if(!is.null(rv)){
     VarName<- toupper(dataList$VarName[dataList$NickName==rv & dataList$IndicatorType %in% c("ResponseV","MresponseV")])
     
     if(is.na(VarName) | VarName=="") k<-NA
     else if(VarName=="NO KEYWORDS") k<-0
      else k<-match(VarName, toupper(colnames(df)))    ### variable name not defined, need some calculation

      print(VarName)
      if(is.na(k) | length(k)==0){
        print(paste("Response variable -- ", rv, "(",  VarName, ") not found"))
        return(NULL)
      }

      if (k>0){
        if(rv %in% c("ProfessionalHelp", "ContraceptiveMethod"))
              df[, k]<-as.character(df[,k])
        else df[, k]<-as.numeric(as.character(df[,k]))
  
        if(!(rv %in% c("EarlyChildBearing", "NoEarlyChildbearing", "ChildMarriage15", "ChildMarriage18", "NoChildMarriage15", "NoChildMarriage18", "InternetUse",
                       "SecondaryEducation2035", "SecondaryEducation35plus", "HigherEducation2535", "HigherEducation35plus", "EarlyEducation24", "EarlyEducation36", 
                       "ContraceptiveMethod")))
                    df<-df[!is.na(df[,k]), ]  ### for Thailand 2019, they have missing value for access to electricity and must be excluded.
                                              ### this may be the wrong way to do it
        
        
        if(length(df[!is.na(df[, k]), k])<=1) {
         print(paste("Response variable -- ", rv, "(",  VarName, ") has not valid observations"))
         return(NULL)
        }

      }


             if (rv == "MobilePhoneHH" ) datause<-MobilePhoneHH(df, k, dataList)
             else if (rv == "PhysicalViolence") datause<-PhysicalViolence(df, dataList)
             else if (rv == "SexualViolence") datause<-SexualViolence(df, dataList)
             else if (rv == "EmotionalViolence") datause<-EmotionalViolence(df, dataList)
             else if (rv == "SexualPhysicalViolence") datause<-SexualPhysicalViolence(df, dataList)
             else if (rv == "NoSexualViolence") datause<-NoSexualViolence(df, dataList)
             else if (rv == "AllViolence") datause<-AllViolence(df, dataList)
             else if (rv == "NoSexualPhysicalViolence") datause<- NoSexualPhysicalViolence(df, dataList)
             else if(rv=="AccessElectricity") datause<-AccessElectricity(df, dataList, k)
             else if(rv=="CleanFuel") datause <- CleanFuel(df, k)
             else if(rv=="BankCardHH") datause <- BankCardHH(df, k)
             else if(rv=="CleanWater") datause<-CleanWater(df, k, svnm)
             else if(rv=="BasicWater") datause<-BasicWater(df, dataList, k, svnm)
             else if(rv=="SafeSanitation") datause<- SafeSanitation(df, dataList, k, svnm)
             else if(rv=="HouseholdBasic") datause<- HouseholdBasic(df, dataList, svnm)
             else if(rv=="HouseholdTechNeed") datause<- HouseholdTechNeed(df, dataList)
             else if(rv=="Land") datause<- Land(df, dataList, k)
             else if(rv=="MultiDeprivation") datause<- MultiDeprivation(df, dataList, k)
             else if(rv=="SecondaryEducation2035") datause<- SecondaryEducation2035(df, dataList, k, educationList)
             else if(rv=="SecondaryEducation35plus") datause<- SecondaryEducation35plus(df, dataList, k, educationList)
             else if(rv=="HigherEducation2535")    datause<- HigherEducation2535(df, dataList, k, educationList)
             else if(rv=="HigherEducation35plus") datause<- HigherEducation35plus(df, dataList, k, educationList)
             else if(rv=="NotStunting") datause<- NotStunting(df, dataList, k)
             else if(rv=="NotOverweight") datause<- NotOverweight(df, dataList, k)
             else if(rv=="NotWasting") datause<- NotWasting(df, dataList, k)
             else if(rv=="ChildHealth") datause<- ChildHealth(df, dataList, k)
             else if(rv=="Stunting") datause<- Stunting(df, dataList, k)
             else if(rv=="Overweight") datause<-  Overweight(df, dataList, k)
             else if(rv== "Wasting") datause<- Wasting(df, dataList, k)
             else if(rv== "ContraceptiveMethod") datause<- ContraceptiveMethod(df, dataList, k, svnm)
             else if(rv== "HealthInsurance") datause<- HealthInsurance(df, dataList, k)
             else if(rv=="ProfessionalHelp") datause<- ProfessionalHelp(df, dataList, k, svnm)  ### no need for k for this one variable
             else if(rv=="AdolescentBirthRate") datause<- AdolescentBirthRate(df, dataList, k) 
             else if(rv== "MobilePhone" ) datause<-MobilePhone(df, k)
             else if(rv== "InternetUse" ) datause<-InternetUse(df, dataList, k, svnm)
             else if(rv== "ChildMarriage15" ) datause<-ChildMarriage15(df, dataList, k)
             else if(rv== "ChildMarriage18" ) datause<-ChildMarriage18(df, dataList, k)
             else if(rv== "NoChildMarriage15" ) datause<-NoChildMarriage15(df, dataList, k)
             else if(rv== "NoChildMarriage18" ) datause<-NoChildMarriage18(df, dataList, k)
             else if(rv== "TeenPregnancy" ) datause<-TeenPregnancy(df, dataList, k)
             else if(rv== "ReasonBeating" ) datause<-ReasonBeating(df, dataList, k)
             else if(rv== "NoViolenceJustifiedAgainstWomen" ) datause<-NoViolenceJustifiedAgainstWomen(df, dataList, k)
             else if(rv== "EarlyChildBearing" ) datause<-  EarlyChildBearing(df, dataList, k)
             else if(rv== "NoEarlyChildbearing" ) datause<-  NoEarlyChildbearing(df, dataList, k)
             else if(rv== "HandWash" ) datause<-HandWash(df, dataList)
             else if(rv== "NotCrowded" ) datause<-  NotCrowded(df, dataList)
             else if(rv== "Covid1" ) datause<-  Covid1(df, dataList, svnm)
             else if(rv== "Covid2" ) datause<-  Covid2(df, dataList, svnm)
             else if(rv== "Covid" ) datause<-  Covid(df, dataList, svnm)
             else if(rv== "LearningHL" ) datause<-  Learning(df, dataList)
             else if(rv== "WaterOnstieHL" ) datause<-  WaterOnsite(df, dataList, svnm)
             else if(rv==  "HandwashHL" ) datause<-  HandWash(df, dataList)
             else if(rv==  "SafeSanitationHL" ) datause<-  SafeSanitationHL(df, dataList, svnm)
             else if(rv==  "NotCrowdedHL" ) datause<-  NotCrowded(df, dataList)
             else if(rv== "FinancialInclusion" ) datause<-  FinancialInclusion(df, dataList)
             else if(rv== "EarlyEducation24")  datause<-  EarlyEducation24(df, dataList, k)
             else if(rv== "EarlyEducation36")  datause<-  EarlyEducation36(df, dataList, k)
             else datause<-tabulateV(df, k)
      
             if(is.null(datause)) {
                 print("No data available")
             return(NULL)
             }
             else  print(sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight))
  }
  else datause<-df
 
# if(length(datause$var2tab[datause$var2tab>=1])>0 & length(datause$var2tab[datause$var2tab==0])>0) {

   for(iv in indvar){
       
       VarName<- toupper(dataList$VarName[dataList$NickName==iv & dataList$IndicatorType=="IndependentV"])
       
       k<-match(VarName, toupper(colnames(datause)), nomatch = 0)
      if(k == 0 & (iv %in% c("Religion", "Ethnicity", "Language")))
        k<-match(iv, colnames(datause), nomatch = 0)

       if(!(is.na(VarName) | (VarName=="") | VarName=="NO KEYWORDS") & (length(k)==0)){
         print(paste("Independent variable -- ", iv, "(",  VarName, ") not found"))
         return(NULL)
        }


     if(iv=="HighestEducation") datause<-HighestEducation(datause, dataList, educationList)
     else if(iv=="MotherEducation") datause<-MotherEducation(datause, dataList, k, educationList)
     else if(iv=="PoorerHousehold") datause<-PoorerHousehold(datause, dataList, k)
     else if(iv=="aGroup") datause<-aGroup(datause, k)
     else if(iv=="aGroupHL") datause<-aGroupHL(datause, k)
     else if(iv=="NUnder5") datause<-NUnder5(datause)
     else if(iv=="Residence") datause<-Residence(datause, dataList, k)
     else if(iv=="Sex") datause<-Sex(datause, dataList, k)
     else if(iv=="HHSex") datause<-HHSex(datause, dataList, k)
     else if(iv=="MarriageStatus") datause<-MarriageStatus(datause, dataList, k)
     else if(iv=="Education") datause<-Education(datause, dataList, k, educationList)
     else if(iv=="EducationHL") datause<-EducationHL(datause, dataList, k, educationList)
     else if(iv=="Religion") datause<-Religion(datause, dataList, k, religion_data)
     else if(iv=="Language") datause<-Language(datause, dataList, k, religion_data)
     else if(iv=="Ethnicity") datause<-Ethnicity(datause, dataList, k, religion_data)
     else {
       #### if no need for recoding
       colnames(datause)[k]<-iv      
     }
   }
   return(datause)
}



indList<-function(rv){

  if(rv %in% c("CleanWater", "HouseholdBasic", "SafeSanitation", "AccessElectricity",
                "CleanFuel", "HouseholdTechNeed", "MobilePhoneHH", "BankCardHH", "HandWash", "NotCrowded", "BasicWater"))
    return(c("PoorerHousehold", "Residence", "HighestEducation"))
  else if(rv %in% c("ChildHealth", "NotStunting", "Stunting", "NotOverweight", 
                    "NotWasting", "Overweight", "Wasting", "EarlyEducation24", "EarlyEducation36"))
    return(c("PoorerHousehold", "Residence", "MotherEducation", "NUnder5", "Sex"))
  else if (rv %in% c("HigherEducation2535", "HigherEducation35plus",
                     "SecondaryEducation2035", "SecondaryEducation35plus"))
    return(c("PoorerHousehold", "Residence", "Sex"))
  else if(rv=="Land") 
    return(c("PoorerHousehold", "HHSex", "HighestEducation"))
  else if(rv=="ProfessionalHelp") 
    return(c("PoorerHousehold", "Residence", "aGroup", "MarriageStatus", "NUnder5", "Education"))
  else if (rv %in% c("ContraceptiveMethod", "ReasonBeating", "NoViolenceJustifiedAgainstWomen", "PhysicalViolence", 
                     "SexualViolence", "EmotionalViolence", "SexualPhysicalViolence", "NoSexualViolence", "AllViolence", "NoSexualPhysicalViolence"))
    return(c("PoorerHousehold", "Residence", "aGroup", "NUnder5", "Education"))
  else if (rv %in% c("AdolescentBirthRate", "ChildMarriage15", "ChildMarriage18", "TeenPregnancy", "EarlyChildBearing", "NoEarlyChildbearing",
                     "NoChildMarriage15", "NoChildMarriage18" ))
    return(c("PoorerHousehold", "Residence", "Education"))
  else if (rv %in% c("MobilePhone", "HealthInsurance", "InternetUse", "FinancialInclusion"))
    return(c("PoorerHousehold", "Residence", "aGroup", "Education"))
  else if(rv %in% c("Covid1", "Covid2", "Covid", "LearningHL", "WaterOnstieHL", "HandwashHL", "SafeSanitationHL", "NotCrowdedHL"))
    return(c("PoorerHousehold", "Residence", "aGroupHL", "EducationHL", "Sex"))   ### education
}

tabulateV<-function(df, k){
  vl<-unique(df[, k])
    print(c(2, sum(df$SampleWeight[df[,k]==2])/sum(df$SampleWeight)))
  return(NULL)
}

###############cacluation of response variable: creat var2tab (1 or 0)
MobilePhoneHH<-function(datause, k, dataList){
  {
    datause$var2tab<- 0
    levels<- unique(datause[, k])
    levels<-levels[!is.na(levels) & !levels==9]
    ln<-length(levels)-1
    levels<-max(1,ln)
    datause$var2tab[datause[, k] %in% c(1: levels)]<-1   # 1 means "smart phone", 2 means analogue 3 means both
    
    varNames<-dataList$VarName[dataList$NickName=="SmartPhoneHH"]
    if(length(varNames)>0) {
          varK<-match(varNames, colnames(datause))
          if(!is.na(varK)) datause$var2tab[datause[, varK] == 1]<-1
    }
    return(datause)
  }
}

MobilePhone<-function(datause, k){
  {
    print(c("Mobilephone levels"))
    datause$var2tab<- 0
    levels<- as.numeric(as.character(unique(datause[, k])))
    print( levels)
    levels<-levels[!levels==9]
    levels<-levels[order(levels)]
    print( levels)
    ln<-max(1, length(levels)-1)
    levels<-levels[c(1:ln)]
    print( levels)
    datause$var2tab[datause[, k] %in% levels]<-1   # 1 means "yes"
    return(datause)
  }
}

InternetUse<-function(datause, dataList, k, svnm){
  {

    
    if(svnm %in% c("Kazakhstan2015", "Turkmenistan2015", "Mongolia2013", "VietNam2013", "Kyrgyzstan2014", "Lao2011", 
                   "Kazakhstan2010")){
      ageV<-dataList$VarName[dataList$NickName=="Age"]
      ageK<-match(ageV, colnames(datause), nomatch = 0)
      
      if(ageK==0) {
        print("Age for internetuse can be found when needed")
        return(NULL)
      }

      datause$Age<-as.numeric(as.character(datause[,ageK]))

      datause<-datause[!is.na(datause$Age),]
      datause<- datause[datause$Age<=24, ]

    }


    levels<-unique(datause[,k])
    nocode<-0
    if(nocode %in% levels){
      levels<-c(1, 2, 3)
    }
    else {
    levels<-levels[!levels==9]
    levels<-levels[!is.na(levels)]
    
    print(levels)
    levels<-levels[order(levels)]
    print(levels)
    n<-max(1, (length(levels)-1))
    levels<-levels[c(1:n)]
    }
    print(levels)
    datause$var2tab<- 0
    datause$var2tab[datause[, k] %in% levels]<-1   # 1 means "yes"

    return(datause)
  }
}


FinancialInclusion<-function(datause, dataList){
  varNames<-dataList$VarName[dataList$IndicatorType=="FinancialInclusion"]
  varNames<-varNames[!is.na(varNames)]
  if(length(varNames)<1) return(NULL)
  datause$var2tab<- 0
  for(v in varNames){
    k<-match(v, colnames(datause))

    if(length(k)>0) {
      if(!is.na(k)) datause$var2tab[datause[, k]==1]<-1
    }
  }
  return(datause)
}

AccessElectricity<-function(datause, dataList, k){

  datause$var2tab<- 0
  levels<- unique(datause[, k])
  levels<-levels[!levels==9]
  no_levels<-levels[!is.na(levels)]
  ln<-max(1, length(no_levels)-1)
  levels<-c(1:ln)
  datause$var2tab[datause[,k] %in% levels]<-1 # 1 means "yes"
  solV<-dataList$VarName[dataList$NickName == "SolarElectricity"]
  solk<-match(solV, colnames(datause))
  if(length(solk)>0) datause$var2tab[datause[,solk] == 1]<-1  # 1 means "yes"
  return(datause)
}


CleanFuel<-function(datause, k){
  #### recoding?
  if(k==0) return(NULL)
  print(table(datause[,k]))
  datause[,k]<-as.numeric(as.character(datause[, k]))

  fuel_code<-cookfuel_code()

  datause$var2tab<- 0
  datause$var2tab[datause[, k] %in% fuel_code]<-1

  return(datause)
}


BankCardHH<-function(datause, k){

  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1  # 1= yes
  return(datause)
}


CleanWater<-function(datause, k, svnm){
  datause$var2tab<-0
  iws_code<-water_code(svnm)
  datause$var2tab[datause[, k] %in% iws_code]<- 1
  return(datause)
}

WaterOnsite<-  function(datause, dataList, svnm){
    datause$var2tab<-0
    v<-dataList$VarName[dataList$NickName == "BasicWater"]
    k<-match(v, colnames(datause))
    wlV<-dataList$VarName[dataList$NickName == "WaterLocation"]
    wlK<-match(wlV, colnames(datause))
    wtV<-dataList$VarName[dataList$NickName == "WaterTime"]
    wtK<-match(wtV, colnames(datause))
    
    if(length(wlK)==0 | length(wtK)==0){
      print("For BasicWater, water location or time variable not found")
      return(NULL)
    }
    datause[ , wtK]<-as.numeric(as.character(datause[ , wtK]))
    more0<- !(is.na(datause[ , wtK]))
    more0<- more0 & datause[ , wtK]>0
    
    iws_code<-water_code(svnm)
    datause$var2tab[(datause[, k] %in% iws_code) & !more0]<- 1
    
    return(datause)
  }
  
  
  
  
BasicWater<-function(datause, dataList, k, svnm){
  datause$var2tab<-0
  wlV<-dataList$VarName[dataList$NickName == "WaterLocation"]
  wlK<-match(wlV, colnames(datause))
  wtV<-dataList$VarName[dataList$NickName == "WaterTime"]
  wtK<-match(wtV, colnames(datause))
  
  if(length(wlK)==0 | length(wtK)==0){
    print("For BasicWater, water location or time variable not found")
    return(NULL)
  }
  datause[ , wtK]<-as.numeric(as.character(datause[ , wtK]))

  more30<- !(is.na(datause[ , wtK]))
  
  if(svnm=="Mongolia2013") more30<-more30 & datause[ , wtK]>=3
  else more30<- more30 & datause[ , wtK]>=30
  more30<- more30 & (datause[ , wlK] ==3)
  iws_code<-water_code(svnm)
  print(table(datause[,k])/nrow(datause)*100)
  datause$var2tab[(datause[, k] %in% iws_code) & !more30]<- 1
  
  return(datause)
}


HandWash<-function(datause, dataList){
  
  handwashlist<-dataList[dataList$IndicatorType=="Handwash",]
  nhw<-nrow(handwashlist)
  for(i in c(1:nhw)){
    if(!is.na(handwashlist$VarName[i]) & !handwashlist$VarName[i]=="")
    {
      k<-which(colnames(datause) == handwashlist$VarName[i])
      if(length(k)>0) {
        if(!is.na(k)) colnames(datause)[k]<-handwashlist$NickName[i]
      }
    }
  }
  
  place <- (datause$HandWashPlace %in% c(1, 2, 3))
  water<- (datause$HandWashWater ==1)
  soap_code<- c("A", "B", "C")
  
  
  
  soap<-!is.na(datause$HandWashSoap) & (datause$HandWashSoap==1)
  
  if(!is.na(match("Soap1", colnames(datause))))    soap<-  soap | (!is.na(datause$Soap1) & trimws(datause$Soap1) %in% soap_code)
  if(!is.na(match("Soap2", colnames(datause))))    soap<- soap | (!is.na(datause$Soap2) & trimws(datause$Soap2) %in% soap_code)
  if(!is.na(match("Soap3", colnames(datause))))    soap<- soap | (!is.na(datause$Soap3) & trimws(datause$Soap3) %in% soap_code)
  if(!is.na(match("SoapB1", colnames(datause))))  soap<-  soap | (!is.na(datause$SoapB1) & trimws(datause$SoapB1) %in% soap_code)
  if(!is.na(match("SoapB2", colnames(datause))))   soap<- soap | (!is.na(datause$SoapB2) & trimws(datause$SoapB2) %in% soap_code)
  if(!is.na(match("SoapB3", colnames(datause))))  soap<-  soap | (!is.na(datause$SoapB3) & trimws(datause$SoapB3) %in% soap_code)
  
  datause$var2tab<-0
  datause$var2tab[place & water & soap]<-1
  
  return(datause)
}

NotCrowded<-function(datause, dataList){
  notcrowdedList<-dataList[dataList$IndicatorType=="NotCrowded",]
  nnc<-nrow(notcrowdedList)
  for(i in c(1:nnc)){
    if(!is.na(notcrowdedList$VarName[i]) & !notcrowdedList$VarName[i]=="")
    {
      colnames(datause)[which(colnames(datause) == notcrowdedList$VarName[i])]<-notcrowdedList$NickName[i]
    }
    else {
      print(c(notcrowdedList$NickName[i], "is not found"))
      return(NULL)
    }
  }
  
  datause$NumberMember<-as.numeric(as.character(datause$NumberMember))
  datause$NumberRoom<-as.numeric(as.character(datause$NumberRoom))
  datause<-datause[!is.na(datause$NumberRoom), ]
  datause$Density[datause$NumberRoom>0]<-datause$NumberMember[datause$NumberRoom>0]/datause$NumberRoom[datause$NumberRoom>0]
  datause$Density[datause$NumberRoom==0]<-datause$NumberMember[datause$NumberRoom==0]
  
  datause$var2tab<-0
  datause$var2tab[datause$Density<=2]<-1
  return(datause)
}


SafeSanitation<-function(datause, dataList, k, svnm){
  datause$var2tab<-0
  isf_code<-sanitation_code(svnm)
  datause$var2tab[datause[, k] %in% isf_code]<- 1
  ssV<-dataList$VarName[dataList$NickName=="SharedToilet"]
  ssk<-match(ssV, colnames(datause))
  datause[,ssk]<-as.numeric(as.character(datause[,ssk]))
  if(!is.na(ssk)) { #print("============shared toilet==============")
    datause$var2tab[datause[,ssk]== 1]<-0  # 1 means "yes"
  }
  return(datause)
}

SafeSanitationHL<-function(datause, dataList, svnm){
  
  VarName<- toupper(dataList$VarName[dataList$NickName=="SafeSanitation"])
  k<-match(VarName, toupper(colnames(datause)), nomatch = 0) 
  if(k==0) {
            print("Safe Sanitation not found in HL")
            return(NULL)
  }
  
  datause$var2tab<-0
  isf_code<-sanitation_code(svnm)
  datause$var2tab[datause[, k] %in% isf_code]<- 1
  ssV<-dataList$VarName[dataList$NickName=="SharedToilet"]
  ssk<-match(ssV, colnames(datause))
  datause[,ssk]<-as.numeric(as.character(datause[,ssk]))
  if(!is.na(ssk)) { #print("============shared toilet==============")
    datause$var2tab[datause[,ssk]== 1]<-0  # 1 means "yes"
  }
  return(datause)
}


HouseholdBasic<-function(datause, dataList, svnm){
  waterK<-match(dataList$VarName[dataList$NickName=="CleanWater"], colnames(datause))
  if(is.na(waterK)){
    print("No water variable for housebasic")
    return(NULL)
  }
  datause<- CleanWater(datause, waterK, svnm)
  datause$var2tab1<-datause$var2tab
  
  sanitationK<-match(dataList$VarName[dataList$NickName=="SafeSanitation"], colnames(datause))
  if(is.na(sanitationK)) {
    print("No sanitation variable for housebasic")
    return(NULL)
  }
  datause<- SafeSanitation(datause, dataList, sanitationK, svnm)
  datause$var2tab2<-datause$var2tab
  
  electricityK<-match(dataList$VarName[dataList$NickName=="AccessElectricity"], colnames(datause))
  if(is.na(electricityK)) {
    print("No electricity variable for housebasic")
    return(NULL)
  }
  datause<- AccessElectricity(datause, dataList, electricityK)
  datause$var2tab3<-datause$var2tab
  
  
  fuelK<-match(dataList$VarName[dataList$NickName=="CleanFuel"], colnames(datause))
  if(is.na(fuelK)) {
    print("No fuel variable for housebasic")
    return(NULL)
  }
  datause<- CleanFuel(datause, fuelK)
  datause$var2tab4<-datause$var2tab
  
  datause$var2tab<- datause$var2tab1 * datause$var2tab2 * datause$var2tab3 *datause$var2tab4
  
  return(datause)
}


HouseholdTechNeed<-function(datause, dataList){
  mobileK<-match(dataList$VarName[dataList$NickName=="MobilePhoneHH"], colnames(datause))
  if(is.na(mobileK)){
    print("No mobile phone variable for housebasic")
    return(NULL)
  }
  datause<- MobilePhoneHH(datause, mobileK, dataList)
  datause$var2tab1<-datause$var2tab
  
  bankK<-match(dataList$VarName[dataList$NickName=="BankCardHH"], colnames(datause))
  if(is.na(bankK)) {
    print("No bank card variable for housebasic")
    return(NULL)
  }
  datause<- BankCardHH(datause, bankK)
  datause$var2tab2 <-datause$var2tab
  
  datause$var2tab <- datause$var2tab1* datause$var2tab2

  return(datause)
}


Land<-function(datause, dataList, k){
  ## only for rural residents
  resV<-dataList$VarName[dataList$NickName=="Residence"]
  resK<-match(resV, colnames(datause))
  datause[, resK]<-as.numeric(as.character(datause[, resK]))
  if(!is.na(resK)) datause<-datause[datause[, resK]== 2, ]   # 2 = rural
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1   # 1 = yes
  return(datause)

}


MultiDeprivation<-function(datause, dataList, k){
  return(NULL)     #### no definition yet
}


SecondaryEducation2035<-function(datause, dataList, k, educationList){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  gradeV<-dataList$VarName[dataList$NickName=="Grade"]
  gradeK<-match(gradeV, colnames(datause))
  datause[ , gradeK]<-as.numeric(as.character(datause[ , gradeK]))
  noGrade<-is.na(datause[ , gradeK])
  datause[noGrade , gradeK]<-0
  
  
  if(is.na(ageK)) {
    print("Age for SecondaryEducation2035 can't be found")
    return(NULL)
  }

  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[datause$Age>=20 & datause$Age<=35, ]
  
  nrow_grd<-nrow(educationList)
  educationList$Grade<-as.numeric(as.character(educationList$Grade))
  educationList$Levels<-as.numeric(as.character(educationList$Levels))
  
  datause$var2tab<-0
  max_level<-max(educationList$Levels)
  datause$var2tab[datause[, k] > max_level & datause[, k] < 8 & !is.na(datause[,k])]<-1
  

  for(i in c(1:nrow_grd)){
    datause$var2tab[datause[, k] == educationList$Levels[i] & (datause[ , gradeK]>= educationList$Grade[i] & datause[ , gradeK]<90) & !is.na(datause[,k])]<-1
  }
  
 
  return(datause)
}


SecondaryEducation35plus<-function(datause, dataList, k, educationList){
 
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  gradeV<-dataList$VarName[dataList$NickName=="Grade"]
  gradeK<-match(gradeV, colnames(datause))
  datause[ , gradeK]<-as.numeric(as.character(datause[ , gradeK]))
  if(is.na(ageK)) {
    print("Age for SecondaryEducation2035 can't be found")
    return(NULL)
  }
  
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=35, ]
  
  nrow_grd<-nrow(educationList)
  educationList$Grade<-as.numeric(as.character(educationList$Grade))
  educationList$Levels<-as.numeric(as.character(educationList$Levels))
  
  datause$var2tab<-0
  max_level<-max(educationList$Levels)
  datause$var2tab[datause[, k] > max_level & datause[, k] < 8 & !is.na(datause[,k])]<-1
  
  for(i in c(1:nrow_grd)){
  datause$var2tab[datause[, k] == educationList$Levels[i] & (datause[ , gradeK]>= educationList$Grade[i] & datause[ , gradeK]<90) & !is.na(datause[,k])]<-1
  }
  
  return(datause)
}


HigherEducation2535<-function(datause, dataList, k, educationList){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(is.na(ageK)) {
    print("Age for HigherEducation2535 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=25 & datause$Age<=35 , ]
  
  datause$var2tab<-0
  max_level<-max(educationList$Levels)
  datause$var2tab[datause[, k] > max_level & datause[, k] < 8 ]<-1
  
  #prin(table(datause[,k]))
  
  return(datause)
  }


HigherEducation35plus<-function(datause, dataList, k, educationList){

  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(is.na(ageK)) {
    print("Age for HigherEducation35plus can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >35, ]
  
  datause$var2tab<-0
  max_level<-max(educationList$Levels)
  datause$var2tab[datause[, k] > max_level & datause[, k] < 8 ]<-1
  
  return(datause)
}

AdolescentBirthRate<-function(datause, dataList, k) {

  ### for women age 20-24
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(is.na(ageK)) {
    print("Age for Adolescent Birth rate can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=20 & datause$Age <=24, ]
  
  sexV<-dataList$VarName[dataList$NickName=="Sex"]
  sexK<-match(sexV, colnames(datause))
  if(is.na(sexK)) {
    print("Sex for Adolescent Birth rate can't be found")
    return(NULL)
  }
  datause$Sex<-as.numeric(as.character(datause[,sexK]))
  datause<-datause[!is.na(datause$Sex) & datause$Sex ==2, ]
  
  datause$numberBirths[is.na(datause$numberBirths)]<-0
  datause$var2tab<-datause$numberBirths/4*1000


  print(paste("average birthrate is ", sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)))
  return(datause)
}


NotStunting<-function(datause, dataList, k){
  datause<-Stunting(datause, dataList, k)
  
  
  ###Not
  datause$var2tab<- 1- datause$var2tab
  return(datause)
}


NotOverweight<-function(datause, dataList, k){
  datause<-Overweight(datause, dataList, k)
  
  ###Not
  datause$var2tab<- 1- datause$var2tab
  return(datause)
}


NotWasting<-function(datause, dataList, k){
  datause<-Wasting(datause, dataList, k)
  
  datause$var2tab<- 1-datause$var2tab
  
  return(datause)
}


ChildHealth<-function(datause, dataList, k){
  return(NULL)
}


Stunting<-function(datause, dataList, k){
  if(sum(is.na(datause[,k]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  VarName<- dataList$VarName[dataList$NickName=="ChildHealth" & dataList$IndicatorType=="ResponseV0"]
  k0<-match(VarName, colnames(datause))
  
  if(length(k0)>0)  {
      if(!is.na(k0)) datause<-datause[datause[, k0] %in% c(1, 2), ]
  }
  # 1- Lying down 
  # 2 - Standing up
  
  VarName<- dataList$VarName[dataList$NickName=="Age" & dataList$IndicatorType=="IndependentV"]
  k0<-match(VarName, colnames(datause))
  datause<-datause[!is.na(datause[, k0]) & datause[, k0]<= 60, ]
  datause[, k]<-as.numeric(datause[, k])
  datause<-datause[!is.na(datause[, k]) & datause[, k]< 6 & datause[ ,k] > -6, ]
  datause$var2tab<-0
  datause$var2tab[datause[,k]<= -2 ]<-1
  
  return(datause)
}


Overweight<-function(datause, dataList, k){
  if(sum(is.na(datause[,k]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  
  VarName<- dataList$VarName[dataList$NickName=="ChildHealth" & dataList$IndicatorType=="ResponseV0"]
  k0<-match(VarName, colnames(datause))
  
  if(length(k0)>0) {
    if(!is.na(k0)) datause<-datause[datause[, k0] %in% c(1, 2), ]
  }
  # 1- Lying down 
  # 2 - Standing up
  
  VarName<- dataList$VarName[dataList$NickName=="Age" & dataList$IndicatorType=="IndependentV"]
  k0<-match(VarName, colnames(datause))
  datause<-datause[!is.na(datause[, k0]) & datause[, k0]<= 60, ]

  datause<-datause[!is.na(datause[, k]) & datause[, k]< 6 & datause[ ,k] > -6, ]
  datause$var2tab<-0
  datause$var2tab[datause[,k] >= 2 ]<-1
  
  return(datause)
}


Wasting<-function(datause, dataList, k){
  if( sum(is.na(datause[,k]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  VarName<- dataList$VarName[dataList$NickName=="ChildHealth" & dataList$IndicatorType=="ResponseV0"]
  k0<-match(VarName, colnames(datause))
  
  if(length(k0)>0) {
    if(!is.na(k0)) datause<-datause[datause[, k0] %in% c(1, 2), ]
  }
  # 1- Lying down 
  # 2 - Standing up
  
  VarName<- dataList$VarName[dataList$NickName=="Age" & dataList$IndicatorType=="IndependentV"]
  k0<-match(VarName, colnames(datause))
  datause<-datause[!is.na(datause[, k0]) & datause[, k0]<= 60, ]
  datause[, k]<-as.numeric(datause[, k])
  datause<-datause[!is.na(datause[, k]) & datause[, k]< 6 & datause[ ,k] > -6, ]
  datause$var2tab<-0
  datause$var2tab[datause[,k]<= -2 ]<-1
  
  return(datause)
  
}
EarlyEducation24<-function(datause,dataList, k){
  VarName<- dataList$VarName[dataList$NickName=="Age" & dataList$IndicatorType=="IndependentV"]
  k0<-match(VarName, colnames(datause))
  datause$age<-as.numeric(as.character(datause[, k0]))
  datause<-datause[!is.na(datause$age) & datause$age<= 59  & datause$age >= 24, ]
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1  # 1= yes
  datause$var2tab[is.na(datause[, k])]<-0
  print(table(datause[, k]))
  print(sum(datause$SampleWeigt))
  return(datause)
}

EarlyEducation36<-function(datause, dataList, k){
  VarName<- dataList$VarName[dataList$NickName=="Age" & dataList$IndicatorType=="IndependentV"]
  k0<-match(VarName, colnames(datause))
  datause$age<-as.numeric(as.character(datause[, k0]))
  datause<-datause[!is.na(datause$age) & datause$age<= 59  & datause$age >= 36, ]
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1  # 1= yes
  datause$var2tab[is.na(datause[, k])]<-0
  print(table(datause[, k]))
  print(sum(datause$SampleWeigt))
  return(datause)
}

BankCardHH<-function(datause, k){
  
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1  # 1= yes
  return(datause)
}


EarlyChildBearing<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause), nomatch = 0)
  if(ageK==0) {
    print("Age for EarlyChildBearing can't be found")
    return(NULL)
  }
  
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age),]
  datause<-datause[datause$Age<=24 & datause$Age>=20, ]

  fbV<-dataList$VarName[dataList$NickName== "DateFB"]
  fbK<-match(fbV, colnames(datause), nomatch = 0)
  if(fbK>0) {
    wmV<-dataList$VarName[dataList$NickName== "WMBirthDate"]
    wmK<-match(wmV, colnames(datause))

    agefb<-(datause[,fbK]-datause[ , wmK])/12
  }
  else
    { YearFBV<-dataList$VarName[dataList$NickName=="YearFB"]
      YearFBK<-match(YearFBV, colnames(datause), nomatch = 0)
      monthFBV<-dataList$VarName[dataList$NickName=="MonthFB"]
      monthFBK<-match(monthFBV, colnames(datause), nomatch = 0)
      
      YearWBV<-dataList$VarName[dataList$NickName=="YearWB"]
      YearWBK<-match(YearWBV, colnames(datause), nomatch = 0)
      monthWBV<-dataList$VarName[dataList$NickName=="MonthWB"]
      monthWBK<-match(monthWBV, colnames(datause), nomatch = 0)
      
      if(YearFBK>0){
         #vec <- c("January","Febuary","March","April", "May", "June", "July", "August", "September", "October", "November", "December")
          agefb<-(as.numeric(as.character(datause[, YearFBK]))-as.numeric(as.character(datause[, YearWBK])))+(as.numeric(datause[, monthFBK])-as.numeric(datause[,monthWBK]))/12
        }
    }
  datause$var2tab<-0
  datause$var2tab[!is.na(agefb) & agefb<=19]<-1   
  if(sum(datause$var2tab)>0 & sum(datause$var2tab)< nrow(datause)) return(datause)
  else return(NULL)
}


NoEarlyChildbearing<-function(datause, dataList, k){
 datause<-EarlyChildBearing(datause, dataList, k)
 datause$var2tab<-1-datause$var2tab
return(datause)
}

######### IR response variables
ContraceptiveMethod<-function(datause, dataList, k, svnm){
  contraceptionList<-dataList$VarName[dataList$IndicatorType=="ModernContraceptive"]

  if (is.na(k)) {
    datause$var2tab<-0
    print("No pregnancy information")
    return(datause)
  }
  else {

   datause<-unmet_need(datause, dataList, svnm)
   
    keep <- datause$UnmetNeed %in% c( 1, 2, 3, 4) & datause$MSTATUS==1

    print(table(datause$UnmetNeed))
    datause<-datause[keep, ]
    print("number of women needed for family planning")
    print(sum(datause$SampleWeight))
    datause$var2tab<- 0
    
    for( jV in contraceptionList){
      jK<-match(jV, colnames(datause))
      if(length(jK)>0){
        if(!is.na(jK)){
          datause[, jK]<-trimws(datause[, jK])
          datause$var2tab[!(is.na(datause[, jK])) & !(datause[, jK]=="") ]<- 1
        }
      }
    }
    print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
    return(datause)
  }
}
HealthInsurance<-function(datause, dataList, k){
  print(table(datause[,k]))
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
  return(datause)
}

ProfessionalHelp<-function(datause, dataList, k, svnm){
      datause<-datause[!is.na(datause[, k]), ]   #### having reported about birth/delivery place in the years specified by the survey
      datause$var2tab <- 0
    
      # Professional assistance for home delivery includes assistance by a doctor, nurse/midwife, 
      # auxiliary nurse midwife (ANM), 
      # lady health visitor (LHV), or other health professional. 
      # https://www.dhsprogram.com/pubs/pdf/WP28/WP28.pdf
      
      if(svnm=="Georgia2018"){
        datause$var2tab[datause[, k] %in% c(41, 42, 43, 46)]<-1
        
      }

  
      else {
        phV<-dataList$VarName[dataList$IndicatorType =="ProfessionalHelp"]
        for(phvi in phV){
          phki<-match(phvi, colnames(datause))
          if(length(phki)>0) {
            if(!is.na(phki)){
              datause[, phki]<-trimws(datause[, phki])
              datause$var2tab[!(is.na(datause[, phki])) & !(datause[, phki]=="")]<-1 
            }
          }
        }
      }

  return(datause)
}
  
ChildMarriage18<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for childmarriage 18 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age),]
  datause<-datause[datause$Age>=20 & datause$Age<=24, ]

  datause$var2tab<- 0
  datause$var2tab[datause[, k]<18]<-1
  return(datause)
  
}

ChildMarriage15<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for childmarriage 15 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age),]
  datause<-datause[datause$Age>=20 & datause$Age<=24, ]
  datause$var2tab<- 0
  datause$var2tab[datause[, k]<15]<-1
  return(datause)
  
}

NoChildMarriage18<-function(datause, dataList, k){
  datause<-ChildMarriage18(datause, dataList, k)
  datause$var2tab<- 1- datause$var2tab
  return(datause)
  
}

NoChildMarriage15<-function(datause, dataList, k){
  datause<-ChildMarriage15(datause, dataList, k)
  datause$var2tab<- 1- datause$var2tab
  return(datause)
  
}


TeenPregnancy<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for TeenPregnancy can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age),]
  datause<-datause[datause$Age>=15 & datause$Age<=19, ]
  
  datause$var2tab<- 0
  datause$var2tab[datause[, k]==1]<-1   # alreay have children
  pregnantV<-dataList$VarName[dataList$NickName=="PregnantNow"]
  pregnantK<-match(pregnantV, colnames(datause))
  if(length(pregnantK)==0 ) {
    print("Pregnancy information for TeenPregnancy can't be found")
  }
  else if(!is.na(pregnantK)) datause$var2tab[datause[, pregnantK]==1]<-1
  
  return(datause)
  
}


ReasonBeating<-function(datause, dataList, k){
  datause$var2tab<-0
  rbV<-dataList$VarName[dataList$IndicatorType =="ReasonBeating"]
  for(rbvi in rbV){
    rbki<-match(rbvi, colnames(datause))
    if(length(rbki)>0) {
      if(!is.na(rbki)) datause$var2tab[datause[ ,rbki]==1]<- 1 
    }
  }
  return(datause)
  
}
NoViolenceJustifiedAgainstWomen<-function(datause, dataList, k){
  datause<-ReasonBeating(datause, dataList, k)
  datause$var2tab<- 1- datause$var2tab
  return(datause)
}

DVdata<-function(df, dataList){
  pV<-dataList$VarName[dataList$NickName=="Privacy"]
  cmV<-dataList$VarName[dataList$NickName=="CurrentlyMarried"]
  emV<-dataList$VarName[dataList$NickName=="EverMarried"]
  wV<-toupper(dataList$VarName[dataList$NickName=="Dvweight"])
  
  pK<-match(pV, colnames(df))
  cmK<-match(cmV, colnames(df))
  emK<-match(emV, colnames(df))
  wK<-match(wV, colnames(df))
  print(wK)
  if(length(wK)>0) df$SampleWeight<-as.numeric(as.character(df[, wK]))
  
  if(length(pK)==0 | length(cmK)==0 | length(emK)==0) {
    print("lacking information on Privacy or marriage status")
    return(NULL)
  }
  else if(is.na(pK) | is.na(cmK) | is.na(emK)) {
    print("can't find information on Privacy or marriage status")
    return(NULL)
  }
  else {
    datause<-df[!is.na(df[, pK]) & df[, pK]==1, ]
    datause1<-datause[datause[,cmK] %in% c(1,2), ]
    datause2<-datause[!is.na(datause[,emK]) & datause[,emK] %in% c(1,2) & datause[,cmK]==3, ]
    datause<-rbind(datause1, datause2)
  }


  print(nrow(datause))
  print(sum(datause$SampleWeight))
  return(datause)
}

PhysicalViolence<-function(df, dataList){
 datause<-DVdata(df, dataList)
 if(!is.null(datause)){
   datause$var2tab<-0
   rbV<-dataList$VarName[dataList$IndicatorType =="PhysicalViolence"]
   for(rbvi in rbV){
     rbki<-match(rbvi, colnames(datause))
     if(length(rbki)>0) {
       if(!is.na(rbki)) datause$var2tab[datause[ ,rbki]==1]<- 1 
     }
   }
 }
 return(datause)
}

SexualViolence<-function(df, dataList){
  datause<-DVdata(df, dataList)
  if(!is.null(datause)){
    datause$var2tab<-0
    rbV<-dataList$VarName[dataList$IndicatorType =="SexualViolence"]
    for(rbvi in rbV){
      rbki<-match(rbvi, colnames(datause))
      if(length(rbki)>0) {
        if(!is.na(rbki)) datause$var2tab[datause[ ,rbki]==1]<- 1 
      }
    }
  }
  return(datause)
}

EmotionalViolence<-function(df, dataList){
  datause<-DVdata(df, dataList)
  if(!is.null(datause)){
    datause$var2tab<-0
    rbV<-dataList$VarName[dataList$IndicatorType =="EmotionalViolence"]
    for(rbvi in rbV){
      rbki<-match(rbvi, colnames(datause))
      if(length(rbki)>0) {
        if(!is.na(rbki)) datause$var2tab[datause[ ,rbki]==1]<- 1 
      }
    }
  }
  return(datause)
}

SexualPhysicalViolence<-function(df, dataList){
  datause<-PhysicalViolence(df, dataList)
  if(!is.null(datause)){
    datause$var2tab1<-datause$var2tab
    datause<-SexualViolence(datause, dataList)
    datause$var2tab[datause$var2tab1==1]<-1
  }
 return(datause)
}

NoSexualPhysicalViolence<-function(df, dataList){
  datause<-SexualPhysicalViolence(df, dataList)
  if(!is.null(datause)){
    datause$var2tab<-1-datause$var2tab
  }
  return(datause)
}

NoSexualViolence<-function(df, dataList){
  datause<-SexualViolence(df, dataList)
  if(!is.null(datause))
    datause$var2tab<-1-datause$var2tab
  return(datause)
}


AllViolence<-function(df, dataList){
  datause<-PhysicalViolence(df, dataList)
  if(!is.null(datause)){
    datause$var2tab1<-datause$var2tab
    datause<-SexualViolence(datause, dataList)
    datause$var2tab1[datause$var2tab==1]<-1
    datause<-EmotionalViolence(datause, dataList)
    datause$var2tab[datause$var2tab1==1]<-1
  }
  return(datause)
  
}


# defined via email in dec 2020
# Access to the internet, TV, phone or radio (learning)
# +  The household has water pipes into the dwelling or yard, or other private water source (preventing)   --- using our old construction on sources of drinking water but requires less than 30 minutes of time to fetch water
# +  The household has a place for washing (hands, ?)
Learning<-function(datause, dataList){
  datause$var2tab<-0
  
  lV<-dataList$VarName[dataList$IndicatorType=="Learning"]
  #print("=========begin eanring======")
  for(lVi in lV){
    if(lVi %in% colnames(datause)) {
      k<-match(lVi, colnames(datause))
      datause$var2tab[datause[, k]==1]<-1
    }
    else print("A learning variable Not Found")
    
  }
  
  #print("===========end of learning =========")
  return(datause)
}

Covid1<-function(datause, dataList, svnm){
  
  datause<-Learning(datause, dataList)
  if(is.null(datause)) {
    print("no learning data for covid, no data generaged")
    return(NULL)
  }
  print("Learning: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$learning<-datause$var2tab
  
  k<-match(dataList$VarName[dataList$NickName=="BasicWater"], colnames(datause))
  datause<-BasicWater(datause, dataList, k, svnm)
  if(is.null(datause)) {
    print("no water data for covid, no data generaged")
    return(NULL)
  }
  print("Basic Water: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$basicwater<-datause$var2tab

  datause<-HandWash(datause, dataList) 
  if(is.null(datause)) {
    print("no handwashing data for covid, no data generaged")
    return(NULL)
  }
  print("Hand Wash: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  
  datause$var2tab<-datause$basicwater * datause$learning * datause$var2tab
  print("Covid 1: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  
  return(datause)
}  

# Covid1 +
#   
#   No more than 2 people per sleeping room (isolating)
#  + The household has a toilet which is not shared with other households (preventing)   ---- using our old definition
#  + The household has walls and ceilings (isolating) (do we have any requirement on materials? or just as long as there are all & roof?)

WallRoof<-function(datause, dataList){
  print("=============beginning of wall and roof=========")
  varlist<-dataList$VarName[dataList$IndicatorType=="WallRoof"]
  datause$var2tab<-1
  for (vi in varlist){
    if(vi %in% colnames(datause)) datause$var2tab[datause[, vi]==11]<-0
    else print("Not Found")
  }
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print("=========end of wall and roof ============")
  return(datause)
}


Covid2<-function(datause, dataList, svnm){
  datause<-Covid1(datause, dataList, svnm)
  if(is.null(datause)) {
    print("no covid1 data for covid, no data generaged")
    return(NULL)
  }
  print("Covid1 :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$covid1<-datause$var2tab
  #look for k
  ncV<-dataList$VarName[dataList$NickName=="NotCrowded"]
  k<-match(ncV, colnames(datause))
  datause<-NotCrowded(datause, dataList)
  if(is.null(datause)) {
    print("no crowded data for covid, no data generaged")
    return(NULL)
  }
  print("Not crowded :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$notcrowded<-datause$var2tab
  #look for k
  ssV<-dataList$VarName[dataList$NickName=="SafeSanitation"]
  k<-match(ssV, colnames(datause))
  datause<-SafeSanitation(datause, dataList, k, svnm)
  if(is.null(datause)) {
    print("no sanitation data for covid, no data generaged")
    return(NULL)
  }
  print("SafeSanitation :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$safsanitation<-datause$var2tab
  
  datause<-WallRoof(datause, dataList)
  if(is.null(datause)) {
    print("no wall and roof data for covid, no data generaged")
    return(NULL)
  }
  print("wall and roof: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$var2tab <- datause$covid1 * datause$notcrowded *datause$safsanitation * datause$var2tab
  
  print("covid 2: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  return(datause)
}

Covid<-function(datause, dataList, svnm){
  datause<-Learning(datause, dataList)
  if(is.null(datause)) {
    print("no learning data for covid, no data generaged")
    return(NULL)
  }
  print("Learning: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$learning<-datause$var2tab
  
  # k<-match(dataList$VarName[dataList$NickName=="BasicWater"], colnames(datause))
  datause<-WaterOnsite(datause, dataList, svnm)
  if(is.null(datause)) {
    print("no water data for covid, no data generaged")
    return(NULL)
  }
  print("on site Water: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$wateronsite<-datause$var2tab
  
  datause<-HandWash(datause, dataList) 
  if(is.null(datause)) {
    print("no handwashing data for covid, no data generaged")
    return(NULL)
  }
  print("Hand Wash: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$handwash<-datause$var2tab

  #look for k
  ncV<-dataList$VarName[dataList$NickName=="NotCrowded"]
  k<-match(ncV, colnames(datause))
  datause<-NotCrowded(datause, dataList)
  if(is.null(datause)) {
    print("no crowded data for covid, no data generaged")
    return(NULL)
  }
  print("Not crowded :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$notcrowded<-datause$var2tab
  #look for k
  # ssV<-dataList$VarName[dataList$NickName=="SafeSanitation"]
  # k<-match(ssV, colnames(datause))
  datause<-SafeSanitationHL(datause, dataList, svnm)
  if(is.null(datause)) {
    print("no sanitation data for covid, no data generaged")
    return(NULL)
  }
  print("SafeSanitation :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$safsanitation<-datause$var2tab
  
  datause$var2tab <- datause$learning * datause$wateronsite * datause$handwash * datause$notcrowded *datause$safsanitation 
  
  print("covid (new): ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  return(datause)
}




########### PR independent variables
PoorerHousehold<-function(datause, dataList, k){
    datause$PoorerHousehold<-"0"
    datause$PoorerHousehold[datause[,k] %in% c( 1, 2)]<-"1"
  return(datause)
}


MotherEducation<-function(datause, dataList, k, educationList){
    datause$MotherEducation<-"Lower" 
    datause$MotherEducation[datause[,k] <= educationList$Grade]<-"Lower"
    datause$MotherEducation[datause[,k]>educationList$Grade &  datause[,k]<= educationList$Levels]<-"Secondary"
    datause$MotherEducation[datause[,k]>educationList$Levels & datause[,k]<8]<-"Higher"
    datause$MotherEducation<-factor(datause$MotherEducation, levels = c("Lower" , "Secondary", "Higher"), ordered = TRUE)

#### country dependent, has to be specified in MICSStandard.csv
  return(datause)
}


NUnder5<-function(datause){
  datause$NUnder5<-as.numeric(as.character(datause$NUnder5))
  return(datause) 
}
  


 Sex<-function(datause, dataList, k){

     datause$Sex<-"Male"
     datause$Sex[datause[, k]==2]<-"Female"
     datause$Sex<-factor(datause$Sex, levels = c("Male" , "Female"))

   return(datause)
 }
 
 
 HHSex<-function(datause, dataList, k){

     datause$HHSex<-"Male"
     datause$HHSex[datause[, k]==2]<-"Female"
     datause$HHSex<-factor(datause$HHSex, levels = c("Male" , "Female"))

   return(datause)
 }
 
 
 Residence<-function(datause, dataList, k){

     datause$Residence<-"Urban"
     datause$Residence[datause[, k]==2]<-"Rural"
     datause$Residence<-factor(datause$Residence, levels = c("Urban" , "Rural"))

   return(datause)
 }
 
 
 EducationHL<-function(datause, dataList, k, educationList){
   
   datause$Elevel<-as.numeric(as.character(datause[, k]))
   datause$EducationHL <-"Lower"
   max_level<-max(educationList$Levels)
   min_level<-min(educationList$Levels)
   datause$EducationHL[datause$Elevel >=min_level & datause$Elevel <=max_level ]<-"Secondary"
   datause$EducationHL[datause$Elevel> max_level & datause$Elevel < 8 ]<-"Higher"
   datause$EducationHL<-factor(datause$EducationHL, levels = c("Lower", "Secondary", "Higher"), ordered = TRUE)
   
   return(datause)
 }
 
############ HR independent variables
HighestEducation<-function(datause, dataList, educationList){

    datause$Elevel<-as.numeric(as.character(datause$HighestEducation))
    datause$HighestEducation <-"Lower"
    max_level<-max(educationList$Levels)
    min_level<-min(educationList$Levels)
    datause$HighestEducation[datause$Elevel >=min_level & datause$Elevel <=max_level ]<-"Secondary"
    datause$HighestEducation[datause$Elevel> max_level & datause$Elevel < 8 ]<-"Higher"
    datause$HighestEducation<-factor(datause$HighestEducation, levels = c("Lower", "Secondary", "Higher"), ordered = TRUE)
    

  return(datause)
}

#######333 IR independent variables
aGroup<-function(datause, k){

    datause$Age<-as.numeric(as.character(datause[ ,k]))
    datause<-datause[!is.na(datause$Age),]
    datause$aGroup<-"Missing"
    datause$aGroup[!is.na(datause$Age) & datause$Age<15 ]="0-14"
    datause$aGroup[!is.na(datause$Age) & datause$Age>=15 & datause$Age<25 ]="15-24"
    datause$aGroup[!is.na(datause$Age) & datause$Age>=25 & datause$Age<35  ]="25-34"
    datause$aGroup[!is.na(datause$Age) & datause$Age>=35 & datause$Age<98]="35+"
    datause$aGroup<-factor(datause$aGroup, levels=c("0-14", "15-24", "25-34", "35+")) 

  return(datause)
}

aGroupHL<-function(datause, k){
  
  datause$Age<-as.numeric(as.character(datause[ ,k]))
  datause<-datause[!is.na(datause$Age),]
  datause$aGroupHL<-"Missing"
  datause$aGroupHL[!is.na(datause$Age) & datause$Age<25 ]="0-24"
  datause$aGroupHL[!is.na(datause$Age) & datause$Age>=25 & datause$Age<59  ]="25-59"
  datause$aGroupHL[!is.na(datause$Age) & datause$Age>=60]="60+"
  datause$aGroupHL<-factor(datause$aGroupHL, levels=c("0-24", "25-59", "60+")) 
  
  return(datause)
}


MarriageStatus<-function(datause, dataList, k){
    datause$MarriageStatus<-"NeverMarried"
    datause$MarriageStatus[datause[, k]==1 ]<- "CurrentlyMarried"
    datause$MarriageStatus[datause[, k]==2 ]<-"FormerlyMarried"
    datause$MarriageStatus<-factor(datause$MarriageStatus, levels=c("CurrentlyMarried", "FormerlyMarried", "NeverMarried"))

    return(datause)
}


Education<-function(datause, dataList, k, educationList){
  datause$Education<-"Lower" 
  datause$Education[datause[,k] <= educationList$Grade]<-"Lower"
  datause$Education[datause[,k]>educationList$Grade &  datause[,k]<= educationList$Levels]<-"Secondary"
  datause$Education[datause[,k]>educationList$Levels & datause[,k]<8]<-"Higher"
  datause$Education<-factor(datause$Education, levels = c("Lower" , "Secondary", "Higher"), ordered = TRUE)
  
  #### country dependent, has to be specified in MICSStandard.csv
  return(datause)
  
  # gradeV<-dataList$VarName[dataList$NickName=="Grade"]
  # gradek<-match(gradeV, colnames(datause))
  # 
  # datause$Education<-"Lower" 
  # 
  # ngrd<-nrow(educationList)
  # if(ngrd>1){
  #   for(i in c(1:ngrd)){
  #     if(educationList$Grade[i]==0) datause$Education[datause[, k]== educationList$Levels[i]]<-"Secondary"
  #     else datause$Education[datause[,gradek]>=educationList$Grade[i] & datause[,gradek]<90 &  datause[, k]== educationList$Levels[i]]<-"Secondary"
  #   }
  # }
  # else datause$Education[datause[, gradek]>=educationList$Grade  & datause[,gradek]<90 &  datause[,k]== educationList$Levels]<-"Secondary"
  # 
  # datause$Education[datause[,k]>max(educationList$Levels) & datause[,k]<8]<-"Higher"
  # 
  # datause$Education<-factor(datause$Education, levels = c("Lower" , "Secondary", "Higher"), ordered = TRUE)
  # return(datause)
}


Religion<-function(datause, dataList, k, religion_data){
  religion_code<-religion_data[religion_data$NickName=="Religion", ]
  rk<-nrow(religion_code)
  datause$tmp<-datause[, k]
  if(length(k)>0 & rk>0){
    if(is.na(k)) return(datause)
    datause$Religion<-"MinorReligion"
    for(i in c(1:rk)){
      codei<-religion_code$Levels[i]
      labeli<-religion_code$Labels[i]
      datause$Religion[datause$tmp==codei]<-labeli
      }
  }
  return(datause)
}

Language<-function(datause, dataList, k, religion_data){
  
  Language_code<-religion_data[religion_data$NickName=="Language", ]
  rk<-nrow(Language_code)
  
  datause$tmp<-datause[, k]
  if(length(k)>0 & rk>0){
    datause$Language<-"MinorLanguage"
    for(i in c(1:rk)){
      codei<-Language_code$Levels[i]
      labeli<-Language_code$Labels[i]
      datause$Language[datause$tmp==codei]<-labeli
    }
  }
  return(datause)
}

Ethnicity<-function(datause, dataList, k, religion_data){
  Ethnicity_code<-religion_data[religion_data$NickName=="Ethnicity", ]

  rk<-nrow(Ethnicity_code)
  
  datause$tmp<-datause[, k]
  if(length(k)>0 & rk>0){
    datause$Ethnicity<-"MinorEthnicity"
    for(i in c(1:rk)){
      codei<-Ethnicity_code$Levels[i]
      labeli<-Ethnicity_code$Labels[i]
      datause$Ethnicity[datause$tmp==codei]<-labeli
    }
  }
  return(datause)
  
}

#### codes
water_code<-function(svnm){
  #### clean
  print(svnm)
  if(svnm %in% c("Kazakhstan2010", "Turkmenistan2015"))
    return(c(11, 12, 13, 14, 15, 16, 17, 21, 31, 41, 51, 52, 53, 54, 71, 72, 73, 82, 91, 93, 94) )
  if(svnm %in% c("Kazakhstan2015"))
    return(c(11, 12, 13, 14, 21, 31, 41, 91) )
  else if(svnm %in% c("Afghanistan2010")) return(c(11, 12, 13, 14, 21, 31, 41, 51, 91) )
  else if(svnm %in% c("Mongolia2013")) return(c(11, 12, 13, 14, 15, 16, 17, 22, 31, 41, 51, 61, 91) )
  else if(svnm %in% c("Mongolia2018")) return(c(11, 12, 13, 14, 15, 16, 17, 21, 31, 41, 51, 61, 62, 63, 71, 72, 73, 91) )
  else return(c(11, 12, 13, 14, 15, 16, 17, 21, 31, 41, 51, 52, 53, 54, 61, 71, 72, 73, 82, 91, 93, 94) )
                                    
  # 11 - Piped into dwelling 
  # 12 - Piped into compound, yard or plot 
  # 13 - Piped to neighbour 
  # 14 - Public tap / standpipe 
  # 15 - Piped into dwelling from central system 
  # 16 - Piped into dwelling from individual system 
  # 17 - Public water kiosk connected with central system 
  # 21 - Tube well, Borehole
  # 22 - Tube well, Borehole, unprotected
  # 31 - Protected well 
  # 41 - Protected spring 
  # 51 - Rainwater collection
  # 91 - Bottled water
  
  # new added for the new SDG standard
  # 52, 53, 54 = cement
  # 61 - tanker truck   ---- for Turkmenistan 2019, this is considered clean water source
  # 72 - WATER KIOSK CONNECTED WITH PIPED WATER
  # 73 - WATER KIOSK NOT CONNECTED WITH PIPED WATER
  # Packaged water (bottled water and sachet water) and delivered water (tanker truck and cart with small drum/tank) are treated as improved based in new
  # SDG definition.   --- from Mongolia 2018 final report
  # 93 - PACKAGED WATER: GLASS / CUP WATER
  # 94 - COIN-OPERATED WATER DISPENSER
  # 71 - CART WITH SMALL TANK
  # 82 - DISALINATION PLANT WATER
  
}

sanitation_code<-function(svnm){
  #### safe
 #return(c(14, 15,  23,  30, 31, 32,  42, 43, 51, 61, 96) )
  if(svnm=="Mongolia2013")
  return(c(11, 12, 13, 16, 21, 31))
  else return(c(11, 12, 13, 16, 21, 22, 31))
  
  # 11- Flush to piped sewer system                         
  # 12- Flush to septic tank 
  # 13 - Flush to pit (latrine) 
  # 16 -  Flush to septic tank (with soak pit) 
  # 21 - Ventilated Improved Pit latrine (VIP) 
  # 22 -  Pit latrine with slab
  # 31 - Composing toilet 

}

cookfuel_code<-function(){
  #### clean
return(c(1, 2, 3, 4, 5))   #  1 = Electricity 
                        #  2 = Liquefied Petroleum Gas (LPG)
                        #  3 = Natural gas 
                        #  4 = Biogas
                        #  5 = solar
}

############################################################# 

importMICSdata<-function(data_folder, country_code, year_code, data_type, var_list, regionVar=NULL, provinceVar=NULL, la=FALSE){
  v<-toupper(var_list)

  country_directory <- paste(data_folder, country_code, year_code, sep = "")
  data_type_directory <- paste(country_directory, paste(data_type, "sav", sep="."), sep = "/")
  
  df<-read.spss(data_type_directory, 
                use.value.labels = la, trim_values=TRUE, to.data.frame = TRUE)
  
  col_index<-match(v, toupper(colnames(df)))
  col_index<-col_index[!is.na(col_index)]
  
  df<-df[ , col_index]

  if(!is.null(regionVar)){
    if(length(regionVar)>0){
          dfr<-read.spss(data_type_directory,
                  use.value.labels = TRUE, trim_values=TRUE, to.data.frame = TRUE)
          k<-match(regionVar, toupper(colnames(dfr)))
          if(!is.na(k)>0)   df$Region<-dfr[ , k]
      }
  }
  if(!is.null(provinceVar)){
    if(length(provinceVar)>0){
        dfr<-read.spss(data_type_directory,
                   use.value.labels = TRUE, trim_values=TRUE, to.data.frame = TRUE)
        k<-match(provinceVar, toupper(colnames(dfr)))
        if(!is.na(k)) df$Province<-dfr[ , k]
    }
  }
  return(df)
}


add_highestEducation <- function(df, meta_data, data_folder, country_code, version_code,
                                 dataList) {
  
  dataList2<-meta_data[meta_data$DataSet=="hl", ]
  df2<-importMICSdata(data_folder, country_code, version_code, "hl", unique(dataList2$VarName))  
  colnames(df2)<-toupper(colnames(df2))
  id1<-dataList2$VarName[dataList2$NickName=="cluster_id"]
  k1<-match(id1, colnames(df2))
  id2<-dataList2$VarName[dataList2$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df2))
  id3<-dataList2$VarName[dataList2$NickName=="Education"]
  k3<-match(id3, colnames(df2))
  colnames(df2)[c(k1, k2, k3)]<-c("cluster_id", "HouseholdNumber", "Education")
  df2<-df2[!is.na(df2$Education) & df2$Education<8, ]
  max_edu<-aggregate(df2$Education, by=list(df2$cluster_id, df2$HouseholdNumber), FUN=max)
  colnames(max_edu)<-c("cluster_id", "HouseholdNumber", "HighestEducation")
  
  id1<-dataList$VarName[dataList$NickName=="cluster_id"]
  k1<-match(id1, colnames(df))
  id2<-dataList$VarName[dataList$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df))
  colnames(df)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  
  df<-merge(df, max_edu, by=c("cluster_id", "HouseholdNumber"), all.x=TRUE)

  return(df)  
}

add_reglist<-function(country_code, version_code, datause, meta_data, dataList, regList, religion_data){

  ### 1  import hh data that has the religion/ethnicity/language variable

  dataList2<-meta_data[meta_data$DataSet=="hh" & (meta_data$IndicatorType=="ID" | meta_data$NickName %in% regList), ]
  df2<-importMICSdata(data_folder, country_code, version_code, "hh", unique(dataList2$VarName)) 
  
  ### 2 using get_data function to get the labels of the religion/ethnicity/language variable
  colnames(df2)<-toupper(colnames(df2))
  #df2<-get_data(df2, rv=NULL, dataList2, regList, educationList=NULL,religion_data)
  
  ### 3 merge the data with datause by cluster number and household number
  id1<-dataList2$VarName[dataList2$NickName=="cluster_id"]
  k1<-match(id1, colnames(df2))
  id2<-dataList2$VarName[dataList2$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df2))
  id3<-dataList2$VarName[dataList2$NickName==regList[1]]
  k3<-match(id3, colnames(df2))
  colnames(df2)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  colnames(df2)[k3]<-regList[1]
  
  id1<-dataList$VarName[dataList$NickName=="cluster_id"]
  k1<-match(id1, colnames(datause))
  id2<-dataList$VarName[dataList$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(datause))
  if(!is.na(k1)) colnames(datause)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  
  datause<-merge(datause, df2, by=c("cluster_id", "HouseholdNumber"), all.x=TRUE)
  return(datause)  
}


add_hhdata <- function(df, data_folder, country_code, version_code, 
                       dataList, dataList2) {
  

  df2<-importMICSdata(data_folder, country_code, version_code, "hh", unique(dataList2$VarName))  
  colnames(df2)<-toupper(colnames(df2))
  
  id1<-dataList2$VarName[dataList2$NickName=="cluster_id"]
  k1<-match(id1, colnames(df2), nomatch = 0)
  id2<-dataList2$VarName[dataList2$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df2), nomatch = 0)
  if(k1>0 & k2>0) colnames(df2)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  
  id1<-dataList$VarName[dataList$NickName=="cluster_id"]
  k1<-match(id1, colnames(df), nomatch = 0)
  id2<-dataList$VarName[dataList$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df), nomatch = 0)
  if(k1>0 & k2>0) colnames(df)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  #print(colnames(df))
  
  df<-merge(df, df2, by=c("cluster_id", "HouseholdNumber"), all.x=TRUE)   
  
  return(df)
  
}



add_childrenU5 <- function(df, meta_data, data_folder, country_code, version_code, 
                           dataList) {
  
  dataList2<-meta_data[meta_data$DataSet=="hh", ]
  df2<-importMICSdata(data_folder, country_code, version_code, "hh", unique(dataList2$VarName))  
  colnames(df2)<-toupper(colnames(df2))
  
  id1<-dataList2$VarName[dataList2$NickName=="cluster_id"]
  k1<-match(id1, colnames(df2))
  id2<-dataList2$VarName[dataList2$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df2))
  id3<-dataList2$VarName[dataList2$NickName=="NUnder5"]
  k3<-match(id3, colnames(df2))

  colnames(df2)[c(k1, k2, k3)]<-c("cluster_id", "HouseholdNumber", "NUnder5")
  df2<-df2[, c(k1, k2, k3)]
  
  id1<-dataList$VarName[dataList$NickName=="cluster_id"]
  k1<-match(id1, colnames(df))
  id2<-dataList$VarName[dataList$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df))
  colnames(df)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
  
  df<-merge(df, df2, by=c("cluster_id", "HouseholdNumber"), all.x=TRUE)   
  
  return(df)
  
}



add_birthhistory <- function(df, dataList) {
  
  df2<-df
  colnames(df2)<-toupper(colnames(df2))
  id1<-dataList$VarName[dataList$NickName=="cluster_id"]
  k1<-match(id1, colnames(df2))
  id2<-dataList$VarName[dataList$NickName=="HouseholdNumber"]
  k2<-match(id2, colnames(df2))
  id3<-dataList$VarName[dataList$NickName=="MotherLineNumber"]
  k3<-match(id3, colnames(df2))
  id4<-dataList$VarName[dataList$NickName=="Age"]
  k4<-match(id4, colnames(df2))
  colnames(df2)[c(k1, k2, k3, k4)]<-c("cluster_id", "HouseholdNumber", "RLNumber", "kidAge")
  df2<-df2[, c(k1, k2, k3, k4)]
  
  df1<-df
  id3<-dataList$VarName[dataList$NickName=="RLNumber"]
  k3<-match(id3, colnames(df))
  colnames(df1)[c(k1, k2, k3, k4)]<-c("cluster_id", "HouseholdNumber", "RLNumber", "Age")
  df1<-df1[, c(k1, k2, k3, k4)]
  
  
  df2<-merge(df1, df2, by=c("cluster_id", "HouseholdNumber", "RLNumber"), all.x=TRUE) 

  
  df2$kidAge<-as.numeric(df2$kidAge)
  df2$Age<-as.numeric(df2$Age)
  
  df2<-df2[!is.na(df2$kidAge), ]
  df2<-df2[df2$Age-df2$kidAge<=19 & df2$Age-df2$kidAge>=15, ]   ## kids born when mother is 15-19
  df2$ct<-1
  kids<-aggregate(df2$ct, by=list(df2$cluster_id, df2$HouseholdNumber, df2$RLNumber), FUN=sum)
  colnames(kids)<-c("cluster_id", "HouseholdNumber", "RLNumber", "numberBirths")
  colnames(df)[c(k1, k2, k3)]<-c("cluster_id", "HouseholdNumber", "RLNumber")
  df<-merge(df, kids, by=c("cluster_id", "HouseholdNumber", "RLNumber"), all.x=TRUE) 
  
  return(df)
  
}


merge_mr <- function(mr_ds, meta_data, datause, dataList, country_code, version_code, data_folder, rv, indvar, svnm, educationList,religion_data, region_flag) {
  
  mrdataList<-meta_data[meta_data$DataSet==mr_ds, ]

  regionVar<-mrdataList$VarName[mrdataList$NickName=="Region"]
  provinceVar<-mrdataList$VarName[mrdataList$NickName=="Province"]

  mrdf<-importMICSdata(data_folder, country_code, version_code, mr_ds, unique(mrdataList$VarName), regionVar, provinceVar)
  colnames(mrdf)<-toupper(colnames(mrdf))
  
  
    swV<-mrdataList$VarName[mrdataList$NickName=="SampleWeight"]
    swK<-match(swV, colnames(mrdf))
    if(length(swK)==0 | nrow(is.na(mrdf[, swK]))==nrow(mrdf))
      mrdf$SampleWeight<-1
    else mrdf$SampleWeight<-as.numeric(as.character(mrdf[, swK]))/1000000
    
    
    mrdf$SampleWeight[is.na(mrdf$SampleWeight)] <- 0
    
    if(!is.null(mrdf)) {
      is<-intersect(indvar, c("Religion", "Ethnicity", "Language"))
      if(length(is)>0) {
        regList<-unique(religion_data$NickName)
        #### first, if religion information exists in hh dataset, as indicated in the relidionMICS.csv, not in other datasets, we have to add it
        religionVar<-dataList$VarName[dataList$NickName=="Religion"]
        k<-match(religionVar, colnames(mrdf), nomatch = 0)
        k1<-length(intersect(c("Religion"), regList))
        if(k==0 & k1>0){
          mrdf<-add_reglist(country_code, version_code, mrdf, meta_data, dataList, c("Religion"), religion_data)
        }
        
        #### second, if Ethnicity exists in hh dataset, not in other datasets, we need to add one of them, 
        ethnicityVar<-dataList$VarName[dataList$NickName=="Ethnicity"]
        k<-match(ethnicityVar, colnames(mrdf), nomatch = 0)
        k1<-length(intersect(c("Ethnicity"), regList))
        if(k==0 & k1>0){
          mrdf<-add_reglist(country_code, version_code, mrdf, meta_data, dataList, c("Ethnicity"), religion_data)
        }
        
        #### Third, when ethnicity is not available, but Language exist in hh dataset, not in other datasets, we need to add one of them, 
        languageVar<-dataList$VarName[dataList$NickName=="Language"]
        k2<-match(languageVar, colnames(mrdf), nomatch = 0)
        k3<-length(intersect(c("Language"), regList))
        if(k==0 & k1==0 & k2==0 & k3>0){
          mrdf<-add_reglist(country_code, version_code, mrdf, meta_data, dataList, c("Language"), religion_data)
        }
      }
      
      mrdatause<-get_data(mrdf, rv, mrdataList, indvar, svnm, educationList,religion_data)
    }
    print(colnames(mrdf))
    print("mr data size:")

    
    if(!is.null(mrdatause)) {
      
      id1<-mrdataList$VarName[mrdataList$NickName=="cluster_id"]
      k1<-match(id1, colnames(mrdatause), nomatch = 0)
      id2<-mrdataList$VarName[mrdataList$NickName=="HouseholdNumber"]
      k2<-match(id2, colnames(mrdatause), nomatch = 0)

      if(k1>0 & k2>0) colnames(mrdatause)[c(k1, k2)]<-c("cluster_id", "HouseholdNumber")
      
      commonVar<-c(intersect(c("SampleWeight", indvar,  "var2tab"), colnames(mrdatause)), "Sex", "cluster_id", "HouseholdNumber") 
      if(region_flag) commonVar<-c(commonVar, "REGION")
      
      datause$Sex<-1
      mrdatause$Sex<-2
      datause<-datause[, commonVar]
      mrdatause<-mrdatause[, commonVar]
      datause<-rbind(datause, mrdatause)
      datause$Sex<-factor(datause$Sex)
      levels(datause$Sex)<-c("Female", "Male")
    }
  
  
  return(datause) 
}



write_value<-function(datause, country_code, version_code, rv,  ds, ds_output_folder){
  
  surveyID<-paste(country_ISO(country_code), version_code, sep="")
  SurveyIndicator<-paste(surveyID, rv, sep="+")
  if(is.null(datause)) overallMean<- "DataNotGenerated" 
  else overallMean<-sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)
  
  results<-data.frame(surveyID=surveyID, country_code=country_code, version_code=version_code, dataset=ds, SurveyIndicator=SurveyIndicator, IndicatorName=rv, MeanY=overallMean)
  writefile<-paste(ds_output_folder, "overallmean.csv", sep="")
  if(file.exists(writefile)) 
    catch_error(write.table(results, writefile,
                            sep=",", append = TRUE,   col.names = F, row.names = F))
  else catch_error(write.table(results, writefile,
                               sep=",", append = F,   col.names = T, row.names = F))
}


write_tree <- function(datause, country_code, version_code, 
                       title_string, formula_string, sub_string, 
                       rv, rtp, religion, output_folder, ds, filename) {
  
  pass_message <- "Successfully built Tree"

  tree_stat<- catch_error(build_tree(output_folder, country_code, version_code, datause, rv, rtp, 
                        formula_string, title_string, sub_string, filename, e = religion))

    
  
  tree_stat<-t(c(country_code, version_code, title_string, tree_stat))

  tree_stat$Source<-"MICS"
  if (!is.null(tree_stat)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  resave(tree_stat, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output  
  pass_message <- "Successfully wrote Tree.csv"
  catch_error(write.table(tree_stat, file=paste(output_folder, "Tree.csv", sep=""),
              sep=",", append = TRUE,   col.names = F, row.names = F))
  
}

write_HOI_D <- function(datause, country_code, version_code, title_string,
                        indvar, output_folder, filename) {
  
  # Calculate 
  pass_message <- "Successfully calculated HOI and D"
  
  # catch_error does not seem to work
  # result<-catch_error(cal_HOI_shapley(datause, indvar))

  result<-cal_HOI_shapley(datause, indvar)
  result<-c(country_code, version_code, title_string, result)
  
  if (!is.null(result)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  resave(result, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output 
  pass_message <- "Successfully wrote D.csv"
  catch_error(write.table(t(result), file=paste(output_folder, "D.csv", sep=""),
                          sep=",", append = TRUE,   col.names = F, row.names = F)) 
  
}

write_glm <- function(datause, rtp, country_code, version_code, title_string, 
                      indvar, output_folder, filename) {
  
  # Build Logistic Regression model 
  pass_message <- "Successfully built glm"
  # catch_error does not seem to workk here 
  
  for(ivm in indvar) {
    
    if(!(ivm=="NUnder5")) datause[ , ivm]<-factor(datause[ , ivm], ordered = F)
  }
  
  
  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  
  s.glm <- catch_error_prod(logistic(datause, rtp, formula_string, filename))

  if (!is.null(s.glm)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  resave(s.glm, file = paste("md",filename,".Rdata", sep=""))
  
  file_write<-paste(output_folder, "MICSLogit.csv", sep="")
  
  # Write to output 
  write.table(t(c(country_code, version_code, title_string)) , file_write,
              sep=",", append = TRUE,   col.names = F, row.names = F)
  pass_message <- "Successfully wrote MICSLogit.csv"
  
  # catch_error does not seem to work here
  # catch_error(write.table(s.glm,  file_write,
  #                         sep=",", append = TRUE,   col.names = T, row.names = T, na="")) 
  
  write.table(s.glm,  file_write,
                          sep=",", append = TRUE,   col.names = T, row.names = T, na="")

  }


write_crosstab <- function(datause, country_code, version_code, title_string,
                           indvar, output_folder, filename) {
  
  if(!is.na(match("Language", indvar))) indvar<-indvar[!indvar=="Language"]
  formula_string<-paste("SampleWeight", paste(indvar, collapse=" + "), sep=" ~ ")
  idv_sum<-aggregate(as.formula(formula_string), data=datause, sum)

  datause$SampleWeight2<-datause$SampleWeight*datause$var2tab
  formula_string<-paste("SampleWeight2", paste(indvar, collapse=" + "), sep=" ~ ")
  idv_sum2<-aggregate(as.formula(formula_string), data=datause, sum)
  idv_sum$pct<-idv_sum2$SampleWeight2/idv_sum$SampleWeight*100


  file_write<-paste(output_folder, "CrossTab.csv", sep="")

  # Write to output
  write.table(t(c(country_code, version_code, title_string)) , file_write,
              sep=",", append = TRUE,   col.names = F, row.names = F)
  pass_message <- "Successfully wrote MICSLogit.csv"

  # catch_error does not seem to work here
  # catch_error(write.table(s.glm,  file_write,
  #                         sep=",", append = TRUE,   col.names = T, row.names = T, na=""))

  write.table(idv_sum,  file_write,
              sep=",", append = TRUE,   col.names = T, row.names = T, na="")
  
}

country_ISO<-function(country_code){
  
  if(country_code=="Bangladesh") iso<-"BGD"
  else if(country_code=="Afghanistan") iso<-"AFG"
  else if(country_code=="Bhutan") iso<-"BTN"
  else if(country_code=="Georgia") iso<-"GEO"
  else if(country_code=="Kazakhstan") iso<-"KAZ"
  else if(country_code=="Kyrgyzstan") iso<-"KGZ"
  else if(country_code=="Kiribati") iso<-"KIR"
  else if(country_code=="Lao") iso<-"LAO"
  else if(country_code=="Mongolia") iso<-"MNG"
  else if(country_code=="Nepal") iso<-"NPL"
  else if(country_code=="Thailand") iso<-"THA"
  else if(country_code=="Tonga") iso<-"TON"
  else if(country_code %in% c("VietNam", "Vietnam")) iso<-"VNM"
  else if(country_code=="Turkmenistan") iso<-"TKM"
  else iso<-country_code  
  return(iso)
  
}




# Region 
region_prod <- function(output_folder, country_code, version_code, 
                   datause, rv,
                   formula_string, title_string, sub_string,
                   filename, indvar) {
  
  region_list <- unique(datause$RegionName)
  
  #filename = REBhutanHH2017
  region_filename <- paste("RE",filename, sep = "")
  region_tree_stat <- c()
  region_result <- c()
  
  for (region in region_list) {
    
    message <- paste("REGION: ", region, "  ------------------")
    print(message)
    info(logger, message)
    message <- paste("Current Region: ", match(region, region_list), " of ", length(region_list))
    print(message)
    info(logger, message)
    
    datause_region <- filter(datause, RegionName == region)
    
    #### Construct Decision Tree
    sub_string<-NULL
    religion<-FALSE
    
    region_folder <- paste(output_folder, "Region", "/", sep = "")
    ifelse(!dir.exists(region_folder), dir.create(region_folder), FALSE)
    
    region_output_folder <- paste(region_folder, region, "/", sep = "")
    ifelse(!dir.exists(region_output_folder), dir.create(region_output_folder), FALSE)
    
    pass_message <- "Successfully built Tree"
    tree_stat<- catch_error(build_tree(region_output_folder, country_code, version_code, 
                                       datause_region, rv,
                                       formula_string, title_string, sub_string, 
                                       e=religion, filename, region))
    
    tree_stat<-t(c(country_code, version_code, title_string, tree_stat))
    
    # Add region name to object 
    tree_stat <- c(region, tree_stat)
    
    if (!is.null(tree_stat)) { 
      
      info(logger, paste(pass_message))
      
    }
    
    # Append data for .Rdata
    region_tree_stat <- rbind(region_tree_stat, tree_stat)
    
    #### Construct HOI
    # Calculate 
    pass_message <- "Successfully calculated HOI and D"

    result<-catch_error(cal_HOI_shapley(datause_region, indvar))

    result<-c(country_code, version_code, title_string, result)
    
    if (!is.null(result)) { 
      
      info(logger, paste(pass_message))
      
    }

    region_result <- rbind(region_result, result)
    
    #### Construct Logistic Regression
    # Null for now: add if required 
    
  }
  
  save(region_tree_stat, file = paste(region_filename, ".Rdata", sep=""))
  resave(region_result, file = paste(region_filename, ".Rdata", sep=""))
  
}




region_dev<-function(datause, country_code, version_code, 
                     title_string, formula_string, sub_string, rv, rtp, religion, ds_output_folder, ds, filename){
  
  
  write_tree(datause, country_code, version_code, 
             title_string, formula_string, sub_string, rv, rtp, religion, ds_output_folder, ds, filename) 
  
  
  #### HOI and dis-similarity index calculation
  #### not sure if this works for numeric
  write_HOI_D(datause, country_code, version_code, title_string, indvar, ds_output_folder, filename)
  
  #### logistic regression
  #### have to use lm for numeric here
  write_glm(datause, rtp, country_code, version_code, title_string, formula_string, ds_output_folder, filename)
  
}
# Function to catch errors  
catch_error_prod <- function(code) {
  out <- tryCatch(code,
                  error = function(c) {
                    error(logger, paste(c$message))
                  },
                  warning = function(c) { 
                    warn(logger, paste(c$message))
                  }
  )
  return(out)
}

catch_error_dev <- function(code) {
  return(code)
}

ds_output_dev <- function(output_folder, ds){
  ds_output_folder<-paste(output_folder, ds, "/",sep = "")
  ifelse(!dir.exists(ds_output_folder), dir.create(ds_output_folder), FALSE)
  return(ds_output_folder)
}

rv_Rdata_dev<-function(mics_Rdata_folder, rv){
  rv_Rdata_folder <- paste(mics_Rdata_folder, rv, sep = "/")
  ifelse(!dir.exists(rv_Rdata_folder), dir.create(rv_Rdata_folder), FALSE)
  setwd(rv_Rdata_folder)
  return(rv_Rdata_folder)
}

