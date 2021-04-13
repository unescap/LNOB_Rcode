

get_data<-function(df, rv, dataList, indvar, svnm, eth = NULL){
  
  VarName<- dataList$VarName[dataList$NickName==rv & dataList$IndicatorType %in% c("ResponseV", "MresponseV", "PresponseV")]
  k<-match(VarName, colnames(df), nomatch = 0)
  l<-length(k)
  
  if(sum(k)==0 ){
    print(paste("Response variable -- ", rv, "(",  VarName, ") not found"))
    return(NULL)
  }

  if(!rv=="AdolescentBirthRate") {
  for(i in c(1:l))
    if(is.na(k[i])) {
      print(paste("Response variable -- ", rv, "(",  VarName[i], ") not found"))
      return(NULL)
    }
    df[, k[i]]<-as.numeric(as.character(df[,k[i]]))

  
   if(length(df[!is.na(df[, k[i]]), k[i]])<=1) {
      print(paste("Response variable -- ", rv, "(",  VarName, ") has not valid observations"))
     return(NULL)
    }
  }
  ###### Programmer Note1:
  ###### if you want to add a new response variable, 
  ###### you have to add it here
  
  if (rv == "MobilePhone" ) datause<-MobilePhone(df, k)
  else if (rv == "MobilePhoneHH" ) datause<-MobilePhone(df, k)
  else if(rv=="AccessElectricity") datause<-AccessElectricity(df, dataList, k)
  else if(rv=="CleanFuel") datause <- CleanFuel(df, k)
  else if (rv=="BankCardHH") datause <- BankCardHH(df, k)
  else if (rv=="CleanWater") datause<-CleanWater(df, dataList, k, svnm)
  else if (rv=="BasicWater") datause<-BasicWater(df, dataList, k, svnm)
  else if (rv=="SafeSanitation") datause<- SafeSanitation(df, dataList, k, svnm)
  else if (rv=="HouseholdBasic") datause<- HouseholdBasic(df, dataList, svnm)
  else if (rv=="HouseholdTechNeed") datause<- HouseholdTechNeed(df, dataList)
  else if (rv=="Land") datause<- Land(df, dataList, k)
  else if (rv=="MultiDeprivation") datause<- MultiDeprivation(df, dataList, k, svnm) 
  else if (rv=="SecondaryEducation2035") datause<- SecondaryEducation2035(df, dataList, k)
  else if (rv=="SecondaryEducation35plus") datause<- SecondaryEducation35plus(df, dataList, k)
  else if (rv=="HigherEducation2535")    datause<- HigherEducation2535(df, dataList, k)
  else if (rv=="HigherEducation35plus") datause<- HigherEducation35plus(df, dataList, k)
  else if (rv=="NotStunting") datause<- NotStunting(df, dataList, k)
  else if (rv=="NotOverweight") datause<- NotOverweight(df, dataList, k)
  else if (rv=="NotWasting") datause<- NotWasting(df, dataList, k)
  else if (rv=="ChildHealth") datause<- ChildHealth(df, dataList, k)
  else if (rv=="Stunting") datause<- Stunting(df, dataList, k)
  else if (rv=="Overweight") datause<-  Overweight(df, dataList, k)
  else if (rv== "Wasting") datause<- Wasting(df, dataList, k)
  else if (rv== "ContraceptiveMethod") datause<- ContraceptiveMethod(df, dataList, k)
  else if (rv=="ProfessionalHelp") datause<- ProfessionalHelp(df, dataList, svnm)  ### no need for k for this one variable
  else if (rv=="NoSexualViolence") datause<-NoSexualViolence(df, dataList, k)
  else if (rv=="HealthInsurance") datause<- HealthInsurance(df, dataList, k, mrDatalist)
  else if (rv=="InternetUse") datause<- InternetUse(df, dataList, k)
  else if (rv=="ChildMarriage15") datause<- ChildMarriage15(df, dataList, k)
  else if (rv=="ChildMarriage18") datause<- ChildMarriage18(df, dataList, k)
  else if (rv=="TeenagePregnancy") datause<- TeenagePregnancy(df, dataList, k)
  else if (rv=="AllViolence") datause<- AllViolence(df, dataList, k)
  else if (rv=="SexualPhysicalViolence") datause<- SexualPhysicalViolence(df, dataList, k)
  else if (rv=="PhysicalViolence") datause<- PhysicalViolence(df, dataList, k)
  else if (rv=="SexualViolence") datause<- SexualViolence(df, dataList, k)
  else if (rv=="EmotionalViolence") datause<- EmotionalViolence(df, dataList, k)
  else if (rv=="MobileFinance") datause<- MobileFinance(df, dataList, k)
  else if (rv=="BankAccount") datause<- BankAccount(df, dataList, k)
  else if (rv=="HandWash") datause<- HandWash(df, dataList, k)
  else if (rv=="NotCrowded") datause<- NotCrowded(df, dataList, k)
  else if (rv=="AdolescentBirthRate") datause<- AdolescentBirthRate(df, dataList, k)
  else if (rv=="FinancialInclusion") datause<- FinancialInclusion(df, dataList, k)
  else if (rv=="Covid1") datause<- Covid1(df, dataList, k, svnm)  
  else if (rv=="Covid2") datause<- Covid2(df, dataList, k, svnm)   
  else if (rv=="Covid") datause<- Covid(df, dataList, k, svnm)    # to be defined
  else if (rv=="HandWashPR") datause<- HandWash(df, dataList, k)
  else if (rv=="SafeSanitationPR") datause<- SafeSanitation(df, dataList, k, svnm)
  else if (rv=="WaterOnsitePR") datause<-WaterOnsitePR(df, dataList, k, svnm)   # to be defined
  else if (rv=="LearningPR") datause<-Learning(df, dataList)   
  else if (rv=="NotCrowdedPR") datause<-NotCrowded(df, dataList, k)   
  
  
  ###### End of Programmer Note1
  
  # print(sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)*100)
  # print(mean(datause$var2tab)*100)
  
  if(is.null(datause)) {
    print("No data available")
    return(NULL)
  }
  
    for(iv in indvar){
      VarName<- dataList$VarName[dataList$NickName==iv & dataList$IndicatorType=="IndependentV"]
      k<-match(VarName, colnames(datause))
      
      if(length(k)==0){
        print(paste("Independent variable -- ", iv, "(",  VarName, ") not found"))
        return(NULL)
      }
      
      # print(c(iv, VarName, k))
      # print(colnames(datause))
      if(!(iv=="NUnder5")) datause[, k]<-as.numeric(as.character(datause[,k]))
       
      if(iv=="HighestEducation") datause<-HighestEducation(datause, dataList, k)
      else if(iv=="MotherEducation") datause<-MotherEducation(datause, dataList, k)
      else if(iv=="PoorerHousehold") datause<-PoorerHousehold(datause, dataList, k)
      else if(iv=="aGroup") datause<-aGroup(datause, dataList, k)
      else if(iv=="aGroupPR") datause<-aGroupPR(datause, dataList, k)
      else if(iv=="Under5") datause<-Under5(datause, dataList, k)
      else if(iv=="NUnder5") datause<-NUnder5(datause, dataList, k)
      else if(iv=="Residence") datause<-Residence(datause, dataList, k)
      else if(iv=="Sex") datause<-Sex(datause, dataList, k)
      else if(iv=="HHSex") datause<-HHSex(datause, dataList, k)
      else if(iv=="MarriageStatus") datause<-MarriageStatus(datause, dataList, k)
      else if(iv=="Education") datause<-Education(datause, dataList, k)
      else if(iv=="EducationPR") datause<-EducationPR(datause, dataList, k)  # to be defined
      else if (iv=="Caste") datause<- Caste(datause, dataList, k, eth)  ### no need for k for this one variable
      else if (iv=="Religion") datause<- Religion(datause, dataList, k, eth)  ### no need for k for this one variable
      else if (iv=="HusbandAge") datause<-HusbandAge(datause, dataList, k)
      else if (iv=="HusbandEducation") datause<-HusbandEducation(datause, dataList, k)
      else if (iv=="HusbandOccupation") datause<-HusbandOccupation(datause, dataList, k)
      else if (iv=="HusbandAlcohol") datause<-HusbandAlcohol(datause, dataList, k)
      else if (iv=="FatherBeatMother") datause<-FatherBeatMother(datause, dataList, k)
      else if (iv=="AgeDifference") datause<-AgeDifference(datause, dataList, k)
      else if (iv=="EducationDifference") datause<-EducationDifference(datause, dataList, k)
      else {
        #### if no need for recoding
        colnames(datause)[k]<-iv      
      }
      
    }
    return(datause)
    
    
  # if(length(datause$var2tab[datause$var2tab>0])>0 & length(datause$var2tab[datause$var2tab==0])>0) {
  # }
  # else {
  #   print(paste(rv, " has only one level", sep=""))
  #   return(NULL)
  # }
}


indList<-function(rv, caste = FALSE ){
  if(rv %in% c("CleanWater", "HouseholdBasic", "SafeSanitation", "AccessElectricity",
               "CleanFuel", "HouseholdTechNeed", "MobilePhoneHH", "BankCardHH", "MultiDeprivation", "HandWash", "NotCrowded", "BasicWater"))
    iv<-c("PoorerHousehold", "Residence", "HighestEducation")
  else if(rv %in% c("ChildHealth", "NotStunting", "Stunting", "NotOverweight", 
                    "NotWasting", "Overweight", "Wasting"))
    iv<-c("PoorerHousehold", "Residence", "MotherEducation", "Under5", "Sex")
  else if (rv %in% c("HigherEducation2535", "HigherEducation35plus",
                     "SecondaryEducation2035", "SecondaryEducation35plus"))
    iv<-c("PoorerHousehold", "Residence", "Sex") 
  else if (rv=="Land") 
    iv<-c("PoorerHousehold", "HHSex", "HighestEducation") 
  else if(rv=="ProfessionalHelp") 
    iv<-c("PoorerHousehold", "Residence", "aGroup", "MarriageStatus", "NUnder5", "Education")
  else if (rv %in% c("ContraceptiveMethod")) 
    iv<-c("PoorerHousehold", "Residence", "aGroup", "NUnder5", "Education")
  else if (rv %in% c("NoSexualViolence",  "AllViolence", "SexualPhysicalViolence", "PhysicalViolence", "SexualViolence", "EmotionalViolence"))
    iv<-c("PoorerHousehold", "Residence", "aGroup", "NUnder5", "Education", "HusbandAge", "HusbandEducation", "HusbandAlcohol", "FatherBeatMother", 
          "AgeDifference", "EducationDifference")
  
  else if(rv %in% c("HealthInsurance", "InternetUse", "MobileFinance", "FinancialInclusion", "BankAccount", "MobilePhone") )
    ### some survey only has married women or men in the IR or MR dataset, in that case we need to exclude Marriage Status from the model
    iv<-c("PoorerHousehold", "Residence", "aGroup",  "Education") 
  
  else if(rv %in% c("ChildMarriage15", "ChildMarriage18", "AdolescentBirthRate", "TeenagePregnancy") )
    iv<-c("PoorerHousehold", "Residence",  "Education") 
  else if(rv %in% c("Covid1", "Covid2", "Covid", "LearningPR", "WaterOnsitePR", "SafeSanitationPR", "HandWashPR", "NotCrowdedPR"))
    iv<-c("PoorerHousehold", "Residence", "aGroupPR", "EducationPR", "Sex")
  #iv<-c(iv, "Region")
  
  if (caste==TRUE)
    return(c(iv, "Caste", "Religion"))
  else return(iv)
  
  
}


####### Programmer Note2: define the function for the response variable 
###############cacluation of response variable: creat var2tab (1 or 0)


NoSexualViolence<-function(datause, dataList, k){
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]

  varw<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  wk<-match(varw, colnames(datause))  
  if(length(wk)==0){
    datause<-datause[!is.na(datause[, wk]), ]
    datause$SampleWeight<-datause[, wk]/1000000
  }
  datause$var2tab<-0
  datause$var2tab[datause[,k]==0]<-1
  return(datause)
}

MobilePhoneHH<-function(datause, k){
  {
    datause$var2tab<- 0
    datause$var2tab[datause[, k] == 1]<-1
    return(datause)
  }
}


MobilePhone<-function(datause, k){
  {
    datause$var2tab<- 0
    datause$var2tab[datause[, k] == 1]<-1
    return(datause)
  }
}


AccessElectricity<-function(datause, dataList, k){
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
  solV<-dataList$VarName[dataList$NickName == "SolarElectricity"]
  solk<-match(solV, colnames(datause))
  if(length(solk)>0) {
    if(!is.na(solk)) datause$var2tab[datause[,solk] == 1]<-1
  }
  return(datause)
}


CleanFuel<-function(datause, k){
  #### recoding?
  datause[,k]<-as.numeric(as.character(datause[, k]))
  fuel_code<-cookfuel_code()
  datause$var2tab<- 0
  datause$var2tab[datause[, k] %in% fuel_code]<-1
  return(datause)
}


BankCardHH<-function(datause, k){
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1
  return(datause)
}

CleanWater<-function(datause, dataList, k, svnm){
  datause$var2tab<-1
  iws_code<-water_code(svnm)
  datause$var2tab[datause[, k] %in% iws_code]<- 0
  return(datause)
}

WaterOnsitePR<-function(datause, dataList, k, svnm){
  wtV<-dataList$VarName[dataList$NickName=="WaterTime"]
  wtk<-match(wtV, colnames(datause))
  if(length(wtk)==0) {
    print("No water collection time")
    return(NULL)
  }
  datause[,wtk]<-as.numeric(as.character(datause[,wtk]))
  onpremise<-(datause[,wtk]==996)
  datause[onpremise,wtk]<-0
  more_1<- datause[,wtk]>=1
  datause$var2tab<-1
  iws_code<-water_code(svnm)
  datause$var2tab[datause[, k] %in% iws_code]<- 0
  datause$var2tab[more_1]<- 0
  return(datause)
}

BasicWater<-function(datause, dataList, k, svnm){
  wtV<-dataList$VarName[dataList$NickName=="WaterTime"]
  wtk<-match(wtV, colnames(datause))
  if(length(wtk)==0) {
    print("No water collection time")
    return(NULL)
  }
  datause[,wtk]<-as.numeric(as.character(datause[,wtk]))
  onpremise<-(datause[,wtk]==996)
  datause[onpremise,wtk]<-0
  more_30<- datause[,wtk]>=30
  datause$var2tab<-1
  iws_code<-water_code(svnm)
  datause$var2tab[datause[, k] %in% iws_code]<- 0
  datause$var2tab[more_30]<- 0
  return(datause)
}


SafeSanitation<-function(datause, dataList, k, svnm){
  datause$var2tab<-1
  isf_code<-sanitation_code(svnm)
  datause$var2tab[datause[, k] %in% isf_code]<- 0
  ssV<-dataList$VarName[dataList$NickName=="SharedToilet"]
  ssk<-match(ssV, colnames(datause))
  datause[,ssk]<-as.numeric(as.character(datause[,ssk]))
  if(!(length(ssk)==0)) { #print("============shared toilet==============")
    datause$var2tab[datause[,ssk]== 1]<-0
  }
  return(datause)
}


HouseholdBasic<-function(datause, dataList, svnm){
  print("household basic")
  print(svnm)
  waterK<-match(dataList$VarName[dataList$NickName=="CleanWater"], colnames(datause))
  if(is.na(waterK)){
    print("No water variable for housebasic")
    return(NULL)
  }
  datause<- CleanWater(datause, dataList, waterK, svnm)
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
  datause<- MobilePhoneHH(datause, mobileK)
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
  if(!(length(resK)==0)) datause<-datause[datause[, resK]== 2, ]
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 1]<-1
  return(datause)
  
}


MultiDeprivation<-function(datause, dataList, k, svnm){
  return(NULL)     #### no definition yet
}


SecondaryEducation2035<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for SecondaryEducation2035 can't be found")
    return(NULL)
  }
  
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[datause$Age>=20 & datause$Age<=35, ]
  datause$var2tab<-1
  datause$var2tab[is.na(datause[, k])]<-0
  datause$var2tab[datause[, k] %in% c(8, 0, 1, 2, 3)]<-0
  
  
  return(datause)
}


SecondaryEducation35plus<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for SecondaryEducation2035 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=35, ]
  
  datause$var2tab<-1
  datause$var2tab[is.na(datause[, k])]<-0
  datause$var2tab[datause[, k] %in% c(8, 0, 1, 2, 3)]<-0
  
  return(datause)
}


HigherEducation2535<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for HigherEducation2535 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=25 & datause$Age<=35 , ]
  datause$var2tab<-1
  datause$var2tab[is.na(datause[, k])]<-0
  datause$var2tab[datause[, k] %in% c(8, 0, 1, 2, 3, 4)]<-0
  return(datause)
}


HigherEducation35plus<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for HigherEducation35plus can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >35, ]
  
  datause$var2tab<-1
  datause$var2tab[is.na(datause[, k])]<-0
  datause$var2tab[datause[, k] %in% c(8, 0, 1, 2, 3, 4)]<-0
  return(datause)
}


NotStunting<-function(datause, dataList, k){
  datause<-Stunting(datause, dataList, k)
  if(!is.null(datause)) datause$var2tab<- 1- datause$var2tab
  return(datause)
}


NotOverweight<-function(datause, dataList, k){
  datause<-Overweight(datause, dataList, k)
  if(!is.null(datause)) datause$var2tab<- 1- datause$var2tab
  return(datause)
}


NotWasting<-function(datause, dataList, k){
  datause<-Wasting(datause, dataList, k)
  if(!is.null(datause)) datause$var2tab<- 1- datause$var2tab
  return(datause)
}


ChildHealth<-function(datause, dataList, k){
  return(NULL)
}


Stunting<-function(datause, dataList, k){
  chV<-dataList$VarName[dataList$NickName=="ChildHealth"]
  chK<-match(chV, colnames(datause))
  if(length(chK)==0 | sum(is.na(datause[,chK]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0 | sum(is.na(datause[,ageK]))==nrow(datause) ) {
    print("Children Age variable can't be found")
    return(NULL)
  }
  datause[, ageK]<-as.numeric(as.character(datause[, ageK]))
  datause<-datause[!is.na(datause[, ageK]), ]
  datause<-datause[datause[, ageK]<=5, ]
  
  
  datause$malnutrition<-as.numeric(as.character(datause[, k]))
  
  #### children measured with numeric measurement
  datause<-datause[datause[, chK] == 0 & !is.na(datause[,chK]) 
                   & !is.na(datause$malnutrition) & datause$malnutrition>=-600 & datause$malnutrition<=600, ]
  
  datause$var2tab<-0
  datause$var2tab[datause$malnutrition<= -200]<-1
  return(datause)
}


Overweight<-function(datause, dataList, k){
  chV<-dataList$VarName[dataList$NickName=="ChildHealth"]
  chK<-match(chV, colnames(datause))
  if(length(chK)==0 | sum(is.na(datause[,chK]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0 | sum(is.na(datause[,ageK]))==nrow(datause) ) {
    print("Children Age variable can't be found")
    return(NULL)
  }
  datause[, ageK]<-as.numeric(as.character(datause[, ageK]))
  datause<-datause[!is.na(datause[, ageK]), ]
  datause<-datause[datause[, ageK]<=5, ]
  
  
  datause$malnutrition<-as.numeric(as.character(datause[, k]))
  #### children measured with numeric measurement
  datause<-datause[datause[, chK] == 0 & !is.na(datause[, chK]) 
                   & !is.na(datause$malnutrition) & datause$malnutrition>=-600 & datause$malnutrition<=600, ]
  
  datause$var2tab<-0
  datause$var2tab[datause$malnutrition>= 200]<-1
  return(datause)
}


Wasting<-function(datause, dataList, k){
  chV<-dataList$VarName[dataList$NickName=="ChildHealth"]
  chK<-match(chV, colnames(datause))
  if(length(chK)==0 | sum(is.na(datause[,chK]))==nrow(datause) ) {
    print("Children measured variable can't be found")
    return(NULL)
  }
  
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0 | sum(is.na(datause[,ageK]))==nrow(datause) ) {
    print("Children Age variable can't be found")
    return(NULL)
  }
  datause[, ageK]<-as.numeric(as.character(datause[, ageK]))
  datause<-datause[!is.na(datause[, ageK]), ]
  datause<-datause[datause[, ageK]<=5, ]
  
  datause$malnutrition<-as.numeric(as.character(datause[, k]))
  #### children measured with numeric measurement
  datause<-datause[datause[, chK] == 0 & !is.na(datause[,chK]) 
                   & !is.na(datause$malnutrition) & datause$malnutrition>=-600 & datause$malnutrition<=600, ]
  
  datause$var2tab<-0
  datause$var2tab[datause$malnutrition<= -200]<-1
  return(datause)
  
}



######### IR response variables
ContraceptiveMethod<-function(datause, dataList, k){
  umnV<-dataList$VarName[dataList$NickName=="UnmetNeed"]
  umnK<-match(umnV, colnames(datause))
  if(length(umnK)==0) datause<-unmet_cal(datause)
  else { 
    colnames(datause)[umnK]<- "UnmetNeed"
    datause$UnmetNeed<-as.numeric(as.character(datause$UnmetNeed))
  }
  keep <- datause$UnmetNeed %in% c( 1, 2, 3, 4, 5, 6)  #suitable for both definitions
  datause<-datause[keep, ]
  
  datause$var2tab<- 0
  datause$var2tab[datause[, k] == 3]<- 1
  
  return(datause)
}


ProfessionalHelp<-function(datause, dataList, svnm){
  bhV<-dataList$VarName[dataList$NickName=="NUnder5"]
  bhK<-match(bhV, colnames(datause))
  if(length(bhK)==0)
  {
    print("No Birth History information")
    return(NULL)
  }
  
  datause$NUnder5<-nchar(trimws(as.character(datause[, bhK])))
  
  datause<-datause[!is.na(datause$NUnder5) & datause$NUnder5>0, ]
  
  pa1V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance1"]
  pa1K<-match(pa1V, colnames(datause), nomatch = 0)
  
  pa2V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance2"]
  pa2K<-match(pa2V, colnames(datause), nomatch = 0)
  
  pa3V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance3"]
  pa3K<-match(pa3V, colnames(datause), nomatch = 0)
  
  
  pa4V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance4"]
  pa4K<-match(pa4V, colnames(datause), nomatch = 0)
  
  pa5V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance5"]
  pa5K<-match(pa5V, colnames(datause), nomatch = 0)
  
  pa6V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance6"]
  pa6K<-match(pa6V, colnames(datause), nomatch = 0)
  
  pa7V<-dataList$VarName[dataList$NickName=="ProfessionalAssitance7"]
  pa7K<-match(pa7V, colnames(datause), nomatch = 0)
  
  if(pa1K==0 & pa2K==0 & pa3K==0 ){
    print("No delivery method information found")
    return(NULL)
  }
  
  datause$var2tab<- rep(0, nrow(datause))
  y<-"1"
  
  if(pa1K>0) t1<-grepl(y, as.character(datause[, pa1K]))
  if(pa2K>0) t2<-grepl(y, as.character(datause[, pa2K]))
  if(pa3K>0) t3<-grepl(y, as.character(datause[, pa3K]))
  if(pa4K>0) t4<-grepl(y, as.character(datause[, pa4K]))
  if(pa5K>0) t5<-grepl(y, as.character(datause[, pa5K]))
  if(pa6K>0) t6<-grepl(y, as.character(datause[, pa6K]))
  if(pa7K>0) t7<-grepl(y, as.character(datause[, pa7K]))
  
  if(svnm %in% c("BD61"))
    datause$var2tab[t1 | t2 | t3 | t4 | t5 | t6 | t7] <- 1
  else if(svnm %in% c("TJ70", "ID71", "ID63"))
        datause$var2tab[t1 | t2 | t3 | t4 | t5 | t6] <- 1
  else datause$var2tab[t1 | t2 | t3] <- 1
  return(datause)
}

HealthInsurance<-function(datause, dataList, k, mrDatalist){
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
  return(datause)
}
InternetUse<-function(datause, dataList, k){
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
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
  datause<-datause[datause$Age>=20 & datause$Age<=24, ]
  
  datause$var2tab<- 0
  datause$var2tab[datause[, k]<15]<-1
  
  return(datause)
  
}
ChildMarriage18<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for childmarriage 15 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[datause$Age>=20 & datause$Age<=24, ]
  datause$var2tab<- 0
  datause$var2tab[datause[, k]<18]<-1
  
  return(datause)
}
TeenagePregnancy<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(length(ageK)==0) {
    print("Age for childmarriage 15 can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[datause$Age>=15 & datause$Age<=19, ]
  
  datause$var2tab<-0
  datause$var2tab[!is.na(datause[, k]) & datause[, k]>0]<-1
  vk<-match("V213", colnames(datause))
  if(length(vk)>0){
    datause$var2tab[datause$V213==1]<-1
  }
  return(datause)
}

AdolescentBirthRate0<-function(datause, dataList, k) {
    ### for women age 20-24
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))


  if(is.na(ageK)) {
    print("Age for Adolescent Birth rate can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=15 & datause$Age <=19, ]
  #datause<-datause[!is.na(datause$Age) & datause$Age >=20 & datause$Age <=24, ]
  
  datause[, k]<-trimws(as.character(datause[, k]))
  
  n<-nrow(datause)
  datause$var2tab<-0
  for(i in c(1:n)){
    if(!is.na(datause[i, k])) {
        strn<-nchar(datause[i, k])
        nchild<-floor((strn+1)/2)

       for(j in c(1:nchild)){
         bg<-max(1, strn-2*j+1)
         ed<- strn-2*j+2
         kidAge<-as.numeric(substr(datause[i, k], bg, ed))
         # if(nchild>1) print(c(strn, nchild, bg, ed, datause[i, k], kidAge))
         if(datause$Age[i]-kidAge<=19 & datause$Age[i]-kidAge>=15)
          {
              datause$var2tab[i]<-datause$var2tab[i]+1000/(datause$Age[i]-14)  ###  /4*1000
        }
       }
    }
    
  }
  print(paste("average birthrate is ", sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)))
  return(datause)
}

AdolescentBirthRate<-function(datause, dataList, k) {
  ### for women age 20-24
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  
  bdV<-dataList$VarName[dataList$NickName=="BirthDate"]
  bdK<-match(bdV, colnames(datause))
  
  if(is.na(ageK)) {
    print("Age for Adolescent Birth rate can't be found")
    return(NULL)
  }

  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause<-datause[!is.na(datause$Age) & datause$Age >=20 & datause$Age <=24, ]
  #datause<-datause[!is.na(datause$Age) & datause$Age >=15 & datause$Age <=19, ]

  if(length(bdV)==0){
    datause$var2tab<-0
    return(datause)
  }
  
  datause$bd<-as.numeric(as.character(datause[,bdK]))
  
  datause[, k]<-trimws(as.character(datause[, k]))
  
  n<-nrow(datause)
  datause$var2tab<-0
  for(i in c(1:n)){
    if(!is.na(datause[i, k])) {
      strn<-nchar(datause[i, k])
      nchild<-floor((strn+3)/4)
      if(nchild>=1) {
        #datause$var2tab[i]<-datause$var2tab[i]+1000*nchild
        for(j in c(1:nchild)){
          bg<-max(1, strn-4*j+1)
          ed<- strn-4*j+4
          kidbd<-as.numeric(substr(datause[i, k], bg, ed))

        if((kidbd-datause$bd[i])/12 <=19 & (kidbd-datause$bd[i])/12 >=15)
        # if(datause$bd - kidbd <= 36)
        {
         # datause$var2tab[i]<-datause$var2tab[i]+1000/(datause$Age[i]-14)  ###  /4*1000
          datause$var2tab[i]<-datause$var2tab[i]+1000/4

        }
        }
      }
    }
    
  }
  
  # datause$var2tab[datause$Age==16]<-datause$var2tab[datause$Age==16]/2
  # datause$var2tab[datause$Age==17]<-datause$var2tab[datause$Age==17]/3
  # datause$var2tab[datause$Age==18]<-datause$var2tab[datause$Age==18]/4
  # datause$var2tab[datause$Age==19]<-datause$var2tab[datause$Age==19]/5
  print(paste("average birthrate is ", sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)))
  return(datause)
}




  AdolescentBirthRate0<-function(datause, dataList, k) {
    ### for women age 15-19
    ageV<-dataList$VarName[dataList$NickName=="Age"]
    ageK<-match(ageV, colnames(datause))



    if(is.na(ageK)) {
      print("Age for Adolescent Birth rate can't be found")
      return(NULL)
    }
    datause$Age<-as.numeric(as.character(datause[,ageK]))
    datause<-datause[!is.na(datause$Age) & datause$Age >=15 & datause$Age <=18, ]

    datause[, k]<-as.numeric(as.character(datause[, k]))

    datause[is.na(datause[, k]), k]<-0

    print(table(datause[, k]))
    
    
    datause$var2tab<-0

    datause$var2tab[datause$Age==15]<-datause[datause$Age==15, k]/1000
    datause$var2tab[datause$Age==16]<-datause[datause$Age==16, k]/2*1000
    datause$var2tab[datause$Age==17]<-datause[datause$Age==17, k]/3*1000
    datause$var2tab[datause$Age==18]<-datause[datause$Age==18, k]/4*1000
    datause$var2tab[datause$Age==19]<-datause[datause$Age==19, k]/5*1000

  print(paste("average birthrate is ", sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)))
  return(datause)
}


AllViolence<-function(datause, dataList, k){
  l<-length(k)
  print(sum(datause$SampleWeight))
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]
  print(nrow(datause))
  
  SwV<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  SwK<-match(SwV, colnames(datause))
  print(sum(datause$SampleWeight))
  datause$SampleWeight<-datause[, SwK]/1000000
  print(sum(datause$SampleWeight))
  
  datause$var2tab<- 0
  
  for(i in c(1:l)){
    ki<-k[i]
    datause$var2tab[datause[ ,ki]==1]<-1
    print(table(datause$var2tab)/nrow(datause))
  }
  
  return(datause)
}
SexualPhysicalViolence<-function(datause, dataList, k){
  l<-length(k)
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]
  
  SwV<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  SwK<-match(SwV, colnames(datause))
  
  datause$SampleWeight<-datause[, SwK]/1000000
  datause$var2tab<- 0
  for(i in c(1:l))
    datause$var2tab[datause[,k[i]]==1]<-1 
  
  return(datause)
}

PhysicalViolence<-function(datause, dataList, k){
  l<-length(k)
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]
  
  SwV<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  SwK<-match(SwV, colnames(datause))
  
  datause$SampleWeight<-datause[, SwK]/1000000
  datause$var2tab<- 0
  for(i in c(1:l))
    datause$var2tab[datause[,k[i]]==1]<-1 
  
  return(datause)
}

SexualViolence<-function(datause, dataList, k){
  
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]
  
  SwV<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  SwK<-match(SwV, colnames(datause))
  
  datause$SampleWeight<-datause[, SwK]/1000000
  datause$var2tab<- 0
  datause$var2tab[datause[,k]==1]<-1
  
  return(datause)
  
}
EmotionalViolence<-function(datause, dataList, k){
  ViV<-dataList$VarName[dataList$NickName=="ViolenceInterview"]
  ViK<-match(ViV, colnames(datause))
  datause<-datause[datause[, ViK]==1 & datause$V502 %in% c(1,2), ]
  
  SwV<-dataList$VarName[dataList$NickName=="ViolenceWeight"]
  SwK<-match(SwV, colnames(datause))
  
  datause$SampleWeight<-datause[, SwK]/1000000
  datause$var2tab<- 0
  datause$var2tab[datause[,k]==1]<-1
  
  return(datause)
}

MobileFinance<-function(datause, dataList, k){
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
  return(datause)
}

BankAccount<-function(datause, dataList, k){
  datause$var2tab<- 0
  datause$var2tab[datause[,k] == 1]<-1
  return(datause)
}


HandWash<-function(datause, dataList, k){
  datause$var2tab<- 0
  print(k)
  datause[, k]<-as.numeric(as.character(datause[, k]))
  max_i<-max(datause[datause[,k]!=9,k])

  if(max_i==4) place<-(datause[,k]==1)
  if(max_i==5) place<-(datause[,k]<=2)
  
  wV<-dataList$VarName[dataList$NickName == "HandWashWater"]
  wk<-match(wV, colnames(datause), nomatch = 0)
  
  sV<-dataList$VarName[dataList$NickName == "HandWashSoap"]
  sk<-match(sV, colnames(datause), nomatch = 0)

  sV<-dataList$VarName[dataList$NickName == "HandWashSoap2"]
  sk2<-match(sV, colnames(datause), nomatch = 0)

  sV<-dataList$VarName[dataList$NickName == "HandWashSoap3"]
  sk3<-match(sV, colnames(datause), nomatch = 0)

  sV<-dataList$VarName[dataList$NickName == "HandWashSoap4"]
  sk4<-match(sV, colnames(datause), nomatch = 0)
 
  if(wk>0) water<-(datause[, wk]==1)
  else print("no info on water for washing hands")
  if(sk>0 & length(unique(datause[, sk]))>1) soap<-(datause[, sk]==1)
  else soap<-rep(FALSE, nrow(datause))
  if(sk2>0)
    if(length(unique(datause[, sk2]))>1) soap<- (soap | (datause[, sk2]==1))
  if(sk3>0)
    if(length(unique(datause[, sk3]))>1) soap<- (soap | (datause[, sk3]==1))
  if(sk4>0)
    if(length(unique(datause[, sk4]))>1) soap<-(soap | (datause[, sk4]==1))
  
  datause$var2tab[place & water & soap] <-1
  
  print(table(datause$var2tab))
  return(datause)
}

NotCrowded<-function(datause, dataList, k){
  datause$var2tab<- 0
  datause[, k]<-as.numeric(as.character(datause[, k]))
  nV<-dataList$VarName[dataList$NickName == "NumberMember"]
  nk<-match(nV, colnames(datause))
  datause[, nk]<-as.numeric(as.character(datause[, nk]))
  density<-datause[, nk]
  room<-!is.na(datause[, k]) & datause[, k]>0 & datause[, k]<99
  density[room]<-datause[room, nk]/datause[room, k]
  
  print(summary(density))
  datause$var2tab[density<=2]<-1
  return(datause)
}

FinancialInclusion<-function(datause, dataList, k){
  # this variable is defined by 2 columns
  datause$var2tab<-0
  
  for(i in c(1,2)){
    print(k[i])
    ki<-k[i]
    vi<-datause[,ki]
    datause$var2tab[vi==1]<-1
    
  }
  
  print(paste("average financial inclusion is ", sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)))
  
  
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

Covid1<-function(datause, dataList, k, svnm){

datause<-Learning(datause, dataList)
print("Learning: ")
print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
datause$learning<-datause$var2tab
k<-match(dataList$VarName[dataList$NickName=="BasicWater"], colnames(datause))
datause<-BasicWater(datause, dataList, k, svnm)
print("Basic Water: ")
print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
datause$basicwater<-datause$var2tab

k<-match(dataList$VarName[dataList$NickName=="HandWashPR"], colnames(datause), nomatch = 0)

if(k>0) {
  datause<-HandWash(datause, dataList, k) 
  print("Hand Wash: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
}
else {
  print("No information on handwash")
  return(NULL)
}
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
    print(vi)
    if(vi %in% colnames(datause)) datause$var2tab[datause[, vi]==11]<-0
    else print("Not Found")
  }
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print("=========end of wall and roof ============")
  return(datause)
}


Covid2<-function(datause, dataList, k, svnm){
  datause<-Covid1(datause, dataList, k, svnm)
  print("Covid1 :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$covid1<-datause$var2tab
  #look for k
  ncV<-dataList$VarName[dataList$NickName=="NotCrowded"]
  k<-match(ncV, colnames(datause))
  datause<-NotCrowded(datause, dataList, k)
  print("Not crowded :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$notcrowded<-datause$var2tab
  #look for k
  ssV<-dataList$VarName[dataList$NickName=="SafeSanitation"]
  k<-match(ssV, colnames(datause))
  datause<-SafeSanitation(datause, dataList, k, svnm)
  print("SafeSanitation :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$safsanitation<-datause$var2tab
  
  datause<-WallRoof(datause, dataList)
  print("wall and roof: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  print(table(datause$covid1, datause$var2tab))
  datause$var2tab <- datause$covid1 * datause$notcrowded *datause$safsanitation * datause$var2tab
  
  print("covid 2: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  return(datause)
  }
    
  
Covid<-function(datause, dataList, k, svnm){
  datause<-Learning(datause, dataList)
  print("Learning: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$learning<-datause$var2tab
  k<-match(dataList$VarName[dataList$NickName=="WaterOnsitePR"], colnames(datause))
  datause<-WaterOnsitePR(datause, dataList, k, svnm)
  print("Water on site: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$WaterOnsitePR<-datause$var2tab
  k<-match(dataList$VarName[dataList$NickName=="HandWashPR"], colnames(datause))
  datause<-HandWash(datause, dataList, k) 
  print("Hand Wash: ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  datause$HandWash<-datause$var2tab

  #look for k
  ncV<-dataList$VarName[dataList$NickName=="NotCrowdedPR"]
  k<-match(ncV, colnames(datause))
  datause<-NotCrowded(datause, dataList, k)
  print("Not crowded :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))

  datause$notcrowded<-datause$var2tab
  #look for k
  ssV<-dataList$VarName[dataList$NickName=="SafeSanitationPR"]
  k<-match(ssV, colnames(datause))
  datause<-SafeSanitation(datause, dataList, k, svnm)
  print("SafeSanitation :")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))

  datause$safsanitation<-datause$var2tab
  
  datause$var2tab <- datause$learning * datause$WaterOnsitePR * datause$HandWash * datause$notcrowded *datause$safsanitation 
  
  print("covid (new): ")
  print(sum(datause$SampleWeight[datause$var2tab==1])/sum(datause$SampleWeight))
  return(datause)
}

####### End of Programmer Note2





########### independent variables
PoorerHousehold<-function(datause, dataList, k){
  
  datause$PoorerHousehold<-"0"
  datause$PoorerHousehold[datause[,k] %in% c(0, 1, 2)]<-"1"
  
  return(datause)
}


MotherEducation<-function(datause, dataList, k){
  datause$MotherEducation<-"Lower" 
  datause$MotherEducation[datause[,k] ==0]<-"Lower"
  datause$MotherEducation[datause[,k] ==1]<-"Lower"
  datause$MotherEducation[datause[,k] == 2]<-"Secondary"
  datause$MotherEducation[is.na(datause[,k]==3)]<-"Higher"
  datause$MotherEducation<-factor(datause$MotherEducation, levels = c("Lower", "Secondary", "Higher"), ordered = TRUE)
  #}
  return(datause)
}

Caste<-function(datause, dataList, k, eth){
    if(eth %in% c("IA71", "IA74")) {
      
#       Item S116: Belong to a scheduled caste, a scheduled tribe, other backwa
#       ... tbd-name: '.RECODE6.REC91.S116'
#       _____________________________ _____________ _____________
#       Categories                           Frequency       CumFreq      %  Cum %  Net %|cNet %
#         _______________________________ _____________________________ _____________ _____________
#       1 Schedule caste                       142611         142611   20.4   20.4   21.2   21.2
#       2 Schedule tribe                        64132         206742    9.2   29.5    9.5   30.7
#       3 OBC                                  303910         510652   43.4   73.0   45.2   75.9
#       4 None of them                         157734         668386   22.5   95.5   23.4   99.3
#       8 Don't know                             4492         672878    0.6   96.2    0.7  100.0
# _______________________________ _____________________________ _____________ _____________
#  NotAppl                                 26808         699686    3.8  100.0
# _______________________________ _____________________________ _____________              
#  TOTAL                                  699686         699686  100.0  100.0
      
      datause$tmp<-as.numeric(as.character(datause[, k]))
      datause$Caste<-"DoNotKnow"
      datause$Caste[datause$tmp==1]<-"ScheduleCaste"
      datause$Caste[datause$tmp==2]<-"ScheduleTribe"
      datause$Caste[datause$tmp==3]<-"OtherBackwardClass"
      datause$Caste[datause$tmp==4]<-"OtherClass"
      
      datause$Caste<-factor(datause$Caste, levels = c("OtherClass", "DoNotKnow", "OtherBackwardClass", "ScheduleTribe", "ScheduleCaste"), ordered = T)
    }
    return(datause)
}


Religion<-function(datause, dataList, k, eth){
  if(eth %in% c("IA71", "IA74")) {
    
    datause$tmp<-as.numeric(as.character(datause[, k]))
    datause$Religion<-"MinorReligions"
    datause$Religion[datause$tmp==1]<-"Hindu"
    datause$Religion[datause$tmp==2]<-"Muslim"

    datause$Religion<-factor(datause$Religion, levels = c("Hindu", "Muslim", "MinorReligions"), ordered = T)
    
  }
  return(datause)
}

Under5<-function(datause, dataList, k){
  
  datause$Under5<-datause[, k]
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

HusbandOccupation<-function(datause, dataList, k){
  
  
}

HusbandAlcohol<-function(datause, dataList, k){
  datause$HusbandAlcohol<-"No"
  datause$HusbandAlcohol[datause[, k]==1]<-"Yes"
  
  datause$HusbandAlcohol<-factor(datause$HusbandAlcohol, levels = c("No" , "Yes"))
  return(datause)
}

FatherBeatMother<-function(datause, dataList, k){
  datause$FatherBeatMother<-"DontKnow"
  datause$FatherBeatMother[datause[, k]==0]<-"No"
  datause$FatherBeatMother[datause[, k]==1]<-"Yes"
  
  datause$FatherBeatMother<-factor(datause$FatherBeatMother, levels = c("DontKnow", "No" , "Yes"))
  return(datause)
}

AgeDifference<-function(datause, dataList, k){
  ageV<-dataList$VarName[dataList$NickName=="Age"]
  ageK<-match(ageV, colnames(datause))
  if(is.na(ageK)) {
    print("Age for Age difference can't be found")
    return(NULL)
  }
  datause$Age<-as.numeric(as.character(datause[,ageK]))
  datause$AgeH<-as.numeric(as.character(datause[,k]))
  datause$AgeDifference<-NA
  datause$AgeDifference[is.na(datause$Age) | is.na(datause$AgeH)]<-"Missing"
  datause$AgeDifference[datause$Age + 9 < datause$AgeH]<-"10+Younger"
  datause$AgeDifference[datause$Age + 9 >= datause$AgeH]<-"5-9Younger"
  datause$AgeDifference[datause$Age + 4 >= datause$AgeH]<-"1-4Younger"
  datause$AgeDifference[datause$Age == datause$AgeH]<-"SameAge"
  datause$AgeDifference[datause$Age > datause$AgeH]<-"WifeOlder"
  datause$AgeDifference<-factor(datause$AgeDifference, levels=c("Missing", "WifeOlder", "SameAge", "1-4Younger", "5-9Younger", "10+Younger"))
  print(table(datause$AgeDifference))
  return(datause)
}


EducationDifference<-function(datause, dataList, k){
  edV<-dataList$VarName[dataList$NickName=="Education"]
  edK<-match(edV, colnames(datause))
  if(is.na(edK)) {
    print("Education for education difference can't be found")
    return(NULL)
  }
  datause$Ed<-as.numeric(as.character(datause[,edK]))
  datause$EdH<-as.numeric(as.character(datause[,k]))
  datause$EducationDifference<-NA
  datause$EducationDifference[is.na(datause$Ed) | is.na(datause$EdH)] <-"Missing"
  datause$EducationDifference[datause$Ed==8 | datause$EdH==8]<-"Missing"
  datause$EducationDifference[datause$Ed==0 & datause$EdH==0]<-"NeitherEducated"
  datause$EducationDifference[datause$Ed==datause$EdH & datause$Ed>0]<-"EquallyEducated"
  datause$EducationDifference[datause$Ed > datause$EdH & is.na(datause$EducationDifference)]<-"WifeBetterEducated"
  datause$EducationDifference[datause$Ed < datause$EdH & is.na(datause$EducationDifference)]<-"HusbandBetterEducated"
  
  datause$EducationDifference<-factor(datause$EducationDifference, levels=c("Missing", "NeitherEducated", "EquallyEducated", "WifeBetterEducated", "HusbandBetterEducated"))
  print(table(datause$EducationDifference))
  return(datause)
}



Residence<-function(datause, dataList, k){
  
  datause$Residence<-"Urban"
  datause$Residence[datause[, k]==2]<-"Rural"
  datause$Residence<-factor(datause$Residence, levels = c("Urban" , "Rural"))
  
  return(datause)
}

############ HR independent variables
HighestEducation<-function(datause, dataList, k){
  
  datause$HighestEducation<-"Lower"
  datause$HighestEducation[regexpr("1", datause[,k])>0]<-"Lower"
  datause$HighestEducation[regexpr("2", datause[,k])>0]<-"Secondary"
  datause$HighestEducation[regexpr("3", datause[,k])>0]<-"Higher"
  datause$HighestEducation<-factor(datause$HighestEducation, levels = c("Lower", "Secondary", "Higher"), ordered = TRUE)
  
  return(datause)
}

#######333 IR independent variables
aGroup<-function(datause, dataList, k){
  
  datause$Age<-datause[ , k]
  datause$aGroup<-"Missing"
  datause$aGroup[!is.na(datause$Age) & datause$Age<15 ]="0-14"
  datause$aGroup[!is.na(datause$Age) & datause$Age>=15 & datause$Age<25 ]="15-24"
  datause$aGroup[!is.na(datause$Age) & datause$Age>=25 & datause$Age<35  ]="25-34"
  datause$aGroup[!is.na(datause$Age) & datause$Age>=35 & datause$Age<98]="35+"
  datause$aGroup<-factor(datause$aGroup, levels=c("0-14", "15-24", "25-34", "35+")) 
  
  return(datause)
}

aGroupPR<-function(datause, dataList, k){
  
  datause$Age<-datause[ , k]
  datause$aGroupPR<-"Missing"
  datause$aGroupPR[!is.na(datause$Age) & datause$Age<25 ]="0-24"
  datause$aGroupPR[!is.na(datause$Age) & datause$Age>=25 & datause$Age<60  ]="25-59"
  datause$aGroupPR[!is.na(datause$Age) & datause$Age>=60]="60+"
  datause$aGroupPR<-factor(datause$aGroupPR, levels=c("0-24", "25-59", "60+")) 
  
  return(datause)
}

HusbandAge<-function(datause, dataList, k)
{
  
  datause$AgeH<-datause[ , k]
  datause$HusbandAge<-"Missing"
  datause$HusbandAge[!is.na(datause$AgeH) & datause$AgeH<15 ]="0-14"
  datause$HusbandAge[!is.na(datause$AgeH) & datause$AgeH>=15 & datause$AgeH<25 ]="15-24"
  datause$HusbandAge[!is.na(datause$AgeH) & datause$AgeH>=25 & datause$AgeH<35  ]="25-34"
  datause$HusbandAge[!is.na(datause$AgeH) & datause$AgeH>=35 & datause$AgeH<98]="35+"
  datause$HusbandAge<-factor(datause$aGroup, levels=c("0-14", "15-24", "25-34", "35+")) 
  
  return(datause)
}

NUnder5<-function(datause, dataList, k){
  datause$NUnder5<-nchar(trimws(as.character(datause[,k])))
  return(datause) 
}


MarriageStatus<-function(datause, dataList, k){
  
  datause$MarriageStatus<-"NeverMarried"
  datause$MarriageStatus[datause[, k]==1 ]<- "CurrentlyMarried"
  datause$MarriageStatus[datause[, k]==2 ]<-"FormerlyMarried"
  datause$MarriageStatus<-factor(datause$MarriageStatus, levels=c("NeverMarried", "CurrentlyMarried", "FormerlyMarried"))
  
  return(datause)
}


Education<-function(datause, dataList, k){
  
  datause$Education<-"Lower"
  datause$Education[datause[, k]== 0] <-"Lower"
  datause$Education[datause[, k]== 1] <-"Lower"
  datause$Education[datause[, k]== 2] <-"Secondary"
  datause$Education[datause[, k]== 3] <-"Higher"
  datause$Education<-factor(datause$Education, levels=c("Lower", "Secondary", "Higher"), ordered = TRUE)
  
  return(datause)
}

EducationPR<-function(datause, dataList, k){
  datause$EducationPR<-"Lower"
  datause$EducationPR[datause[, k] %in% c(0, 1, 2)] <-"Lower"
  datause$EducationPR[datause[, k] %in% c(3, 4)] <-"Secondary"
  datause$EducationPR[datause[, k]== 5] <-"Higher"
  datause$EducationPR<-factor(datause$Education, levels=c("Lower", "Secondary", "Higher"), ordered = TRUE)
  
  return(datause)
}


HusbandEducation<-function(datause, dataList, k){
  
  datause$HusbandEducation<-"Lower"
  datause$HusbandEducation[datause[, k]== 0] <-"Lower"
  datause$HusbandEducation[datause[, k]== 1] <-"Lower"
  datause$HusbandEducation[datause[, k]== 2] <-"Secondary"
  datause$HusbandEducation[datause[, k]== 3] <-"Higher"
  datause$HusbandEducation<-factor(datause$Education, levels=c("Lower", "Secondary", "Higher"), ordered = TRUE)
  
  return(datause)
}



#### codes
water_code<-function(svnm){
  #### not clean
  if(svnm=="ID63") return(c(32, 33, 34, 35, 42, 43, 44, 45, 46, 51, 62, 91, 96, 61) )
  else return(c(32, 33, 34, 35, 42, 43, 44, 45, 46,  62, 91, 96, 61) )
  ### 51-rain water  61-Tanker truck   71- Bottled water  
  ### 51, 61, 71,   some times Tanker truck is still not improved
  ### 38, protected public well
  ### 81 refill
  ### 34 Open well in yard/polt
  ### 35 Open public well 
  ### 45 River/stream   
  ### 46 Pond/lake 
  ### 47 Dam        
}

sanitation_code<-function(svnm){
  #### not safe
  if (svnm %in% c("ID63", "ID71"))  return(c(13, 14, 15, 17, 21, 23,30, 31, 32, 33, 41, 42, 43, 51, 61, 96) )
  else return(c(14, 15, 23, 30, 31, 32,  42, 43, 51, 61, 96) )
  
  #### 22 Pit latrine with slab 41 Composting toilet  
  #### 22, 41,
  #### 13 public/shared ID63
  #### 21 VIP for ID63 & 71
  #### 17 public/shared ID71
}

cookfuel_code<-function(){
  #### clean
  return(c(1, 2, 3, 4))
}


#### calculating UnmetNeed in case V626A is not in the data

unmet_cal<-function(datause){
  datause$UnmetNeed<- NA
  k<-match("V626A", colnames(datause))
  
  if(length(k)==0){
    datause$UnmetNeed <- as.numeric(as.character(datause[,k]))
  }
  else {
    k<-match("V626", colnames(datause))
    if(length(k)==0){
      datause$UnmetNeed <- as.numeric(as.character(datause[,k]))
      datause$UnmetNeed[datause$UnmetNeed==5]<-3
      datause$UnmetNeed[datause$UnmetNeed==6]<-4
      
    }
    
    else {
      datause$UnmetNeed <- NA
      print("Unmet need variable not found")
    }
  }
  return(datause)
}


############################################################# 


scale_sample_weight <- function(df, swV) {
  
  swK<-match(swV, colnames(df))
  if(length(swK)==0 | sum(is.na(df[, swK]))==nrow(df))
    df$SampleWeight<-1
  else df$SampleWeight<-as.numeric(as.character(df[, swK]))/1000000
  
  df$SampleWeight[is.na(df$SampleWeight)] <- 0
  
  return(df)
  
}

merge_mr <- function(mr_ds, meta_data, datause, dataList, country_code, version_code, data_folder, rv, indvar, svnm, eth, caste, Flag_New) {
  
  mrdataList<-meta_data[meta_data$DataSet==mr_ds, ]
  swV<-mrdataList$VarName[mrdataList$NickName=="SampleWeight"]
  filename<-paste(country_code, mr_ds, version_code, "FL", sep="")
  if(file.exists(paste(paste(data_folder, filename, sep="/"), ".DCF", sep=""))){
  
    #modified for YW
    #data_folder <- paste(data_folder, paste(filename, "/", sep = ""), sep="")
    mrdf<-importDHSDAT(paste(data_folder, filename, sep="/"), Flag_New, mrdataList$VarName)
    swK<-match(swV, colnames(mrdf))
    if(length(swK)==0 | sum(is.na(mrdf[, swK]))==nrow(mrdf))
      mrdf$SampleWeight<-1
    else mrdf$SampleWeight<-as.numeric(as.character(mrdf[, swK]))/1000000
  
    mrdf$SampleWeight[is.na(mrdf$SampleWeight)] <- 0
    # if( caste == TRUE ){
    #   eth<-dataList[mrdataList$DataType==paste(country_code, mr_ds, version_code,sep="") & mrdataList$IndicatorType=="caste", c(1,2)]
    # }
    if(!is.null(mrdf)) {
      mrdatause<-catch_error(get_data(mrdf, rv, mrdataList, indvar, svnm, eth))
    }
    if(!is.null(mrdatause)) {
      commonVar<-c("SampleWeight", indvar, "Sex",
                 "var2tab", "RegionName")
    
      datause$Sex<-1
      mrdatause$Sex<-2
      datause<-datause[, commonVar]
      mrdatause<-mrdatause[, commonVar]
      datause<-rbind(datause, mrdatause)
      datause$Sex<-factor(datause$Sex)
      levels(datause$Sex)<-c("Female", "Male")
    }
  }
  
  return(datause) 
}

# add religion and caste for India
add_reglist<-function(country_code, version_code, datause, meta_data, dataList, regList, religion_data){}



merge_pr <- function(pr_ds, meta_data,datause, dataList, country_code, version_code, data_folder, rv, indvar, svnm, eth, caste, Flag_New) {
  
  prdataList<-meta_data[meta_data$DataSet==pr_ds, ]
  swV<-prdataList$VarName[prdataList$NickName=="SampleWeight"]
  filename<-paste(country_code, pr_ds, version_code, "FL", sep="")
  print(filename)

  prdf<-importDHSDAT(paste(data_folder, filename, sep="/"), Flag_New, prdataList$VarName)
  
  print(colnames(prdf))
  swK<-match(swV, colnames(prdf))
  if(length(swK)==0 | sum(is.na(prdf[, swK]))==nrow(prdf))
    prdf$SampleWeight<-1
  else prdf$SampleWeight<-as.numeric(as.character(prdf[, swK]))/1000000
  
  prdf$SampleWeight[is.na(prdf$SampleWeight)] <- 0
  
  # if( caste == TRUE ){
  #   eth<-dataList[prdataList$DataType==paste(country_code, pr_ds, version_code,sep="") & prdataList$IndicatorType=="caste", c(1,2)]
  # }
  
  prdf<-prdf[prdf$HV104==2 & prdf$HV117==0 & (prdf$HV116==" " | prdf$HV116==0), ]   ### only female, ineligible and not married
  
  print(table(prdf$HV013))
  if(!is.null(prdf)) prdatause<-get_data(prdf, rv, prdataList, indvar, svnm, eth)
  
  if(!is.null(prdatause)) {
    id_var<-prdataList[prdataList$DataType=="ID", ]
    id_l<-nrow(id_var)
    for(v in c(1:id_l)){
      k<-match(id_var$VarName[v], colnames(prdatause))
      colnames(prdatause)[k]<-id_var$NickName[v]
    }
    
    id_var<-dataList[dataList$DataType=="ID", ]
    id_l<-nrow(id_var)
    for(v in c(1:id_l)){
      k<-match(id_var$VarName[v], colnames(datause))
      colnames(datause)[k]<-id_var$NickName[v]
    }
    commonVar<-c("cluster_id", "HouseholdNumber", "RLNumber", "SampleWeight", indvar, "var2tab", "RegionName")
    
    print(colnames(prdatause))
    datause<-datause[, commonVar]
    prdatause<-prdatause[, commonVar] 
    prdatause$var2tab<-0 ### assume unmarried teenagers have no birth 
    datause<-rbind(datause, prdatause)  #, by=c("cluster_id", "HouseholdNumber", "RLNumber"), all.y=TRUE)
    
    # datause<-datause[(datause$HV117==0 & datause$HV116==0)
    #                  | (datause$HV117==1 & !is.na(datause$V502)), ]   
    # ###(Keep not eligible and never married + eligible and has valid marriage status)
    # datause$Under5<-as.numeric(as.character(datause$HV014))
    # datause$var2tab[is.na(datause$var2tab) & datause$Under5>0]<-1
    # datause$var2tab[is.na(datause$var2tab)]<-0
    
    return(datause)
  } 
}


write_tree <- function(datause, country_code2, year_code, 
                       title_string, formula_string, sub_string, 
                       rv, rtp, filename,  caste, output_folder) {
  
  # Build Model
  sub_string<-NULL
  pass_message <- "Successfully built Tree"

  print(output_folder)
  
  source1<-"DHS"
  tree_stat<- catch_error(build_tree(output_folder, country_code2, year_code,  datause, source1, rv, rtp,
                        formula_string, title_string, sub_string, filename, e=caste, region = FALSE))
  


  tree_stat<-t(c(country_code2, year_code, title_string, tree_stat, source1))
  # tree_stat$source<-"DHS"
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

write_HOI_D <- function(datause, country_code2, year_code, title_string,
                        indvar, output_folder, filename) {

  # Calculate 
  pass_message <- "Successfully calculated HOI and D"
  result<-catch_error(cal_HOI_shapley(datause, indvar))
  
  result<-c(country_code2, year_code,  title_string, result)
  result$source<-"DHS"
  
  if (!is.null(result)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  print(paste("md",filename,".Rdata", sep=""))
  # Saving object as .Rdata file for Shiny output
  resave(result, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output 
  pass_message <- "Successfully wrote D.csv"
  catch_error(write.table(t(result), file=paste(output_folder, "D.csv", sep=""),
              sep=",", append = TRUE,   col.names = F, row.names = F)) 

}

write_glm <- function(datause, rtp, country_code2, year_code, title_string, 
                      indvar, output_folder, filename) {

  # Build Logistic Regression model 

  pass_message <- "Successfully built glm"
  
  for (ivm in indvar) datause[, ivm]<- factor(datause[, ivm], ordered = F)
  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  
  s.glm<- catch_error_prod(logistic(datause, rtp,  formula_string)) #catch_error()

  if (!is.null(s.glm)) {
    info(logger, paste(pass_message))
  }
  
  # Saving object as .Rdata file for Shiny output

  catch_error_prod(resave(s.glm, file = paste("md",filename,".Rdata", sep="")))
  
  file_write<-paste(output_folder, "DHSLogit.csv", sep="")
  print(file_write)
  # Write to output 
  write.table(t(c(country_code2, year_code, title_string)) , file_write,
              sep=",", append = TRUE,   col.names = F, row.names = F)
  pass_message <- "Successfully wrote DHSLogit.csv"
  catch_error_prod(write.table(s.glm,  file_write,
              sep=",", append = TRUE,   col.names = T, row.names = T, na="")) #catch_error() 
  
  print("glm result written")
}

# List of formula components 
# Arguments: [1] Formula string: E.g. y ~ x1 + x2 
# Returns: List of independent variable components 
list_of_formula <- function(formula_string) {
  
  first_split <- formula_string %>% str_split(pattern = "~") %>%
    unlist %>%
    tail(1) 
  
  second_split <- gsub(" ", "", first_split)
  
  formula_components <- second_split %>% str_split(pattern = "\\+") %>%
    unlist
  
  return(formula_components)
}

# Reconstruct formula 
# Arguments: [1] List of independent variables
# Returns: Formula String E.g. y ~ x1 + x2 
construct_formula <- function(indvar_list) {
  
  formula <- paste("var2tab", paste(indvar_list, collapse=" + "), sep=" ~ ")
  
  return(formula)
}


# Changes factor variable to numeric 
# Arguments: [1] Single factor value 
# Returns: Single numeric value 
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


# Region 
region <- function(output_folder, country_code, version_code, 
                   datause, rv,
                   formula_string, title_string, sub_string, 
                   caste, filename, indvar) {
  
  # List of the regions   
  region_list <- unique(datause$RegionName)
  
  #filename = REAFIR70FL
  region_filename <- paste("RE",filename, sep = "")
  region_tree_stat <- c()
  region_treefit <- c()
  region_result <- c()
  region_glm <- c()
  
  for (region in region_list) {
    
    message <- paste("REGION: ", region, "  ------------------")
    print(message)
    info(logger, message)
    message <- paste("Current Region: ", match(region, region_list), " of ", length(region_list))
    print(message)
    info(logger, message)
    
    # Filter for the region data
    datause_region <- filter(datause, RegionName == region)

    # Reweight SampleWeight
    datause_region <- datause_region %>%
                        mutate(SampleWeight = SampleWeight*nrow(datause_region)/sum(SampleWeight))
    
    # Region output folder 
    region_folder <- paste(output_folder, "Region", "/", sep = "")
    ifelse(!dir.exists(region_folder), dir.create(region_folder), FALSE)
    
    region_output_folder <- paste(region_folder, region, "/", sep = "")
    ifelse(!dir.exists(region_output_folder), dir.create(region_output_folder), FALSE)
    
    #### Construct Decision Tree
    if(nrow(datause_region)>0){
        sub_string<-NULL
        pass_message <- "Successfully built Stat Tree"
        tree_stat<- catch_error(build_tree(output_folder, country_code, version_code, 
                                       datause_region, rv,
                                       formula_string, title_string, sub_string, 
                                       e=caste, filename, region))
        
        if (!is.null(tree_stat)) { 
          
          info(logger, paste(pass_message))
          
        } 
    
        tree_stat<-t(c(country_code, version_code, title_string, tree_stat))
    
        # Add region name to object 
        tree_stat <- c(region, tree_stat)
      
        # Append data for .Rdata
        region_tree_stat <- rbind(region_tree_stat, tree_stat)
    } 
          
    #### Construct Decision Tree Model Only 
    cp_chosen<- 1
  	if(caste == FALSE) minb_chosen = 11 
  	else minb_chosen = 19
  
  	treefit <- catch_error(rpart(as.formula(formula_string), 
                   data = datause_region,  weights=SampleWeight, 
                   method="anova", control = rpart.control(cp = cp_chosen/nrow(datause_region), maxdepth=6, 
                                                           minbucket =nrow(datause_region)/minb_chosen)))

  	# Append treefit raw model for .Rdata 
    region_treefit[[region]] <- as.party(treefit)     
    
    pass_message <- "Successfully built Tree"
    if (!is.null(treefit)) { 
      
          info(logger, paste(pass_message))

    }
    
    #### Construct HOI
    # Calculate 
    result<-catch_error(cal_HOI_shapley(datause_region, indvar))

    result<-c(country_code, version_code, title_string, result)

    # Add region name to object 
    result <- c(region, result)
    
    pass_message <- "Successfully calculated HOI and D"
    if (!is.null(result)) { 

            info(logger, paste(pass_message))
      
    }

    region_result <- rbind(region_result, result)
    
    ### Construct Logistic Regression
    # Change response variable to factor
    datause_region$var2tab <- factor(datause_region$var2tab)
    
    # Need to check that each res/ind variable has more than two factors
    # if not then remove. 
    
    # List of formula components 
    formula_components <- list_of_formula(formula_string)
    
    # Check formula variables have more than one value 
    for (component in formula_components) {
      
      if (length(unique(datause_region[[component]])) <= 1) {
        formula_components <- formula_components[formula_components != component]
      }
      
    }
    
    # Reconstruct formula 
    formula_string <- construct_formula(formula_components)
    
    # Construct glm model 
    if (length(unique(datause_region[["var2tab"]])) > 1) {
      
      # Change response variable to numeric 
      datause_region$var2tab <- as.numeric.factor(datause_region$var2tab)
      
      # Model 
      var2tab.glm <- catch_error(glm(as.formula(formula_string), family=binomial,
                                     weights=SampleWeight, data=datause_region))
    	s.glm <- as.data.frame(summary(var2tab.glm)$coefficients)

    	region_glm[[region]] <- s.glm

    	pass_message <- "Successfully built logistic regression."
    	if (!is.null(s.glm)) {

        info(logger, paste(pass_message))

      }
      
    }
  

  }

  # Resave after first time otherwise will be overwritten 
  save(region_tree_stat, file = paste(region_filename, ".Rdata", sep=""))
  resave(region_treefit, file = paste(region_filename, ".Rdata", sep=""))
  resave(region_result, file = paste(region_filename, ".Rdata", sep=""))
  
  if (length(unique(datause_region["var2tab"])) > 1) {
    resave(region_glm, file = paste(region_filename, ".Rdata", sep=""))
  }
}


# Catch error: production version 
catch_error_prod <- function(code) {
  out <- tryCatch(code,
           error = function(c) {
             error(logger, paste(c$message))
           }
           
          #  warning = function(c) { 
          #    warn(logger, paste(c$message))
          # }
  )
  return(out)
}

# Catch error: development version
catch_error_dev <- function(code) {
  return(code)
}

# Response variable directory: production version 
rv_Rdata_prod <- function(dhs_Rdata_folder, rv) {
  
  rv_Rdata_folder <- paste(dhs_Rdata_folder, rv, sep = "/")
  return(rv_Rdata_folder)
  
}

# Response variable directory: development version
rv_Rdata_dev <- function(dhs_Rdata_folder) {
  
  return(dhs_Rdata_folder)
  
}

iso_code<-function(country_code){
  if(country_code=="IA") iso<-"IND"
  else if(country_code=="AF") iso<-"AFG"
  else if(country_code=="AM") iso<-"ARM"
  else if(country_code=="BD") iso<-"BGD"
  else if(country_code=="KH") iso<-"KHM"
  else if(country_code=="MV") iso<-"MDV"
  else if(country_code=="MM") iso<-"MMR"
  else if(country_code=="NP") iso<-"NPL"
  else if(country_code=="PK") iso<-"PAK"
  else if(country_code=="PG") iso<-"PNG"
  else if(country_code=="PH") iso<-"PHL"
  else if(country_code=="TJ") iso<-"TJK"
  else if(country_code=="TL") iso<-"TLS"
  else if(country_code=="ID") iso<-"IDN"
  else iso<-"NotFound"
  
  return(iso)
  
}
