library(haven)
library(dplyr) # To arrange and clean our dataset and files
library(rpart)
library(rpart.plot)
source("/home/yw/Workspace/rstudio/LNOB_Rcode/Rfiles/TreeAndLogisticUnicef.R")


importData<-function(data_folder, dt, varList){
  # data_folder where the mics data is stored
  # dt is name of data set from MICS, can be hh, hl, ch, wm, mn
  # meta_data provided by importing a csv file
  
  # ecd: encoding, different for wm
  if(dt!="wm") ecd<-NULL
  else ecd<- 'latin1'
    
  df<-read_sav(paste(data_folder, dt, ".sav", sep=""), encoding = ecd, user_na = TRUE)

  # dataList<-meta_data[meta_data$DataSet==dt,]
  # varList<-unique(toupper(dataList$VarName))

  # print(colnames(df))
  col_index<-match(varList, toupper(colnames(df)))
  col_index<-col_index[!is.na(col_index)]
  
  df<-df[ , col_index]
  
  if(dt=="hl") {
    k_hl<-match("HL1", toupper(colnames(df)), nomatch = 0)
    if(k_hl==0) {
      print("-HL1- not found in imported hl data")
      return(NULL)
    }
    else colnames(df)[k_hl]<-"LN"
  }
  k_cn<-length(colnames(df))
  # for(i in c(1:k_cn)){
  #   if(!(colnames(df)[i] %in% c("HH1", "HH2", "LN")))
  #     colnames(df)[i]<-paste(colnames(df)[i], dt, sep=".")
  # }
  print(colnames(df))
  return(df)
}

mics_merge<-function(base_data, adding_data){
  # check if the adding_data is individual level or household level
  k_ln<-match("LN", colnames(adding_data), nomatch = 0)
  if(k_ln==0) by_var<-c("HH1", "HH2")
    else by_var<-c("HH1", "HH2", "LN")
  
    Mergedfile <- merge(base_data, adding_data, by= by_var, all.x = TRUE) 
    return(Mergedfile)
}

# Children_Masterfile1 <- filter(Masterfile1, HL6 < 18)


crowdingdep<-function(Children_Masterfile1){
  # severcrowdingdep
  # modercrowdingdep
  
  Children_Masterfile1 <- Children_Masterfile1 %>% mutate(personperroom = ifelse(HC3 != 99, HH48/HC3, NA))
  Children_Masterfile1$severcrowdingdep <- ifelse(Children_Masterfile1$personperroom >= 0.25 & Children_Masterfile1$personperroom < 5, 0, ifelse(Children_Masterfile1$personperroom >=5 & Children_Masterfile1$personperroom <=10 , 1,NA))
  Children_Masterfile1$modercrowdingdep <- ifelse(Children_Masterfile1$personperroom >= 0.25 & Children_Masterfile1$personperroom < 3, 0, ifelse(Children_Masterfile1$personperroom >= 3 & Children_Masterfile1$personperroom <= 10, 1,NA))
  Children_Masterfile1$crowdingMissing<-0
  Children_Masterfile1$crowdingMissing[is.na(Children_Masterfile1$modercrowdingdep)]<-1
  print("Tabulation of missing data on number of rooms")
  print(table(Children_Masterfile1$crowdingMissing))
  print("Tabulation of severe overcrowding")
  print(table(Children_Masterfile1$severcrowdingdep))
  print("Tabulation of moderate overcrowding")
  print(table(Children_Masterfile1$modercrowdingdep))
  return(Children_Masterfile1) #[Children_Masterfile1$crowdingMissing==0, ])
  
}

waterdep<-function(Children_Masterfile1){
  # severewaterdep
  # moderatewaterdep
  Children_Masterfile1$severewaterdep <- ifelse(Children_Masterfile1$WS1 != 81 & Children_Masterfile1$WS1 != 99, 0, ifelse(Children_Masterfile1$WS1 == 81, 1, ifelse(Children_Masterfile1$WS1 == 99, NA, NA)) )
  Children_Masterfile1$moderatewaterdep <- ifelse(Children_Masterfile1$WS1 == 81|Children_Masterfile1$WS1 == 22|Children_Masterfile1$WS1 == 32|Children_Masterfile1$WS1 == 42|Children_Masterfile1$WS1 == 96 , 1, ifelse(Children_Masterfile1$WS1 == 99, NA, 0))
  
  print("Severe water problem")
  print(table(Children_Masterfile1$severewaterdep))
  print("Moderate water problem")
  print(table(Children_Masterfile1$moderatewaterdep))
  return(Children_Masterfile1)
}

sanitationdep<-function(Children_Masterfile1){
  # severesanitationdeprived
  # moderatesanitationdeprived
  Children_Masterfile1$severesanitationdeprived <- ifelse(Children_Masterfile1$WS11 == 95 , 1, ifelse(Children_Masterfile1$WS11 == 99, NA, 0))
  Children_Masterfile1$moderatesanitationdeprived <- ifelse(Children_Masterfile1$WS11 == 95|Children_Masterfile1$WS11 == 23|Children_Masterfile1$WS11 == 41|Children_Masterfile1$WS11 == 51|Children_Masterfile1$WS11 == 96 , 1, ifelse(Children_Masterfile1$WS11 == 99, NA, 0))
  
  print("Severe sanitation problem")
  print(table(Children_Masterfile1$severesanitationdeprived))
  print("Moderate sanitation problem")
  print(table(Children_Masterfile1$moderatesanitationdeprived))
  return(Children_Masterfile1)
}

stunting<-function(Children_Masterfile1){
  # severestunting
  # moderatestunting
  
  Children_Masterfile1$severestunting <- ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2 > -3.0 & Children_Masterfile1$HAZFLAG==0, 0, ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2<=-3.0 & Children_Masterfile1$HAZFLAG==0, 1, NA))
  Children_Masterfile1$moderatestunting <- ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2 > -2.0 & Children_Masterfile1$HAZFLAG==0, 0, ifelse(Children_Masterfile1$HL6<5 & Children_Masterfile1$HAZ2<= -2.0 & Children_Masterfile1$HAZFLAG==0, 1, NA))
  
  print("Severe stunting problem")
  print(table(Children_Masterfile1$severestunting))
  print("Moderate stunting problem")
  print(table(Children_Masterfile1$moderatestunting))
  
  return(Children_Masterfile1)
}

educationdep<-function(Children_Masterfile1){
  
  # severeducdepbelow15
  # severeducdepbelow15a
  # severeducdepbelow15b
  
  Children_Masterfile1$severeducdepbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4 ==1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4 ==2, 1, NA))
  Children_Masterfile1$severeducdepbelow15a <- ifelse(Children_Masterfile1$ED4 == 2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeducdepbelow15)
  Children_Masterfile1$severeducdepbelow15b <- ifelse(Children_Masterfile1$ED4 == 1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeducdepbelow15)
  
  
  Children_Masterfile1$moderateedudeprivbelow15 <- Children_Masterfile1$severeducdepbelow15b
  Children_Masterfile1$moderateedudeprivbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 2, 1, Children_Masterfile1$severeducdepbelow15b))
  Children_Masterfile1$moderateedudeprivbelow15a <- ifelse(Children_Masterfile1$ED4==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$moderateedudeprivbelow15)
  
  Children_Masterfile1$severeedudeprived15older <- ifelse(((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4==1)) | ((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED9==1)), 0, ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4==2), 1, NA))
  #Please keep in mind that there's no direct relationship between ED5A and ED6; 
  #we use here simply to say that children who have not completed any grade within 
  #primary or less are severely deprived
  Children_Masterfile1$severeedudeprived15olderA <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED5A <= 1 & Children_Masterfile1$ED6==2 & Children_Masterfile1$ED9==2, 1, Children_Masterfile1$severeedudeprived15older)
  Children_Masterfile1$severeedudeprived15olderB <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9 == 2 & (Children_Masterfile1$ED6==1|Children_Masterfile1$ED6==8) & Children_Masterfile1$ED5A==1 & Children_Masterfile1$ED5B<6, 1, Children_Masterfile1$severeedudeprived15olderA)
  Children_Masterfile1$severeedudeprived15olderC <- ifelse(Children_Masterfile1$ED4==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeedudeprived15olderB)
  Children_Masterfile1$severeedudeprived15olderD <- ifelse(Children_Masterfile1$ED4==1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeedudeprived15olderC)
  
  
  Children_Masterfile1$moderateedudeprived15older <- Children_Masterfile1$severeedudeprived15olderD
  # Child attended school the year of the survey but below secondary OR Child did not attend the year of survey OR Child did attend, but primary 
  Children_Masterfile1$moderateedudeprived15olderA <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==1 & Children_Masterfile1$ED10A<2 & Children_Masterfile1$ED5A<2, 1, Children_Masterfile1$moderateedudeprived15older)
  # Child did attended the year of the survey and did so in secondary or higher,
  #OR child has completed all grades higher than secondary (irresp, OR child has completed
  #the highest grade in senior secondary (irrespective of attendance)
  Children_Masterfile1$moderateedudeprived15olderB <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==2, 1, Children_Masterfile1$moderateedudeprived15olderA)
  Children_Masterfile1$moderateedudeprived15olderC <- ifelse((Children_Masterfile1$HL6 >=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9==1 & Children_Masterfile1$ED10A==1, 1, Children_Masterfile1$moderateedudeprived15olderB)
  ##Line below sends the following children to non-deprived: 
  ##Child did attended the year of the survey and did so in secondary or higher,
  ##OR child has completed all grades higher than secondary (irresp, OR child has completed
  ##the highest grade in senior secondary (irrespective of attendance): 
  Children_Masterfile1$moderateedudeprived15olderD <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17)&Children_Masterfile1$ED9==1 & (Children_Masterfile1$ED10A>=2 & Children_Masterfile1$ED10A<=4)|(Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED5A>=4 & Children_Masterfile1$ED5A<=5) & (Children_Masterfile1$ED6==1)|(Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17)&Children_Masterfile1$ED5A==3 &Children_Masterfile1$ED5B==4 & Children_Masterfile1$ED6==1, 0, Children_Masterfile1$moderateedudeprived15olderC)
  #Send to missing inconsistencies between never attended and current attendance
  #Send to missing inconsistencies between ever attended and no response in current attendance 
  Children_Masterfile1$moderateedudeprived15olderE <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & ((Children_Masterfile1$ED4==2 & Children_Masterfile1$ED9==1)|(Children_Masterfile1$ED4==1 & Children_Masterfile1$ED9==9)), NA, Children_Masterfile1$moderateedudeprived15olderD)

  
  
  Children_Masterfile1$SumSevedugroup <- rowSums(Children_Masterfile1[,c("severeducdepbelow15b", "severeedudeprived15olderD")], na.rm=TRUE)
  # I then replace the 0 with NAs to avoid counting the NA as o and thus as non-deprived:
  Children_Masterfile1$SumSevedugroup <- ifelse(is.na(Children_Masterfile1$severeducdepbelow15b) & is.na(Children_Masterfile1$severeedudeprived15olderD), NA, Children_Masterfile1$SumSevedugroup)
  # I can now generate the group severe deprivation dimension in education
  Children_Masterfile1$sevedugroup <- ifelse(Children_Masterfile1$SumSevedugroup>=1, 1, ifelse(Children_Masterfile1$SumSevedugroup==0, 0, NA))
  
  ## Moderate (both ages):
  
  # To generate the group aggregate I operate a sum of the two severe deprivation indicators in education first:
  
  Children_Masterfile1$SumModeredugroup <- rowSums(Children_Masterfile1[,c("moderateedudeprivbelow15a", "moderateedudeprived15olderE")], na.rm=TRUE)
  # I then replace the 0 with NAs to avoid counting the NA as o and thus as non-deprived:
  Children_Masterfile1$SumModeredugroup <- ifelse(is.na(Children_Masterfile1$moderateedudeprivbelow15a) & is.na(Children_Masterfile1$moderateedudeprived15olderE), NA, Children_Masterfile1$SumModeredugroup)
  # I can now generate the group severe deprivation dimension in education
  Children_Masterfile1$Moderedugroup <- ifelse(Children_Masterfile1$SumModeredugroup>=1, 1, ifelse(Children_Masterfile1$SumModeredugroup==0, 0, NA))
  
  print("Severe education problem")
  print(table(Children_Masterfile1$sevedugroup))
  print("Moderate education problem")
  print(table(Children_Masterfile1$Moderedugroup))
  
  return(Children_Masterfile1)
}

vaccinedep<-function(Children_Masterfile1){
  Children_Masterfile1$measlesdeprived <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1D==0|Children_Masterfile1$IM26==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4, 1, NA)
  Children_Masterfile1$measlesdeprivedA <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE <= 35 & Children_Masterfile1$IM26==1, 0, Children_Masterfile1$measlesdeprived)
  Children_Masterfile1$measlesdeprivedB <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1D>=1 & Children_Masterfile1$IM6M1D<= 66 , 0, Children_Masterfile1$measlesdeprivedA) 
  Children_Masterfile1$measlesdeprivedC <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1M>=1 & Children_Masterfile1$IM6M1M<= 66 , 0, Children_Masterfile1$measlesdeprivedB) 
  Children_Masterfile1$measlesdeprivedD <- ifelse(Children_Masterfile1$CAGE>= 12 & Children_Masterfile1$CAGE<= 35 & Children_Masterfile1$IM6M1Y>=2012 & Children_Masterfile1$IM6M1Y<= 6666 , 0, Children_Masterfile1$measlesdeprivedC) 
  
  Children_Masterfile1$dpt1deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA1D==0 | Children_Masterfile1$IM20==2 | Children_Masterfile1$IM2==4 | Children_Masterfile1$IM5==4,1, NA)
  Children_Masterfile1$dpt1deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21>=1 & Children_Masterfile1$IM21<=7, 0, Children_Masterfile1$dpt1deprived)
  # we then the folks below to missing:
  Children_Masterfile1$dpt1deprivedB <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==8|Children_Masterfile1$IM21==9, NA, Children_Masterfile1$dpt1deprivedA)
  # We then generate the vaccination that capture the day (no need for the year and month here, but it can bring changes):
  Children_Masterfile1$dpt1deprivedC <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA1D>=1 & Children_Masterfile1$IM6PENTA1D<=66, 0, Children_Masterfile1$dpt1deprivedB)
  
  
  Children_Masterfile1$dpt2deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==1 & Children_Masterfile1$IM6PENTA2D==0|Children_Masterfile1$IM20==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4, 1, NA)
  Children_Masterfile1$dpt2deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21>=2 & Children_Masterfile1$IM21<=7, 0, Children_Masterfile1$dpt2deprived)
  Children_Masterfile1$dpt2deprivedB <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM21==8 | Children_Masterfile1$IM21==9, NA, Children_Masterfile1$dpt2deprivedA)
  
  
  Children_Masterfile1$dpt3deprived <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & (Children_Masterfile1$IM6PENTA3D==0|Children_Masterfile1$IM20==2|Children_Masterfile1$IM2==4|Children_Masterfile1$IM5==4),1, NA)
  Children_Masterfile1$dpt3deprivedA <- ifelse(Children_Masterfile1$CAGE>=12 & Children_Masterfile1$CAGE<=35 & Children_Masterfile1$IM6PENTA3D>=1 & Children_Masterfile1$IM6PENTA3D<=66, 0, Children_Masterfile1$dpt3deprived)
  
  Children_Masterfile1$hasmissvaccines <- rowSums(is.na(Children_Masterfile1[,c("measlesdeprivedD", "dpt1deprivedC", "dpt2deprivedB", "dpt3deprivedA")]), na.rm = TRUE)
  Children_Masterfile1$sumvaccines <- rowSums(Children_Masterfile1[,c("measlesdeprivedD", "dpt1deprivedC", "dpt2deprivedB", "dpt3deprivedA")], na.rm = TRUE)
  Children_Masterfile1$sumvaccines <- ifelse(is.na(Children_Masterfile1$measlesdeprivedD) & is.na(Children_Masterfile1$dpt1deprivedC) & is.na(Children_Masterfile1$dpt2deprivedB) & is.na(Children_Masterfile1$dpt3deprivedA), NA, Children_Masterfile1$sumvaccines)
  
  Children_Masterfile1$moderatevaccinesdeprived <- ifelse(Children_Masterfile1$sumvaccines>=1, 1, ifelse(Children_Masterfile1$sumvaccines == 0, 0, NA))
  Children_Masterfile1$severevaccinesdeprived <- ifelse(Children_Masterfile1$sumvaccines == 4, 1, ifelse(Children_Masterfile1$sumvaccines == 0, 0, NA))

  print("Severe vaccine problem")
  print(table(Children_Masterfile1$severevaccinesdeprived))
  print("Moderate vaccine problem")
  print(table(Children_Masterfile1$moderatevaccinesdeprived))
  
  
  return(Children_Masterfile1)
  
  
}

aridep<-function(Children_Masterfile1){
  
  Children_Masterfile1$arisymptoms <- ifelse(Children_Masterfile1$CAGE>=36 & Children_Masterfile1$CAGE>=59 & Children_Masterfile1$CA16==1 & Children_Masterfile1$CA17==1 & Children_Masterfile1$CA18==1 | Children_Masterfile1$CA18==3, 1, NA)
  Children_Masterfile1$ariseverlydeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==2, 1, ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==1, 0, NA))
  
  Children_Masterfile1$arimoderatedeprived <- Children_Masterfile1$ariseverlydeprived
  Children_Masterfile1$arimoderatedeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21A=="A"|Children_Masterfile1$CA21B=="B"|Children_Masterfile1$CA21D=="D"|Children_Masterfile1$CA21F=="F"|Children_Masterfile1$CA21I=="I"|Children_Masterfile1$CA21J=="J"|Children_Masterfile1$CA21K=="K"|Children_Masterfile1$CA21W=="W"), 0, ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21P=="P"|Children_Masterfile1$CA21Q=="Q"|Children_Masterfile1$CA21R=="R"|Children_Masterfile1$CA21X=="X"), 1, NA))
  
  print("Severe ari problem")
  print(table(Children_Masterfile1$ariseverlydeprived))
  print("Moderate ari problem")
  print(table(Children_Masterfile1$arimoderatedeprived))
  
  return(Children_Masterfile1)
}

healthdep<-function(Children_Masterfile1){
  
  Children_Masterfile1$hasmissevhealth <- rowSums(is.na(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")]), na.rm = TRUE)
  Children_Masterfile1$sumsevhealth <- rowSums(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")], na.rm = TRUE)
  Children_Masterfile1$sumsevhealth <- ifelse(is.na(Children_Masterfile1$severevaccinesdeprived) & is.na(Children_Masterfile1$ariseverlydeprived), NA, Children_Masterfile1$sumsevhealth)
  Children_Masterfile1$severehealth <- ifelse(Children_Masterfile1$sumsevhealth == 1, 1, ifelse(Children_Masterfile1$sumsevhealth == 0, 0, NA))
  
  Children_Masterfile1$hasmissmoderhealth <- rowSums(is.na(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")]), na.rm = TRUE)
  Children_Masterfile1$summoderhealth <- rowSums(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")], na.rm = TRUE)
  Children_Masterfile1$summoderhealth <- ifelse(is.na(Children_Masterfile1$moderatevaccinesdeprived) & is.na(Children_Masterfile1$arimoderatedeprived), NA, Children_Masterfile1$summoderhealth)
  Children_Masterfile1$moderatehealth <- ifelse(Children_Masterfile1$summoderhealth >= 1, 1, ifelse(Children_Masterfile1$summoderhealth == 0, 0, NA))
  
  
  print("Severe health problem")
  print(table(Children_Masterfile1$severehealth))
  print("Moderate health problem")
  print(table(Children_Masterfile1$moderatehealth))
  
  return(Children_Masterfile1)
}

informationdep<-function(Children_Masterfile1){
  
  Children_Masterfile1$Radiodep <- ifelse(Children_Masterfile1$HC7B == 2, 1, ifelse(Children_Masterfile1$HC7B == 1, 0, NA))
  Children_Masterfile1$TVdep <- NA
  Children_Masterfile1$TVdep[Children_Masterfile1$HC9A == 1]<-0
  Children_Masterfile1$TVdep[Children_Masterfile1$HC9A == 2] <-1
  
  Children_Masterfile1$Compdep <- ifelse(Children_Masterfile1$HC11 == 1, 0, ifelse(Children_Masterfile1$HC11 == 2, 1, NA))
  
  
  Children_Masterfile1$Mobdep <- ifelse(Children_Masterfile1$HC12 == 1, 0, ifelse(Children_Masterfile1$HC12 == 2, 1, NA))
  Children_Masterfile1$Internetdep <- ifelse(Children_Masterfile1$HC13==1, 0, ifelse(Children_Masterfile1$HC13 == 2, 1, NA))
  Children_Masterfile1$hasmissinformation <- rowSums(is.na(Children_Masterfile1[,c("Radiodep", "TVdep", "Compdep", "Mobdep", "Internetdep")]), na.rm = TRUE)
  Children_Masterfile1$suminformation <- rowSums(Children_Masterfile1[,c("Radiodep", "TVdep", "Compdep", "Mobdep", "Internetdep")], na.rm = TRUE)
  Children_Masterfile1$Suminformation <- ifelse(is.na(Children_Masterfile1$Radiodep) & is.na(Children_Masterfile1$TVdep) & is.na(Children_Masterfile1$Compdep) & is.na(Children_Masterfile1$Mobdep) & is.na(Children_Masterfile1$Internetdep), NA, Children_Masterfile1$suminformation)
  Children_Masterfile1$Severeinfo <- ifelse(Children_Masterfile1$Suminformation == 5, 1, ifelse(Children_Masterfile1$Suminformation <= 5, 0, NA))
  Children_Masterfile1$Moderateinfo <- ifelse(Children_Masterfile1$Suminformation == 4, 1, ifelse(Children_Masterfile1$Suminformation < 4, 0, NA))
  
  print("Severe info problem")
  print(table(Children_Masterfile1$Severeinfo))
  print("Moderate info problem")
  print(table(Children_Masterfile1$Moderateinfo))
  
  return(Children_Masterfile1)
}

finalResult<-function(Children_Masterfile1){

Children_Masterfile1$hassmissmoderatepoor <-rowSums(is.na(Children_Masterfile1[,c("modercrowdingdep", "moderatewaterdep", 
                        "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth")]), na.rm = TRUE)
Children_Masterfile1$hassmissmoderatepoorInfo<-Children_Masterfile1$hassmissmoderatepoor
                                    + is.na(Children_Masterfile1[,c("Moderateinfo")])

print("Missing data on moderate")
print(table(Children_Masterfile1$hassmissmoderatepoor))
print("Missing data on moderate with info")
print(table(Children_Masterfile1$hassmissmoderatepoorInfo))


Children_Masterfile1$summoderpoor <- rowSums(Children_Masterfile1[, c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth")], na.rm = TRUE)
Children_Masterfile1$Summoderpoor <- ifelse(Children_Masterfile1$hassmissmoderatepoor == 6, ".", Children_Masterfile1$summoderpoor)
Children_Masterfile1$moderatelydeprived <- ifelse(Children_Masterfile1$Summoderpoor == 0, 0, ifelse(Children_Masterfile1$Summoderpoor >= 1, 1, NA))

Children_Masterfile1$summoderpoorInfo <- rowSums(Children_Masterfile1[, c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth", "Moderateinfo")], na.rm = TRUE)
Children_Masterfile1$SummoderpoorInfo <- ifelse(Children_Masterfile1$hassmissmoderatepoorInfo == 6, ".", Children_Masterfile1$summoderpoorInfo)
Children_Masterfile1$moderatelydeprivedInfo <- ifelse(Children_Masterfile1$SummoderpoorInfo == 0, 0, ifelse(Children_Masterfile1$SummoderpoorInfo >= 1, 1, NA))



Children_Masterfile1$hassmissseverepoor <-rowSums(is.na(Children_Masterfile1[,c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth")]), na.rm = TRUE)
Children_Masterfile1$hassmissseverepoorInfo<-Children_Masterfile1$hassmissseverepoor
                                             + is.na(Children_Masterfile1[,c("Severeinfo")])

print("Missing data on serever")
print(table(Children_Masterfile1$hassmissseverepoor))
print("Missing data on serever with info")
print(table(Children_Masterfile1$hassmissseverepoorInfo))

Children_Masterfile1$summoderpoor <- rowSums(Children_Masterfile1[, c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth")], na.rm = TRUE)
Children_Masterfile1$Summoderpoor <- ifelse(Children_Masterfile1$hassmissseverepoor == 6, ".", Children_Masterfile1$summoderpoor)
Children_Masterfile1$severelydeprived <- ifelse(Children_Masterfile1$Summoderpoor == 0, 0, ifelse(Children_Masterfile1$Summoderpoor >= 1, 1, NA))

Children_Masterfile1$summoderpoorInfo <- rowSums(Children_Masterfile1[, c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth", "Severeinfo")], na.rm = TRUE)
Children_Masterfile1$SummoderpoorInfo <- ifelse(Children_Masterfile1$hassmissseverepoorInfo == 6, ".", Children_Masterfile1$summoderpoorInfo)
Children_Masterfile1$severelydeprivedInfo <- ifelse(Children_Masterfile1$SummoderpoorInfo == 0, 0, ifelse(Children_Masterfile1$SummoderpoorInfo >= 1, 1, NA))


print("Severe deprived problem")
print(table(Children_Masterfile1$severelydeprived))
print("Moderate deprived problem")
print(table(Children_Masterfile1$moderatelydeprived))

print("Severe deprived problem with info")
print(table(Children_Masterfile1$severelydeprivedInfo))
print("Moderate deprived problem with info")
print(table(Children_Masterfile1$moderatelydeprivedInfo))


return(Children_Masterfile1)
}

missingTab<-function(Children_Masterfile1){
  varlist<-c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", 
             "moderatehealth")
  missingcount<-0
  for (vl in varlist){
    print(paste("Missing value tab for ", vl, "======"))
    print(table(Children_Masterfile1$moderatelydeprived, is.na(Children_Masterfile1[, vl])))
    missingcount<-missingcount+sum(is.na(Children_Masterfile1[, vl]))
  }
  print(missingcount)

  varlist<-c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", 
  "sevedugroup", "severehealth")
  
  missingcount<-0
  for (vl in varlist){
    print(paste("Missing value tab for ", vl, "======"))
    print(table(Children_Masterfile1$severelydeprived, is.na(Children_Masterfile1[, vl])))
    missingcount<-missingcount+sum(is.na(Children_Masterfile1[, vl]))
  }
  print(missingcount)
  
}

createIndvar<-function(datause){
  datause$AgeGroup<-"0-5"
  datause$AgeGroup[datause$HL6>=5 & datause$HL6<=7]<-"5-7"
  datause$AgeGroup[datause$HL6>=8 & datause$HL6<=14]<-"8-14"
  datause$AgeGroup[datause$HL6>=15]<-"15-17"
  
  datause$PoorerHousehold<-"Non_Poor"
  datause$PoorerHousehold[datause$windex5 %in% c( 1, 2)]<-"Poor"
  
  datause$Residence<-"Rural"
  datause$Residence[datause$HH6==1]<-"Urban"
  
  datause$Sex<-"Male"
  datause$Sex[datause$HL4==2]<-"Female"
  
  datause$HighestEducation<-"Lower"
  datause$HighestEducation[datause$HEducation %in% c(2, 3)]<-"Secondary"
  datause$HighestEducation[datause$HEducation==4]<-"Higher"
  datause$HighestEducation<-factor(datause$HighestEducation, levels = c("Lower", "Secondary", "Higher"), ordered = TRUE)
  return(datause)
}

generate_trees<-function(source_folder, rvList, iv, datause, title, e=FALSE, region="National"){
  print(title)
    for (rv in rvList){
    print(table(datause[, rv]))
    datause<-datause[!is.na(datause[, rv]), ]
    #filename<-paste(title, rv, sep="_")
    formula_string<-paste(rv, paste(iv, collapse=" + "), sep=" ~ ")
    title_string<-paste(title, paste(iv, collapse=" + "), sep=" : ")
    tree_stat<-build_tree(source_folder, "Mongolia", "2018", datause, rv,
             "Factor", formula_string, title_string, sub_string=NULL, title, e=FALSE, region)
    # print(tree_stat$tree_stat)
  }  
}


fourComponents<-function(datause){
  rvList<-c("moderatelydeprived","severelydeprived")
  iv<-c("PoorerHousehold", "Residence", "Sex", "HighestEducation" )
  
  ChildHealth<-datause[!is.na(datause$CAGE), ]
  ChildHealth$summoderpoor <- rowSums(ChildHealth[, c("moderatestunting", "moderatehealth")], na.rm = TRUE)
  ChildHealth$sumseverpoor <- rowSums(ChildHealth[, c("severestunting", "severehealth")], na.rm = TRUE)
  print(summary(ChildHealth$summoderpoor))
  print(summary(ChildHealth$sumseverpoor))
  ChildHealth$moderatelydeprived <- ifelse(ChildHealth$summoderpoor == 0, 0, ifelse(ChildHealth$summoderpoor >= 1, 1, NA))
  ChildHealth$severelydeprived <- ifelse(ChildHealth$sumseverpoor == 0, 0, ifelse(ChildHealth$sumseverpoor >= 1, 1, NA))
  title<-"ChildHealth"
  generate_trees(source_folder, rvList, iv, ChildHealth, title, e=FALSE, region="National")
  #  c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived", "moderatestunting", "Moderedugroup", "moderatehealth")], na.rm = TRUE)
  # c("severcrowdingdep", "severewaterdep", "severesanitationdeprived", "severestunting", "sevedugroup", "severehealth")]), na.rm = TRUE)

  ObligatoryEducation<-datause[datause$HL6>=7 & datause$HL6<=14, ]
  print(summary(ObligatoryEducation$Moderedugroup))
  print(summary(ObligatoryEducation$sevedugroup))
  ObligatoryEducation$moderatelydeprived <- ifelse(ObligatoryEducation$Moderedugroup == 0, 0, ifelse(ObligatoryEducation$Moderedugroup >= 1, 1, NA))
  ObligatoryEducation$severelydeprived <- ifelse(ObligatoryEducation$sevedugroup == 0, 0, ifelse(ObligatoryEducation$sevedugroup >= 1, 1, NA))
  title<-"ObligatoryEducation"
  generate_trees(source_folder, rvList, iv, ObligatoryEducation, title, e=FALSE, region="National")
  
  UpperSecondary<-datause[datause$HL6>=15 & datause$HL6<=17, ]
  print(summary(UpperSecondary$Moderedugroup))
  print(summary(UpperSecondary$sevedugroup))
  print(table(UpperSecondary$PoorerHousehold))
  print(sum(UpperSecondary$SampleWeight[UpperSecondary$PoorerHousehold=="Poor"])/sum(UpperSecondary$SampleWeight))
  print(sum(UpperSecondary$SampleWeight[UpperSecondary$PoorerHousehold=="Non_Poor"])/sum(UpperSecondary$SampleWeight))
  
  UpperSecondary$moderatelydeprived <- ifelse(UpperSecondary$Moderedugroup == 0, 0, ifelse(UpperSecondary$Moderedugroup >= 1, 1, NA))
  UpperSecondary$severelydeprived <- ifelse(UpperSecondary$sevedugroup == 0, 0, ifelse(UpperSecondary$sevedugroup >= 1, 1, NA))
  title<-"UpperSecondary"
  generate_trees(source_folder, rvList, iv, UpperSecondary, title, e=FALSE, region="National")
  
  Household<-datause[datause$HL6<=17 & datause$crowdingMissing==0, ]
  Household$summoderpoor <- rowSums(Household[, c("modercrowdingdep", "moderatewaterdep", "moderatesanitationdeprived")], na.rm = TRUE)
  Household$sumseverpoor <- rowSums(Household[, c("severcrowdingdep", "severewaterdep", "severesanitationdeprived")], na.rm = TRUE)
  print(table(Household$summoderpoor))
  print(table(Household$sumseverpoor))
  Household$moderatelydeprived <- ifelse(Household$summoderpoor == 0, 0, ifelse(Household$summoderpoor >= 1, 1, NA))
  Household$severelydeprived <- ifelse(Household$sumseverpoor == 0, 0, ifelse(Household$sumseverpoor >= 1, 1, NA))
  title<-"Household"
  generate_trees(source_folder, rvList, iv, Household, title, e=FALSE, region="National")
  
}

data_folder<-"/home/yw/Workspace/rstudio/SDD2017/sav_download/Mongolia2018/"
hh_data<-importData(data_folder, "hh", c("HH1", "HH2", "HH48", "HC3", "HC1I", "WS1", "WS11", 
                                "HC7B", "HC9A", "HC11", "HC12", "HC13", "HH6", "HH6A",
                                "WINDEX5", "HHWEIGHT"))
ch_data<-importData(data_folder, "ch", c("HH1", "HH2", "LN", "HAZ2", "HAZFLAG", "CAGE", 
                                "IM6M1D", "IM6M1M", "IM6M1Y", "IM26", "IM2", 
                                "IM5", "IM6PENTA1D", "IM20", "IM21", "IM6PENTA2D",
                                "IM6PENTA3D", "CA16", "CA17", "CA18", "CA20", 
                                "CA21A", "CA21B", "CA21D", "CA21F", "CA21I", "CA21J",
                                "CA21K", "CA21W", "CA21P", "CA21Q", "CA21R", "CA21X"))
hl_data<-importData(data_folder, "hl", c("HH1", "HH2", "LN", "HL1", "HL6", "ED4", "ED9", "ED5A", 
                                "ED5B", "ED6", "ED10A", "HL4", "ETHNICITY", "RELIGION" ))


hh_data$HC3[is.na(hh_data$HC3)]<-0

merged_data<-mics_merge(hl_data, hh_data)
merged_data<-mics_merge(merged_data, ch_data)
hl_data<-hl_data[!is.na(hl_data$ED5A) & hl_data$ED5A<8, ]
hl_data$ED5A[hl_data$ED5A==1 & hl_data$ED5B>5]<-2
max_edu<-aggregate(hl_data$ED5A, by=list(hl_data$HH1, hl_data$HH2), FUN=max)
colnames(max_edu)<-c("HH1", "HH2", "HEducation")
merged_data<-mics_merge(merged_data, max_edu)
# print(table(merged_data$HL6))
datause<-merged_data[merged_data$HL6<=17, ]
agetab<-tabulate(merged_data$HL6)

datause<-crowdingdep(datause)
agetab2<-tabulate(datause$HL6)

datause<-waterdep(datause)
datause<-sanitationdep(datause)
datause<-stunting(datause)
datause<-vaccinedep(datause)
datause<-educationdep(datause)
datause<-vaccinedep(datause)
datause<-aridep(datause)
datause<-healthdep(datause)
datause<-informationdep(datause)
datause<-finalResult(datause)
datause<-createIndvar(datause)


source_folder<-"/home/yw/Workspace/rstudio/LNOB_Rcode/Unicef Files/"
# filename<-NULL
# region<-"National"
datause$SampleWeight<-datause$hhweight
fourComponents(datause)

iv<-c("PoorerHousehold", "Residence", "Sex" ) 
rvList<-c("crowdingMissing")
title<-"missing number of rooms for sleep"
generate_trees(source_folder, rvList, iv, datause, title, e=FALSE, region="National")


iv<-c("PoorerHousehold", "Residence", "Sex", "HighestEducation" ) 
rvList<-c("moderatelydeprived","severelydeprived")
title<-"with all sample data"
generate_trees(source_folder, rvList, iv, datause, title, e=FALSE, region="National")


datause<-datause[datause$crowdingMissing==0, ]
title<-"exclude missing data"
generate_trees(source_folder, rvList, iv, datause, title, e=FALSE, region="National")


agetab<-agetab[c(1:35)]
agePlot<-as.data.frame(cbind(Age=c(1:35), SampleCount=agetab))

library(ggplot2)
c<-ggplot(agePlot[c(1:35),], aes(Age,SampleCount)) +
   geom_point() +
   # geom_line() +
  scale_x_continuous(breaks=c(1, 5, 7, 10, 14, 15, 17, 20, 25, 30, 35)) +
   scale_color_brewer(palette="Set1") +
  ggtitle("Plot of sample count of household members by Age") 
pdf("/home/yw/Workspace/rstudio/LNOB_Rcode/Unicef Files/agecount.pdf")
print(c)
dev.off()
