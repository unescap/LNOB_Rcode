importData<-function(data_folder, dt, meta_data){
  # data_folder where the mics data is stored
  # dt is name of data set from MICS, can be hh, hl, ch, wm, mn
  # meta_data provided by importing a csv file
  
  # ecd: encoding, different for wm
  if(dt!="wm") ecd<-NULL
  else ecd<- 'latin1'
    
  df<-read_sav(paste(data_folder, dt, ".sav", sep=""), encoding = ecd, user_na = TRUE)

  dataList<-meta_data[meta_data$DataSet==dt,]
  varList<-unique(toupper(dataList$VarName))

  col_index<-match(varList, toupper(colnames(df)))
  col_index<-col_index[!is.na(col_index)]
  
  df<-df[ , col_index]
  
  if(dt=="hl") {
    k_hl<-match("HL1", colnames(df), nomatch = 0)
    if(k_hl==0) {
      print("-HL1- not found in imported hl data")
      return(NULL)
    }
    else colnames(df)[k_hl]<-"LN"
  }
  k_cn<-length(colnames(df))
  for(i in c(1:k_cn)){
    if(!(colnames(df)[i] %in% c("HH1", "HH2", "LN")))
      colnames(df)[i]<-paste(colnames(df)[i], dt, sep=".")
  }
  print(colnames(df))
  return(df)
}

mics_merge<-function(base_data, adding_data){
  k_ln<-match("LN", colnames(adding_data), nomatch = 0)
  if(k_ln==0) by_var<-c("HH1", "HH2")
    else by_var<-c("HH1", "HH2", "LN")
  
    Mergedfile <- merge(base_data, adding_data, by= by_var, all.x = TRUE) 
    return(Mergedfile)
}

crowding_dep<-function(){
  # severcrowdingdep
  # modercrowdingdep
}

water_dep<-function(){
  # severewaterdep
  # moderatewaterdep
}

sanitation_dep<-function(){
  # severesanitationdeprived
  # moderatesanitationdeprived
}

under5health_dep<-function(){
  # severestunting
  # moderatestunting
}

education_dep<-function(){
  
  # severeducdepbelow15
  # severeducdepbelow15a
  # severeducdepbelow15b
  
  Children_Masterfile1$severeducdepbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4.x ==1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED4.x ==2, 1, NA))
  Children_Masterfile1$severeducdepbelow15a <- ifelse(Children_Masterfile1$ED4.x == 2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeducdepbelow15)
  Children_Masterfile1$severeducdepbelow15b <- ifelse(Children_Masterfile1$ED4.x == 1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeducdepbelow15)
  
  
  Children_Masterfile1$moderateedudeprivbelow15 <- Children_Masterfile1$severeducdepbelow15b
  Children_Masterfile1$moderateedudeprivbelow15 <- ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 1, 0, ifelse((Children_Masterfile1$HL6>=7 & Children_Masterfile1$HL6<= 14) & Children_Masterfile1$ED9 == 2, 1, Children_Masterfile1$severeducdepbelow15b))
  Children_Masterfile1$moderateedudeprivbelow15a <- ifelse(Children_Masterfile1$ED4.x==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$moderateedudeprivbelow15)
  
  Children_Masterfile1$severeedudeprived15older <- ifelse(((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4.x==1)) | ((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED9==1)), 0, ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & (Children_Masterfile1$ED4.x==2), 1, NA))
  #Please keep in mind that there's no direct relationship between ED5A and ED6; 
  #we use here simply to say that children who have not completed any grade within 
  #primary or less are severely deprived
  Children_Masterfile1$severeedudeprived15olderA <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED5A.x <= 1 & Children_Masterfile1$ED6.x==2 & Children_Masterfile1$ED9==2, 1, Children_Masterfile1$severeedudeprived15older)
  Children_Masterfile1$severeedudeprived15olderB <- ifelse((Children_Masterfile1$HL6>=15 & Children_Masterfile1$HL6<=17) & Children_Masterfile1$ED9 == 2 & (Children_Masterfile1$ED6.x==1|Children_Masterfile1$ED6.x==8) & Children_Masterfile1$ED5A.x==1 & Children_Masterfile1$ED5B.x<6, 1, Children_Masterfile1$severeedudeprived15olderA)
  Children_Masterfile1$severeedudeprived15olderC <- ifelse(Children_Masterfile1$ED4.x==2 & Children_Masterfile1$ED9==1, NA, Children_Masterfile1$severeedudeprived15olderB)
  Children_Masterfile1$severeedudeprived15olderD <- ifelse(Children_Masterfile1$ED4.x==1 & Children_Masterfile1$ED9==9, NA, Children_Masterfile1$severeedudeprived15olderC)
  
  
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
  
}

vaccine_dep<-function(){
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
  
}

ari_dep<-function(){
  
  Children_Masterfile1$arisymptoms <- ifelse(Children_Masterfile1$CAGE>=36 & Children_Masterfile1$CAGE>=59 & Children_Masterfile1$CA16==1 & Children_Masterfile1$CA17==1 & Children_Masterfile1$CA18==1 | Children_Masterfile1$CA18==3, 1, NA)
  Children_Masterfile1$ariseverlydeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==2, 1, ifelse(Children_Masterfile1$arisymptoms==1 & Children_Masterfile1$CA20==1, 0, NA))
  
  Children_Masterfile1$arimoderatedeprived <- Children_Masterfile1$ariseverlydeprived
  Children_Masterfile1$arimoderatedeprived <- ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21A=="A"|Children_Masterfile1$CA21B=="B"|Children_Masterfile1$CA21D=="D"|Children_Masterfile1$CA21F=="F"|Children_Masterfile1$CA21I=="I"|Children_Masterfile1$CA21J=="J"|Children_Masterfile1$CA21K=="K"|Children_Masterfile1$CA21W=="W"), 0, ifelse(Children_Masterfile1$arisymptoms==1 & (Children_Masterfile1$CA21P=="P"|Children_Masterfile1$CA21Q=="Q"|Children_Masterfile1$CA21R=="R"|Children_Masterfile1$CA21X=="X"), 1, NA))
  
}

health_dep<-function(){
  
  Children_Masterfile1$hasmissevhealth <- rowSums(is.na(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")]), na.rm = TRUE)
  Children_Masterfile1$sumsevhealth <- rowSums(Children_Masterfile1[,c("severevaccinesdeprived", "ariseverlydeprived")], na.rm = TRUE)
  Children_Masterfile1$sumsevhealth <- ifelse(is.na(Children_Masterfile1$severevaccinesdeprived) & is.na(Children_Masterfile1$ariseverlydeprived), NA, Children_Masterfile1$sumsevhealth)
  Children_Masterfile1$severehealth <- ifelse(Children_Masterfile1$sumsevhealth == 1, 1, ifelse(Children_Masterfile1$sumsevhealth == 0, 0, NA))
  
  Children_Masterfile1$hasmissmoderhealth <- rowSums(is.na(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")]), na.rm = TRUE)
  Children_Masterfile1$summoderhealth <- rowSums(Children_Masterfile1[,c("moderatevaccinesdeprived", "arimoderatedeprived")], na.rm = TRUE)
  Children_Masterfile1$summoderhealth <- ifelse(is.na(Children_Masterfile1$moderatevaccinesdeprived) & is.na(Children_Masterfile1$arimoderatedeprived), NA, Children_Masterfile1$summoderhealth)
  Children_Masterfile1$moderatehealth <- ifelse(Children_Masterfile1$summoderhealth >= 1, 1, ifelse(Children_Masterfile1$summoderhealth == 0, 0, NA))
  
}

information_dep<-function(){
  
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
  
}