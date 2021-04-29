unmet_need<-function(datause, dataList, svnm){
  unmetVarname<-dataList[dataList$IndicatorType=="UnmetNeed", ]
  
  nvar<-nrow(unmetVarname)
  # rename all related columns to Nick Name for coding convenience
  for(i in c(1:nvar)){
    if(!is.na(unmetVarname$VarName[i]) & !unmetVarname$VarName[i]=="")
    {
      colnames(datause)[which(colnames(datause) == unmetVarname$VarName[i])]<-unmetVarname$NickName[i]
    }
    
  }

  datause<-datause[datause$InterviewComplete==1, ]   # interview completed
  datause$UnmetNeed<-NA

  #CONTRACEPTIVE USERS ~ GROUP 1
  uck<-which(colnames(datause)==dataList$VarName[dataList$NickName=="ContraceptiveMethod"])
  cond0<- datause[ ,uck]==1  ### cp2 if the respondetns is using contraception
  datause$UnmetNeed[cond0]<-3    ### contraceptive method used
  fsk<-which(colnames(datause)==dataList$VarName[dataList$NickName=="ModernContra1"])
  msk<-which(colnames(datause)==dataList$VarName[dataList$NickName=="ModernContra2"])
  cond4<- trimws(datause[, fsk])=='A' | trimws(datause[ , msk])=='B' | datause$WantChild2 %in% c(2, 3)  | datause$WantChild %in% c(2, 3)  | datause$WaitN == 94
  # CP3A and CP3B sterilization for male and female
  ### sterilled, want no more children, or infecund
  datause$UnmetNeed[cond0 & cond4]<-4
  
  # PREGNANT or POSTPARTUM - group2
  tsinceb <- datause$DateInterviewed-datause$DateLastBirth  # time since last birth
  
  tsincep<-rep(NA, nrow(datause))      # time since last period 
  cond0<- (!is.na(datause$LastPeriodN) & datause$LastPeriodN>=0 & datause$LastPeriodN<=90)
  
  tsincep[datause$LastPeriodU == 1 & cond0] <- trunc(datause$LastPeriodN[datause$LastPeriodU == 1 & cond0]/30)
  tsincep[datause$LastPeriodU == 2 & cond0] <- trunc(datause$LastPeriodN[datause$LastPeriodU == 2 & cond0] /4.3)
  tsincep[datause$LastPeriodU == 3 & cond0] <- datause$LastPeriodN[datause$LastPeriodU == 3 & cond0]
  tsincep[datause$LastPeriodU == 4 & cond0] <- datause$LastPeriodN[datause$LastPeriodU == 4 & cond0] * 12
  
 pregPPA<-rep(0, nrow(datause))
 
 pregPPA[datause$PregnantNow==1 | trimws(datause$R3NoPeriod)=="C"]<-1   #pregnant now or peroid not back after last birth

#  *   if last period is before last birth in last 5 years.
  pregPPA[!is.na(tsinceb) & !is.na(tsincep) & tsincep>tsinceb & tsinceb<60 & trimws(datause$R3NoPeriod)==""]<-1
#  *   if said "before last birth" to time since last period in the last 5 years.
  pregPPA[!is.na(tsinceb) & datause$LastPeriodN==95 & tsinceb< 60 & trimws(datause$R3NoPeriod)==""] <- 1

#  * select only women who are pregnant or PPA for <24 months.
  pregPPA24<-rep(0, nrow(datause))
  pregPPA24[datause$PregnantNow==1 | (pregPPA==1 & tsinceb<24)]<-1
  
 # * Classify wantedness of current preg.nancy/last birth.
#  * current pregnancy.
  wantedlast<-rep(0, nrow(datause))
  cond0<-!is.na(datause$PregnantNow) & datause$PregnantNow==1 &!is.na(datause$WantChild)

  datause$WantChild[is.na(datause$WantChild)]<-99
  datause$WantedLater[is.na(datause$WantedLater)]<-99
  wantedlast[cond0]<-datause$WantChild[cond0]
  wantedlast[cond0 & datause$WantChild==2 & !(datause$WantedLater==9)]<- datause$WantedLater[cond0 & datause$WantChild==2 & !(datause$WantedLater==9)] +1
  wantedlast[cond0 & datause$WantChild==2 & datause$WantedLater==9]<- 9
  
#  * last birth.
  cond0<-!is.na(datause$PregnantNow) & !(datause$PregnantNow==1) & !is.na(datause$WantedLast) 

  wantedlast[cond0]<-datause$WantedLast[cond0]
  wantedlast[cond0 & datause$WantedLast==2 & !(datause$WantedLast==9) ]<-datause$WantedLast[cond0 & datause$WantedLast==2 & !(datause$WantedLast==9)] +1
  wantedlast[cond0 & datause$WantedLast==2 & datause$WantedLast==9 ]<- 9
  wantedlast[is.na(wantedlast)]<- 9
  
 cond0<- is.na(datause$UnmetNeed) & pregPPA24==1
 

 datause$UnmetNeed[cond0 & wantedlast==1]<-1   # changed here, last change
 if(svnm %in% c("Georgia2018", "Kazakhstan2010",  "Kyrgyzstan2018", "Lao2011", "Mongolia2018")) datause$UnmetNeed[cond0 & wantedlast==1]<-7
 datause$UnmetNeed[cond0 & wantedlast==2]<-1
 datause$UnmetNeed[cond0 & wantedlast==3]<-2
 datause$UnmetNeed[cond0 & wantedlast==9]<-99

 
 # print(table(cond0, datause$WantChild, datause$UnmetNeed))
 # print(table(cond0, datause$WantChild2, datause$UnmetNeed))
 # print(table(cond0, wantedlast, datause$UnmetNeed))
 
 
 # sb3u and sb3n not in the csv file
 # cond0<- is.na(datause$UnmetNeed)
 # sexact<-rep(0, nrow(datause))
 # sexact[datause$WaitU == 1 & datause$WaitN <= 30] <-1
 # sexact[datause$WaitU == 2 & datause$WaitN <= 4] <-1
 # datause$UnmetNeed[cond0 & !(datause$MSTATUS==1) & !(sexact==1)] <- 97
 
 # **DETERMINE FECUNDITY - GROUP 4.
 infec<-rep(0, nrow(datause))
 cond0<- is.na(datause$UnmetNeed)  & !(datause$PregnantNow==1) & !(pregPPA24==1) 
 
 # * married 5+ years ago, no children in past 5 years, never used contraception.
 # UN11H, UN6, UN7 is not in the csv file
 
 infec[cond0 & datause$MSTATUS==1 & datause$DateInterviewed-datause$DateMarried>=60 & (is.na(tsinceb) | tsinceb>=60) & !(datause$EverContraception==1)]<-1 
 infec[cond0 & datause$MSTATUS==1 & datause$DateInterviewed-datause$DateMarried>=60 & (is.na(tsinceb) | tsinceb>=60) & (trimws(datause$R5WantPregnant)=='E' | datause$R8TooOld=='H')]<-1
 infec[cond0 & (datause$WantChild2==3 | datause$WaitN==94)] <- 1 
 infec[cond0 & (trimws(datause$R2Menopausal)=='B' | trimws(datause$R4Hysterectomy)=='D')] <- 1
 infec[cond0 & trimws(datause$R3NoPeriod)=='C' & (is.na(tsinceb) | tsinceb>=60)] <- 1
 infec[cond0 & !is.na(tsincep) & tsincep>=6 & !pregPPA==1] <- 1
 infec[cond0 & datause$LastPeriodN==94] <- 1
 infec[cond0 & datause$LastPeriodN==96 & (is.na(tsinceb) | tsinceb>=60)] <- 1
 infec[cond0 & datause$LastPeriodN==95 & !is.na(tsinceb) & tsinceb>=60] <- 1
 infec[cond0 & datause$LastPeriodN==95 & is.na(tsinceb)] <- 1
 

 datause$UnmetNeed[cond0 & infec==1]<-9

 #  **NO NEED FOR UNMARRIED WOMEN WHO ARE NOT SEXUALLY ACTIVE.

 if(length(which(colnames(datause)=="AgeFirstSex"))>0){
     datause$UnmetNeed[datause$AgeFirstSex==0]<-99   #### no sex, no need for family planning
 }
 else  datause$UnmetNeed[is.na(datause$UnmetNeed) & cond0 & ! datause$MSTATUS==1]<-9   # not pregnant and not married
 

 
 cond0<- is.na(datause$UnmetNeed)
 

 datause$UnmetNeed[cond0 & datause$WantChild==1 & datause$WaitN %in% c(95, 96, 98)]<-1
 
 datause$UnmetNeed[cond0 & datause$WantChild==8]<-1
 
 if(svnm %in% c("Kazakhstan2010")) datause$UnmetNeed[cond0 & datause$WantChild==8]<-99
 
 
 datause$UnmetNeed[is.na(datause$UnmetNeed) & datause$WantChild==2]<-2
 
 datause$UnmetNeed[cond0 & datause$WantChild==1 & datause$WaitU==1 & datause$WaitN<24]<-7
 datause$UnmetNeed[cond0 & datause$WantChild==1 & datause$WaitU==2 & datause$WaitN<2]<-7
 datause$UnmetNeed[cond0 & datause$WantChild==1 & datause$WaitN==93]<-7
 datause$UnmetNeed[ is.na(datause$UnmetNeed) & datause$WantChild==1 & datause$WaitN<=90]<-1
 if(svnm %in% c("Kyrgyzstan2014", "Lao2011", "Mongolia2013")) datause$UnmetNeed[is.na(datause$UnmetNeed) &  datause$WantChild2 %in% c(2, 8)]<-1 

 datause$UnmetNeed[is.na(datause$UnmetNeed)]<-99

 print(table(datause$UnmetNeed))
 #print(sum(datause$SampleWeight[datause$UnmetNeed %in% c(1, 2)])/sum(datause$SampleWeight[datause$UnmetNeed %in% c(1, 2, 3, 4)]))
 print(sum(datause$SampleWeight[datause$UnmetNeed %in% c(1, 2) & datause$MSTATUS==1])/sum(datause$SampleWeight[datause$MSTATUS==1]))
 print(sum(datause$SampleWeight[datause$UnmetNeed %in% c(1, 2)])/sum(datause$SampleWeight))

 return(datause)
}










