#source_folder<-"/home/yw/Documents/project/SDD2017/"
#data_folder<-paste(source_folder,"dat_download",sep="")
#setwd(data_folder)
#load("AMCleanFuel.Rdata")
#vm<-c("Bottom40", "Residence", "HighestEducation")

################################
## 
##         sum(abs(mean_i-overall_mean)*sampleproportion)
##  D=         --------------------------------
##                       2*overall_mean
################################
cal_D<-  function(datause, vm) {
  if(length(vm)>0){
    formula_string<-paste("SampleWeight", paste(vm, collapse=" + "), sep=" ~ ")
    idv_sum<-aggregate(as.formula(formula_string), data=datause, sum)
    tmp<-datause
    tmp$SampleWeight<-tmp$SampleWeight*tmp$var2tab
    vv_sum<-aggregate(as.formula(formula_string), data=tmp, sum)
    total_sw<-sum(datause$SampleWeight)
    tab_sum<-merge(idv_sum, vv_sum, by=vm, all.x=T)
    tab_sum$SampleWeight.y[is.na(tab_sum$SampleWeight.y)]<-0
    overall_mean<-sum(datause$SampleWeight*datause$var2tab)/sum(datause$SampleWeight)
    tab_sum<-tab_sum[tab_sum$SampleWeight.x>0, ]
    if(is.na(overall_mean)) return(0)
    else if(overall_mean>0) {
      
      tab_sum$SampleP<-tab_sum$SampleWeight.x/total_sw
      tab_sum$SampleM<-tab_sum$SampleWeight.y/tab_sum$SampleWeight.x-overall_mean
      tab_sum$abs_mean<-abs(tab_sum$SampleM)* tab_sum$SampleP
      D<-sum(tab_sum$abs_mean)/(2*overall_mean)

      return(D)
    }

    else return(0)   ### when overall average is 0, D is 0
  }
  else return(0)  #### when no factor is considered, D is 0
}
#######################################################################
############### calculating the shapley value, with na and a, 
############### a is the factor 
############### na is set of the reamining factors
###############
###############                                 (n-|s|-1)! |s|!
###############     Da   <-   sum         (  -------------------[cal_D(aUs) - cal_D(s)])
###############        s: all subset (vm-a)            n!
##########################################################################
cal_Da<-function(datause, a, na){
  n<-length(na)
  nfactorial<-factorial(n+1)
  da<-0
  for(i in c(0:n)) {
    c<-combn(n, i)
    ck<-nrow(c)    # ck = |s|
    cnc<- factorial(ck)*factorial(n-ck)
    for (j in c(1:ncol(c)))
    {
    s_<-na[c[,j]]
    da1<-cal_D(datause, c(s_, a))
    da2<-cal_D(datause, s_)
    da<-da+abs((da1-da2)*cnc/nfactorial)
    }
  }
  #print(paste(a, "-D value: ", da))
  return(da)
}

cal_HOI_shapley<-function(datause, vm)
{
  Overall_Mean<-sum(datause$SampleWeight*datause$var2tab)/sum(datause$SampleWeight)
  Overall_D<-cal_D(datause, vm)
  HOI<-(1-Overall_D)*Overall_Mean
  
  result = list(
      mean = Overall_Mean,
      HOI = HOI,
      D = Overall_D
    )
  #### disable the decomposition
  # for(a in vm){
  #   na<-vm[-match(a, vm)]
  #   Da<-cal_Da(datause, a, na)
  #   Pa<-Da/Overall_D
  #   result<-c(result, a, Pa)
  # }

  return(list(Overall_D=Overall_D, HOI=HOI, drupalData=result))
}

#cal_HOI_shapley(datause, vm)
#D<-cal_D(datause, vm)
#d1<-cal_Da(datause, "Bottom40", c("Residence", "HighestEducation"))
#d2<-cal_Da(datause,"Residence" , c("Bottom40", "HighestEducation"))
#d3<-cal_Da(datause, "HighestEducation" , c("Residence", "Bottom40"))