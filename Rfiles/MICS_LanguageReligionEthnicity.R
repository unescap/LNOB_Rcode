library(data.table)
library(foreign)
source_folder <- "/home/yw/Workspace/rstudio/LNOB_Rcode/"
csv_folder<-paste(source_folder, "MICScsv/", sep="")
r_folder<-paste(source_folder, "Rfiles/", sep="")
data_folder<-"/home/yw/Workspace/rstudio/SDD2017/sav_download/"

source(paste(r_folder,"MICS_get_data.R",sep=""))

# Run this function before run_togther(.) to pre-process education variables. 
run_together_rel<-function(source_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, Flag_New=TRUE, religion=FALSE)
{
  csvfile_name<-paste(country_code, version_code, csvfile_name, sep="")
  logcsv<-paste(source_folder, "RelgionEthnicityLanguageTab.csv", sep="")
  print(paste(source_folder, csvfile_name, ".csv", sep=""))
  meta_data<-read.table(paste(source_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  rvList<-c("Religion", "Ethnicity", "Language")
  dataSet<-unique(meta_data$DataSet)
  # dataSet<-dataSet[dataSet!="DataSet"]
  dataSet<-"hh"
  
  for(rv in rvList){
  for(ds in dataSet){
    print("----------------------------------")
      print(c("dataset: ", ds, rv))
   
      dataList<-meta_data[meta_data$DataSet==ds & meta_data$NickName==rv, ]
      print(nrow(dataList))
      if(nrow(dataList)==0) print(paste(rv, "does not exist in", csvfile_name))
      else if ( dataList$VarName=="" | dataList$VarName=="NA" | is.na(dataList$VarName)) print(paste(rv, "does not exist in the data"))
      else {
         df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName))
         if(!is.null(df)) {
           print("·································")
           print(c(country_code, version_code, ds, rv))
           df<-as.numeric(as.character(df))
           # df[is.na(df)]<-999
           tabdata0<-as.data.frame(table(df)/length(df)*100)
           tabdata0<-tabdata0[tabdata0[, 2]>0, ]
           total<-sum(tabdata0[, 2])
           tabdata0<-tabdata0[order(-tabdata0[, 2]), ]
           tabdata<-cbind(surveyname=paste(country_code, version_code, sep=""), varName=rv, 
                          DataType=c("Code", "PctByCount"), Total=c("Total", total),
                          t(tabdata0))

           if(file.exists(logcsv)){
             write.table(paste("---------- tabulation on ", country_code, version_code, rv), logcsv, sep=",",
                         append = TRUE,   col.names = F, row.names = F)
             write.table(tabdata, logcsv, sep=",",
                         append = TRUE,   col.names = F, row.names = F)
           }
           else {

             write.table(tabdata, logcsv, sep=",",
                         append = FALSE,   col.names = T, row.names = F)
           }
           df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName), la=TRUE)
           tabdata0<-as.data.frame(table(df)/length(df)*100)
           print(tabdata0[,2])
           tabdata0<-tabdata0[tabdata0[, 2]>0, ]
           total<-sum(tabdata0[, 2])
           tabdata0<-tabdata0[order(-tabdata0[, 2]), ]
           tabdata<-cbind(surveyname=paste(country_code, version_code, sep=""), varName=rv, 
                          DataType=c("Name", "PctByCount"), Total=c("Total", total),
                          t(tabdata0))
           
           write.table(tabdata, logcsv, sep=",",
                       append = TRUE,   col.names = F, row.names = F)

         }
         
      }
    }
  }
}


csvfile_name<-"MICS"
run_together_rel(csv_folder, data_folder, output_folder,"Afghanistan",	"2010",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Bhutan",	"2010",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Kazakhstan",	"2010",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Kiribati", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Kyrgyzstan",	"2014",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Lao",	"2011",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Mongolia",	"2013",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Nepal", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Thailand",	"2012",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Thailand", "2015",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Tuvalu",	"2019",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder,"Vietnam",	"2010",  csvfile_name)
run_together_rel(csv_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name)



# checking water code
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=TRUE)
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=FALSE)

# checking internet code
# df<-importMICSdata(data_folder,  "Kiribati", "2019", "hh", "WS4", la=TRUE)
# print(table(df)/length(df))
# print(table(df))
# df<-importMICSdata(data_folder,  "Kiribati", "2019", "hh", "WS4", la=FALSE)
# print(table(df))


# df<-importMICSdata(data_folder,  "Tonga", "2019", "wm", "DVD5C", la=TRUE)
# print(table(df)/length(df))
# print(table(df))
# df<-importMICSdata(data_folder,  "Tonga", "2019", "wm", "DVD5C", la=FALSE)
# print(table(df))
