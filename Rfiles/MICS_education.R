library(data.table)
library(foreign)


csv_folder<-paste(source_folder, "MICScsv/", sep="")
r_folder<-paste(source_folder, "Rfiles/", sep="")

source(paste(r_folder,"MICS_get_data.R",sep=""))
# Run this function before run_togther(.) to pre-process education variables. 
run_together_edu<-function(source_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, Flag_New=TRUE, religion=FALSE)
{
  csvfile_name<-paste(country_code, version_code, csvfile_name, sep="")
  
  meta_data<-read.table(paste(source_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  dataSet<-unique(meta_data$DataSet)
  dataSet<-c("hl", "wm", "ch")

  
  for(ds in dataSet){
    print(ds)
    if (ds=="ch") nn<-"MotherEducation"
    else nn<-"Education"

    dataList<-meta_data[meta_data$DataSet==ds, ]

    df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName))

    colnames(df)<-toupper(colnames(df))
    vn<-toupper(dataList$VarName[dataList$NickName== nn])
    print(vn)
    vk<-match(vn, colnames(df))
    print(table(df[, vk]))
    if(ds=="hl"){
    gn<- toupper(dataList$VarName[dataList$NickName=="Grade"])
    print(gn)
    gk<-match(gn, colnames(df))
    print(table(df[, vk], df[, gk]))
    }

    
    df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName), la=TRUE)
    colnames(df)<-toupper(colnames(df))
    vn<-toupper(dataList$VarName[dataList$NickName==nn])
    vk<-match(vn, colnames(df))
    print(table(df[, vk]))
    if(ds=="hl"){
      gn<- dataList$VarName[dataList$NickName=="Grade"]
      gk<-match(gn, colnames(df))
      print(table(df[, vk], df[, gk]))
    }
  }
}

csvfile_name<-"MICS"
# run_together_edu(source_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Kazakhstan", "2010",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Thailand", "2012",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Kyrgyzstan", "2014",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Mongolia", "2013",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Vietnam", "2010",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name)

# run_together_edu(source_folder, data_folder, output_folder, "Turkmenistan", "2015",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Kiribati", "2019",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name)
# run_together_edu(source_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name)

# run_together_edu(source_folder, data_folder, output_folder, "Nepal", "2019",  csvfile_name)
# checking water code
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=TRUE)
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=FALSE)

# checking internet code
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "HC8", la=TRUE)
# print(table(df)/length(df))
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "HC8", la=FALSE)
# print(table(df))

# checking woman burth date
# df<-importMICSdata(data_folder,  "Lao", "2017", "hh", "HH6A", la=FALSE)
# print(table(df))
# df<-importMICSdata(data_folder, "Lao", "2017", "hh", "HH6A", la=TRUE)
# print(table(df))
# print(table(df)/length(df))

# 
# df<-importMICSdata(data_folder,  "Kyrgyzstan", "2014", "wm", "cage", la=FALSE)
# print(table(df))
# df<-importMICSdata(data_folder, "Kyrgyzstan", "2014", "wm", "cage", la=TRUE)
# print(table(df))
# print(table(df)/length(df))

 
 # checking water code
 df<-importMICSdata(data_folder,  "Kyrgyzstan", "2018", "hh", "WS11", la=TRUE)
 print(table(df))
 df<-importMICSdata(data_folder,  "Kyrgyzstan", "2018","hh", "WS11", la=FALSE)
 print(table(df))
 
 
 
 df<-importMICSdata(data_folder,   "Afghanistan", "2010", "hh", "WS4", la=TRUE)
 print(table(df))
 df<-importMICSdata(data_folder,   "Afghanistan", "2010", "hh", "WS4", la=FALSE)
 print(table(df))
 
 
# 
# # run_together_edu(csv_folder, data_folder, output_folder, "Afghanistan", "2010",  csvfile_name)
# run_together_edu(csv_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name)