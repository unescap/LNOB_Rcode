library(data.table)
library(foreign)

source(paste(source_folder,"MICS_get_data.R",sep=""))

# Run this function before run_togther(.) to pre-process education variables. 
run_together_rel<-function(source_folder, data_folder, output_folder, country_code, version_code,  csvfile_name, Flag_New=TRUE, religion=FALSE)
{
  csvfile_name<-paste(country_code, version_code, csvfile_name, sep="")
  
  meta_data<-read.table(paste(source_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
  rvList<-c("Religion", "Ethnicity", "Language")
  dataSet<-unique(meta_data$DataSet)
  dataSet<-dataSet[dataSet!="DataSet"]

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
         if(!is.null(df)) print(table(df)/length(df)*100)
         df<-importMICSdata(data_folder, country_code, version_code, ds, unique(dataList$VarName), la=TRUE)
         if(!is.null(df)) print(table(df)/length(df)*100)
      }
    }
  }
}

csvfile_name<-"MICS"


run_together_rel(source_folder, data_folder, output_folder, "Bhutan", "2010",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name)

# run_together_rel(source_folder, data_folder, output_folder, "Nepal", "2019",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Georgia", "2018",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Kazakhstan", "2015",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Kyrgyzstan", "2018",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Lao", "2017",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Mongolia", "2018",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Tonga", "2019",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Kiribati", "2019",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Thailand", "2019",  csvfile_name)
# run_together_rel(source_folder, data_folder, output_folder, "Bangladesh", "2019",  csvfile_name)

# run_together_rel(source_folder, data_folder, output_folder, "Thailand", "2015",  csvfile_name)

# run_together_rel(source_folder, data_folder, output_folder, "VietNam", "2013",  csvfile_name)

# run_together_rel(source_folder, data_folder, output_folder, "Turkmenistan", "2019",  csvfile_name)

# checking water code
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=TRUE)
# df<-importMICSdata(data_folder,  "Turkmenistan", "2019", "hh", "WS1", la=FALSE)

# checking internet code
# df<-importMICSdata(data_folder,  "Kiribati", "2019", "hh", "WS4", la=TRUE)
# print(table(df)/length(df))
# print(table(df))
# df<-importMICSdata(data_folder,  "Kiribati", "2019", "hh", "WS4", la=FALSE)
# print(table(df))





