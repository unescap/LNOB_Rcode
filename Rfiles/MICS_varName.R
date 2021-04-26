#####################################################################################

# This R-code file use package foreign to read in SPSS data files .sav, reorganize  #
# data and variable names 
# it then create a data file that stores the variable names and description
# It them looks for useful variable names
#
# I hope the end result is a csv file that can be used as the MICSstandard.csv
# but it is more survey specific

library(data.table)
library(foreign)

csv_folder<-paste(source_folder, "MICScsv/", sep="")
csvfile_name<-"MICS_Var_Search_keys"
#csvfile_name<-"MICS_NewKeys_covid"
get_label_data<-function(data_folder, country_code, year_code, data_type){
  savfilename<-paste(data_folder, 
            paste(country_code, year_code, sep=""), 
            paste(data_type, "sav", sep="."), sep="/")

  if(file.exists(savfilename)){ # check if the dataset is available
  
  df<-read.spss(savfilename, use.value.labels = TRUE, to.data.frame = TRUE)

  dfattr<-attr(df, "variable.labels")
  #print(attr(df, "table.labels"))
  dfnames<-data.frame(cbind(names(dfattr), dfattr))
  dfnames$dfattr<-tolower(dfnames$dfattr)
  write.table(dfnames, paste(data_folder, 
                             paste(country_code, year_code, sep=""), 
                             paste(data_type, "_header.csv", sep=""), sep="/"))
  return(dfnames)
  }
  else return(NULL)
}

# to deal with cases when mother education is not well labeled in the data header
mthedu<-function(var_search, dfnames){
  me_exist<-dfnames[grepl("MELEVEL", toupper(dfnames$V1)), ]
  if(nrow(me_exist)>0){
    var_search$VarName[var_search$NickName=="MotherEducation"]<-"MELEVEL"
    var_search$Search_result[var_search$NickName=="MotherEducation"]<-"1 var names found"
  }  
  return(var_search)
}

keysearch<-function(var_search, dfnames){
  var_search$VarName<-var_search$UsualVar
  var_search$Key1<-trimws(tolower(var_search$Key1))
  var_search$Key2<-trimws(tolower(var_search$Key2))
  var_search$Key3<-trimws(tolower(var_search$Key3))
  var_search$Search_result<-NA
  dfnames$dfattr<-tolower(dfnames$dfattr)
  n<-nrow(var_search)
  for(i in c(1:n)){
    if(var_search$Default[i]=="no")
    {
      if(var_search$Type[i]=="Only"){
        vnames<-dfnames$V1[dfnames$dfattr==var_search$Key1[i]]
      }
      else  if(var_search$Type[i]=="and") {
        c1<-grepl(var_search$Key1[i], dfnames$dfattr)
        if(!is.na(var_search$Key2[i]))
          c1<- c1 & grepl(var_search$Key2[i], dfnames$dfattr)
        if(!is.na(var_search$Key3[i]))
          c1<- c1 & grepl(var_search$Key3[i], dfnames$dfattr)
        vnames<-dfnames$V1[c1]
      }
      if(var_search$Type[i]=="or23") {
        c1<-grepl(var_search$Key1[i], dfnames$dfattr)
        if(!is.na(var_search$Key2[i]))
          c2<- c1 & grepl(var_search$Key2[i], dfnames$dfattr)
        if(!is.na(var_search$Key3[i]))
          c3<- c1 & grepl(var_search$Key3[i], dfnames$dfattr)
        vnames<-dfnames$V1[c2|c3]
      }
      if(var_search$Type[i]=="only12") {
        c1<-(dfnames$dfattr==var_search$Key1[i])
        c2<-(dfnames$dfattr==var_search$Key2[i])
        vnames<-dfnames$V1[c2|c1]
      }
      k<-length(vnames)
      if(k>1){
        var_search$VarName[i]<-paste(toupper(vnames), collapse=",")
      }
      else if(k==1)
        var_search$VarName[i]<-toupper(as.character(vnames))
      else var_search$VarName[i]<-NA
      
      var_search$Search_result[i]<-paste(k, "var names found", sep=" ")
    }
    else if(var_search$Default[i]=="no keywards" ){
      var_search$Search_result[i]<-"NoKeywords"
    }
    else {
      var_search$Search_result[i]<-"Default"
    }  
    
  }
  
  cn1<-var_search$Search_result %in% c("0 var names found", "1 var names found", "Default", "Default", "NoKeywords")
  cn2<-cn1
  n<-nrow(var_search)
  for(i in c(1:n))
    cn2[i]<- grepl(toupper(var_search$UsualVar[i]), var_search$VarName[i])
  var_search$VarName[cn2 & !cn1]<-var_search$UsualVar[cn2 & !cn1]
  var_search$Search_result[cn2 & !cn1]<-"More than one columns, usual name chosen"

  
  return(var_search)
  
}

attributesMICSdata<-function(data_folder, country_code, year_code, csv_folder, csvfile_name) {
  
   varName<-read.table(paste(csv_folder, csvfile_name, ".csv", sep=""), sep=",", header=T, colClasses="character")
   varName<-varName[!is.na(varName$NickName), ]
   ds<-unique(varName$DataSet)

   survey_name<-paste(country_code, year_code, sep="")
   print("============================")
   print(survey_name)
   # ds<-c("hl")
   for(data_type in ds){
     df<-get_label_data(data_folder, country_code, year_code, data_type)
     if(!is.null(df)){
      var_datatype<-varName[varName$DataSet==data_type, ]
      var_search<-keysearch(var_datatype, df)
      if(data_type=="ch") var_search<-mthedu(var_search, df)
      csvfilename<-paste(csv_folder, survey_name, "MICS.csv", sep="")
      write.table(var_search, csvfilename, sep=",", col.names = TRUE  , row.names = FALSE, append = TRUE)
    # print(as.data.frame(table(var_search$Search_result)))
    print("============================")
    print(paste(data_type, "has " , nrow(var_search[var_search$Search_result=="0 var names found", ]), "varname not found"))
    print("============================")
    if (nrow(var_search[var_search$Search_result=="0 var names found", ])>0)
       print(as.data.frame(table(var_search$IndicatorType[var_search$Search_result=="0 var names found"])))
    else print("all is well")
    print("============================")
    print(var_search$NickName[!is.na(var_search$Search_result) & var_search$Search_result=="0 var names found"])
     }
     else print(paste(data_type, "data not found", sep=" "))
  }
}

 # attributesMICSdata(data_folder, "Bangladesh", "2019", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Bhutan", "2010", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Georgia", "2018", csv_folder, csvfile_name) 
 # attributesMICSdata(data_folder, "Kiribati", "2019", csv_folder, csvfile_name) 
 # attributesMICSdata(data_folder, "Kazakhstan", "2015", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Kyrgyzstan", "2018", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Lao", "2017", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Mongolia", "2018", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Uzbekistan", "2006", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Vanuatu", "2007", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "VietNam", "2013", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Thailand", "2019", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Tonga", "2019", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Turkmenistan", "2019", csv_folder, csvfile_name)

 # attributesMICSdata(data_folder, "Kyrgyzstan", "2014", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder,  "Turkmenistan", "2015", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder,   "Lao", "2011", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder,  "Thailand", "2012", csv_folder, csvfile_name)
 # attributesMICSdata(data_folder, "Vietnam", "2010", csv_folder, csvfile_name)
 
# attributesMICSdata(data_folder, "Kazakhstan", "2010", csv_folder, csvfile_name)
# attributesMICSdata(data_folder, "Mongolia", "2013", csv_folder, csvfile_name)
 
attributesMICSdata(data_folder, "Afghanistan", "2010", csv_folder, csvfile_name)
attributesMICSdata(data_folder,  "Thailand", "2015", csv_folder, csvfile_name)
 
#attributesMICSdata(data_folder, "Nepal", "2019", csv_folder, csvfile_name)

# sink(paste(csv_folder, "MICS_all_VarNamesCheck.txt", sep = ""))
# country_code<-c( "Afghanistan", "Bhutan", "Georgia", "Kazakhstan", "Kazakhstan",
#                  "Kyrgyzstan",   "Kyrgyzstan", "Lao", "Lao",  "Mongolia", 
#                  "Mongolia", "Thailand", "Thailand", "Turkmenistan", "Uzbekistan",
#                  "Vanuatu", "Vietnam", "VietNam", "Gambia")
# year_code<-c("2010", "2010", "2018",  "2010", "2015",
#              "2014", "2018", "2011", "2017",  "2013", 
#              "2018", "2012", "2015", "2015", "2006",
#              "2007", "2010", "2013", "2018")
# 
# for(i in c(7)){   # for handwashing, 
#     print(i)
#     attributesMICSdata(data_folder, country_code[i], year_code[i], csv_folder, csvfile_name)
#   }
# sink()


