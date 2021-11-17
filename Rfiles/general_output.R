#### output functions
#### All things related to output

#### generating data ready for publication
# drupal_data<-function()

#### in use_version 3, checking if the overall level is within 1% of the validated results
output_data<-function(datause, survey_source, country_code, version_code, country_ISO, year_code, rv, 
                      rtp, indvar, ds, ds_output_folder, validationdata, 
                      religion, region_flag, use_version, survey_version, drupalIndex)
  {


  #### to validate if this is needed
  print("########### generating output now #############")
  ### initializing the output file to be written
  result_log_National<-ResultList(survey_source, country_code, version_code, country_ISO, year_code, ds, rv, religion, "National", !is.null(datause))

  overallmean<-write_value(datause, country_code, version_code, rv, ds, ds_output_folder)
  validation<-FALSE
  if(use_version>=2){

    validation_result<-validate(country_code, version_code, rv, overallmean, validationdata)
    validation<-validation_result$validation
    result_log_National$NationalValidated=validation

    result_log_National$ValidationResult=validation_result$validationResult
    result_log_National$NationalmeanYFromValidation=validation_result$mean
  }
  
  result_log_National$NationalmeanYfromR<-overallmean

  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  title_string<-paste(rv, paste(indvar, collapse=" + "), sep=" ~ ")

  #### tree
  #### add a data type parameter, if numeric, we use a different criterion
  sub_string<-NULL
  if(use_version==1 | (use_version==3 & validation)){
    
    if(region_flag){
      regionList<-unique(datause$REGION)
    } else regionList<-c("National")


    NewLNOBcsv<-paste(ds_output_folder, "NewLNOBdata.csv", sep="")
    for(rg in regionList){
      print(rg)
      if(rg=="National") {
        datauseRG<-datause
        logcsv<-paste(ds_output_folder, "NationalLogfile.csv", sep="")
      }
      else {
        datauseRG<-datause[datause$REGION==rg, ]
        logcsv<-paste(ds_output_folder, "RegionalLogfile.csv", sep="")
      }
      
      result_log<-result_log_National
      result_log$RegionName<-rg


      result_log$formula<-formula_string
      SampleSize<-sum(datauseRG$SampleWeight)
      result_log$SampleSize<-SampleSize

      SampleMean<-sum(datauseRG$SampleWeight[datauseRG$var2tab==1]) / sum(datauseRG$SampleWeight)
      result_log$SampleMean<-SampleMean
      
      t0<-drupalIndex


      # 
      #   newLNOBdata<-newLNOBList(survey_source, ds, country_code, country_ISO, rg, version_code, year_code, religion)
      #   newLNOBdata<-t(unlist(updateLNOBdata(newLNOBdata, rg, validation, title_string,
      #                                        SampleMean, SampleSize, "No Analysis", NULL, NULL)))
      #   result_log$DindexFileID="NoFILE"
      #   result_log$d_index<-NA
      #   result_log$HOI<-NA
      #   result_log$TreeFileID="NoFILE"
      #   result_log$tree_stat<-NULL
      # }
      # else {
      
      if(SampleMean<0.99 & SampleMean>0.01){
            tree_result<-write_tree(survey_source, datauseRG, country_ISO, year_code, rg, 
                              formula_string, title_string,  sub_string, rv, rtp, 
                              religion, ds_output_folder, ds, filename, use_version, drupalIndex)
  
             drupalIndex<-c(tree_result$drupalIndex)
      }
      else tree_result<-NULL
      
       if(drupalIndex>t0  | use_version==1)  {      
          result_log$TreeFileID=paste("R", t0, sep="")
          result_log$tree_stat<-c(tree_result$tree_stat)
        }
        else {
          result_log$TreeFileID="NoFILE"
          result_log$tree_stat<-NULL
        }
        #### disable D and Logistic for validation
        #### HOI and dis-similarity index calculation
        #### not sure if this works for numeric
        t0<-drupalIndex

        d_result<-write_HOI_D(survey_source, datauseRG, country_ISO, year_code, rg, rv, ds, religion, indvar, 
                            ds_output_folder, filename, use_version, drupalIndex)

        drupalIndex<-c(d_result$drupalIndex)

        if(drupalIndex>t0 | use_version==1) {       
          result_log$DindexFileID=paste("R", t0, sep="")
          result_log$d_index<-c(d_result$Overall_D)
          result_log$HOI<-c(d_result$HOI)
        }
        else {
          result_log$DindexFileID="NoFILE"
          result_log$d_index<-NA
          result_log$HOI<-NA
        }
      
        result_log<-t(unlist(result_log)) 
        newLNOBdata<-newLNOBList(survey_source, ds, country_code, country_ISO, rg, version_code, year_code, survey_version, rv, religion)
        newLNOBdata<-t(unlist(updateLNOBdata(newLNOBdata, rg, validation, title_string, 
                                           SampleMean, SampleSize, d_result$Overall_D, tree_result$tree_stat)))
      # }
        if(file.exists(logcsv)){
          write.table(result_log, logcsv, sep=",", 
                      append = TRUE,   col.names = F, row.names = F)
        }
        else {
          write.table(result_log, logcsv, sep=",", 
                      append = FALSE,   col.names = T, row.names = F)
        }
        if(file.exists(NewLNOBcsv)){
          write.table(newLNOBdata, NewLNOBcsv, sep=",", 
                    append = TRUE,   col.names = F, row.names = F)
        }
        else {
        write.table(newLNOBdata, NewLNOBcsv, sep=",", 
                    append = FALSE,   col.names = T, row.names = F)
        }
    }
    return(drupalIndex)
  }
  # else if(use_version==4 & validation){
  #   # LCA analysis
  # }
  else {
    result_log_National<-t(result_log_National)
    logcsv<-paste(ds_output_folder, "noAnalysislogfile.csv", sep="")
    if(file.exists(logcsv))
      write.table(result_log_National, logcsv, sep=",", 
                  append = TRUE,   col.names = F, row.names = F)
    else write.table(result_log_National, logcsv, sep=",", 
                     append = FALSE,   col.names = T, row.names = F)
    return(drupalIndex)
  }

}



validate<-function(country_code, version_code, rv, overallmean, validationdata){
  #### using original mean value from orlando
  y<-validationdata$MeanY[validationdata$country_code==country_code & validationdata$version_code==version_code & validationdata$IndicatorName==rv]
  y<-as.numeric(as.character(y))
  print("y value from validated file")
  print(y)
  if(overallmean=="DataNotGenerated") {
    print("############# validation failed, data not generated #################")
    return(list(validation=FALSE, mean=y, validationResult="DataNotGenerated"))
  }
  else {
    if(length(y)>0){
      if(is.na(y)){
        print("############# validation failed, validated value not found #################")
        return(list(validation=FALSE, mean=NA, validationResult="NoValidationData"))
      }
      else {
        if(abs(overallmean-y)>0.01){
          print("############# validation failed, difference > 0.01 #################")
          return(list(validation=FALSE, mean=y, validationResult="DifferenceGT0.01"))
        }
        else {
          print("############# validation succeeded, on to the trres and D #################")
          return(list(validation=TRUE, mean=y, validationResult="Success"))
        }
      }
    }
    else return(list(validation=FALSE, mean=y, validationResult="Failed"))
  } 
}

dataForDrupal<-function(data, type, survey_type, ds, title, formula, country_code, version_code, rv, region, moderation_state=NULL){
  drupal_data = list(
    type = type,
    field_survey_type = survey_type,
    field_dataset = ds,
    title = paste(title, V, sep="---"),   ### V is the version, a global variable specified in Config_ywdrupal.R
    field_geo = country_code,
    field_year = version_code,
    field_indicator = rv,
    field_data = toString(data)
    # field_region = region,
    # moderation_state = moderation_state
  )
  if(!(region=="National")) drupal_data[["field_region"]] = region
  return(drupal_data)
  
}


write_value<-function(datause, country_code, version_code, rv,  ds, ds_output_folder){
  
  surveyID<-paste(country_ISO(country_code), version_code, sep="")
  SurveyIndicator<-paste(surveyID, rv, sep="+")
  if(is.null(datause)) overallMean<- "DataNotGenerated" 
  else overallMean<-sum(datause$var2tab*datause$SampleWeight)/sum(datause$SampleWeight)
  
  results<-data.frame(surveyID=surveyID, country_code=country_code, version_code=version_code, 
                      dataset=ds, SurveyIndicator=SurveyIndicator, IndicatorName=rv, MeanY=overallMean)
  writefile<-paste(ds_output_folder, "overallmean.csv", sep="")
  if(file.exists(writefile)) 
    catch_error(write.table(results, writefile,
                            sep=",", append = TRUE,   col.names = F, row.names = F))
  else catch_error(write.table(results, writefile,
                               sep=",", append = F,   col.names = T, row.names = F))
  return(overallMean)
}


write_tree <- function(survey_source, datause, country_code, version_code, region, 
                        formula_string, title_string, sub_string, 
                       rv, rtp, religion, output_folder, ds, filename, use_version, drupalIndex) {
  
  tree_stat<- catch_error(build_tree(output_folder, country_code, version_code, datause, rv, rtp, 
                                     formula_string, title_string, sub_string, filename, e = religion, 
                                     region))
  if(!is.null(tree_stat$tree_stat))
  {
    ### for version 3, we store the data in one folder for publication

    if(use_version==3){
      type<- ifelse(region=="National",  "tree_data", "region_tree_data")
      title_string<-paste(country_code, version_code, rv, region, ifelse(religion, "Religion", "NoReligion"), sep="-")
      drupal_data<-dataForDrupal(toString(toJSON(tree_stat$data2, flatten = TRUE)), type, survey_source, ds, title_string, formula_string, 
                                 country_code, version_code, rv, region)
      
      rdsname<-paste(paste("R", drupalIndex, sep=""), "rds", sep=".")
      wd_datatype(drupal_folder, type)
      saveRDS(drupal_data, file = rdsname)
      drupalIndex<-drupalIndex+1
    }
    tree_stat<-c(tree_stat$tree_stat)
    return(list(drupalIndex=drupalIndex, tree_stat=tree_stat)) 
  }  
 
  else return(list(drupalIndex=drupalIndex, tree_stat=NULL))
}


write_HOI_D <- function(survey_source, datause, country_code, version_code, region, rv, ds, religion,
                        indvar, output_folder, filename, use_version, drupalIndex) {
  
  # Calculate 
  pass_message <- "Successfully calculated HOI and D"

  # catch_error does not seem to work
  # result<-catch_error(cal_HOI_shapley(datause, indvar))
  
  result<-cal_HOI_shapley(datause, indvar)
  if(use_version==3 & !is.null(result))
  {
    title_string<-paste(country_code, version_code, rv, region, ifelse(religion, "Religion", "NoReligion"), sep="-")
    ### for version 3, we store the data in one folder for publication
    type<-ifelse(region=="National", "d_index", "region_d_index")
    drupal_data<-dataForDrupal(toJSON(result$drupalData, auto_unbox = TRUE), type, survey_source, ds, title_string, formula_string, 
                                 country_code, version_code, rv, region)
      

    wd_datatype(drupal_folder, type)
    rdsname<-paste(paste("R", drupalIndex, sep=""), "rds", sep=".")
    saveRDS(drupal_data, file = rdsname)
    
    drupalIndex<-drupalIndex+1
    
  }

    return(list(drupalIndex=drupalIndex, Overall_D=result$Overall_D, HOI=result$HOI))
}

write_glm <- function(survey_source, datause, rtp, country_code, version_code, rv, ds, title_string, 
                      indvar, output_folder, filename, use_version, drupalIndex) {
  
  # Build Logistic Regression model 
  pass_message <- "Successfully built glm"
  # catch_error does not seem to workk here 
  
  for(ivm in indvar) {
    
    if(!(ivm=="NUnder5")) datause[ , ivm]<-factor(datause[ , ivm], ordered = F)
  }
  
  title_string<-paste(rv, paste(indvar, collapse=" + "), sep=" ~ ")
  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  
  s.glm <- catch_error_prod(logistic(datause, rtp, formula_string, filename))
  if (!is.null(s.glm)) { 
    info(logger, paste(pass_message))
  }
  
  if(use_version==3)
  {
    ### for version 3, we store the data in one folder for publication
    drupal_data<-dataForDrupal(s.glm, "logit", survey_source, ds, title_string, formula_string, 
                               country_code, version_code, rv)
    rdsname<-paste(paste("R", drupalIndex, sep=""), "rds", sep=".")
    saveRDS(drupal_data, file = rdsname)
    print(c(drupalIndex, rdsname, "------ saved in", getwd()))
    return(drupalIndex+1)
    
  }
  else  {
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
  else if(country_code=="Tuvalu") iso<-"TUV"
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


ResultList<-function(survey_source, country_code, version_code, country_ISO, year_code, ds, rv, religion, region, isNullData){
  
  Result<-list(
    country=country_ISO,
    year=year_code,
    countryCode=country_code,
    version=version_code, 
    dataSource=survey_source,
    dataSet=ds,
    indicator=rv,
    ReligionFlag=religion,
    DataAvailable=isNullData, 
    NationalValidated =NA,
    ValidationResult=NA,
    NationalmeanYFromValidation=NA,
    NationalmeanYfromR=NA,
    RegionName=region,
    SampleSize=NA,
    SampleMean=NA,
    TreeFileID=NA,
    DindexFileID=NA, 
    formula=NA,
    d_index=NA,
    HOI=NA,
    tree_stat=list()
  )
  return(Result)
}



isoToCountry<-function(iso)
{
  if(iso=="AFG") country<-"Afghanistan"
  else if(iso=="ARM") country<-"Armenia"
  else if(iso=="BGD") country<-"Bangladesh"
  else if(iso=="BTN") country<-"Bhutan"
  else if(iso=="GEO") country<-"Georgia"
  else if(iso=="IND") country<-"India"
  else if(iso=="IDN") country<-"Indonesia" 
  else if(iso=="KAZ") country<-"Kazakhstan"
  else if(iso=="KHM") country<-"Cambodia"
  else if(iso=="KGZ") country<-"Kyrgyzstan"
  else if(iso=="KIR") country<-"Kiribati"
  else if(iso=="LAO") country<-"Lao"
  else if(iso=="MNG") country<-"Mongolia"
  else if(iso=="MDV") country<-"Maldives"
  else if(iso=="MMR") country<-"Myanmar"
  else if(iso=="NPL") country<-"Nepal"
  else if(iso=="PAK") country<-"Pakistan"
  else if(iso=="PNG") country<-"Papua New Guinea"
  else if(iso=="PHL") country<-"Philippines"
  else if(iso=="THA") country<-"Thailand"
  else if(iso=="TJK") country<-"Tajikistan"
  else if(iso=="TLS") country<-"Timor-Leste"
  else if(iso=="TON") country<-"Tonga"
  else if(iso=="TUR") country<-"Turkey"
  else if(iso=="VNM") country<-"VietNam"
  else if(iso=="TKM") country<-"Turkmenistan"
  else if(iso=="TUV") country<-"Tuvalu"
  else country<-paste("NotFound", iso)
    
  return(country)
}

newLNOBList<-function(Data_source, Data_set, Country_code, Country_ISO, Region,
                      Version, Year, survey_version, rv, Additional_variable){
  
        Result<-list(DataSource=Data_source,
                     DataSet=Data_set,
                     CountryCode=Country_code, 
                     Country=isoToCountry(Country_ISO), 
                     Province=NA,
                     VersionCode=Version, 
                     Year=Year, 
                     Latest=survey_version,
                     IndicatorName=rv,
                     Additional_variable=ifelse(Additional_variable, 1, 0),
                      Validation_results=" ", 
                      Region=Region, 
                      Analysis_model=" ", 
                      Sample_Size=" ",
                      D_Index=" ",
                      Overall_Mean=" ",
                      Max_Leaf_Size=" ",
                      Max_Leaf_Access=" ",
                      Min_Leaf_Size=" ",
                      Min_Leaf_Access=" ",
                      Gap=" ",
                      Max_Leaf_Characteristics=" ",
                      Min_Leaf_Characteristics=" ",
                      MIV_1=" ",
                      MIV_2=" ",
                      MIV_3=" ",
                      MIV_4=" ",
                      MIV_5=" ",
                      MIV_6=" ",
                      MIV_7=" ",
                      MIV_8=" ",
                      MIV_9=" ",
                      MIV_1_Value=" ",
                      MIV_2_Value=" ",
                      MIV_3_Value=" ",
                      MIV_4_Value=" ",
                      MIV_5_Value=" ",
                      MIV_6_Value=" ",
                      MIV_7_Value=" ",
                      MIV_8_Value=" ",
                      MIV_9_Value=" ")
  return(Result)
}


updateLNOBdata<-function(newLNOBdata, rg, validation, title_string, SampleMean, SampleSize, Overall_D, tree_stat, mivs)
{
  if(!rg=="National") newLNOBdata$Region<-"Sub-National"
  newLNOBdata$Validation_results=ifelse(validation, 1, 0)
  newLNOBdata$Analysis_model<-title_string
  newLNOBdata$Province<-ifelse(rg=="National", "--", rg)
  newLNOBdata$Sample_Size<-SampleSize
  newLNOBdata$D_Index<-Overall_D
  newLNOBdata$Overall_Mean<-SampleMean
  if(!is.null(tree_stat)){
    newLNOBdata$Max_Leaf_Size<-tree_stat$max$wt
    newLNOBdata$Max_Leaf_Access<-tree_stat$max$yval
    newLNOBdata$Min_Leaf_Size<-tree_stat$min$wt
    newLNOBdata$Min_Leaf_Access<-tree_stat$min$yval
    newLNOBdata$Gap<-tree_stat$max$yval - tree_stat$min$yval
    newLNOBdata$Max_Leaf_Characteristics<-tree_stat$max_node
    newLNOBdata$Min_Leaf_Characteristics<-tree_stat$min_node
  
    nv<-length(tree_stat$importance)
    for(i in c(1:nv)) {
      newLNOBdata[paste("MIV", i, sep="_")]<-tree_stat$importance[i]
      newLNOBdata[paste("MIV", i, "Value", sep="_")]<-tree_stat$importancevalue[i]
    }
  }
  else newLNOBdata$Max_Leaf_Size<-tree_stat$max$wt<-"No tree generated"
  return(newLNOBdata)
}
