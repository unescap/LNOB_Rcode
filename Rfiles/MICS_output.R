
write_tree <- function(datause, country_code, version_code, 
                       title_string, formula_string, sub_string, 
                       rv, rtp, religion, output_folder, ds, filename, use_version) {
  
  pass_message <- "Successfully built Tree"
  
  tree_stat<- catch_error(build_tree(output_folder, country_code, version_code, datause, rv, rtp, 
                                     formula_string, title_string, sub_string, filename, e = religion, use_version))
  
  
  
  tree_stat<-t(c(country_code, version_code, title_string, tree_stat))
  
  tree_stat$Source<-"MICS"
  if (!is.null(tree_stat)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  resave(tree_stat, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output  
  pass_message <- "Successfully wrote Tree.csv"
  catch_error(write.table(tree_stat, file=paste(output_folder, paste(ds, "Tree.csv", sep=""), sep=""),
                          sep=",", append = TRUE,   col.names = F, row.names = F))
  
}

write_HOI_D <- function(datause, country_code, version_code, title_string,
                        indvar, output_folder, filename, use_version) {
  
  # Calculate 
  pass_message <- "Successfully calculated HOI and D"
  
  # catch_error does not seem to work
  # result<-catch_error(cal_HOI_shapley(datause, indvar))
  
  two_results<-cal_HOI_shapley(datause, indvar)
  result<-two_results[1]
  result<-c(country_code, version_code, title_string, result)
  
  if (!is.null(result)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  resave(result, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output 
  pass_message <- "Successfully wrote D.csv"
  catch_error(write.table(t(result), file=paste(output_folder, "D.csv", sep=""),
                          sep=",", append = TRUE,   col.names = F, row.names = F)) 
  
  if(use_version==3)  {
    result<-toJSON(two_results[2], auto_unbox = TRUE)
    MICS_TBD_WriteDindex(output_folder, title_string, country_code, version_code, result)
  }
  
}

write_glm <- function(datause, rtp, country_code, version_code, title_string, 
                      indvar, output_folder, filename, use_version) {
  
  # Build Logistic Regression model 
  pass_message <- "Successfully built glm"
  # catch_error does not seem to workk here 
  
  for(ivm in indvar) {
    
    if(!(ivm=="NUnder5")) datause[ , ivm]<-factor(datause[ , ivm], ordered = F)
  }
  
  
  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  
  s.glm <- catch_error_prod(logistic(datause, rtp, formula_string, filename))
  
  if (!is.null(s.glm)) { 
    
    info(logger, paste(pass_message))
    
  }
  
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
  
  if(use_version==3) MICS_TBD_Writeglm(s.glm, output_folder, title_string, country_code, version_code)
  
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
# Function to catch errors  
catch_error_prod <- function(code) {
  out <- tryCatch(code,
                  error = function(c) {
                    error(logger, paste(c$message))
                  },
                  warning = function(c) { 
                    warn(logger, paste(c$message))
                  }
  )
  return(out)
}

catch_error_dev <- function(code) {
  return(code)
}

ds_output_dev <- function(output_folder, ds){
  ds_output_folder<-paste(output_folder, ds, "/",sep = "")
  ifelse(!dir.exists(ds_output_folder), dir.create(ds_output_folder), FALSE)
  return(ds_output_folder)
}

rv_Rdata_dev<-function(mics_Rdata_folder, rv){
  rv_Rdata_folder <- paste(mics_Rdata_folder, rv, sep = "/")
  ifelse(!dir.exists(rv_Rdata_folder), dir.create(rv_Rdata_folder), FALSE)
  setwd(rv_Rdata_folder)
  return(rv_Rdata_folder)
}

