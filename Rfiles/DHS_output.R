#### output functions
#### All things related to output

write_tree <- function(datause, country_code2, year_code, 
                       title_string, formula_string, sub_string, 
                       rv, rtp, filename,  caste, output_folder) {
  
  # Build Model
  sub_string<-NULL
  pass_message <- "Successfully built Tree"
  
  print(output_folder)
  
  source1<-"DHS"
  tree_stat<- catch_error(build_tree(output_folder, country_code2, year_code,  datause, source1, rv, rtp,
                                     formula_string, title_string, sub_string, filename, e=caste, region = FALSE))
  
  
  
  tree_stat<-t(c(country_code2, year_code, title_string, tree_stat, source1))
  # tree_stat$source<-"DHS"
  if (!is.null(tree_stat)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  # Saving object as .Rdata file for Shiny output
  
  resave(tree_stat, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output  
  pass_message <- "Successfully wrote Tree.csv"
  writefile<-paste(output_folder, "Tree.csv", sep="")
  if(file.exists(writefile)) 
    catch_error(write.table(tree_stat, writefile,
                          sep=",", append = TRUE,   col.names = F, row.names = F))
  else catch_error(write.table(tree_stat, writefile,
                               sep=",", append = F,   col.names = T, row.names = F))
  
}

write_HOI_D <- function(datause, country_code2, year_code, title_string,
                        indvar, output_folder, filename) {
  
  # Calculate 
  pass_message <- "Successfully calculated HOI and D"
  result<-catch_error(cal_HOI_shapley(datause, indvar))
  
  result<-c(country_code2, year_code,  title_string, result)
  result$source<-"DHS"
  
  if (!is.null(result)) { 
    
    info(logger, paste(pass_message))
    
  }
  
  print(paste("md",filename,".Rdata", sep=""))
  # Saving object as .Rdata file for Shiny output
  resave(result, file = paste("md",filename,".Rdata", sep=""))
  
  # Write to output 
  pass_message <- "Successfully wrote D.csv"
  writefile<-paste(output_folder, "D.csv", sep="")
  if(file.exists(writefile)) catch_error(write.table(t(result), writefile,
                               sep=",", append = TRUE,   col.names = F, row.names = F)) 
  else   catch_error(write.table(t(result), writefile,
                          sep=",", append = F,   col.names = T, row.names = F)) 
  
}

write_glm <- function(datause, rtp, country_code2, year_code, title_string, 
                      indvar, output_folder, filename) {
  
  # Build Logistic Regression model 
  
  pass_message <- "Successfully built glm"
  
  for (ivm in indvar) datause[, ivm]<- factor(datause[, ivm], ordered = F)
  formula_string<-paste("var2tab", paste(indvar, collapse=" + "), sep=" ~ ")
  
  s.glm<- catch_error_prod(logistic(datause, rtp,  formula_string)) #catch_error()
  # s.glm<- logistic(datause, rtp,  formula_string)
  if (!is.null(s.glm)) {
    info(logger, paste(pass_message))
  }
  
  # Saving object as .Rdata file for Shiny output
  
  # catch_error_prod(resave(s.glm, file = paste("md",filename,".Rdata", sep="")))
  
  file_write<-paste(output_folder, "DHSLogit.csv", sep="")
  print(s.glm)
  # Write to output 
  write.table(t(c(country_code2, year_code, title_string)) , file_write,
              sep=",", append = TRUE,   col.names = F, row.names = F)
  pass_message <- "Successfully wrote DHSLogit.csv"
  # catch_error_prod(write.table(s.glm,  file_write,
  #                              sep=",", append = TRUE,   col.names = T, row.names = T, na="")) #catch_error() 
  write.table(s.glm,  file_write,
              sep=",", append = TRUE,   col.names = T, row.names = T, na="")
  
  print("glm result written")
}

# List of formula components 
# Arguments: [1] Formula string: E.g. y ~ x1 + x2 
# Returns: List of independent variable components 
list_of_formula <- function(formula_string) {
  
  first_split <- formula_string %>% str_split(pattern = "~") %>%
    unlist %>%
    tail(1) 
  
  second_split <- gsub(" ", "", first_split)
  
  formula_components <- second_split %>% str_split(pattern = "\\+") %>%
    unlist
  
  return(formula_components)
}

# Reconstruct formula 
# Arguments: [1] List of independent variables
# Returns: Formula String E.g. y ~ x1 + x2 
construct_formula <- function(indvar_list) {
  
  formula <- paste("var2tab", paste(indvar_list, collapse=" + "), sep=" ~ ")
  
  return(formula)
}


# not sure this is the right way to do region
# Region 
region <- function(output_folder, country_code, version_code, 
                   datause, rv,
                   formula_string, title_string, sub_string, 
                   caste, filename, indvar) {
  
  # List of the regions   
  region_list <- unique(datause$RegionName)
  
  #filename = REAFIR70FL
  region_filename <- paste("RE",filename, sep = "")
  region_tree_stat <- c()
  region_treefit <- c()
  region_result <- c()
  region_glm <- c()
  
  for (region in region_list) {
    
    message <- paste("REGION: ", region, "  ------------------")
    print(message)
    info(logger, message)
    message <- paste("Current Region: ", match(region, region_list), " of ", length(region_list))
    print(message)
    info(logger, message)
    
    # Filter for the region data
    datause_region <- filter(datause, RegionName == region)
    
    # Reweight SampleWeight
    datause_region <- datause_region %>%
      mutate(SampleWeight = SampleWeight*nrow(datause_region)/sum(SampleWeight))
    
    # Region output folder 
    region_folder <- paste(output_folder, "Region", "/", sep = "")
    ifelse(!dir.exists(region_folder), dir.create(region_folder), FALSE)
    
    region_output_folder <- paste(region_folder, region, "/", sep = "")
    ifelse(!dir.exists(region_output_folder), dir.create(region_output_folder), FALSE)
    
    #### Construct Decision Tree
    if(nrow(datause_region)>0){
      sub_string<-NULL
      pass_message <- "Successfully built Stat Tree"
      tree_stat<- catch_error(build_tree(output_folder, country_code, version_code, 
                                         datause_region, rv,
                                         formula_string, title_string, sub_string, 
                                         e=caste, filename, region))
      
      if (!is.null(tree_stat)) { 
        
        info(logger, paste(pass_message))
        
      } 
      
      tree_stat<-t(c(country_code, version_code, title_string, tree_stat))
      
      # Add region name to object 
      tree_stat <- c(region, tree_stat)
      
      # Append data for .Rdata
      region_tree_stat <- rbind(region_tree_stat, tree_stat)
    } 
    
    #### Construct Decision Tree Model Only 
    cp_chosen<- 1
    if(caste == FALSE) minb_chosen = 11 
    else minb_chosen = 19
    
    treefit <- catch_error(rpart(as.formula(formula_string), 
                                 data = datause_region,  weights=SampleWeight, 
                                 method="anova", control = rpart.control(cp = cp_chosen/nrow(datause_region), maxdepth=6, 
                                                                         minbucket =nrow(datause_region)/minb_chosen)))
    
    # Append treefit raw model for .Rdata 
    region_treefit[[region]] <- as.party(treefit)     
    
    pass_message <- "Successfully built Tree"
    if (!is.null(treefit)) { 
      
      info(logger, paste(pass_message))
      
    }
    
    #### Construct HOI
    # Calculate 
    result<-catch_error(cal_HOI_shapley(datause_region, indvar))
    
    result<-c(country_code, version_code, title_string, result)
    
    # Add region name to object 
    result <- c(region, result)
    
    pass_message <- "Successfully calculated HOI and D"
    if (!is.null(result)) { 
      
      info(logger, paste(pass_message))
      
    }
    
    region_result <- rbind(region_result, result)
    
    ### Construct Logistic Regression
    # Change response variable to factor
    datause_region$var2tab <- factor(datause_region$var2tab)
    
    # Need to check that each res/ind variable has more than two factors
    # if not then remove. 
    
    # List of formula components 
    formula_components <- list_of_formula(formula_string)
    
    # Check formula variables have more than one value 
    for (component in formula_components) {
      
      if (length(unique(datause_region[[component]])) <= 1) {
        formula_components <- formula_components[formula_components != component]
      }
      
    }
    
    # Reconstruct formula 
    formula_string <- construct_formula(formula_components)
    
    # Construct glm model 
    if (length(unique(datause_region[["var2tab"]])) > 1) {
      
      # Change response variable to numeric 
      datause_region$var2tab <- as.numeric.factor(datause_region$var2tab)
      
      # Model 
      var2tab.glm <- catch_error(glm(as.formula(formula_string), family=binomial,
                                     weights=SampleWeight, data=datause_region))
      s.glm <- as.data.frame(summary(var2tab.glm)$coefficients)
      
      region_glm[[region]] <- s.glm
      
      pass_message <- "Successfully built logistic regression."
      if (!is.null(s.glm)) {
        
        info(logger, paste(pass_message))
        
      }
      
    }
    
    
  }
  
  # Resave after first time otherwise will be overwritten 
  save(region_tree_stat, file = paste(region_filename, ".Rdata", sep=""))
  resave(region_treefit, file = paste(region_filename, ".Rdata", sep=""))
  resave(region_result, file = paste(region_filename, ".Rdata", sep=""))
  
  if (length(unique(datause_region["var2tab"])) > 1) {
    resave(region_glm, file = paste(region_filename, ".Rdata", sep=""))
  }
}



# Catch error: production version 
catch_error_prod <- function(code) {
  out <- tryCatch(code,
                  error = function(c) {
                    error(logger, paste(c$message))
                  }
                  
                  #  warning = function(c) { 
                  #    warn(logger, paste(c$message))
                  # }
  )
  return(out)
}

# Catch error: development version
catch_error_dev <- function(code) {
  return(code)
}




# Response variable directory: production version 
rv_Rdata_prod <- function(dhs_Rdata_folder, rv) {
  
  rv_Rdata_folder <- paste(dhs_Rdata_folder, rv, sep = "/")
  return(rv_Rdata_folder)
  
}

# Response variable directory: development version
rv_Rdata_dev <- function(dhs_Rdata_folder) {
  
  return(dhs_Rdata_folder)
  
}