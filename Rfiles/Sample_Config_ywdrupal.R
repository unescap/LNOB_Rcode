#### before you run this config file, please run ConfigInput.R first
#### there 
#### 1) source_folder     ---- path for all r-script files
#### 2) dhs_data_folder   ---- path for all dhs data
#### 3) mics_data_folder  ---- pàth for all mics data
#### 4) catch_error(code) ---- code to skip error
#### are defined

#### you can specify where the output shoud go. I set it as a subfolder of source_folder 
output_folder<-paste(source_folder, "output/", sep="")
ifelse(!dir.exists(output_folder), dir.create(output_folder), FALSE)

# Function to catch errors, XXX_prod allows the program to ignore error message and keep running 
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

# Function to catch errors, XXX_dev will stop the program to debug

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


#### in my config, I choose the debugging function of XXX_dev
catch_error<-function(code)
{
  return(catch_error_dev(code))
}


#### output function, you can also specify your own output folder here.
### likely not in use for now.
ds_output <- function(output_folder, ds){
  # ds_output_folder<-ds_output_dev(output_folder, "validationm")
  return(paste(output_folder, "validation/",sep = "/"))
}

rv_Rdata<-function(mics_Rdata_folder, rv){
  rv_Rdata_folder <- rv_Rdata_dev(mics_Rdata_folder, rv)
  return(rv_Rdata_folder)
}

