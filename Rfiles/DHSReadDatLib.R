
##################################################################
# This R-code file is written to read .dat fiels downloaded from #
# DHS websites. very technical                                   #
# Example given towards to the end of the file                   #
# main function:                                                 #
# importDHSDAT(filename, has_DCF, var_list,nb_observations=-1)   #
##################################################################

# to be able to read in Indian data, I had deleted one line 
# from the .DCF file 

library(stringr)

#Check if the field is among what we selected to extract
do_we_need_to_save <- function(list_of_fields_we_want,field_name)
{
  save<-FALSE
  # case of excluding some columns
  if(length(list_of_fields_we_want)>0) 
  {
    # field_name is in the list_of_fields_we_want
    if(!is.na(match(field_name,list_of_fields_we_want))) 
    {
      save<-TRUE
    } 
  } else 
  {
    # we want ALL the columns
    save<-TRUE
  }
  return(save)
}


# in  R 3.3.0 there is a startsWith... in the meantime, let's use this one
local_startsWith <- function(full_string,start_string, ignore.case = TRUE) {
  
  if (ignore.case) start_string <- tolower(start_string)
  n <- nchar(start_string)

  if (ignore.case) full_string <- tolower(full_string)
  return (substr(full_string, 1, n) == start_string)
}

# same as above
local_endsWith <- function(full_string,end_string, ignore.case = TRUE) {
  
  if (ignore.case) end_string <- tolower(end_string)
  n <- nchar(end_string)

  if (ignore.case) full_string <- tolower(full_string)
  length <- nchar(full_string)

  return (substr(full_string, pmax(1, length - n + 1), length) == end_string)
  

}

#information of the selected variables (in var_list) to be used to construct each column
# base_filename is filename without extension. Both .DO and .DCT are read to build a dictionary
#=======================================================================================
build_dictionary_for_older_data <- function(base_filename,var_list)
{
    # we read the whole DO file in memory,
    # from it we extract recode information
  do_file<-readLines(paste(base_filename, "DO", sep="."))
  
  fields<-list()
  
  
  # we check if we extract ALL fields or just a subset    
  fields_saved <-0
  subset_of_fields <- FALSE
  if(length(var_list)>0)
  {
    subset_of_fields <-TRUE
    nb_fields_to_save<- length(var_list)
    
  }


  file_length<-length(do_file)
  being_parsed <- list()
  recode_being_parsed <- list()
  recode_parsing <- FALSE
  hasNA <- FALSE
  hasMISSING <- FALSE
  field_being_recoded <- ""
  
  for(line in c(1:file_length))
  {
    # we exit if we have the nb of fields we want
    if(isTRUE(subset_of_fields) && (fields_saved==nb_fields_to_save))
    {
      break
    }
    
    if(do_file[line]=="")
    {
      next
    }
    
    
    # if line starts with "label variable " then we parse its name
    if(local_startsWith(do_file[line],"label variable "))
    {
      # we extract the code of the variable and its name
      # first we split on " to get the label (aka pretty version of the variable)
      split<-str_split(do_file[line], "\"",n=3)[[1]]
      label <- split[2]
      
      # we now remove "label variable " (of size 15) from split[1]
      name <- trimws(substring(trimws(split[1]),16))
      
      # the column name in the dataframe is expected to be UPPERCASE
      being_parsed[name] <- list(list("Name"=toupper(name),"Label"=label))

    }
    
    # to parse the recoding part
    if(recode_parsing && (do_file[line] ==";"))
    {
      if(hasNA){
        # remove it from recodeDict, because it is handled "automatically"
        # level not in the levels list are encoded as NA by factor
        # we know NA is ALWAYS last, so we remove the last element
        recodeDict<-recodeDict[-length(recodeDict)]
      }
      if(hasMISSING){
        # also handled automatically, and we consider equivalent to NA
        # we know it is now the last entry in recodeDict
        # we change it to NaN
        recodeDict[length(recodeDict)] <- NaN
      }
      
      recode_being_parsed[field_being_recoded] <- list(list("levels" = names(recodeDict),"labels" = unlist(recodeDict,use.names = FALSE) ))
      
      recode_parsing <-FALSE
      field_being_recoded <- ""
      hasNA <- FALSE
      hasMISSING <- FALSE
    }
    
    if(recode_parsing){
      recodeInfo <- str_split(trimws(do_file[line]), "\"",n=3)[[1]]
      recodeDict[trimws(recodeInfo[1])] <- recodeInfo[2]
      
      if(recodeInfo[2]== "Missing")
      {
        hasMISSING<- TRUE
      }
      if(recodeInfo[2]== "NaN")
      {
        hasNA<- TRUE
      }
      
    }
    
    # if line starts with "label define " then we parse the recode information
    if(local_startsWith(do_file[line],"label define "))
    {
      # label not yet extracted not yet
      field_being_recoded <- str_split(trimws(do_file[line]), " ",n=3)[[1]][3]
      recode_parsing <-TRUE
      recodeDict<-list()
    }
    
    
    # if line starts wiht "label values "hv114_20 HV114
    ## this is where we associate a recode and a field
    
    if(local_startsWith(do_file[line],"label values "))
    {
      field_names <- str_split( trimws(substring(do_file[line],14))," ",n=2)[[1]]
      
      being_parsed[[field_names[1]]]["levels"] = recode_being_parsed[[trimws(field_names[2])]]["levels"]
      being_parsed[[field_names[1]]]["labels"] = recode_being_parsed[[trimws(field_names[2])]]["labels"]
      being_parsed[[field_names[1]]]["DataType"] <- "factor" # this indicate a field with recode

    }
  }


  # now we parse the DCT
  dct_file<-readLines(paste(base_filename, "DCT", sep="."))
  file_length<-length(dct_file)
  for(line in c(1:file_length))
  {

    # we only parse 1 lines file
    if(local_endsWith(trimws(dct_file[line])," lines"))
    {
      nb_lines <- as.integer(trimws(str_split( trimws(dct_file[line]),"lines")[[1]][1]))
      if(nb_lines>1)
      {
        print(paste("Parsing code can only handle '1 lines'. This file has [",trimws(dct_file[line]),"]",sep=""))
        quit()
      }
    }
    
    # we only care about lines starting with byte int float long  double str (per stata documentation)
     if( local_startsWith(trimws(dct_file[line]),"byte")
      || local_startsWith(trimws(dct_file[line]),"int")
      ||local_startsWith(trimws(dct_file[line]),"float")
      || local_startsWith(trimws(dct_file[line]),"long")
      ||local_startsWith(trimws(dct_file[line]),"double")
      ||local_startsWith(trimws(dct_file[line]),"str") )
     {
       # line is something like   str      hhid        1:   1-12 
       
       # first we split on space
       field_names <- str_split( trimws(dct_file[line])," ")[[1]]
       # removing empty fields ("")
       field_names <-  field_names[lapply(field_names,nchar)>0]
       
       # if we only want a subset, we check if name matches
       if(subset_of_fields &&  is.na(match(field_names[2],var_list)))
       {
         next
       }
       
       
       dataType="Alpha"
       if ((field_names[1]=="byte")||(field_names[1]=="int")||(field_names[1]=="long"))
       {
         dataType="integer"
       }
        
       if(!("DataType" %in% names(being_parsed[[field_names[2]]]) ))
       {  
        being_parsed[[field_names[2]]]["DataType"]=dataType
       } else if((field_names[1]=="byte") && (being_parsed[[field_names[2]]]["DataType"]=="factor")){
          # we convert the interval to a sequence of levels -> labels 
          if (is.null(being_parsed[[field_names[2]]][["levels"]][[1]])) {
            first_level = 0
          }  else {
            first_level =  as.integer(being_parsed[[field_names[2]]][["levels"]][[1]])-1
          }

          if(first_level >1) {
            being_parsed[[field_names[2]]][["levels"]] = c(as.character(c(0: first_level)), being_parsed[[field_names[2]]][["levels"]])
            being_parsed[[field_names[2]]][["labels"]] = c(as.character(c(0: first_level)), being_parsed[[field_names[2]]][["labels"]])
          }
       } else if(field_names[2] %in% c('hc70','hc71','hc72')){
          # special case because it has negative value
          # so we force it as a string
         being_parsed[[field_names[2]]]["DataType"]="Alpha"
        }
      
       
        #this is to handle the case 1:1121-1124  instead of 1: 121-124
       if(length(field_names)<4){
         # we split the 3rd field on :
         temp <-   str_split( field_names[3], ":")
         field_names[4] <- temp[[1]][2]
       }
       
        start_end <- str_split( trimws(field_names[4]),"-")[[1]]
        being_parsed[[field_names[2]]]["Start"] <- start_end[1]
        being_parsed[[field_names[2]]]["Len"] <- as.integer(start_end[2])-as.integer(start_end[1]) + 1
      
        fields <- append(fields,list(being_parsed[[field_names[2]]]))
      
        if(subset_of_fields)
        {
          nb_fields_to_save <- nb_fields_to_save -1
          if(nb_fields_to_save==0)
          {
           break
          }
        }
     }
  }
  #print(fields)
  if(subset_of_fields>0){print(var_list)}
  print(paste("Size of fields: ",length(fields),sep=""))
  
  return (fields)
  
}

#information of the selected variables (in var_list) to be used to construct each column
#=======================================================================================
build_dictionary <- function(filename,var_list)
{
  # we read the whole dictionary in memory
  dt<-readLines(paste(filename, "DCF", sep="."))
  
  Field_Used_for_Offset<- "Start"
  Field_Used_for_Name <- "Name"
  Field_Used_for_Width <- "Len"
  Field_Used_for_DataType <- "DataType"
  Field_Used_for_Occurrences <- "Occurrences"
  
  fields<-list()
  fieldsDefinitionStarted<-FALSE
  currentFields<-list()
  subItem<-FALSE
  valueSet<-FALSE
  recodeHeader<-NULL
  recodeDict<-list()
  skipToNextItem <-FALSE
  interval<-FALSE
  hasMISSING<-FALSE
  hasNA<-FALSE

  
  fields_saved <-0
  subset_of_fields <- FALSE
  if(length(var_list)>0)
  {
    subset_of_fields <-TRUE
    nb_fields_to_save<- length(var_list)
  }

  
  dt_length<-length(dt)
  for(i in c(1:dt_length))
  {
    if(isTRUE(subset_of_fields) && (fields_saved==nb_fields_to_save))
    {
      break
    }
    if(dt[i]=="")
    {
      next
    }
    if(dt[i]=="[IdItems]") 
    {
      # IdItems starts the section on columns/fields definition  
      fieldsDefinitionStarted<-TRUE
      next
    }
    
    if(!isTRUE(fieldsDefinitionStarted))
    {
      # util we find [IdItems] we ignore what we read
      next
    }
    
    if(skipToNextItem && (dt[i]!="[Item]"))
    {
      # when we only extract partial records, 
      # we skip we ignore everything we don't wantutil we find [IdItems] we ignore the data
      next
    }
    

    
    # now we have a line of data we want to parse
    # either it is a header (marked with [])
    if(substring(dt[i],1,1)=="[")
    {
      # [Item] is a special header that we handle
      if(dt[i]=="[Item]")
      {
        skipToNextItem <-FALSE
        
        # current field contains what we have parsed previously
        if(length(currentFields)>0)
        {
          valueSet<-FALSE
          
          # if we have a recode section
          if(length(recodeDict)>0)
          { 
            if(hasNA){
              # remove it from recodeDict
              # we know NA is ALWAYS last, so we remove the last element
              recodeDict<-recodeDict[-length(recodeDict)]
            }
            if(hasMISSING){
              # we know it is now the last entry in recodeDict
              # we change it to NaN
              recodeDict[length(recodeDict)] <- NaN
            }
            
            if(length(recodeDict)>0)
            {
              # we save levels and labels
              currentFields["levels"] <- list(names(recodeDict))
              # this is to get the list of keys. There might be a better way to do this
              currentFields["labels"] <- list(unlist(recodeDict,use.names = FALSE))
              currentFields[Field_Used_for_DataType]="factor" # this indicate a field with recode
            } 
            else {
              currentFields[Field_Used_for_DataType]="integer"
            }
            recodeDict<-list()
          } else
          {
            if(is.na(names(currentFields[Field_Used_for_DataType])))
            {
              currentFields[Field_Used_for_DataType]="integer"
            }
          }
            
          if(do_we_need_to_save(var_list,currentFields[Field_Used_for_Name]))
          {
            fields <- append(list(currentFields),fields)
            fields_saved<-fields_saved+1
            
          }
          
          # as we saved, we clear it for the next usage 
          currentFields <- list()
          subItem <- FALSE
          valueSet<-FALSE
          interval<-FALSE
          hasMISSING<-FALSE
          hasNA<-FALSE

        }
          
          
      } else 
      {
        # It'a another header  - we save the header value without the [ ]
        #currentFields["headerType"] <- substring(dt[i],2,nchar(dt[i])-1)
        subItem <- TRUE
        if(dt[i]=="[ValueSet]")
        {
          valueSet <- TRUE
        }
      }
        
    } else
    {
      # or it's a key = value
      # we parse it then store it
      split<-str_split(dt[i], "=",n=2)[[1]]
      if(length(split)!=2)
      {
        #print(split)
        stop(paste("line with error ",dt[i],sep=""))
      }
      if(isTRUE(subItem))
      {
        key<-paste("SI",split[1],sep="-")
      } else 
      {
        key=split[1]
      }


      if(key==Field_Used_for_Occurrences|key==Field_Used_for_Name||key==Field_Used_for_Offset||key==Field_Used_for_Width||key==Field_Used_for_DataType)
      {
        currentFields[key]<-split[2]
      }
      
      if(isTRUE(valueSet) )
      {
        if(key=="SI-Value")
        {
          # case with ;
          recodeInfo<-str_split(split[2], ";",n=2)[[1]]
          if(length(recodeInfo)>1)
          {
            recodeDict[recodeInfo[1]] <- recodeInfo[2]
          } else {
            # check if we have interval values
            intervalValues<-str_split(split[2], ":",n=2)[[1]]
            if(length(intervalValues)>1)
            {
              # just in case we need to know it's an interval
              interval <- TRUE

              # we convert the interval to a sequence of levels -> labels 
              for(x in as.integer(intervalValues[1]):as.integer(intervalValues[2]))
              {
                recodeDict[as.character(x)] <- as.character(x)

              }

            }
          }
        } else if(key=="SI-Name")
        {
          special<-str_split(split[2], ",",n=2)[[1]]
          if(length(special)>1)
          {
            if(special[1]=="MISSING")
            {
              hasMISSING<- TRUE
            }
            if(special[1]=="NOTAPPL")
            {
              hasNA<- TRUE
            }
            if((special[1]!="MISSING") && (special[1]!="NOTAPPL"))
            {
              print("Special case "+special[1])
              
            }
          }
        }
      } else if(!is.null(currentFields[[Field_Used_for_Name]])&& (length(var_list)>0) && is.na(match(currentFields[Field_Used_for_Name],var_list)))
      {
        # the name is not on the list of field we care, so we skip until next [Item]
        skipToNextItem <-TRUE
        
        currentFields <- list()
        valueSet <- FALSE
        subItem <- FALSE
        valueSet<-FALSE
        interval<-FALSE
        hasMISSING<-FALSE
        hasNA<-FALSE

      }
    }
      
  }
    
  # last 'block' need to be saved
  # same logic as above, except we don't need to reset currentFields nor other flags
  if(length(currentFields)>0)
  {  
    if(length(recodeDict)>0)
    { 
      if(hasNA){
        recodeDict<-recodeDict[-length(recodeDict)]
      }
      if(hasMISSING){
        recodeDict[length(recodeDict)] <- NaN
      }
      
      if(length(recodeDict)>0)
      {
        # we save levels and labels
        currentFields["levels"] <- list(names(recodeDict))
        # this is to get the list of keys. There might be a better way to do this
        currentFields["labels"] <- list(unlist(recodeDict,use.names = FALSE))
        currentFields[Field_Used_for_DataType]="factor" # this indicate a field with recode
      } 
      else {
        currentFields[Field_Used_for_DataType]="integer"
      }
    } else
    {
      if(is.na(names(a[Field_Used_for_DataType])))
      {
        currentFields[Field_Used_for_DataType]="integer"
      }
    }
    if(do_we_need_to_save(var_list,currentFields[Field_Used_for_Name]))
    {
      fields <- append(list(currentFields),fields)
      
    }
  }
    
  return (fields)
}

#### the main function, read the ascii files according to the data dictionary constructed above
#### parameters: file (full path+file name), 
####             old_style (without ) or new style (with DCF)  
####             list of variable (obtained from DHS data dictionary), 
####             number of rows to be read, default is all

importDHSDAT<-function(filename, has_DCF, var_list,nb_observations=-1)
{
  if(has_DCF)
  {
    #print("We have a DCF so we parse it")
    fields <- build_dictionary(filename,var_list)
  } else
  {
    #print("We do not have a DCF so we try to parse DO file instead")
    fields <- build_dictionary_for_older_data(filename,tolower(var_list))
  }
  data_set <- readLines(paste(filename, "DAT", sep="."),  n=nb_observations)

  #print("******Data read*****")
  tmp_df<-NULL
  
  # extract columns we want and do dataType conversion for each column
  for(field in fields)
  {
    startpos<-as.integer(field["Start"])
    endpos<-as.integer(field["Len"])+startpos-1
    
    # here we check if we have a field["Occurences"]
    if ("Occurrences" %in% names(field) ) {
      # Better way  is to repeat the extraction
      # but unclear how we store it
      
      # so for now, we multiply the length instead
      endpos<-as.integer(field["Len"])*as.integer(field["Occurrences"])+startpos-1
    }
    tmpcol<-substr(data_set, startpos, endpos)
    if ("Occurrences" %in% names(field) ) {
     # for occurance make it a string
      field["DataType"]="Alpha"
    }
    
    if(field["DataType"]=="integer")
    {
      tmpcol <- as.integer(tmpcol)
    }
    tmp_df<-data.frame(cbind(tmpcol, tmp_df), stringsAsFactors=(field["DataType"]=="Alpha"))
    colnames(tmp_df)[1]<-field["Name"]
    
    if(field["DataType"]=="factor")
    {
      # factor expects integer
      intcol <- as.integer(tmpcol)
      if(colnames(tmp_df)[1] %in% c("HV024", "V024", "MV024"))
      {
        tmp_df<-data.frame(cbind(tmpcol, tmp_df), stringsAsFactors=(field["DataType"]=="Alpha"))
        tmp_df[1] <- factor(intcol, levels=field[['levels']],labels=field[['labels']])
        colnames(tmp_df)[1]<-"RegionName"
      }
    }

  }
  
  return(tmp_df)
  
}


######example of usage 
# source_folder<-"/home/yw/workspace/rstudio/SDD2017/"
# data_folder<-paste(source_folder,"dat_download",sep="")
# 
#  country_code<-"AF"
#  data_type<-"MR"
#  version_code<-"70"
#  
#  filename<-paste(country_code, data_type, version_code, "FL", sep="")
 # Variable lists examples, to identify variable to be  in the lists, use .DCF file
 # privar_name for data_type=="PR, 
 # privar_name<-c("HHID", "HV001", "HV002", "HVIDX", "HV005", "HV270", "HV024", "HV025", "HV105", "HV104", "HC13", "HC61", "HV014", "HV109") 
 

 # hrivar_name for data_type=="HR, 
 # hrivar_name<-c("HHID", "HV001", "HV002", "HV003", "HV005", "HV270", "HV024", "HV025", "HV220", "HV219", 
 # "HV217", "HV014", "HV225")
 
 # ivar_name for data_type=="IR, 
 # ivar_name<-c("V001", "V002", "V024")
 
 # mvar_name for data_type=="MR, 
 # mvar_name<-c("MV001", "MV002", "MV024")
 # df<-  importDHSDAT(paste(data_folder, filename, sep="/"), TRUE,  mvar_name)
 
