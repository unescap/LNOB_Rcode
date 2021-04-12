
build_tree<-function(source_folder, country_code, version_code, datause, Response_var, datatype, formula_string, 
                     title_string, sub_string=NULL, filename, e=FALSE, region = FALSE){  
  

  print(source_folder)
  cp_chosen<- 1
  if(e==FALSE) minb_chosen = 11   # temporary, for Tonga we use 19
  else minb_chosen = 19
  min_node<-max(49, nrow(datause)/minb_chosen)
  if (datatype=="Factor") treemethod<-"anova"
  else treemethod<-"anova"
  
  treefit <- rpart(as.formula(formula_string), 
                   data = datause,  weights=SampleWeight, 
                   method=treemethod, control = rpart.control(cp = cp_chosen/nrow(datause), maxdepth=6, 
                                                              minbucket = min_node, minsplit=2*min_node))


  
  # First save: Saving object as .Rdata file for Shiny output
  if (region == FALSE) { 
    save(treefit, file = paste("md",filename,".Rdata", sep="")) 
  }
  
  # if(e==FALSE)
  #   filename<-paste(source_folder, country_code, version_code, Response_var, sep="")
  # else filename<-paste(source_folder, country_code, version_code, Response_var, "_ethnicity", sep="")
  
  if(e==FALSE)
    filename<-paste(source_folder, filename, sep="")
  else filename<-paste(source_folder, filename, "_ethnicity", sep="")
  

  data2 <- get_tree(treefit)
  data2<-Education_Lable(data2)

  write.table(data2, paste(filename, ".csv", sep=""), sep=",", col.names = FALSE  , row.names = FALSE, append = FALSE)
  
  
  
  data2$CC<-country_code
  data2$VC<-version_code
  data2$RV<-Response_var
  data2$EC<-e
#  data2$filename<-filename
  data2$Source<-"MICS"
  write.table(data2, paste(source_folder, "ALLTrees.csv", sep=""), sep=",", 
              col.names = FALSE  , row.names = FALSE, append = TRUE)
  
  
  if(!is.null(treefit$splits)) {  
    frame1<-treefit$frame
    
    ####commented out on May 25th, only write the tress structures
    frame1$rows<-as.numeric(rownames(frame1))
    frame_max<-frame1[frame1$var=="<leaf>" & frame1$yval==max(frame1$yval), c(3,5)] #extract total weight, yval
    frame_min<-frame1[frame1$var=="<leaf>" & frame1$yval==min(frame1$yval), c(3,5)]
    title_string<-paste( title_string, ": \n", country_code, version_code)
    
    row_max<-frame1$rows[frame1$var=="<leaf>" & frame1$yval>=max(frame1$yval)]
    row_min<-frame1$rows[frame1$var=="<leaf>" & frame1$yval<=min(frame1$yval)]
    
    if(length(row_max)>1) row_max<-max(row_max)
    if(length(row_min)>1) row_min<-max(row_min)
    
    ###### start summarizing the max leaf and min leaf #####################
    where_max<-match(row_max, frame1$rows)
    description_max<-NULL
    while(row_max>1)
    {
      
      row_max<-row_max %/% 2
      dscr<- as.character(frame1$var[match(row_max, frame1$rows)])
      if(dscr %in% description_max) {
        h<-match(dscr, description_max)
        description_max<-description_max[-h]
        description_max<-c(dscr, description_max)
      }
      else description_max<-c(dscr, description_max)
      
    }
    
    formula_string<-paste("SampleWeight", paste(description_max, collapse=" + "), sep=" ~ ")
    t_max<-aggregate(as.formula(formula_string), data=datause[treefit$where==where_max, ] , FUN=sum)
    #### getting one row, column names, plus row values, perfect
    
    xy_max<-"MAX-leaf"
    for(t in description_max) {
      xy_max<-paste(xy_max, paste(t, paste(unique(t_max[, t]), collapse=" + "), sep=" ~ "), sep=" // ")
    }
    
    where_max<-match(row_min, frame1$rows)
    description_max<-NULL
    while(row_min>1)
    {
      row_min<-row_min %/% 2
      dscr<- as.character(frame1$var[match(row_min, frame1$rows)])
      if(dscr %in% description_max) {
        h<-match(dscr, description_max)
        description_max<-description_max[-h]
        description_max<-c(dscr, description_max)
      }
      else description_max<-c(dscr, description_max)
    }
    
    formula_string<-paste("SampleWeight", paste(description_max, collapse=" + "), sep=" ~ ")
    t_max<-aggregate(as.formula(formula_string), data=datause[treefit$where==where_max, ] , FUN=sum)
    #### getting one row, column names, plus row values, perfect
    
    xy_min <- "MIN-leaf"
    for(t in description_max) {
      xy_min<-paste(xy_min, paste(t, paste(unique(t_max[, t]), collapse=" + "), sep="~"), sep=" // ")
    }
    
    ###### end summarizing the max leaf and min leaf #####################
    
    total_weight<-sum(datause$SampleWeight)
    y_bar<-sum(datause$SampleWeight*datause$var2tab)/total_weight
    vi_list<-names(treefit$variable.importance)
    tree_stat<-t(c(total_weight, y_bar, unname(frame_max), unname(frame_min), xy_max, xy_min, vi_list))
    
    ##### calculating weighted sample %
    tree_stat[[3]]<-tree_stat[[3]]/tree_stat[[1]]
    tree_stat[[5]]<-tree_stat[[5]]/tree_stat[[1]]
    
    
    if(e==TRUE) pdf(paste(source_folder, "Tree_ethnicity_", Response_var,  country_code,
                          version_code,  ".pdf", sep = ""))
    else pdf(paste(source_folder, "Tree", Response_var,  country_code,
                   version_code,  ".pdf", sep = ""))
    
    treeplot<- prp(treefit, main=title_string, sub=sub_string,
                   type=4, fallen=T, branch=.3, round=0, leaf.round=9,
                   clip.right.labs=F, under.cex=1,
                   box.palette="GnYlRd",
                   prefix=paste(Response_var, "\n"), branch.col="gray", branch.lwd=2,
                   extra=101, under=T, lt=" < ", ge=" >= ", cex.main=1.0, cex.sub=0.7)
    dev.off()
    
    return(tree_stat)
  }
  else { print("No tree generated")
  }
  #tree_stat<-print(treefit) 
  return(NULL)
}

logistic<-function(datause, rtp, formula_string, filename){
  if(rtp=="Factor")
         var2tab.glm<-glm(as.formula(formula_string), family=binomial,  weights=SampleWeight, data=datause)
  else var2tab.glm<-glm(as.formula(formula_string), family=gaussian,  weights=SampleWeight, data=datause)
  
  
  s.glm<-as.data.frame(summary(var2tab.glm)$coefficients)
  
  s.glm$Stars<-""
  s.glm$Stars[s.glm[,4]<=0.1]<-"*"
  s.glm$Stars[s.glm[,4]<=0.05]<-"**"
  s.glm$Stars[s.glm[,4]<=0.01]<-"***"
  s.glm$Coeff<-paste(round(s.glm[,1] , digits = 2), s.glm$Stars)
  s.glm$SE<-round(s.glm[,2], digits = 2)
  if(rtp=="Factor"){
    s.glm$OR<-round(exp(s.glm[,1]), digits = 3)
    s.glm$OR[1]<-NA
    s.glm$OR[s.glm$Stars==""]<-NA
    s.glm<-s.glm[, c(6, 7, 8)]
    
  }
  else {
  s.glm<-s.glm[, c(6, 7)]
  }

  return(s.glm)
}


Education_Lable<-function(data2){
  if(nrow(data2)==1) return(data2)
  # get rid of ) in Node column
  NodeNo<-data2$node
  #identify education variables
  data2$z<-as.character(data2$z)
  Ed_flag<- grepl("Education", data2$z)
  Ed_value<- strsplit(data2$z,"=")
  k<-nrow(data2)
  for(i in c(2:k)){
    ## only look if education
    ## the following method only works because education has obnly three levels
    ## suitable for ordinal valriable with three levels
    if(Ed_flag[i]){
      Ed_valuei<-strsplit(Ed_value[[i]][2], ",")[[1]]
      if(length(Ed_valuei)<2)
        Ed_flag[i]<- FALSE
    }
  }
  
  for(i in c(2:k)){
    if(Ed_flag[i]){
      nt<- NodeNo[i] %/% 2
      while(nt>1){
        i_nt<-which(NodeNo==nt)
        if(Ed_flag[i_nt]){
          Ed_valuei<-strsplit(data2$z[i],"=")
          Ed_valuent<-strsplit(data2$z[i_nt],"=")
          replaceValue<-intersect(strsplit(Ed_valuei[[1]][2], ",")[[1]], strsplit(Ed_valuent[[1]][2], ",")[[1]])
          data2$z[i]<-paste(strsplit(data2$z[i],"=")[[1]][1], replaceValue, sep="=")
        }
        nt<- (nt %/% 2)
      }
    }
  }
  data2$node<-paste(data2$node, ")", sep="")
  return(data2)
} 

#### get a data.frame of the tree for export
#### treefit is an object result from rpart() function
get_tree<-function(treefit, digits=2){
  
  # modified from source code for rpart print
  # print.rpart <- function(x, minlength = 0L, spaces = 2L, cp,
  #                         digits = getOption("digits"), ...)
  
  if (!inherits(treefit, "rpart")) stop("Not a legitimate \"rpart\" object")
  
  x<-treefit
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  node <- as.numeric(row.names(frame))
  depth <- rpart:::tree.depth(node)
  
  tfun <- (x$functions)$print
  #### trasform y-value if needed
  yval <- if (!is.null(tfun)) {
    if (is.null(frame$yval2)) tfun(frame$yval, ylevel, digits)
    else tfun(frame$yval2, ylevel, digits)
  } else format(signif(frame$yval, digits))
  
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  z <- labels(x, digits=digits, minlength=0)
  n <- frame$n
  dvc<-format(signif(frame$dev, digits))
  z <- data.frame(node, z, n, dvc, yval, term)
  return(z)
} 


