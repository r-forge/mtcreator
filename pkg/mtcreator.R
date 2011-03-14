##################################################
## Main function called by user
##################################################
mtcreator <- function(interact = FALSE, rawbool=TRUE, rawlocation=".", expressionset, output_path="",...)
{
  logic_raw <- FALSE
  if(interact == TRUE){
    output_path <- .manage_output(interact, output_path)
    logic_raw <- .manage_raw(output_path)
  }
  else{
    if(.check_input(rawbool, rawlocation, expressionset, output_path)){
      if(rawbool==TRUE){
        logic_raw <- .manage_raw(output_path, interact=FALSE, rawlocation)
      }
    }
    else{
      cat("The given parameters do not seem correct, please check them and try again!")
    }
  }
#########
## interaction independent
#########
  .manage_eset(output_path, expressionset)
  if(logic_raw){cat(paste("Rawdata has been imported successfully!\n Please check the output files in",output_path,"for mistakes and missing values!"))}
  else{cat(paste("No rawdata was given -> has to be added manually.\n Please check the output files in",output_path,"for mistakes and missing values!"))}
}  

##################################################
## Raw data
##################################################
.manage_raw <- function(output_path, interact=TRUE, rawlocation=".")
{
  if(interact){
    rawdat <- ""
    rawbool <- FALSE
    while (rawdat == ""){
      rawdat <- readline("Do you want to add raw data files to your MAGE-TAB folder? This step is not required, but strongly recommended! Please enter y(es), n(o) or h(elp)!\n")
      if(rawdat == "y"){
        rawlocation <- readline("Please, name the path from your current location to the directory, that contains the raw data!\n")
        rawpath <- .create_path(rawlocation)
        if(.check_path(rawpath)){ 
          rawlist <- list.files(rawpath)
          if(length(rawlist)==0){
            cat(paste("The directory",rawpath,"is empty!\n"))
            rawdat <- ""
          }
          else{
            cat(paste("\nRaw data is taken from", rawpath,".\n The following files are treated as raw data:\n "))
            cat(paste(rawlist,"\n"))
            outraw <- .create_path("raw",out=output_path) 
            rawcopy <- readline(paste("Do you want them to be copied into",outraw,"? Please enter y(es) or n(o)!\n"))
            if(rawcopy == "y"){                               
              if(.check_path(outraw)==FALSE){
	dir.create(outraw,recursive=TRUE)
              }       
              lapply(rawlist,FUN=.copyfun,outraw)
              cat(paste("Data has been successfull copied from", rawpath, "into", outraw,"!\n"))
              rawbool <- TRUE
            } 
          }
        }
        else{
          cat(paste(rawpath,"does not seem to be a valid path to raw data!\n"))
          rawdat <- ""
        }
      }
      else{cat("You decided not to add raw data!\n")}
    }
  }
  else{
    
  }
  return(rawbool)
}
        
##################################################  
## Analyse expressionset                                       
##################################################          
.manage_eset <- function(output_path, expressionset)        
{                
  ######## Processed data ###########################################
  write.table(exprs(expressionset),file=paste(output_path,"/processed.table",sep=""),sep="\t", quote=FALSE)
  ######## SRDF #####################################################
  write.table(pData(expressionset),file=paste(output_path,"/sdrf.table",sep=""),sep="\t",col.names=names(pData(expressionset)), quote=FALSE)
  ######## IDF ######################################################
  write.table(expinfo(experimentData(expressionset)),file=paste(output_path,"/idfinfo.table",sep=""),sep="\t", quote=FALSE)  
  pubmedvec<-c("PubMed ID",pubMedIds(experimentData(expressionset)))
  pubmedstr<-paste(pubmedvec,collapse="\t")
  write(pubmedstr,file=paste(output_path,"/idfinfo.table",sep=""),append=TRUE, quote=FALSE)
  abstractstr<-paste("Experiment Description",abstract(experimentData(expressionset)),collapse="\t")
  write(abstractstr,file=paste(output_path,"/idfinfo.table",sep=""),append=TRUE, quote=FALSE)               
}               

##################################################
## Output Directory
##################################################
.manage_output <- function(interact,output_path)
{
  if(interact){
    outputdir <- ""
    currentwd <- getwd()
    while (outputdir == ""){
      outputdir <- readline(paste("Please, name the path from your current location to the output directory, that should be used by mtcreator. \nYour current working directory is",currentwd,".\n"))
      path <- .create_path(outputdir)
      if(.check_path(path))
        {cat("You choosed ", path, " as output directory!\n")}
      else{
        create <- readline(paste("The given path: ", path, " seems not to lead to a valid and existing output directory. Should it be created? y(es) or n(o)\n"))
        if(create == "y")
          {dir.create(path, recursive=TRUE)}
        else{
          cat("Previous step failed, please try again!\n")
          outputdir <-""
        }                     
      }
    }
  }
#  else{
#    if(.check_path(output_path)==FALSE){
#       path<-.create_path(output_path)
#       dir.create(path, recursive=TRUE)
#    }
#  }
  return(path)
} 

##################################################
## Create Path
##################################################  
.create_path <- function(userinput, out=""){
    if(out==""){
      currentwd <- getwd()
    }
    else{
      currentwd <- out
    }
    userinput <- gsub("(\")","",userinput)
    path <- paste(currentwd,"/",userinput,sep="")
    path <- gsub("//","/",path)
    path <- gsub("/$","",path)
    return(path)
}

##################################################
## Check Path
##################################################  
.check_path <- function(path){
    file <- gsub("/$","",path)
    checkbool <- TRUE
    if(file.exists(file)){
      checkbool <- TRUE
    }
    else{
      checkbool <- FALSE
    }
    return(checkbool)
}

################################################## 
## Copy files from list to outraw           
################################################## 
.copyfun <- function(x,outraw){
  file.copy(paste("input",x,sep="/"),outraw,recursive=TRUE,overwrite=TRUE) 
} 

################################################## 
## Check user given input information           
################################################## 
.check_input <- function(rawbool, rawlocation, expressionset, output_path){
  output <- TRUE
  if(rawbool ==TRUE){
    if(.check_path(rawlocation)){
      rawlist <- list.files(rawlocation)
      if(length(rawlist)==0){
        #cat(paste("The directory",rawlocation,"is empty!\n"))
      }
      else{
        cat(paste("\nRaw data is taken from", rawlocation,".\n The following files are treated as raw data:\n "))
        cat(paste(rawlist,"\n"))
        outraw <- .create_path("raw",out=output_path) 
        #rawcopy <- readline(paste("Do you want them to be copied into",outraw,"? Please enter y(es) or n(o)!\n"))
        #if(rawcopy == "y"){                               
          if(.check_path(outraw)==FALSE){
            dir.create(outraw,recursive=TRUE)
          }       
          lapply(rawlist,FUN=.copyfun,outraw)
          #cat(paste("Data has been successfull copied from", rawpath, "into", outraw,"!\n"))
          #rawbool <- TRUE
        } 
      }
      else{
      output <- FALSE
    }
  }
  #check expressionset   
  if(output_path == ""){
    cat("Missing output folder!\n")
    output <- FALSE
  }
  else{
    if(.check_path(output_path) != TRUE && output_path != ""){
      path<-.create_path(output_path)
      dir.create(path, recursive=TRUE)
    }
  }
  return(output)
} 


