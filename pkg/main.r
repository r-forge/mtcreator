##################################################
## Main function called by user
##################################################
mtcreator <- function(interact = 1,...)
{
  output_path <- .manage_output()
  logic_raw <- .manage_raw(output_path)
  logic_rdat <- .manage_rdat(output_path)
  if(logic_raw && logic_rdat){cat(paste("Rawdata has been imported and .rdat file has been interpreted successfully! Please check the output files in",output_path,"for mistakes and missing values!")) }
  else if(logic_raw){cat(paste("Rawdata has been imported successfully! Please check the output file in",output_path,"for mistakes and missing values!"))}
  else if(logic_rdat){cat(paste("R.rdat file has been interpreted successfully! Please check the output files in",output_path,"for mistakes and missing values!"))}
  else{("No data was given. There are no results to check.")}
}  

##################################################
## Raw data
##################################################
.manage_raw <- function(output_path)
{
  rawdat <- ""
  rawbool <- FALSE
  while (rawdat == ""){
    rawdat <- readline("Do you want to add raw data files to your MAGE-TAB folder? This step is not required, but strongly recommended! Please enter y(es), n(o) or h(elp)!\n")
    if(rawdat == "y"){
      rawlocation <- readline("Please, name the path from your current location to the directory, that contains the raw data!\n")
      rawpath <- .create_path(rawlocation)
      #rawpathstar <- .create_path(rawlocation, star=TRUE)
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
          #cat(output_path)                               
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
  return(rawbool)
}
        
##################################################  
## Create Proc-File                                         
##################################################          
.manage_rdat <- function(output_path)        
{ 
  rdat <- ""
  while(rdat == ""){
    rdat <- readline("In the next step the R workspace is interpreted. Do you have an .rdat file? Please enter y(es), n(o) or h(elp)!\n")
    if(rdat == "y"){
       rdatname <- readline("Please, name the path from your current location to the .rdat file!\n")
       rdatpath <- .create_path(rdatname, out="./")
       if(.check_path(rdatpath)){                
        objectname<-load(rdatpath)
        expressionset<-get(objectname)
        # Processed data
        write.table(exprs(expressionset),file=paste(output_path,"/processed.table",sep=""),sep="\t")
        # SRDF
        write.table(pData(expressionset),file=paste(output_path,"/sdrf.table",sep=""),sep="\t",col.names=names(pData(expressionset)))
        # IDF
        write.table(expinfo(experimentData(expressionset)),file=paste(output_path,"/idfinfo.table",sep=""),sep="\t")  
        pubmedvec<-c("PubMed ID",pubMedIds(experimentData(expressionset)))
        pubmedstr<-paste(pubmedvec,collapse="\t")
        write(pubmedstr,file=paste(output_path,"/idfinfo.table",sep=""),append=TRUE)
        abstractstr<-paste("Experiment Description",abstract(experimentData(expressionset)),collapse="\t")
        write(abstractstr,file=paste(output_path,"/idfinfo.table",sep=""),append=TRUE)

       }
       else{
        cat(paste(rdatpath,"does not seem to be a valid path to an .rdat file!\n"))
        rdat <- ""
       }    
    }
    else{cat("You decided not to add data from an .rdat file!\n")}
  }      
  return(TRUE)         
}               

##################################################
## Output Directory
##################################################
.manage_output <- function()
{
  outputdir <- ""
  currentwd <- getwd()
  while (outputdir == ""){
    outputdir <- readline(paste("Please, name the path from your current location to the output directory, that should be used by mtcreator. \nYour current working directory is",currentwd,".\n"))
    #outputdir <- gsub("(\")","",outputdir)
    #outputdir <- gsub("/$","",outputdir)
    #path <- paste(currentwd,"/",outputdir,sep="")
    path <- .create_path(outputdir)
    if(.check_path(path))
      {cat("You choosed ", path, " as output directory!\n")}
    else{
      create <- readline(paste("The given path: ", path, " seems not to lead to a valid and existing output directory. Should it be created? y(es) or n(o)\n"))
      #cat(paste(currentwd,"/",outputdir,sep=""))
      if(create == "y")
        {dir.create(path, recursive=TRUE)}
      else{
        cat("Previous step failed, please try again!\n")
        outputdir <-""
      }                     
    }
  }
  return(path)
} 

##################################################
## Create Path
##################################################  
.create_path <- function(userinput, out=""){#, star=FALSE){
    if(out==""){
      currentwd <- getwd()
    }
    else{
      currentwd <- out
    }
    #if(star){
    #  userinput <- paste(userinput,"/*",sep="")
    #}    
    userinput <- gsub("(\")","",userinput)
    #userinput <- gsub("/$","",userinput)
    path <- paste(currentwd,"/",userinput,sep="")
    path <- gsub("//","/",path)
    path <- gsub("/$","",path)
    #cat(path)
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

 
 
 
 
 
 
 
      #rdatpath <- .create_path(rdatname)
     #if(.check_path(rdatpath)){ 
      #outproc <- .create_path("processed",out=output_path)
      #outidf <- .create_path("idf",out=output_path)
      #outsmtp <- .create_path("smtp",out=output_path)
      #dirvec <- c(outproc, outidf, outsmtp)
      #if(.check_path(dirvec)==FALSE){
      #  dir.create(dirvec, recursive = TRUE)
      #} 