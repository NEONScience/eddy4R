##############################################################################################
#' @title Definition function: Determine the workflow variables

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function to determine the workflow variables by either reading them in from the environmental variables, defining them explicitly, or defining them by default values

#' @param Deve is logical that determines if only a subset of the data should be read in to reduce testing time during development (\code{Deve = TRUE}) or all the input data should be read in (\code{Deve = FALSE})
#' @param DirFilePara is file path for the hdf5 dp0p input data file 
#' @param DirInp is directory path for the hdf5 dp0p input data file 
#' @param DirMnt is the base directory path for where the docker is mounted 
#' @param DirOut is directory path for the output data
#' @param DirTmp is directory path for temporary storage during processing
#' @param DirWrk is directory path for working storage during processing
#' @param FileDp0p is a character string that lists the dates to be processed
#' @param Loc is site where the data was collected (NEON 4 letter code, e.g. SERC)
#' @param Read determine if the data are read from hdf5 dp0p input data file or other input files 
#' @param VersDp is the data product level that will be output 
#' @param VersEddy is the version of the eddy4R docker that is being used to perform the processing
#' @param MethParaFlow is the method used to specify workflow parameters, "EnvVar" will grab ParaFlow parameters from environmental variable and "DfltInp" will use whatever is specified in the function call. 

#' @return \code{ParaFlow} is a list returned that indicates the workflow control parameters, including \code{ParaFlow$DirFilePara},\code{ParaFlow$DirInp}, \code{ParaFlow$DirMnt}, \code{ParaFlow$DirOut}, \code{ParaFlow$DirTmp}, \code{ParaFlow$DirWrk}, \code{ParaFlow$FileDp0p}, \code{ParaFlow$Loc},  \code{ParaFlow$Read}, \code{ParaFlow$VersDp}, \code{ParaFlow$VersEddy}. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823)
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, environment variables, eddy-covariance, ECTE

#' @examples 
#' def.para.flow(DirFilePara = "test.h5", MethParaFlow = "DfltInp")


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-03-12)
#     original creation
#   Dave Durden (2016-04-05)
#     adding Deve parameter

##############################################################################################################
#Start of function call to determine workflow parameters
##############################################################################################################

def.para.flow <- function(
  Deve = TRUE,
  DirFilePara  = NULL,
  DirInp = NA, 
  DirMnt  = NA,
  DirOut  = NA,
  DirTmp  = NA,
  DirWrk  = NA,
  FileDp0p  = NULL,
  Loc  = NULL,
  Read  = "hdf5",
  VersDp  = c("001","004")[1],
  VersEddy  = "latest",
  MethParaFlow = c("DfltInp","EnvVar")[1],
  ...
){
  
  ParaFlow <- list(Deve = Deve, DirFilePara = DirFilePara,DirInp = DirInp,DirMnt = DirMnt,DirOut = DirOut,DirTmp = DirTmp,DirWrk = DirWrk,FileDp0p = FileDp0p,Loc = Loc,MethParaFlow = MethParaFlow,Read = Read,VersDp = VersDp,VersEddy = VersEddy, ...)
  
  if(MethParaFlow == "EnvVar"){
    #Create a list with all the specified function arguments
    
  #lapply across all specified ParaFlow variables  
    lapply(base::names(ParaFlow), function(x) {
      if(base::toupper(x) %in% base::names(base::Sys.getenv())) {
        ParaFlow[[x]] <<- Sys.getenv(base::toupper(x))
        } else {warning(paste0("The variable ParaFlow$",x," is not specified as ENV variable"))}
        })
    
    #Format to grab one variable at a time:
    # if("DIRFILEPARA" %in% base::names(base::Sys.getenv())) {ParaFlow$DirFilePara <- Sys.getenv("DIRFILEPARA")} else {warning("The variable ParaFlow$DirFilePara is not specified as ENV variable")}

  }
  
  # Check if the FileDp0p is specified
    if(is.null(ParaFlow$FileDp0p)|!is.character(ParaFlow$FileDp0p)) {stop("FileDp0p must be defined and a character string.")} else {ParaFlow$FileDp0p <- base::trimws(base::unlist(base::strsplit(x = ParaFlow$FileDp0p, split = ",")))}
   
  # Check if the DirFilePara is specified, if not run gold file example, download gold file from dropbox         
    if(is.null(ParaFlow$DirFilePara)) {
      # input data
      
      # download data
      eddy4R.base::def.dld.zip(Inp = list(Url = "https://www.dropbox.com/s/q1add2a3vg5g228/inpRefe_2.zip?dl=1",
                                          Dir = tempdir()))
      
      # assign corresponding DirFilePara
      ParaFlow$DirFilePara <- paste0(tempdir(), "/inpRefe/", list.files(paste0(tempdir(), "/inpRefe"))[1])
      
      # output data
      eddy4R.base::def.dld.zip(Inp = list(Url = "https://www.dropbox.com/s/65azuhnay8ro463/outRefe_20161227.zip?dl=1",
                                          Dir = tempdir()))
      
    }
    if(is.null(ParaFlow$Loc)) warning("The variable Loc is NULL") 
  #Grab the 

 return(ParaFlow)
  
}
