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
#' @param DateOut is a character string that lists the dates to produce output data
#' @param FileOutBase is a character string that denotes the base file name for output
#' @param Read determine if the data are read from hdf5 dp0p input data file or other input files 
#' @param VersDp is the data product level that will be output 
#' @param VersEddy is the version of the eddy4R docker that is being used to perform the processing
#' @param MethParaFlow is the method used to specify workflow parameters, "EnvVar" will grab ParaFlow parameters from environmental variable and "DfltInp" will use whatever is specified in the function call.
#' @param MethDp01Api is a logical (TRUE/FALSE) determining if Dp01 reingest data for storage exchange workflow should be gathered from the API.
#' @param UrlInpRefe A single-entry vector of class "character" containing the web address of the reference input data zip file to be downloaded.
#' @param UrlOutRefe A single-entry vector of class "character" containing the web address of the reference output data zip file to be downloaded.
#' @param MethCh4Conc is a logical (TRUE/FALSE) determining if Dp01 methane concentration from the Picarro G2131 should be run 
#' @param Tokn is a character vector for API token to prevent rate limiting and provide privileges 

#' @return \code{ParaFlow} is a list returned that indicates the workflow control parameters, including \code{ParaFlow$DirFilePara},\code{ParaFlow$DirInp}, \code{ParaFlow$DirMnt}, \code{ParaFlow$DirOut}, \code{ParaFlow$DirTmp}, \code{ParaFlow$DirWrk}, \code{ParaFlow$DateOut}, \code{ParaFlow$FileOutBase},  \code{ParaFlow$Read}, \code{ParaFlow$VersDp}, \code{ParaFlow$VersEddy}, \code{ParaFlow$MethDp01Api}. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0' data product conversions and calculations (NEON.DOC.000823) \cr
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
#   Ke Xu (2017-05-22)
#     adding parameter MethMeas to distinguish different cases for ecte and ecse
#   Stefan Metzger (2017-08-01)
#     superseed parameter MethMeas with the ability to directly provide reference data urls as arguments UrlInpRefe and UrlOutRefe
#   Stefan Metzger (2017-09-29)
#     fixing construction of ParaFlow$DirFilePara for batch-processing (when not using gold file)
#   Dave Durden (2020-07-06)
#     adding MethDp01Api parameter
#   Chris Florian (2021-09-21)
#     adding MethCh4Conc parameter
#   David Durden (2024-01-16)
#     adding API token

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
  DateOut  = NULL,
  FileOutBase = NULL,
  Read  = "hdf5",
  VersDp  = c("001","004")[1],
  VersEddy  = "latest",
  MethParaFlow = c("DfltInp","EnvVar")[1],
  MethDp01Api = TRUE,
  UrlInpRefe,
  UrlOutRefe,
  MethCh4Conc = FALSE,
  Tokn = NULL,
  ...
){
  
  ParaFlow <- list(Deve = Deve, DirFilePara = DirFilePara,DirInp = DirInp,DirMnt = DirMnt,DirOut = DirOut,DirTmp = DirTmp,DirWrk = DirWrk, DateOut = DateOut, FileOutBase = FileOutBase, MethParaFlow = MethParaFlow,Read = Read,VersDp = VersDp,VersEddy = VersEddy, MethDp01Api = MethDp01Api, MethCh4Conc = MethCh4Conc, Tokn = Tokn, ...)
  
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
  
  # Check if DateOut is specified
  if(is.null(ParaFlow$DateOut)|!is.character(ParaFlow$DateOut)) {
    stop("DateOut must be defined and a character string.")
  } else {
    ParaFlow$DateOut <- base::trimws(base::unlist(base::strsplit(x = ParaFlow$DateOut, split = ",")))
  }
  
  # Check if the DirFilePara is specified, if not run gold file example, download gold file from dropbox     
  if(is.null(ParaFlow$DirFilePara) && !is.na(ParaFlow$DirInp)) {
  
    #DirFilePara
    ParaFlow$DirFilePara <- ifelse(any(grepl(pattern = ParaFlow$DateOut, list.files(ParaFlow$DirInp))),
                                   grep(pattern = paste0(".*",ParaFlow$DateOut,".*.h5?"), list.files(ParaFlow$DirInp, full.names = TRUE), value = TRUE),
                                   NULL)

  } else{
  
    # download data
    eddy4R.base::def.dld.zip(Inp = list(Url = UrlInpRefe, Dir = tempdir()))
    
    # assign corresponding DirFilePara
    ParaFlow$DirFilePara <- paste0(tempdir(), "/inpRefe/", list.files(paste0(tempdir(), "/inpRefe"))[1])
    
    # output data
    eddy4R.base::def.dld.zip(Inp = list(Url = UrlOutRefe, Dir = tempdir()))

  }
  
  if(is.null(ParaFlow$Loc)) warning("The variable Loc is NULL") 
  if(is.null(ParaFlow$FileOutBase)) stop("The variable FileOutBase must either be specified as the ENV variable FILEOUTBASE or in the function call to def.para.flow") 
  
  return(ParaFlow)
  
}
