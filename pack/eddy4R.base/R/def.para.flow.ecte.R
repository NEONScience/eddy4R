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
#' @param UrlInpRefe A single-entry vector of class "character" containing the web address of the reference input data zip file to be downloaded.
#' @param UrlOutRefe A single-entry vector of class "character" containing the web address of the reference output data zip file to be downloaded.

#' @return \code{ParaFlow} is a list returned that indicates the workflow control parameters, including \code{ParaFlow$DirFilePara},\code{ParaFlow$DirInp}, \code{ParaFlow$DirMnt}, \code{ParaFlow$DirOut}, \code{ParaFlow$DirTmp}, \code{ParaFlow$DirWrk}, \code{ParaFlow$DateOut}, \code{ParaFlow$FileOutBase},  \code{ParaFlow$Read}, \code{ParaFlow$VersDp}, \code{ParaFlow$VersEddy}. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
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

##############################################################################################################
#Start of function call to determine workflow parameters
##############################################################################################################


Para$Flow <- def.para.flow.ecte(ParaFlow = Para$Flow
                                )


def.para.flow.ecte <- function(
  ParaFlow
){

  # failsafes ensuring that Para$Flow$Meth is present and one of "dflt",  "host", or "slct"
  
    # ParaFlow$Meth present?
    if(is.null(ParaFlow$Meth)) base::stop('eddy4R.base::def.para.flow.ecte: please provide Para$Flow$Meth.')
  
    # ParaFlow$Meth one of "dflt",  "host", or "slct"?
    if(!ParaFlow$Meth %in% c("dflt",  "host", "slct")) base::stop('eddy4R.base::def.para.flow.ecte: 
                                                                  please ensure that Para$Flow$Meth is one of "dflt",  "host", or "slct".')
  
  # in case of default mode (ParaFlow$Meth == "dflt"), assign default workflow parameters
  # these settings need to change in case a new / different gold file is being used
  if(ParaFlow$Meth == "dflt") {
    
    # sequence of output dates in ISO date format (YYYY-MM-DD) [character]
    # need to correspond to the central day(s) in PrdWndwCalc based on PrdWndwPfDcmp and PrdIncrPfDcmp == PrdIncrCalc
    ParaFlow$DateOut <- base::as.character(base::as.Date(base::as.character(20160424), format = "%Y%m%d"))
    
    # directory for hdf5 L0p input data files in the Docker-internal directory structure [character]
    # needs to be a subdirectory of DirMnt; names of dp0p files in DirInp follow the naming pattern per NEON.DOC.000807 (dp0p ATBD),
    # e.g., ECTE_dp0p_CPER_2016-06-21.h5, so that SCI can reliably split-out the ISO date corresponding to each file
    ParaFlow$DirInp <- base::paste0(base::tempdir(), "/inpRefe")
    
    # mount point of the host operating file system in the Docker-internal directory structure [character]
    # set to NA in case no host file system is mounted
    ParaFlow$DirMnt <- NA
    
    # directory for hdf5 L1 - L4 output data files in the Docker-internal directory structure [character]
    # needs to be a subdirectory of DirMnt
    ParaFlow$DirOut <- base::paste0(base::tempdir(), "/out")
    
    # directory for temporary objects in the Docker-internal directory structure [character]
    # if file-backed objects are to be stored outside of the Docker container, DirTmp needs to be a subdirectory of DirMnt
    # TODO: uses the Docker-internal default temporary directory if set to NA    
    ParaFlow$DirTmp <- base::tempdir()
    
    # working directory in the Docker-internal directory structure [character]
    # the root directory for specifying relative pathes in eddy4R-Docker
    # TODO: uses the Docker-internal default temporary directory if set to NA
    ParaFlow$DirWrk <- base::tempdir()
    
    # optional: sequence of L0p hdf5 file names that should be considered for processing [character]
    # which supports having files for various sites in the same DirInp
    # TODO: defaults to NA / assumed NA if not provided, in which case all files in DirInp are considered for processing
    ParaFlow$FileInp <- NA
    
    # basename for hdf5 L1 - L4 output data files [character]
    # on this basis SCI creates the output files as DirOut/FileOutBase_DateOut_basic.hdf5 and DirOut/FileOutBase_DateOut_expanded.hdf5
    ParaFlow$FileOutBase <- "NEON.D02.SERC.DP4.00200.001.ec-flux"
    
    # sequence of external data product names incl. repository addresses [named character]
    # set to NA in case no external data products are used
    ParaFlow$NameDataExt <- NA
    
    # period increment calculation [integer] {days}
    # step-size by which "period window calculation" moves through L0p data, currently planned by 1 day
    ParaFlow$PrdIncrCalc <- 1
    
    # period increment planar-fit [integer] {days}
    # step-size by which "period window planar-fit" moves through "period window calculation", currently planned by 1 day
    ParaFlow$PrdIncrPf <- 1
    
    # period window calculation [integer] {days}
    # defining the time-block of data that is to be processed by the single execution of a workflow
    # needs to be geq "period window planar-fit", currently planned for 9 days
    ParaFlow$PrdWndwCalc <- 1
    
    # period window planar-fit [integer] {days}
    # defining the time-block of data for which fitting of aerodynamic plane is being performed
    # needs to be leq "period window calculation", currently planned for 9 days
    ParaFlow$PrdWndwPf <- 1
    
    # read L0p data from hdf5 (hdf5) or attempt to read pre-groomed data from fast file-backed object (ff) if available [character]
    ParaFlow$Read <- c("hdf5", "ff")[1]
    
    # data product version; e.g. provisional, 001 etc. [character]
    ParaFlow$VersDp <- "001"
    
    # software version of eddy4R-Docker, e.g. "0.2.1", "latest"… [character]
    ParaFlow$VersEddy <- "latest"
    
  }
  
  # in case of user selection mode (ParaFlow$Meth == "slct")
  # don't modify ParaFlow -> directories are created / set and unmodified ParaFlow are returned below
  
    
  
  # create and set directories
  # TODO: consider moving to eddy4R.base::def.env.glob()

    # input directory
    if(!base::dir.exists(ParaFlow$DirInp)) base::dir.create(ParaFlow$DirInp)

    # output directory
    if(!base::dir.exists(ParaFlow$DirOut)) base::dir.create(ParaFlow$DirOut)

    # temporary directory
    if(!base::dir.exists(ParaFlow$DirTmp)) base::dir.create(ParaFlow$DirTmp)
  
    # working directory
    if(!base::dir.exists(ParaFlow$DirWrk)) base::dir.create(ParaFlow$DirWrk)
    if(base::getwd() != ParaFlow$DirWrk) base::setwd(ParaFlow$DirWrk)

  # return results
  base::return(ParaFlow)
  
  
  
  

  
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
