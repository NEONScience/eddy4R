##############################################################################################
#' @title Definition function: Determine workflow parameters for NEON's eddy-covariance turbulence exchange workflow

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Definition function. Collects the workflow parameters for NEON's eddy-covariance turbulence exchange workflow. It also checks for the presence and correctness of the most important workflow parameters (\code{ParaFlow$Meth}, \code{ParaFlow$DirInp}, \code{ParaFlow$DateOut}, \code{ParaFlow$FileOutBase}, \code{ParaFlow$FileOutBase}), and creates and sets the input, output, temporary and working directories. One of three methods needs to be provided via the \code{ParaFlow$Meth} argument: \cr
#' \enumerate{
#'   \item \code{ParaFlow$Meth == "dflt"} downloads the web-based reference input and output data files, and returns a list of workflow parameters corresponding to the "default mode" (for use with the reference files). This includes all workflow parameters listed in below overview. The arguments \code{UrlInpRefe} and \code{UrlInpRefe} need to be provided. \cr
#'   \item \code{ParaFlow$Meth == "host"} returns a list of workflow parameters corresponding to the environmental-variables-from-host mode (for batch-processing from the command line). For each workflow parameter in below overview the corresponding environmental variable (in all CAPS!) is searched. If the corresponding environmental variable is found, its value is assigned to the workflow parameter. If the corresponding environmental variable is not found, \code{NA} is assigned to the workflow parameter. Additional workflow parameters / environmental variables to be included can be specified via the \code{...} argument. If for a single workflow parameter a KEY-value pair is passed, its corresponding environmental variable needs to be constructed with an equals sign (KEY=value). If for a single workflow parameter multiple values or KEY-value pairs are passed, they need to be separated in the environmental variable by a colon (KEY1=value1:KEY2=value2 ...). \cr
#'   \item \code{ParaFlow$Meth == "slct"} returns a list of workflow parameters corresponding to the user selection mode (for interactive data analysis in Rstudio). The user selection is performed in the workflow file itself (e.g., flow.turb.tow.neon.dp04.r) and merely passed through the def.para.flow.ecte() function (plus some tests and directory settings). \cr
#' } \cr

#' \strong{Overview of workflow parameters}
#' \describe{
#'   \item{\code{ParaFlow$Meth}}{Deploy workflow file in default mode (\code{"dflt"}, uses web-based reference file), in environmental-variables-from-host mode (\code{"host"}, for batch-processing from command line), or in user selection mode (\code{"slct"}, for interactive data analysis in Rstudio). [character]}

#'   \item{\code{ParaFlow$DateOut}}{Sequence of output dates in ISO date format (YYYY-MM-DD). Need to correspond to the central day(s) in \code{ParaFlow$PrdWndwCalc} based on \code{ParaFlow$PrdWndwPf}, \code{ParaFlow$PrdIncrPf == ParaFlow$PrdIncrCalc}. [character / value1:value2 ... environmental variable]}

#'   \item{\code{ParaFlow$DirInp}}{Directory for hdf5 L0p input data files in the Docker-internal directory structure. Needs to be a subdirectory of \code{ParaFlow$DirMnt}; names of L0p files in \code{ParaFlow$DirInp} need to follow the naming pattern per NEON.DOC.000807 (L0p ATBD), e.g., ECTE_dp0p_CPER_2016-06-21.h5, so that SCI can reliably split-out the ISO date corresponding to each file.  [character]}

#'   \item{\code{ParaFlow$DirMnt}}{Mount point of the host operating file system in the Docker-internal directory structure. Set to \code{NA} in case no host file system is mounted. [character]}

#'   \item{\code{ParaFlow$DirOut}}{Directory for hdf5 L1 - L4 output data files in the Docker-internal directory structure. Needs to be a subdirectory of \code{ParaFlow$DirMnt}. [character]}

#'   \item{\code{ParaFlow$DirTmp}}{Directory for temporary objects in the Docker-internal directory structure. If file-backed objects are to be stored outside of the Docker container (required for \code{ParaFlow$PrdWndwPf > 1}!), \code{ParaFlow$DirTmp} needs to be a subdirectory of \code{ParaFlow$DirMnt}. Uses the Docker-internal default temporary directory if set to \code{NA}. [character]}

#'   \item{\code{ParaFlow$DirWrk}}{Working directory in the Docker-internal directory structure. The root directory for specifying relative pathes in eddy4R-Docker. Uses the Docker-internal default temporary directory if set to \code{NA}. [character]}

#'   \item{\code{ParaFlow$FileInp}}{Optional: sequence of L0p hdf5 file names that should be considered for processing. This supports having files for various sites in the same \code{ParaFlow$DirInp}. Defaults to \code{NA} / assumed \code{NA} if not provided, in which case all files in \code{ParaFlow$DirInp} are considered for processing. [character / value1:value2 ... environmental variable]}

#'   \item{\code{ParaFlow$FileOutBase}}{Basename for hdf5 L1 - L4 output data files. On this basis the workflow flow.turb.tow.neon.dp04.r creates the output files as \code{base::file.path(ParaFlow$DirOut, base::paste0(ParaFlow$FileOutBase, "_", ParaFlow$DateOut, "_basic.hdf5"))} and \code{base::file.path(ParaFlow$DirOut, base::paste0(ParaFlow$FileOutBase, "_", ParaFlow$DateOut, "_expanded.hdf5"))}. [character]}

#'   \item{\code{ParaFlow$NameDataExt}}{Sequence of external data product names incl. repository addresses. Set to NA in case no external data products are used. [named character / KEY1=value1:KEY2=value2 ... environmental variable]}

#'   \item{\code{Para$Flow$OutMeth}}{Optional: output hdf5 files (\code{Para$Flow$OutMeth = "hdf5"}) and / or diagnostic .csv files and .png plots (\code{Para$Flow$OutMeth = "diag"}). Defaults to \code{NA} / assumed \code{NA} if not provided, in which case only hdf5 files are produced. [character / value1:value2 ... environmental variable]}

#'   \item{\code{Para$Flow$OutSub}}{Optional: for each day, should all output periods be produced (e.g., 48 half-hours, \code{Para$Flow$OutSub = NA}), or only a subset (e.g., half-hours 5 through 15, \code{Para$Flow$OutSub = 5:15}). This supports shorter test runs irrespective of the choice of \code{Para$Flow$Meth}. Defaults to \code{NA} / assumed \code{NA} if not provided, in which case all output periods are produced. [integer]}

#'   \item{\code{ParaFlow$PrdIncrCalc}}{Period increment calculation, step-size by which \code{ParaFlow$PrdWndwCalc} moves through L0p data, currently planned by 1 day. [integer] {days}}

#'   \item{\code{ParaFlow$PrdIncrPf}}{Period increment planar-fit, step-size by which \code{ParaFlow$PrdWndwPf} moves through "period window calculation", currently planned by 1 day. [integer] {days}}

#'   \item{\code{ParaFlow$PrdWndwCalc}}{Period window calculation, defining the time-block of data that is to be processed by the single execution of a workflow. Needs to be >= \code{ParaFlow$PrdWndwPf}, currently planned for 9 days. [integer] {days}}

#'   \item{\code{ParaFlow$PrdWndwPf}}{Period window planar-fit, defining the time-block of data for which fitting of aerodynamic plane is being performed. Needs to be <= \code{ParaFlow$PrdWndwCalc}, currently planned for 9 days. [integer] {days}}

#'   \item{\code{ParaFlow$Read}}{Read L0p data from hdf5 (\code{"hdf5"}) or attempt to read pre-groomed data from fast file-backed object (\code{"ff"}) if available. [character]}

#'   \item{\code{ParaFlow$VersDp}}{Data product version; e.g. \code{"provisional"}, \code{"001"} etc. [character]}

#'   \item{\code{ParaFlow$VersEddy}}{Software version of eddy4R-Docker, e.g. \code{"0.2.1"}, \code{"latest"} etc. [character]}
#' }

#' @param ParaFlow A list with at least one entry \code{ParaFlow$Meth}.
#' @param UrlInpRefe A single-entry vector containing the web address of the reference input data zip file to be downloaded. [character]
#' @param UrlOutRefe A single-entry vector containing the web address of the reference output data zip file to be downloaded. [character]
#' @param ... Only for \code{ParaFlow$Meth == "host"}: names of additional workflow parameters / environmental variables to be included. [character]

#' @return A list containing the workflow control parameters. See Description for details. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr

#' @keywords NEON, environment variables, eddy-covariance, ECTE

#' @examples
#' def.para.flow.ecte(ParaFlow = list("Meth" = "dflt"),
#' UrlInpRefe = "https://www.dropbox.com/s/qlp1pyanm5rn2eq/inpRefe_20170308.zip?dl=1",
#' UrlOutRefe = "https://www.dropbox.com/s/j0zhjfolx4k9ugm/outRefe_20170918.zip?dl=1")

#' @seealso
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2017-10-01)
#     original creation re-purposing parts from eddy4R.base::def.para.flow.R

##############################################################################################################
#Start of function call to determine workflow parameters
##############################################################################################################
def.para.flow.ecte <- function(
  ParaFlow,
  UrlInpRefe,
  UrlOutRefe,
  ...
){

  
  # error messages ensuring that ParaFlow$Meth is present and one of "dflt",  "host", or "slct"
  
    # ParaFlow$Meth present?
    if(is.null(ParaFlow$Meth)) base::stop('please provide Para$Flow$Meth.')
  
    # ParaFlow$Meth one of "dflt",  "host", or "slct"?
    if(!ParaFlow$Meth %in% c("dflt",  "host", "slct")) base::stop('please ensure that Para$Flow$Meth is one of "dflt",  "host", or "slct".')
  
  
  # in case of default mode (ParaFlow$Meth == "dflt"), assign default workflow parameters
  # in case of environmental-variables-from-host mode (host), only the list names are used further below
  # these settings need to change in case a new / different reference file is being used
  if(ParaFlow$Meth %in% c("dflt", "host")) {
    
    # assign defaults
    # for a detailed description of each workflow parameter see ?eddy4R.base::def.para.flow.ecte, section Overview of workflow parameters
    ParaFlow$DateOut <- base::as.character(base::as.Date(base::as.character(20160424), format = "%Y%m%d"))
    ParaFlow$DirInp <- base::paste0(base::tempdir(), "/inpRefe")
    ParaFlow$DirMnt <- NA
    ParaFlow$DirOut <- base::paste0(base::tempdir(), "/out")
    ParaFlow$DirTmp <- base::tempdir()
    ParaFlow$DirWrk <- base::tempdir()
    ParaFlow$FileInp <- NA
    ParaFlow$FileOutBase <- "NEON.D02.SERC.DP4.00200.001.ec-flux"
    ParaFlow$NameDataExt <- NA
    ParaFlow$OutMeth <- c("hdf5", "diag")
    ParaFlow$OutSub <- 1:10
    ParaFlow$PrdIncrCalc <- 1
    ParaFlow$PrdIncrPf <- 1
    ParaFlow$PrdWndwCalc <- 1
    ParaFlow$PrdWndwPf <- 1
    ParaFlow$Read <- c("hdf5", "ff")[1]
    ParaFlow$VersDp <- "001"
    ParaFlow$VersEddy <- "latest"
    
    # in case of default mode (ParaFlow$Meth == "dflt"), download reference data
    if(ParaFlow$Meth == "dflt") {
    
      # input data
      eddy4R.base::def.dld.zip(Inp = list(Url = UrlInpRefe, Dir = ParaFlow$DirWrk))
      
      # output data
      eddy4R.base::def.dld.zip(Inp = list(Url = UrlOutRefe, Dir = ParaFlow$DirWrk))
      
    }
    
  }
  
  
  # in case of environmental-variables-from-host mode (host), read workflow parameters from environmental variables
  if(ParaFlow$Meth == "host") {

    # determine the entries to search for in the environmental variables
    # these are all entries except for "Meth"
    namePara <- c(base::names(ParaFlow)[!base::names(ParaFlow) == "Meth"], ...)
  
    # remove all default entries from ParaFlow
    # these are all entries except for "Meth"
    ParaFlow <- ParaFlow[base::names(ParaFlow) == "Meth"]

    # read entries for ParaFlow from environmental variables
    lapply(namePara, function(x) {
      
      # in case corresponding environmental variable present
      if(base::toupper(x) %in% base::names(base::Sys.getenv())) {
        
        # assign value of corresponding environmental variable
        ParaFlow[[x]] <<- base::Sys.getenv(base::toupper(x))
        
        # split strings by colon (:) to separate multiple multiple values (if present)
        if(is.character(ParaFlow[[x]])) ParaFlow[[x]] <<- strsplit(x = ParaFlow[[x]], split = ":")[[1]]
        
      # in case corresponding environmental variable NOT present
      } else {
        ParaFlow[[x]] <<- NA
        base::warning(paste0("the workflow parameter ", x, " is not specified as environmental variable."))
      }
    })
    
  }

  
  # in case of user selection mode (ParaFlow$Meth == "slct")
  # don't modify ParaFlow -> directories are created / set and unmodified ParaFlow are returned below
  
  
  # error messages
  
    # ParaFlow$DirInp
    if(is.null(ParaFlow$DirInp) | !is.character(ParaFlow$DirInp)) stop("please specify the workflow parameter DirInp and ensure it is of type character.")
    if(base::length(base::dir(ParaFlow$DirInp, pattern = "*.h5")) == 0) stop(base::paste0("please provide input files in ", ParaFlow$DirInp, "."))
  
    # ParaFlow$DateOut
    if(is.null(ParaFlow$DateOut) | !is.character(ParaFlow$DateOut)) stop("please specify the workflow parameter DateOut and ensure it is of type character.")
    
    # ParaFlow$FileOutBase
    if(is.null(ParaFlow$FileOutBase) | !is.character(ParaFlow$FileOutBase)) stop("please specify the workflow parameter FileOutBase and ensure it is of type character.")
  
    # ParaFlow$FileOutBase
    if(is.null(ParaFlow$Read) | !is.character(ParaFlow$Read) | !ParaFlow$Read %in% c("hdf5",  "ff")) stop("please specify the workflow parameter Read and ensure it is of type character.")

    
  # assign defaults for optional workflow parameters

    # input files: if null or NA, read file names from ParaFlow$DirInp
    if(is.null(ParaFlow$FileInp)) {
      ParaFlow$FileInp <- base::dir(ParaFlow$DirInp, pattern = "*.h5")
    } else if(base::any(base::is.na(ParaFlow$FileInp))) {
      ParaFlow$FileInp <- base::dir(ParaFlow$DirInp, pattern = "*.h5")
    }

    # output method; if null or NA, assign "hdf5"
    if(is.null(ParaFlow$OutMeth)) {
      ParaFlow$OutMeth <- "hdf5"
    } else if(base::any(base::is.na(ParaFlow$OutMeth))) {
      ParaFlow$OutMeth <- "hdf5"
    }

    # output subset
    if(is.null(ParaFlow$OutSub)) ParaFlow$OutSub <- NA

    
  # create and set directories, read list of files in input directory
  # TODO: consider moving to eddy4R.base::def.env.glob()
    
    # output directory
    if(!base::dir.exists(ParaFlow$DirOut)) base::dir.create(ParaFlow$DirOut, recursive = TRUE)

    # temporary directory
    if(is.na(ParaFlow$DirTmp)) ParaFlow$DirTmp <- base::tempdir()
    if(!base::dir.exists(ParaFlow$DirTmp)) base::dir.create(ParaFlow$DirTmp, recursive = TRUE)
  
    # working directory
    if(is.na(ParaFlow$DirWrk)) ParaFlow$DirWrk <- base::tempdir()
    if(!base::dir.exists(ParaFlow$DirWrk)) base::dir.create(ParaFlow$DirWrk, recursive = TRUE)
    if(base::getwd() != ParaFlow$DirWrk) base::setwd(ParaFlow$DirWrk)

  
  # sort results alphabetically for easier navigation
  ParaFlow <- ParaFlow[base::sort(base::names(ParaFlow))]
    
    
  # return results
  base::return(ParaFlow)
 
   
}