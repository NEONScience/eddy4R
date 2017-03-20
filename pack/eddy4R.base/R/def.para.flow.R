##############################################################################################
#' @title Definition function: Determine the workflow variables

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function to determine the workflow variables by either reading them in from the environmental variables, defining them explicitly, or defining them by default values

#' @param 

#' @return \code{ParaFlow} is a list returned that indicates the workflow control parameters, including \code{ParaFlow$DirInp}, \code{ParaFlow$DirMnt}, \code{ParaFlow$DirOut}, \code{ParaFlow$DirTmp}, \code{ParaFlow$DirWrk}, \code{ParaFlow$Loc}, \code{ParaFlow$DirFilePara}, \code{ParaFlow$FileDp0p}, \code{ParaFlow$Read}, \code{ParaFlow$VersDp}, \code{ParaFlow$VersEddy}. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823)
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-03-12)
#     original creation

##############################################################################################################
#Start of function call to determine workflow parameters
##############################################################################################################

def.para.flow <- function(
  DirFilePara  = NULL,
  DirInp = "NA", 
  DirMnt  = "NA",
  DirOut  = "NA",
  DirTmp  = "NA",
  DirWrk  = "NA",
  FileDp0p  = NULL,
  Loc  = NULL,
  Read  = "hdf5",
  VersDp  = c("001","004")[1],
  VersEddy  = "cybi",
  MethParaFlow = c("DfltInp","EnvVar")[1],
  ...
){
  
  ParaFlow <- list(DirFilePara = DirFilePara,DirInp = DirInp,DirMnt = DirMnt,DirOut = DirOut,DirTmp = DirTmp,DirWrk = DirWrk,FileDp0p = FileDp0p,Loc = Loc,MethParaFlow = MethParaFlow,Read = Read,VersDp = VersDp,VersEddy = VersEddy, ...)
  
  if(MethParaFlow == "EnvVar"){
    #Create a list with all the specified function arguments
    
    
    if("DIRFILEPARA" %in% base::names(base::Sys.getenv())) {ParaFlow$DirFilePara <- Sys.getenv("DIRFILEPARA")} else {warning("The variable ParaFlow$DirFilePara is not specified as ENV variable")}
    if("DIRINP" %in% base::names(base::Sys.getenv())) {ParaFlow$DirInp <- Sys.getenv("DIRINP")} else {warning("The variable ParaFlow$DirInp is not specified as ENV variable")}
    if("DIRMNT" %in% base::names(base::Sys.getenv())) {ParaFlow$DirMnt <- Sys.getenv("DIRMNT")} else {warning("The variable ParaFlow$DirMnt is not specified as ENV variable")}
    if("DIROUT" %in% base::names(base::Sys.getenv())) {ParaFlow$DirOut <- Sys.getenv("DIROUT")} else {warning("The variable ParaFlow$DirOut is not specified as ENV variable")}
    if("DIRTMP" %in% base::names(base::Sys.getenv())) {ParaFlow$DirTmp <- Sys.getenv("DIRTMP")} else {warning("The variable ParaFlow$DirTmp is not specified as ENV variable")}
    if("DIRWRK" %in% base::names(base::Sys.getenv())) {ParaFlow$DirWrk <- Sys.getenv("DIRWRK")} else {warning("The variable ParaFlow$DirWrk is not specified as ENV variable")}
    if("FILEDP0P" %in% base::names(base::Sys.getenv())) {ParaFlow$FileDp0p <- Sys.getenv("FILEDP0P")} else {warning("The variable ParaFlow$FileDp0p is not specified as ENV variable")}
    if("LOC" %in% base::names(base::Sys.getenv())) {ParaFlow$Loc <- Sys.getenv("LOC")} else {warning("The variable ParaFlow$Loc is not specified as ENV variable")}
    if("METHPARAFLOW" %in% base::names(base::Sys.getenv())) {ParaFlow$MethParaFlow <- Sys.getenv("METHPARAFLOW")} else {warning("The variable ParaFlow$MethParaFlow is not specified as ENV variable")}
    if("READ" %in% base::names(base::Sys.getenv())) {ParaFlow$Read <- Sys.getenv("READ")} else {warning("The variable ParaFlow$Read is not specified as ENV variable")}
    if("VERSDP" %in% base::names(base::Sys.getenv())) {ParaFlow$VersDp <- Sys.getenv("VERSDP")} else {warning("The variable ParaFlow$VersDp is not specified as ENV variable")}
    if("VERSEDDY" %in% base::names(base::Sys.getenv())) {ParaFlow$VersEddy <- Sys.getenv("VERSEDDY")} else {warning("The variable ParaFlow$VersEddy is not specified as ENV variable")}
    
  }
  
  
    if(is.null(ParaFlow$FileDp0p)|is.null(ParaFlow$DirFilePara)){stop("DirFilePara and FileDp0p must be defined.")}
    if(is.null(ParaFlow$Loc)) warning("The variable Loc is NULL") 
  #Grab the 

 return(ParaFlow)
  
}
