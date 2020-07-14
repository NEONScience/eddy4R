##############################################################################################
#' @title Wrapper function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@@battelleecology.org}

#' @description 
#' Wrapper function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param inpList List consisting of \code{ff::ffdf} file-backed objects, in the format provided by function \code{eddy4R.base::wrap.hdf5.read()}. Of types numeric and integer.
#' @param Sens Character string indicating which sensor will remove high frequency data points that have failed quality flags. Defaults to NULL.
#' @param qfRmv Character string indicating which quality flag will exclude in the processing. Defaults to NULL.
#' @param Vrbs Optional. A logical {FALSE/TRUE} value indicating whether to:\cr
#' \code{Vrbs = FALSE}: (Default) cleaned data set with the bad high frequency quality flagged data replaced with NaN's as part of the \code{inpList} in the same format., or \cr
#' \code{Vrbs = TRUE}: cleaned data set with the bad high frequency quality flagged data replaced with NaN's as part of the \code{inpList} in the same format. In addition, a separate list  \code{qfqmAnls} will be added to the output list \code{rpt} with a list of variables assessed, a list of quality flags for each variable assessed, the number of each quality flag tripped for each variable and the total number of bad data per variable.
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecte". [-]

#' 
#' @return The returned object consistes of \code{inpList}, with the bad high frequency quality flagged data replaced with NaN's. Optionally, (\code{Vrbs = TRUE}) a separate list \code{qfqmAnls} will be added to the output list \code{rpt} with a list of variables assessed, a list of quality flags for each variable assessed, the number of each quality flag tripped for each variable and the total number of bad data per variable.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0' data product conversions and calculations (NEON.DOC.000807) \cr
#' Licor LI7200 reference manual

#' @keywords NEON, qfqm, quality

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-10-27)
#     original creation
#   Natchaya Pingintha-Durden (2018-01-23)
#     added MethMeas to paramerter and modified original function to work with ECSE
#   Natchaya P-Durden (2018-04-13)
#    applied eddy4R term name convention; replaced dfData by inpData
#    replaced dfQf by inpQf
#   Natchaya P-Durden (2019-03-12)
#    added Sens and qfRmv into the function parameter list
#   David Durden (2020-07-14)
#    added despiking routine and output qfSpk and qfNull
##############################################################################################


wrap.qf.rmv.data <- function(
  inpList,
  Sens = NULL,
  qfRmv = NULL,
  Vrbs = FALSE, 
  MethMeas = c("ecte",  "ecse")[1]){
  
  #Initialize reporting list
  rpt <- inpList
  outList <- list()
  
  #Determine the sensors that have data and quality flags
  if (is.null(Sens)){
  Sens <- base::intersect(base::names(inpList$data), base::names(inpList$qfqm))
  }
  
  if(MethMeas == "ecte"){
  # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
  outList <- base::lapply(Sens, function(x){ 
    eddy4R.qaqc::def.qf.rmv.data(inpData = inpList$data[[x]][], inpQf = inpList$qfqm[[x]], Sens = x, qfRmv = qfRmv, Vrbs = Vrbs) #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
  })
  
  #Apply names to the output list
  base::names(outList) <- Sens
 
  #Despiking routine 
base::lapply(Sens, function(x){ 
    #x <- Sens[2] #for testing
    #print(x)
    varDspk <- names(outList[[x]]$inpData)[!names(outList[[x]]$inpData) %in% c("time","idx","frtSet00")]
      base::lapply(varDspk, function(y){
        #print(y)
       #y <- "asrpCo2"
        tmp <- eddy4R.qaqc::def.dspk.br86(
          # input data, univariate vector of integers or numerics
          dataInp = outList[[x]]$inpData[[y]][],
          # filter width
          WndwFilt = as.numeric(ramattribs(inpList$data[[x]][[y]])$`Dspk$Br86$NumWndw`),
          # initial number/step size of histogram bins
          NumBin = as.numeric(ramattribs(inpList$data[[x]][[y]])$`Dspk$Br86$NumBin`),
          # resolution threshold
          ThshReso = as.numeric(ramattribs(inpList$data[[x]][[y]])$`Dspk$Br86$MaxReso`)
        ) #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
        outList[[x]]$inpData[[y]] <<- tmp$dataOut
        
        #Create flag name
        nameQf <- base::paste0("qfSpk",base::toupper(base::substring(y,1,1)),base::substring(y,2,base::nchar(y)))
        #Output despiking flag
        outList[[x]]$qfSpk[[nameQf]] <<- tmp$qfSpk
        
      })  #End lapply for variables
  })#End lapply around sensors
  
  #Applying the bad quality flags to the reported output data
  base::lapply(base::names(outList), function(x) {
    #Outputting the data ffdf's
    rpt$data[[x]] <<- ff::as.ffdf(outList[[x]]$inpData) 
    rpt$data[[x]] <<- eddy4R.base::def.unit.var(samp = rpt$data[[x]], refe = inpList$data[[x]]) #Copy units
    rpt$qfqm[[x]] <<- ff::as.ffdf(base::cbind(rpt$qfqm[[x]][], outList[[x]]$qfNull, as.data.frame(outList[[x]]$qfSpk)))
  })
  
  #If verbose is true write out all the information about the quality flags applied to the raw data
  if(Vrbs == TRUE){
    base::lapply(base::names(outList), function(x) {
      #Write out qfqm analytics as a separate list to the output
      rpt$qfqmAnls[[x]] <<- outList[[x]][!names(outList[[x]]) %in% "inpData"] 
    })}
  }#end of MethMeas == "ecte"
  
  if(MethMeas == "ecse"){
    # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
    # start loop around instruments
    for(idxSens in Sens){
    #for each measurement level
    outList[[idxSens]] <- base::lapply(base::names(inpList$data[[idxSens]]), function(x){ 
      eddy4R.qaqc::def.qf.rmv.data(inpData = inpList$data[[idxSens]][[x]], inpQf = inpList$qfqm[[idxSens]][[x]], Sens = idxSens, qfRmv = qfRmv, Vrbs = Vrbs, TypeData = "real") #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
      })
    #Apply names to the each mesurement level
    base::names(outList[[idxSens]]) <- base::names(inpList$data[[idxSens]])
    
    #Applying the bad quality flags to the reported output data
    base::lapply(base::names(inpList$data[[idxSens]]), function(x) {
      #Outputting the data ffdf's
      rpt$data[[idxSens]][[x]] <<- outList[[idxSens]][[x]]$inpData
      rpt$data[[idxSens]][[x]] <<- eddy4R.base::def.unit.var(samp = rpt$data[[idxSens]][[x]], refe = inpList$data[[idxSens]][[x]]) #Copy units
    })
    
    #If verbose is true write out all the information about the quality flags applied to the raw data
    if(Vrbs == TRUE){
      base::lapply(base::names(inpList$data[[idxSens]]), function(x) {
        #Write out qfqm analytics as a separate list to the output
        rpt$qfqmAnls[[idxSens]][[x]] <<- outList[[idxSens]][[x]][!names(outList[[idxSens]][[x]]) %in% "inpData"] 
      })}
    }#end of each sensor
  }#end of MethMeas == "ecse"
  
  
  #Return the list of information with bad data removed
  return(rpt)
}
