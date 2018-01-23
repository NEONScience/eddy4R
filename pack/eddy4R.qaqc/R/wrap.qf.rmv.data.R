##############################################################################################
#' @title Wrapper function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param inpList List consisting of \code{ff::ffdf} file-backed objects, in the format provided by function \code{eddy4R.base::wrap.neon.read.hdf5.eddy()}. Of types numeric and integer.
#' @param Vrbs Optional. A logical {FALSE/TRUE} value indicating whether to:\cr
#' \code{Vrbs = FALSE}: (Default) cleaned data set with the bad high frequency quality flagged data replaced with NaN's as part of the \code{inpList} in the same format., or \cr
#' \code{Vrbs = TRUE}: cleaned data set with the bad high frequency quality flagged data replaced with NaN's as part of the \code{inpList} in the same format. In addition, a separate list  \code{qfqmAnal} will be added to the output list \code{rpt} with a list of variables assessed, a list of quality flags for each variable assessed, the number of each quality flag tripped for each variable and the total number of bad data per variable.
#' 
#' @return The returned object consistes of \code{inpList}, with the bad high frequency quality flagged data replaced with NaN's. Optionally, (\code{Vrbs = TRUE}) a separate list \code{qfqmAnal} will be added to the output list \code{rpt} with a list of variables assessed, a list of quality flags for each variable assessed, the number of each quality flag tripped for each variable and the total number of bad data per variable.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807) \cr
#' Licor LI7200 reference manual

#' @keywords NEON, qfqm, quality

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-10-27)
#     original creation
##############################################################################################


wrap.qf.rmv.data <- function(
  inpList, 
  Vrbs = FALSE, 
  MethMeas = c("ecte",  "ecse")[1]){
  
  #Initialize reporting list
  rpt <- inpList
  outList <- list()
  
  #Determine the sensors that have data and quality flags
  sens <- base::intersect(base::names(inpList$data), base::names(inpList$qfqm))
  
  if(MethMeas == "ecte"){
  # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
  outList <- base::lapply(sens, function(x){ 
    eddy4R.qaqc::def.qf.rmv.data(dfData = inpList$data[[x]][], dfQf = inpList$qfqm[[x]], Sens = x, Vrbs = Vrbs) #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
  })
  
  #Apply names to the output list
  base::names(outList) <- sens
  
  #Applying the bad quality flags to the reported output data
  base::lapply(base::names(outList), function(x) {
    #Outputting the data ffdf's
    rpt$data[[x]] <<- ff::as.ffdf(outList[[x]]$dfData) 
    rpt$data[[x]] <<- eddy4R.base::def.unit.var(samp = rpt$data[[x]], refe = inpList$data[[x]]) #Copy units
  })
  
  #If verbose is true write out all the information about the quality flags applied to the raw data
  if(Vrbs == TRUE){
    base::lapply(base::names(outList), function(x) {
      #Write out qfqm analytics as a separate list to the output
      rpt$qfqmAnal[[x]] <<- outList[[x]][!names(outList[[x]]) %in% "dfData"] 
    })}
  }#end of MethMeas == "ecte"
  
  if(MethMeas == "ecse"){
    # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
    # start loop around instruments
    for(idxSens in sens){
    #for each measurement level
    outList[[idxSens]] <- base::lapply(base::names(inpList$data[[idxSens]]), function(x){ 
      eddy4R.qaqc::def.qf.rmv.data(dfData = inpList$data[[idxSens]][[x]], dfQf = inpList$qfqm[[idxSens]][[x]], Sens = idxSens, Vrbs = Vrbs , TypeData = "real") #Remove high frequency data that is flagged by sensor specific flags or plausibility tests flags
      })
    #Apply names to the each mesurement level
    base::names(outList[[idxSens]]) <- base::names(inpList$data[[idxSens]])
    
    #Applying the bad quality flags to the reported output data
    base::lapply(base::names(inpList$data[[idxSens]]), function(x) {
      #Outputting the data ffdf's
      rpt$data[[idxSens]][[x]] <<- outList[[idxSens]][[x]]$dfData
      rpt$data[[idxSens]][[x]] <<- eddy4R.base::def.unit.var(samp = rpt$data[[idxSens]][[x]], refe = inpList$data[[idxSens]][[x]]) #Copy units
    })
    
    #If verbose is true write out all the information about the quality flags applied to the raw data
    if(Vrbs == TRUE){
      base::lapply(base::names(inpList$data[[idxSens]]), function(x) {
        #Write out qfqm analytics as a separate list to the output
        rpt$qfqmAnal[[idxSens]][[x]] <<- outList[[idxSens]][[x]][!names(outList[[idxSens]][[x]]) %in% "dfData"] 
      })}
    }#end of each sensor
  }#end of MethMeas == "ecse"
  
  
  #Return the list of information with bad data removed
  return(rpt)
}
