##############################################################################################
#' @title Wrapper function: Validation processing for IRGA

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param qfqmFlag List consisting of \code{ff::ffdf} file-backed objects containing the IRGA quality flags.
#' @param DateProc A vector of class "character" containing the processing date.

#' @return 
#' The returned object consistes of \code{inpList}, with the derived variables added to the respective list entry, and all list levels sorted alphabetically.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, irgaTurb, post-processing, pre-processing, validation

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2018-11-14)
#     original creation
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  DateProc
) {
  #pre-processing date
  DatePre <- base::as.Date(DateProc) - 1
  #post-processing date
  DatePost <- base::as.Date(DateProc) + 1
  #grab 3 days window of irga data and qfqmFlag (pre-processing, processing, and post-processing date)
  #get indecies
  idxSub <- which(as.Date(data$irgaTurb$time[]) == DatePre |
                    as.Date(data$irgaTurb$time[]) == DateProc |
                    as.Date(data$irgaTurb$time[]) == DatePost)
  #subset data
  subData <- data$irgaTurb[][min(idxSub):max(idxSub),]
  #subset qfqmFlag
  subQfqmFlag <- qfqmFlag$irgaTurb[][min(idxSub):max(idxSub),]
  
  #preparing the qfIrgaTurbValiGas01 to 05 data for def.idx.agr()
  #replace NA to the qfIrgaTurbValiGas01 to 05 for which are not equal to 1 


}