##############################################################################################
#' @title Wrapper function: Create NEON Level 1 data product quality flags and quality metrics across list elements

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. Compute NEON Level 1 data product quality flags and quality metrics (qfFinl, qfSciRevw, qmAlph, qmBeta) across list elements. 

#' @param qfqm A data.frame or list containing the L0p input data quality flags (sensor and plausibility flags) at native resolution. Of type numeric or integer. [-]
#' @param idx If data is a list, which list entries should be processed into Level 1 data product quality metrics? Defaults to NULL which expects qfqm to be a data.frame. Of type character. [-]
#' @param TypeMeas A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-]
#' @param RptExpd A logical parameter that determines if the full quality metric \code{qm} is output in the returned list (defaults to FALSE).

#' @return A list of: \cr
#' \code{qm}  A list of data frame's containing quality metrics (fractions) of failed, pass, and NA for each of the individual flag which related to L1 sub-data products if RptExpd = TRUE. [fraction] \cr
#' \code{qmAlph} A dataframe containing metrics in a  columns of class "numeric" containing the alpha quality metric for L1 sub-data products. [fraction] \cr
#' \code{qmBeta} A dataframe containing metrics in a columns of class "numeric" containing the beta quality metric for L1 sub-data products. [fraction] \cr
#' \code{qfFinl} A dataframe containing flags in a columns of class "numeric", [0,1], containing the final quality flag for L1 sub-data products. [-] \cr
#' \code{qfSciRevw} A dataframe containing flags in a columns of class "numeric", [0,1], containing the scientific review quality flag for L1 sub-data products. [-] \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #generate the fake quality flags for each sensor
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' qf <- list()
#' qf$irga <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irga", PcntQf = 0.05)
#' qf$irgaMfcSamp <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaMfcSamp", PcntQf = 0.05)
#' qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' #calculate quality metric, qmAlpha, qmBeta, qfFinl
#' qfqm <- list()
#' qfqm <- wrap.neon.dp01.qfqm.ec(qfqm = wrk$qfqm, idx = c("soni", "soniAmrs", "irgaCo2", "irgaH2o") )

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-04-30)
#     original creation
##############################################################################################


# start function wrap.neon.dp01.qfqm.ec()
wrap.neon.dp01.qfqm.ec <- function(
  qfqm,
  idx = NULL,
  TypeMeas = "samp",
  MethMeas = c("ecte","ecse")[1],
  RptExpd = FALSE
) {
  
  # stop if data is a list but idx is not specified  
  if(!is.list(qfqm)|is.null(idx)) stop("wrap.neon.dp01.qfqm.ec: qfqm is a list please specify idx.")
  
  
  
  # if data is a list, calculate NEON Level 1 data products recursively for each list element
 rpt <- lapply(idx, function(x) {
   eddy4R.qaqc::wrap.neon.dp01.qfqm (qfInput = qfqm, MethMeas = MethMeas, TypeMeas = TypeMeas, dp01=x, RptExpd = RptExpd)
                                     })
  
 names(rpt) <- idx
 # return results
  return(rpt)
  
}
# end function wrap.neon.dp01.qfqm.ec()
