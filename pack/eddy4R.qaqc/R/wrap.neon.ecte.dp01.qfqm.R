##############################################################################################
#' @title Wrapper function: Calculate quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance turbulent exchange data products

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Wrapper function. Calculate quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance turbulent exchange data products.

#' @param \code{qfSens00} A data frame of quality flags, class integer, which related to NEON eddy-covariance turbulent exchange L1 data products that the quality metrics and final quality flags are being calculated. Each column contains the quality flag values [-1,0,1] for that flag. [-] 
#' @param \code{qfSens01} Optional. A data frame of quality flags, class integer, which related to NEON eddy-covariance turbulent exchange L1 data products that the quality metrics and final quality flags are being calculated. Each column contains the quality flag values [-1,0,1] for that flag. [-] 
#' @param \code{qfSens02} Optional. A data frame of quality flags, class integer, which related to NEON eddy-covariance turbulent exchange L1 data products that the quality metrics and final quality flags are being calculated. Each column contains the quality flag values [-1,0,1] for that flag. [-] 
#' @param \code{dp01} A vector of class "character" containing the name of NEON eddy-covariance turbulent exchange L1 data products which the quality metrics and final quality flags are being calculated, c("irgaCo2","irgaH2o","soni","soniAmrs"). Defaults to "irgaCo2". [-]

#' @return A list of: \cr
#' \code{qm}  A list of data frame containing quality metrics (fractions) of failed, pass, and NA for each of the individual flag which related to L1 sub-data products. [fraction] \cr
#' \code{qmAlph} A vector of class "numeric" containing the alpha quality metric for L1 sub-data products. [fraction] \cr
#' \code{qmBeta} A vector of class "numeric" containing the beta quality metric for L1 sub-data products. [fraction] \cr
#' \code{qfFinl} A vector of class "numeric", [0,1], containing the final quality flag for L1 sub-data products. [-] \cr
#' \code{qfSciRevw} A vector of class "numeric", [0,1], containing the scientific review quality flag for L1 sub-data products. [-] \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #generate the fake quality flags for each sensor
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' inp <- list()
#' inp$qf$irga <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irga", PcntQf = 0.05)
#' inp$qf$irgaMfcSamp <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaMfcSamp", PcntQf = 0.05)
#' inp$qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' inp$qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' #calculate quality metric, qmAlpha, qmBeta, qfFinl
#' qfqm <- list()
#' qfqm$irgaCo2 <- wrap.neon.ecte.dp01.qfqm (qfSens00 = inp$qf$irga, qfSens01 = inp$qf$irgaMfcSamp, dp01 = "irgaCo2")
#' qfqm$irgaH2o <- wrap.neon.ecte.dp01.qfqm (qfSens00 = inp$qf$irga, qfSens01 = inp$qf$irgaMfcSamp, dp01 = "irgaH2o")
#' qfqm$soni <- wrap.neon.ecte.dp01.qfqm (qfSens00 = inp$qf$soni, qfSens01 = inp$qf$irga, dp01 = "soni")
#' qfqm$soniAmrs <- wrap.neon.ecte.dp01.qfqm (qfSens00 = inp$qf$soniAmrs, dp01 = "soniAmrs")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2016-12-02)
#     original creation
#   Natchaya P-Durden (2016-12-09)
#     replaced for loop by lapply
##############################################################################################

wrap.neon.ecte.dp01.qfqm <- function(
  qfSens00,
  qfSens01 = NULL,
  qfSens02 = NULL,
  dp01 = c("irgaCo2","irgaH2o","soni","soniAmrs")[1]
) {

  #assign list
  rpt <- list()
  tmp <- list()
  #grouping qf
  inp <- def.neon.ecte.dp01.qf.grup(qfSens00 = qfSens00, qfSens01 = qfSens01, qfSens02 = qfSens02, dp01=dp01)
  
  #calculate qmAlpha, qmBeta, qfFinl
  tmp <- lapply(inp, FUN = eddy4R.qaqc::def.qf.finl)
  
  #assign default qfSciRevw
  lapply(names(tmp), function(x) tmp[[x]]$qfqm$qfSciRevw <<- 0)
  #calculate qmAlph, qmBeta, qfFinl, qfSciRevw
  lapply(names(tmp), function(x) tmp[[x]]$qm <<- eddy4R.qaqc::def.qm(qf=inp[[x]], nameQmOut=NULL))
  #assign return results
  lapply(names(tmp), function(x) rpt$qm[[x]] <<- tmp[[x]]$qm)
  lapply(names(tmp), function(x) rpt$qmAlph[[x]] <<- tmp[[x]]$qfqm$qmAlph)
  lapply(names(tmp), function(x) rpt$qmBeta[[x]] <<- tmp[[x]]$qfqm$qmBeta)
  lapply(names(tmp), function(x) rpt$qfFinl[[x]] <<- tmp[[x]]$qfqm$qfFinl)
  lapply(names(tmp), function(x) rpt$qfSciRevw[[x]] <<- tmp[[x]]$qfqm$qfSciRevw)
  
  #return results
  return(rpt)
  
}
# end function wrap.neon.ecte.dp01.qfqm()