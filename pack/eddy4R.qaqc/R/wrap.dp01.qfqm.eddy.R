##############################################################################################
#' @title Wrapper function: Calculate quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance turbulent and stroage exchange L1 data products

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org} \cr
#' David Durden \email{ddurden@battelleecology.org}

#' @description Wrapper function. Calculate quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance turbulent and storage exchange L1 data products.

#' @param qfInp A list of data frame containing the input quality flag data that related to L1 data products are being grouped. Of class integer". [-]
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-]
#' @param TypeMeas A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]
#' @param RptExpd A logical parameter that determines if the full quality metric \code{qm} is output in the returned list (defaults to FALSE).
#' @param dp01 A vector of class "character" containing the name of NEON ECTE and ECSE L1 data products which the flags are being grouped, \cr
#' c("envHut", "co2Turb", "h2oTurb", "isoCo2", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop"). Defaults to "co2Turb". [-]
#' @param \code{idGas} A data frame contianing gas ID for isoCo2 measurement. Need to provide when dp01 = "isoCo2". Default to NULL. [-]

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
#' qf$irgaTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaTurb", PcntQf = 0.05)
#' qf$mfcSampTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "mfcSampTurb", PcntQf = 0.05)
#' qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' #calculate quality metric, qmAlpha, qmBeta, qfFinl
#' qfqm <- list()
#' qfqm$co2Turb <- eddy4R.qaqc::wrap.dp01.qfqm.eddy (qfInp = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="co2Turb")
#' qfqm$h2oTurb <- eddy4R.qaqc::wrap.dp01.qfqm.eddy (qfInp = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="h2oTurb")
#' qfqm$soni <- eddy4R.qaqc::wrap.dp01.qfqm.eddy (qfInp = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="soni")
#' # Example with expanded quality metrics included
#' qfqm$soniAmrs <- eddy4R.qaqc::wrap.dp01.qfqm.eddy (qfInp = qf, MethMeas = "ecte", TypeMeas = "samp", RptExpd = TRUE, dp01="soniAmrs")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2016-12-02)
#     original creation
#   Natchaya P-Durden (2016-12-09)
#     replaced for loop by lapply
#   Natchaya P-Durden (2017-03-23)
#     revised original function to wrap.neon.dp01.qfqm ()
#     added ECSE quality flags
#   Dave Durden (2017-04-24)
#     Changed output to dataframes, added switch
#     for expanded output and updated the output data type.
#   Natchaya P-Durden (2017-08-02)
#     added idGas and replaced isopCo2 and isopH2o by isoCo2 and isoH2o
#   Natchaya P-Durden (2017-08-17)
#     added co2Stor and h2oStor in dp01 name input
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced qfInput by qfInp
#   Natchaya P-Durden (2018-05-23)
#     rename function from def.neon.dp01.qf.grp() to def.dp01.grp.qf()
#     rename function from wrap.neon.dp01.qfqm() to wrap.dp01.qfqm.eddy()
#   Natchaya P-Durden (2019-07-23)
#     adding one row with qf = -1 in the empty dataframe to eliminate code break in def.qf.finl()
#   Chris Florian (2021-04-09)
#     adding ch4Conc to the dp01 list
#   Chris Florian (2021-11-08)
#     adding logic to allow for more missing data due to variable picarro sampling frequency
#   Chris Florian (2021-11-12)
#     setting the ch4Conc qmBeta weighting to 0.2, this is the lowest weighing we can assign while also flagging intervals that are fully missing
##############################################################################################

wrap.dp01.qfqm.eddy <- function(
  qfInp = list(),
  MethMeas = c("ecte", "ecse")[1],
  TypeMeas = c("samp", "vali")[1],
  RptExpd = FALSE,
  dp01 = c("envHut", "co2Turb", "h2oTurb", "co2Stor", "h2oStor", "isoCo2", "ch4Conc", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")[1],
  idGas =NULL
) {

  #assign list
  rpt <- list()
  tmp <- list()
  #grouping qf
  inp <- eddy4R.qaqc::def.dp01.grp.qf(qfInp = qfInp, MethMeas = MethMeas, TypeMeas = TypeMeas, dp01=dp01, idGas = idGas)

  #adding one row with qf = -1 in the empty dataframe to eliminate code break in def.qf.finl
  lapply(names(inp), function(x) if (nrow(inp[[x]]) == 0) inp[[x]][1,] <<- -1)

  #calculate qmAlpha, qmBeta, qfFinl
  if (dp01 %in% c("envHut", "co2Turb", "h2oTurb", "co2Stor", "isoCo2", "isoH2o", "h2oStor", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")){
    tmp <- lapply(inp, FUN = eddy4R.qaqc::def.qf.finl)
  }
  
  #set qmBeta weight lower methane to account for lower sampling frequency
  if (dp01 %in% c("ch4Conc")){
    tmp <- lapply(inp, FUN = eddy4R.qaqc::def.qf.finl, WghtAlphBeta=c(2,0.2))
  }
  

  #assign default qfSciRevw
  lapply(names(tmp), function(x) tmp[[x]]$qfqm$qfSciRevw <<- 0)
  #Only report expanded quality metrics if producing expanded file
  if(RptExpd == TRUE){
    #calculate quality metrics (pass, fail, NA for each flag)
     lapply(names(tmp), function(x) tmp[[x]]$qm <<- eddy4R.qaqc::def.qm(qf=inp[[x]], nameQmOut=NULL))
  #assign return results for expanded results
 lapply(names(tmp), function(x) rpt$qm[[x]] <<- tmp[[x]]$qm)
 #Add units to the output of expanded quality metrics
 for(idxVar in names(rpt$qm)){
 lapply(names(rpt$qm[[idxVar]]), function(x) attr(rpt$qm[[idxVar]][[x]], which = "unit") <<- "-")
 }
 }

  #assign return results for basic results
  lapply(names(tmp), function(x) rpt$qmAlph[[x]] <<- tmp[[x]]$qfqm$qmAlph)
  lapply(names(tmp), function(x) rpt$qmBeta[[x]] <<- tmp[[x]]$qfqm$qmBeta)
  lapply(names(tmp), function(x) rpt$qfFinl[[x]] <<- as.integer(tmp[[x]]$qfqm$qfFinl))
  lapply(names(tmp), function(x) rpt$qfSciRevw[[x]] <<- as.integer(tmp[[x]]$qfqm$qfSciRevw))

  # Convert output to dataframe's
  rpt$qmAlph <- base::rbind.data.frame(rpt$qmAlph)
  rpt$qmBeta <-  base::rbind.data.frame(rpt$qmBeta)
  rpt$qfFinl <- base::rbind.data.frame(rpt$qfFinl)
  rpt$qfSciRevw <- base::rbind.data.frame(rpt$qfSciRevw)

  lapply(names(rpt$qmAlph), function(x) attr(rpt$qmAlph[[x]], which = "unit") <<- "-")
  lapply(names(rpt$qmBeta), function(x) attr(rpt$qmBeta[[x]], which = "unit") <<- "-")
  lapply(names(rpt$qfFinl), function(x) attr(rpt$qfFinl[[x]], which = "unit") <<- "NA")
  lapply(names(rpt$qfSciRevw), function(x) attr(rpt$qfSciRevw[[x]], which = "unit") <<- "NA")
  #return results
  return(rpt)

}
# end function wrap.neon.ecte.dp01.qfqm()
