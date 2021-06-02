##############################################################################################
#' @title Wrapper function: To perform daily ECSE processing in native resolution

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com} \cr
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. To perform daily ECSE processing in native resolution

#' @param inpList List consisting of input data in the format provided by function \code{eddy4R.base::wrap.hdf5.read()}. Of types numeric, integer,  character and POSIXct.
#' @param Desp De-spiking parameters as mixed list of types integer and character with the following list entries: \code{widt} de-spiking median filter window width (single integer); \code{nbin} de-spiking histogram bins initial number/step size (single integer); \code{rest} de-spiking resolution threshold (single integer); \code{var} sub-list of sensors in \code{inpList}, with each list entry containing the variable names for which to perform de-spiking (character). See \code{?eddy4R.qaqc::def.dspk.br86} for details.
#'
#' @return
#' The returned object consistes of \code{rpt} after applying the daily processing.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords ECSE, daily processing, qfqm, quality

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2018-01-23)
#     original creation
#   Stefan Metzger (2018-01-30)
#     move ECSE de-spiking from wrap.neon.read.hdf5.eddy() to wrap.prd.day.ecse()
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced dataIn by dataInp
#   Natchaya P-Durden (2019-03-14)
#     update input parameters in wrap.qf.rmv.data function
#   Natchaya P-Durden (2019-05-06)
#     adding logic to determine qfFrt00 from mfm
#   Chris Florian (2021-06-02)
#     updating qfRmv to not exclude rtioMoleWetH2o data that are below the low humidity threshold
##############################################################################################

wrap.prd.day.ecse <- function(
  inpList,
  Desp
) {

  #Create a list to hold all the output
  rpt <- list()

  #generate qfFrt00 for mfm to indicate when low flow pass through (pump failure)
  #check if mfm data and qfqm are in the inpList
  if(is.null(inpList$data$mfm) == FALSE & is.null(inpList$qfqm$mfm) == FALSE){
  #critical value for flow rate during pump failure; default as 3 dm3 min-1
  critFrt00 <- eddy4R.base::def.unit.conv(data=3, unitFrom = "dm3 min-1", unitTo = "intl")
  lapply(names(inpList$qfqm$mfm), function(x){
    inpList$qfqm$mfm[[x]]$qfFrt00 <<- ifelse(is.na(inpList$data$mfm[[x]]$frt00), -1,
                                 ifelse(inpList$data$mfm[[x]]$frt00 <= critFrt00, 1, 0))})
  }
  #Removing high frequency flagged data
  #Applying the bad quality flags to the reported output data
  rpt <- eddy4R.qaqc::wrap.qf.rmv.data(inpList = inpList, Vrbs = FALSE, MethMeas = "ecse", Sens = NULL, qfRmv = c("qfCal", "qfRh", "qfTemp", "qfLowRtioMoleWetH2o"))


  # perform de-spiking
  # loop around sensors
  for(sens in base::names(Desp$var)) {
  # sens <- base::names(Desp$var)[1]

    # loop around levels
    for(lvl in base::names(rpt$data[[sens]])) {
    # lvl <- base::names(rpt$data[[sens]])[1]

      # loop around variables
      for(var in Desp$var[[sens]]) {
      # var <- Desp$var[[sens]][1]

        #execute de-spiking algorithm
        rpt$data[[sens]][[lvl]][[var]] <- eddy4R.qaqc::def.dspk.br86(
          # input data, univariate vector of integers or numerics
          dataInp = rpt$data[[sens]][[lvl]][[var]],
          # filter width
          WndwFilt = Desp$widt,
          # initial number/step size of histogram bins
          NumBin = Desp$nbin,
          # resolution threshold
          ThshReso = Desp$rest
        )$dataOut

      }; rm(var)

    }; rm(lvl)

    # print message to screen
    msg <- paste0(sens, " de-spiking complete")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})

  }; rm(sens)


  #return output
  return(rpt)
  }
