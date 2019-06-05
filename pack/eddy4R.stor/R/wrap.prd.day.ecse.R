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
##############################################################################################

wrap.prd.day.ecse <- function(
  inpList,
  Desp
) {
  
  
  #Create a list to hold all the output
  rpt <- list()
  
  
  #Removing high frequency flagged data
  #Applying the bad quality flags to the reported output data
  rpt <- eddy4R.qaqc::wrap.qf.rmv.data(inpList = inpList, Vrbs = FALSE, MethMeas = "ecse", Sens = NULL, qfRmv = c("qfCal", "qfRh", "qfTemp"))
  
  
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
    print(paste0(format(Sys.time(), "%F %T"), ": ", sens, " de-spiking complete"))
    
  }; rm(sens)
  
  
  #return output
  return(rpt)
  }