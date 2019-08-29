##############################################################################################
#' @title Definition function: Stationarity tests

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Stationarity tests based on Vickers and Mahrt (1997) and Foken and Wichura (1996).

#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]
#' @param \code{MethStna} A Vector containing the stationarity test methods. \code{MethStna} = c(1,2,3), where 1 is calculating using trend method (Vickers and Mahrt, 1997) , 2 is calculating using internal stationarity method (Foken and Wichura, 1996) , and 3 is calculating using both methods. Defaults to 2. [-]
#' @param \code{whrVar} Specific column in \code{data} containing the variables to be performed stationarity test. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-] 
#' @param \code{NumSubSamp}  An object of class "numeric" or "integer" containing the number of sub sample over averaing period. For example, \code{NumSubSamp} = 6 if a 30 min averaging period is subsetted into 5 minute intervals. Defaults to 6. [-]
#' @param \code{corTempPot} A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]
#' @param \code{presTempPot} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer" and of the same length as \code{data} or single entry. [Pa]
#' @param \code{PltfEc} A specifier indicating which eddy covariance platform data are processed. Should be either "airc" or "towr". Defaults to "airc". [-]
#' @param \code{flagCh4} A logical indicating whether or not methane flux is processed. Defaults to TRUE. [-]
#' @param \code{vrbs} Logical. Default true. When FALSE supresses warnings when calculating rptStna01 and rptStna02.
#' @param \code{...} Passes additonal arguments to REYNflux. For example pass spcs and rmm when calculating chemistry fluxes. [-]

#' @return Stationarity test result. [percent]

#' @references
#' Foken, T. and Wichura, B.: Tools for quality assessment of surface-based flux measurements, Agricultural and Forest Meteorology, 78, 83-105, (1996) \cr
#' Vickers, D. and Mahrt, L.: Quality control and flux sampling problems for tower and aircraft data, Journal of Atmospheric and Oceanic Technology, 14, 512-526, 1997. \cr

#' @keywords eddy-covariance, stationarity, turbulent flux

#' @examples Will provide in the future.

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Natchaya Pingintha-Durden (2015-07-15)
#     Initail naming convention for eddy4R
#   Ke Xu (2016-09-19)
#     Add two arguments PltfEc and flagCh4 to adjust tower data
##############################################################################################
#STATIONARITY TESTS

def.stna <- function(
  data,  		#data frame with EC data
  MethStna=c(1, 2, 3)[2],	#analysis with trend (1) or internal stationarity (2) or both (3)
  whrVar, #for which fluxes to perform stationarity test?
  NumSubSamp=6,		#number of subsamples for trend==FALSE
  corTempPot=TRUE,
  presTempPot=NULL,
  PltfEc = "airc",
  flagCh4 = TRUE,
  vrbs = TRUE,
  ...
) {
  
  #-----------------------------------------------------------
  #BASIC SETUP
  
  #fluxes including trend
  trnd <- eddy4R.turb::REYNflux_FD_mole_dry(
    data=data,
    AlgBase="mean",
    FcorPOT=corTempPot,
    FcorPOTl=presTempPot,
    PltfEc = PltfEc,
    flagCh4 = flagCh4,
    ...
  )
  
  if(MethStna %in% c(1, 3)) { 
    #-----------------------------------------------------------
    #TREND EFFECT
    
    #fluxes after trend removal
    detr <- eddy4R.turb::REYNflux_FD_mole_dry(
      data=data,
      AlgBase="trnd",
      FcorPOT=corTempPot,
      FcorPOTl=presTempPot,
      PltfEc = PltfEc,
      flagCh4 = flagCh4,
      ...
    )
    
    #deviation [%]
    if(vrbs == TRUE)
      rptStna01 <- ((detr$mn - trnd$mn) / trnd$mn * 100)[whrVar]
    else
      rptStna01 <- suppressWarnings(((detr$mn - trnd$mn) / trnd$mn * 100)[whrVar])
    
    #clean up
    rm(detr)
    
  } else rptStna01 <- NULL
  
  if(MethStna %in% c(2, 3)) {
    #-----------------------------------------------------------
    #INTERNAL INSTATIONARITIES
    
    #class boundaries
    rngClas <- base::round(base::seq(1, nrow(data), length.out=NumSubSamp + 1))
    rngClas[length(rngClas)] <- rngClas[length(rngClas)] + 1
    
    #list with indexes of subsamples
    idxSubSamp <- base::sapply(1:(length(rngClas) - 1), function(x) base::seq(rngClas[x], rngClas[x + 1] - 1))
    
    #results for the subsamples
    outSubSamp <- base::sapply(1:NumSubSamp, function(x) eddy4R.turb::REYNflux_FD_mole_dry(
      data=data[idxSubSamp[[x]],],
      AlgBase="mean",
      FcorPOT=corTempPot,
      FcorPOTl=presTempPot,
      PltfEc = PltfEc,
      flagCh4 = flagCh4,
      ...
    )$mn[,whrVar]
    )
    outSubSamp <- data.frame(base::matrix(unlist(outSubSamp), ncol=length(whrVar), byrow=TRUE))
    dimnames(outSubSamp)[[2]] <- whrVar
    
    #stationarity criteria
    if(vrbs == TRUE)
      rptStna02 <- (base::colMeans(outSubSamp) - trnd$mn[whrVar]) / trnd$mn[whrVar] * 100
    else
      rptStna02 <- suppressWarnings((base::colMeans(outSubSamp) - trnd$mn[whrVar]) / trnd$mn[whrVar] * 100)
    
    #clean up
    rm(trnd, NumSubSamp, rngClas, idxSubSamp, outSubSamp)
    
  } else rptStna02 <- NULL
  
  #-----------------------------------------------------------
  #AGGREGATE AND RETURN RESULTS
  
  #aggregate results
  rpt <- list()
  if(!is.null(rptStna01)) rpt$trnd=rptStna01
  if(!is.null(rptStna02)) rpt$subSamp=rptStna02
  
  #return results
  return(rpt)
  
}
