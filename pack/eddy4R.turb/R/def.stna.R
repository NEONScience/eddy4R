##############################################################################################
#' @title Definition function: Stationarity tests

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Stationarity tests based on Vickers and Mahrt (1997) and Foken and Wichura (1996).

#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]
#' @param \code{MethStna} A Vector containing the stationarity test methods. \code{MethStna} = c(1,2,3), where 1 is calculating using trend method (Vickers and Mahrt, 1997) , 2 is calculating using internal stationarity method (Foken and Wichura, 1996) , and 3 is calculating using both methods. Defaults to 2. [-]
#' @param \code{SubSamp}  An object of class "numeric" containing the time stamp to sub sample over. For example, \code{NumSubSamp} = 5 if a 30 min averaging period is subsetted into 5 minute intervals. [-]
#' @param \code{FcorPOT} A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]
#' @param \code{FcorPOTl} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer" and of the same length as \code{data} or single entry. [Pa]
#' @param \code{PltfEc} A specifier indicating which eddy covariance platform data are processed. Should be either "airc" or "towr". Defaults to "airc". [-]

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
#   Will Drysdale (2018-01-25)
#     Added Support for other species to be defined in SiteInfo
#   Adam Vaughan (2019-01-23)
#     Updated function to use time stamps as subsample parameter
#   Will Drysdale (2019-01-29)
#     Removed SiteInfo dependance, added switches as explicit variables
#   Will Drysdale (2019-02-16)
#     Add check for "date" column before attempting subsampling by date
##############################################################################################
#STATIONARITY TESTS

def.stna <- function(
  data,
  MethStna=c(1, 2, 3)[2],
  SubSamp=5,
  FcorPOT=FcorPOT,
  FcorPOTl=NULL,
  PltfEc = PltfEc,
  Tz,
  ... # parameters for REYN_flux
) {
  #-----------------------------------------------------------
  #BASIC SETUP
  
  #fluxes including trend
  trnd <- eddy4R.turb::REYNflux_FD_mole_dry(
    data=data,
    AlgBase="mean",
    FcorPOT=FcorPOT,
    FcorPOTl=FcorPOTl,
    PltfEc = PltfEc,
    ...)

  whrVar <- c("u_star2_x", "u_star2_y", "u_star")
  x <- trnd$mn %>% dplyr::select(ends_with("_en")) %>% select_if(~ !any(is.na(.))) %>% colnames()
  y <- trnd$mn %>% dplyr::select(ends_with("_mass")) %>% select_if(~ !any(is.na(.))) %>% colnames()
  whrVar <- c(whrVar,x,y)
  rm(x,y)
  
  if(MethStna %in% c(1, 3)) { 
    #-----------------------------------------------------------
    #TREND EFFECT
    
    #fluxes after trend removal
    detr <- eddy4R.turb::REYNflux_FD_mole_dry(
      data=data,
      AlgBase="trnd",
      FcorPOT=F,
      FcorPOTl=eddy4R.base::IntlNatu$Pres00,
      PltfEc = PltfEc,
      ...
    )

    #deviation [%]
    suppressWarnings(rptStna01 <- ((detr$mn - trnd$mn) / trnd$mn * 100)[whrVar])
    #clean up
    rm(detr)
    
  } else rptStna01 <- NULL
  
  if(MethStna %in% c(2, 3)) {
    #-----------------------------------------------------------
    #INTERNAL INSTATIONARITIES
    
    # if date is present use this to create subsampling windows
    if("date" %in% names(data)){
      #calculate start points from time stamps and SubSamp setting
      start_point <- base::as.POSIXlt(seq.POSIXt(from = min(data$date) %>% lubridate::round_date("1 minute"),
                                                 to = (max(data$date)-lubridate::minutes(SubSamp)) %>% lubridate::round_date("1 minute"),
                                                 by = as.numeric(lubridate::minutes(SubSamp)),
                                                 tz = Tz))
      
      end_point <- start_point + lubridate::minutes(SubSamp)
      
      #results for the subsamples
      outSubSamp <- base::sapply(1:length(start), function(x) eddy4R.turb::REYNflux_FD_mole_dry(
        data=data %>% subset(date >= start_point[x] & date <= end_point[x]),
        AlgBase="mean",
        FcorPOT=FcorPOT,
        FcorPOTl=eddy4R.base::IntlNatu$Pres00,
        PltfEc = "towr",
        ...)$mn[,whrVar])
    }else{ # otherwise use segments
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
    }

    
    outSubSamp <- data.frame(base::matrix(unlist(outSubSamp), ncol=length(whrVar), byrow=TRUE))
    dimnames(outSubSamp)[[2]] <- whrVar
    
    #stationarity criteria
    rptStna02 <- (base::colMeans(outSubSamp) - trnd$mn[whrVar]) / trnd$mn[whrVar] * 100
    
    #clean up
    rm(trnd, outSubSamp)
    
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
