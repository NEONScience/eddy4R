##############################################################################################
#' @title Definition function: Streamwise rotation of the wind vector

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. This function rotates the wind vector into the mean wind, thus allowing to separate stream-wise and cross-wind components for subsequent use in footprint models, calculating shear stress etc. It consists of a single azimuth-rotation around the vertical axis. Any more comprehensive rotation such as double rotation or planar fit should be applied prior to calling this function.

#' @param inp A data frame containing the wind vector in meteorological convention with the variables veloXaxs (latitudinal wind speed, positive from west), veloYaxs (longitudinal wind speed, positive from south), and veloZaxs (vertical wind speed, positive from below) of class "numeric, each with unit attribute. [m s-1]

#' @return 
#' The returned object is a list containing the elements data and rot.
#' data is a dataframe with the same number of observations as the function call inputs. It contains the wind vector variables in streamwise convention veloXaxsHor (streamwise wind speed, positive from front), veloYaxsHor (cross-wind speed, positive from left) and veloZaxsHor (vertical wind speed, positive from below) [m s-1], and the wind direction angZaxsErth [rad], each of class "numeric and with unit attribute.
#' rot is a list with the objects used in the rotation, averaged over all observations in the function call inputs. It contains the mean wind direction angZaxsErth [rad], the resulting rotation matrix mtrxRot01 [-] and the transpose of the rotation matrix mtrxRot02 [-], each of class "numeric and with unit attribute. It should be noted that angZaxsErth is calculated by first averaging each horizontal component of the wind vector, which minimizes the mean cross-wind thus satisfying conditions for footprint modeling and separating shear into stream-wise and cross-wind terms.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords rotation, stream-wise, mean wind, footprint, shear

#' @examples
#' Example 1, this will cause an error message due to inp01$veloYaxs is missing:
#' inp01 <- base::data.frame(
#'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
#'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
#' )
#' attr(inp01$veloXaxs,"unit") <- "m s-1"; attr(inp01$veloZaxs,"unit") <- "m s-1"
#' def.rot.ang.zaxs.erth(inp = inp01)
#' base::rm(inp01)
#' Example 2, make sure to assign all variables and units, the function should run ok.
#' inp02 <- base::data.frame(
#'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
#'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
#'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
#' )
#' attr(inp02$veloXaxs,"unit") <- "m s-1"; attr(inp02$veloYaxs,"unit") <- "m s-1"; attr(inp02$veloZaxs,"unit") <- "m s-1"
#' out02 <- def.rot.ang.zaxs.erth(inp = inp02)
#' utils::str(out02)
#' base::rm(inp02, out02)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2021-11-24)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

    
# rotation into the mean wind
def.rot.ang.zaxs.erth <- function(
  
  # input dataframe with variables veloXaxs, veloYaxs, and veloZaxs of class "numeric, each with unit attribute. [m s-1]
  inp
  
) {
  
  # check that input is of class data.frame
  if(base::class(inp) != "data.frame") {
    stop(base::paste0("def.rot.ang.zaxs.erth(): inp is not of class data.frame, please check."))  
  }
  
  # test input variables and unit attributes
  for(idx in c("veloXaxs", "veloYaxs", "veloZaxs")){
  # idx <- "veloXaxs"
    
    # test for presence/absence of variables
    if(!(idx %in% base::names(inp))) {
      stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing."))        }
    
    # test for presence/absence of unit attribute
    if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
      stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing unit attribute."))        }
    
    # test for correct units
    if(attributes(inp[[idx]])$unit != "m s-1") {
      stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, 
                        " input units are not matching internal units, please check."))}
    
  }
  
  # clean up
  base::rm(idx)

  # calculate wind direction
    
    # instantaneous wind direction [rad]
    angZaxsErth <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(cart = base::matrix(c(
      inp$veloYaxs,
      inp$veloXaxs), ncol=2)),
      unitFrom = "deg", unitTo = "rad")
  
    # mean wind direction [rad], based on first averaging each horizontal component of the wind vector
    # minimizes mean cross-wind, thus satisfying conditions for footprint modeling (required) 
    # and separating shear into stream-wise and cross-wind terms (optional)
    # however, dp04 results (124.824 deg) differ from reported dp01 (118.9117 deg,
    # for first 30 min in gold data per 2021-11-23)
    # that is because dp01are based on INSTANTANEOUS wind directions 
    # (eddy4R.base::wrap.dp01.R calls eddy4R.base::def.dir.wind(inp = dataLoca$soni$angZaxsErth, MethVari = "Yama"))
    # how to best reconcile, different community standards for dp01 (states -> 2D sonics) and dp04 (fluxes)?
    angZaxsErthMean <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(cart = base::matrix(c(
      base::mean(inp$veloYaxs, na.rm = TRUE),
      base::mean(inp$veloXaxs, na.rm = TRUE)), ncol=2)),
      unitFrom = "deg", unitTo = "rad")
  
  # rotation angle
  angRot <- (angZaxsErthMean + base::pi) %% (2 * base::pi)
  
  # rotation matrix
  mtrxRot01 <- base::matrix(nrow=3, ncol=3)
    mtrxRot01[1,1] <- base::cos(angRot)
    mtrxRot01[1,2] <- base::sin(angRot)
    mtrxRot01[1,3] <- 0.
    mtrxRot01[2,1] <- -base::sin(angRot)
    mtrxRot01[2,2] <- base::cos(angRot)
    mtrxRot01[2,3] <- 0.
    mtrxRot01[3,1] <- 0.
    mtrxRot01[3,2] <- 0.
    mtrxRot01[3,3] <- 1.
    
  # transpose of rotation matrix
  mtrxRot02 <- base::t(mtrxRot01)
  
  # wind velocity vector in (horizontal) geodetic coordinates
  veloVect <- rbind(inp$veloYaxs, inp$veloXaxs, inp$veloZaxs)
  
  # actual rotation
  veloVectRot <- mtrxRot01 %*% veloVect
    
  # create object for export
  
    # create list
    rpt <- base::list()

    # populate rpt$data
    
      # along-wind
      rpt$data <- base::data.frame(veloXaxsHor = veloVectRot[1,])
      attr(rpt$data$veloXaxsHor,"unit") <- "m s-1"
      
      # cross-wind
      # requires mirroring as output is still in geodetic axes order, downstream impact on diff$u_star2_y
      rpt$data$veloYaxsHor <- -veloVectRot[2,]
      attr(rpt$data$veloYaxsHor,"unit") <- "m s-1"
      
      # vertical wind
      rpt$data$veloZaxsHor <- veloVectRot[3,]
      attr(rpt$data$veloZaxsHor,"unit") <- "m s-1"
      
      # wind direction
      rpt$data$angZaxsErth <- angZaxsErth
      
    # populate rpt$rot
      
      # assign data
      rpt$rot <- base::list(
        angZaxsErth = angZaxsErthMean,
        mtrxRot01 = mtrxRot01,
        mtrxRot02 = mtrxRot02)
      
      # assign output units
      attributes(rpt$rot$angZaxsErth)$unit <- "rad"
      attributes(rpt$rot$mtrxRot01)$unit <- "-"
      attributes(rpt$rot$mtrxRot02)$unit <- "-"

  # clean up
  rm(angRot, angZaxsErth, angZaxsErthMean, inp, mtrxRot01, mtrxRot02, veloVect, veloVectRot)
  
  # return results
  return(rpt) 
  
}
