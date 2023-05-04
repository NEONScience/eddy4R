##############################################################################################
#' @title Definition function: Eddy-covariance turbulent flux calculation for vector quantities

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. This function calculates eddy-covariance turbulent flux for vector quantities, such as the wind vector -> momentum flux and friction velocity.

#' @param inp A data frame containing the instantaneous differences produced by ?def.stat.sta.diff() of 1) the wind vector in meteorological ENU convention with the variables veloXaxs (latitudinal wind speed, positive from west), veloYaxs (longitudinal wind speed, positive from south), and veloZaxs (vertical wind speed, positive from below), and 2) the wind vector in streamwise ENU convention with the variables veloXaxsHor (streamwise wind speed, positive from front), veloYaxsHor (cross-wind speed, positive from left), and veloZaxsHor (vertical wind speed, positive from below) derived from ?def.rot.ang.zaxs.erth, of class "numeric", each with unit attribute [m s-1]. The wind vector inputs can be viewed as a specific example that can be generalized through replacement by other vector quantities that share the same coordinate conventions and consistent units among inp and Unit.
#' @param rot A list of rotation matrices with the list elements mtrxRot01 and mtrxRot02 derived from ?def.rot.ang.zaxs.erth, class "numeric", each with unit attribute. [-]
#' @param Unit A data frame with the entries Inp (input units), Out (output units), and OutSq (squared output units), of class "character".

#' @return 
#' The returned object is a list containing the element dataframes corr, diff, mean, and sd, each of class "numeric" and with unit attribute.
#' The elements corr, mean and sd are all calculated from the same stress tensor based on the inp (veloXaxs, veloYaxs, veloZaxs) and rot (mtrxRot01 and mtrxRot02) arguments. The element corr contains the horizontal-vertical correlations, the element mean contains the horizontal-vertical covariances, and the element sd contains the standard deviation for each wind vector component in streamwise ENU convention, with a single observation each.
#' The element diff contains the instantaneous horizontal-vertical products of inp (veloXaxsHor, veloYaxsHor, veloZaxsHor) with the same number of observations as inp.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords correlation, flux, friction velocity, shear stress, standard deviation, vector

#' @examples
#' Make sure to assign all variables and units, the function should run ok.
#' inp <- base::data.frame(
#'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
#'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
#'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
#' )
#' attr(inp$veloXaxs,"unit") <- "m s-1"; attr(inp$veloYaxs,"unit") <- "m s-1"; attr(inp$veloZaxs,"unit") <- "m s-1"
#' out <- def.flux.vect(
#'   inp = base::cbind(def.stat.sta.diff(inp = inp)$diff,
#'                     def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp)$data)$diff),
#'   rot = def.rot.ang.zaxs.erth(inp = inp)$rot,
#'   Unit = base::data.frame(Inp = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
#' )
#' utils::str(out)
#' base::rm(inp, out)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2022-02-08)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# Eddy-covariance flux calculation for vector quantities
def.flux.vect <- function(
  
  # input dataframe with variables veloXaxs, veloYaxs, and veloZaxs of class "numeric, each with unit attribute. [m s-1]
  # limit to required wind components, use def.rot.ang.zaxs.erth example above
  inp,
  rot,
  Unit = base::data.frame(Inp = "m s-1", Out = "m s-1", OutSq = "m2 s-2")

) {

  # check presence of input arguments and consistent units
  
    # Unit
      
      # check that Unit is of class data.frame
      if(base::class(Unit) != "data.frame") {
        stop(base::paste0("def.flux.vect(): Unit is not of class data.frame, please check."))  
      }
      
      # test that input and output Unit are identical
      if(!(Unit$Inp == Unit$Out)) {
        stop(base::paste0("def.flux.vect(): Unit$Out differs from Unit$Inp, please check"))}
  
    # inp
    
      # check that input is of class data.frame
      if(base::class(inp) != "data.frame") {
        stop(base::paste0("def.flux.vect(): inp is not of class data.frame, please check."))  
      }
      
      # test input variables and unit attributes
      for(idx in c("veloXaxs", "veloYaxs", "veloZaxs", "veloXaxsHor", "veloYaxsHor", "veloZaxsHor")){
        # idx <- "veloXaxs"
        
        # test for presence/absence of variables
        if(!(idx %in% base::names(inp))) {
          stop(base::paste0("def.flux.vect(): inp$", idx, " is missing."))}
        
        # test for presence/absence of unit attribute
        if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
          stop(base::paste0("def.flux.vect(): inp$", idx, " is missing unit attribute."))}
        
        # test for correct units
        if(attributes(inp[[idx]])$unit != Unit$Inp) {
          stop(base::paste0("def.flux.vect(): inp$", idx, 
                            " input units are not matching Unit$Inp, please check."))}
        
      }; base::rm(idx)
    
    # rot
      
      # check that rot is of class list
      if(base::class(rot) != "list") {
        stop(base::paste0("def.flux.vect(): rot is not of class list, please check."))  
      }
      
      # test rot list entries and unit attributes
      for(idx in c("mtrxRot01", "mtrxRot02")){
        # idx <- "mtrxRot01"
        
        # test for presence/absence of list entries
        if(!(idx %in% base::names(rot))) {
          stop(base::paste0("def.flux.vect(): rot$", idx, " is missing."))}

        # test for presence/absence of unit attribute
        if(!("unit" %in% base::names(attributes(rot[[idx]])))) {
          stop(base::paste0("def.flux.vect(): rot$", idx, " is missing unit attribute."))}
        
        # test for correct units
        if(attributes(rot[[idx]])$unit != "-") {
          stop(base::paste0("def.flux.vect(): rot$", idx, 
                            " input units are not matching internal units, please check."))}
        
      }; base::rm(idx)
  
  # instantaneous fluxes from instantaneous wind component differences in streamline coordinates
  # for downstream calculation of integral length scales and statistical errors
  # includes negative sign prefixes commonly used in ENU meteorological convention
  # identical to stress tensor results: veloFric <- (mean(diff$veloFricXaxsSq)^2 + mean(diff$veloFricYaxsSq)^2)^(1/4)
  
    # calculate
    diff <- base::data.frame(
      veloFricXaxsSq = -(inp$veloXaxsHor * inp$veloZaxsHor),
      veloFricYaxsSq = -(inp$veloYaxsHor * inp$veloZaxsHor),
      veloFric = base::rep(x = NaN, length.out = base::nrow(inp))
    )
    
    # assign units
    base::attr(diff$veloFricXaxsSq, which = "unit") <- Unit$OutSq
    base::attr(diff$veloFricYaxsSq, which = "unit") <- Unit$OutSq
    base::attr(diff$veloFric, which = "unit") <- Unit$Out
  
  # calculate stress tensor and rotate into streamline coordinates

    # transpose wind component instantaneous differences from ENU meteorological convention to NED geographic convention
    # this also means that negative-sign prefixes are omitted from veloFric calculations based on stress tensor
    veloXaxsIntl <- inp$veloYaxs
    veloYaxsIntl <- inp$veloXaxs
    veloZaxsIntl <- -inp$veloZaxs
    
    # stress tensor (defined in NED geographic convention)
    mtrxFric <- rbind(
      c(base::mean(veloXaxsIntl * veloXaxsIntl, na.rm = TRUE),
        base::mean(veloXaxsIntl * veloYaxsIntl, na.rm = TRUE),
        base::mean(veloXaxsIntl * veloZaxsIntl, na.rm = TRUE)),
      c(base::mean(veloYaxsIntl * veloXaxsIntl, na.rm = TRUE),
        base::mean(veloYaxsIntl * veloYaxsIntl, na.rm = TRUE),
        base::mean(veloYaxsIntl * veloZaxsIntl, na.rm = TRUE)),
      c(base::mean(veloZaxsIntl * veloXaxsIntl, na.rm = TRUE),
        base::mean(veloZaxsIntl * veloYaxsIntl, na.rm = TRUE),
        base::mean(veloZaxsIntl * veloZaxsIntl, na.rm = TRUE))
    )
    base::attr(mtrxFric, which = "unit") <- Unit$OutSq
    
    # rotate stress tensor into streamline coordinates
    mtrxRot03 <- rot$mtrxRot01 %*% mtrxFric
    base::attr(mtrxRot03, which = "unit") <- Unit$OutSq
    
    mtrxRot04 <- mtrxRot03 %*% rot$mtrxRot02
    base::attr(mtrxRot04, which = "unit") <- Unit$OutSq
    
    # clean up
    base::rm(mtrxFric, mtrxRot03, veloXaxsIntl, veloYaxsIntl, veloZaxsIntl)
  
  
  # friction velocity [m s-1]
  # optionally only considers the along wind stress veloFricXaxsSq; Foken (2008) Eq.(2.23)
  # negative sign prefixes commonly used for ENU Zaxs are omitted because stress tensor is already defined in NED
    
    # calculate
    mean <- base::data.frame(
      veloFricXaxsSq = mtrxRot04[1,3],
      veloFricYaxsSq = mtrxRot04[2,3])
    mean$veloFric <- (mean$veloFricXaxsSq^2 + mean$veloFricYaxsSq^2)^(1/4)
    
    # assign units
    base::attr(mean$veloFricXaxsSq, which = "unit") <- Unit$OutSq
    base::attr(mean$veloFricYaxsSq, which = "unit") <- Unit$OutSq
    base::attr(mean$veloFric, which = "unit") <- Unit$Out


  # standard deviation of wind components; deviations from sd calculated in def.stat.sta.diff are < 2%
    
    # calculate
    sd <- base::data.frame(
      veloXaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[1],
      veloYaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[2],
      veloZaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[3])
    
    # assign units
    base::attr(sd$veloXaxsHor, which = "unit") <- Unit$Out
    base::attr(sd$veloYaxsHor, which = "unit") <- Unit$Out
    base::attr(sd$veloZaxsHor, which = "unit") <- Unit$Out


  # calculate correlations
  # negative sign prefixes commonly used for ENU Zaxs are omitted because stress tensor is already defined in NED
    
    # calculate
    corr <- base::data.frame(
      veloFricXaxsSq = mtrxRot04[1,3] / sd$veloXaxsHor / sd$veloZaxsHor,
      veloFricYaxsSq = mtrxRot04[2,3] / sd$veloYaxsHor / sd$veloZaxsHor,
      veloFric = NaN)
    
    # assign units
    base::sapply(base::names(corr), function(x) {base::attr(corr[[x]], which = "unit") <<- "-"})
  
  
    
  # create object for export
    
    # create list
    rpt <- base::list()
    
    # populate list
    rpt$corr <- corr
    rpt$diff <- diff
    rpt$mean <- mean
    rpt$sd <- sd
    
    # clean up
    base::rm(corr, diff, mean, mtrxRot04, sd)
    
    
  # return results
  return(rpt)


}
