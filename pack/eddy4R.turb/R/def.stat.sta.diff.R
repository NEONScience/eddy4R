##############################################################################################
#' @title Definition function: Summary statistics, base states, instantaneous differences

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. The function calculates summary statistics (min, max, mean), selected base state (mean, trend, 3rd order polynomial), and the instantaneous differences and standard deviations with respect to the selected base state. This enables subsequent calculations (eddy-covariance turbulent fluxes, footprint modeling etc.) with respect to the selected base state.

#' @param inp A data frame containing the variables for which to perform the calculations, class "numeric", each with unit attribute.
#' @param refe A list of reference quantities that can be supplied to overwrite internal calculations, class "numeric", each with unit attribute. Currently implemented only for refe$mean in combination with inp variables in units [rad].
#' @param AlgBase A vector of length 1 that defines the base state with respect to which instantaneous differences and standard deviations are calculated, of class "character" and no unit attribute. Contains one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details. When AlgBase is set to "trnd" or "ord03", the variable inp$idep is required, which provides the independent variable for interpolation.

#' @return 
#' The returned object is a list containing the element dataframes min, max, mean, base, diff, and sd, each of class "numeric" and with unit attributes. The elements min, max and mean are the minimum, maximum and mean of the variables in inp with a single observation. The element base is the base state for each of the variables in inp, with a single observation (AlgBase == "mean") or the same number of observations as inp (AlgBase %in% c("trnd", "ord03")). The element diff are the point-by-point differences from the selected base state for each of the variables in inp, with the same number of observations as inp. The element sd are the standard deviations of diff for each of the variables in inp, with a single observation. It should be noted that the mean (and base state for AlgBase == "mean") for angular quantities with unit [rad] is computed from 1) point-wise unit vector decomposition to polar coordinates [-], 2) averaging the polar coordinates [-], and 3) re-composing the mean polar coordinates to Cartesian angle [rad]. In the special case of wind direction (example: angZaxsErth) it is recommended to supply the argument refe$mean$angZaxsErth to the function call as provided from ?def.rot.ang.zaxs.erth. This ensures consistency with downstream applications for footprint modeling, separating shear into stream-wise and cross-wind terms etc. (minimizes the mean cross-wind from directly averaging the wind vector horizontal components instead of unit vector components).

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords minimum, maximum, mean, statistics, standard deviation, turbulence

#' @examples
#' Example 1, this will cause an error message due to missing unit attribute for inp01$veloYaxs:
#' inp01 <- base::data.frame(
#'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
#'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
#'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
#' )
#' attr(inp01$veloXaxs,"unit") <- "m s-1"; attr(inp01$veloZaxs,"unit") <- "m s-1"
#' def.stat.sta.diff(inp = inp01)
#' base::rm(inp01)
#' Example 2, make sure to assign all variables and units, the function should run ok.
#' inp02 <- base::data.frame(
#'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
#'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
#'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
#' )
#' attr(inp02$veloXaxs,"unit") <- "m s-1"; attr(inp02$veloYaxs,"unit") <- "m s-1"; attr(inp02$veloZaxs,"unit") <- "m s-1"
#' out02 <- def.stat.sta.diff(inp = inp02)
#' utils::str(out02)
#' base::rm(out02)
#' Example 3, difference when computing angular average without and with reference (wind direction).
#' out03 <- def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp02)$data)
#' out04 <- def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp02)$data,
#'                            refe = base::list("mean" = base::list("angZaxsErth" = 
#'                                                                    def.rot.ang.zaxs.erth(inp = inp02)$rot$angZaxsErth))
#'                            )
#' utils::str(out03)
#' utils::str(out04)
#' out03$mean$angZaxsErth - out04$mean$angZaxsErth
#' base::rm(inp02, out03, out04)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2022-02-07)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################


# Summary statistics, base states, instantaneous differences
def.stat.sta.diff <- function(
  
  # input dataframe containing the variables for which to perform the calculations, each with unit attribute
  inp,
  
  # list of reference quantities that can be supplied to overwrite internal calculations
  # currently implemented only for refe$mean in combination with inp variables in units [rad]
  refe = NULL,
  
  # base state, defaults to "mean", additional options are detrending "trnd" and 3rd-order polynomial "ord03"
  AlgBase = c("mean", "trnd", "ord03")[1]
) {

  # check that input is of class data.frame
  if(base::class(inp) != "data.frame") {
    stop(base::paste0("def.stat.sta.diff(): inp is not of class data.frame, please check."))  
  }

    # test input variables and unit attributes
    for(idx in base::names(inp)){
      # idx <- "veloXaxs"
      
      # test for presence/absence of unit attribute
      if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
        stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing unit attribute."))        }
  
    }; base::rm(idx)
  
    # check that inp$idep is present in case AlgBase != "mean"
    if(AlgBase != "mean" & !("idep" %in% base::names(inp))) {
      stop(base::paste0("def.stat.sta.diff(): please specify function argument inp$idep when AlgBase != 'mean'."))}
  
  
  # minimum and maximum
  
    # min
  
      # calculate
      min <- plyr::colwise("min")(inp, na.rm=TRUE)
      
      # apply units from inp to min
      base::sapply(base::names(min), function(x) {base::attr(min[[x]], which = "unit") <<- 
        base::attr(inp[[x]], which = "unit")})
      
    # max
      
      # calculate
      max <- plyr::colwise("max")(inp, na.rm=TRUE)
      
      # apply units from inp to max
      base::sapply(base::names(max), function(x) {base::attr(max[[x]], which = "unit") <<- 
        base::attr(inp[[x]], which = "unit")})

  
  # always calculate means as reference
  
    # calculate means for euclidean (linear, cartensian) quantities
    mean <- plyr::colwise("mean")(inp, na.rm = TRUE)

    # apply units from inp to mean
    base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") <<- 
      base::attr(inp[[x]], which = "unit")})
    
    # re-calculate / re-assign means for circular (polar) quantities as vector average
    
      # test for correct units
    
        # determine if there are any variables in unit "deg"
        tmp01 <- base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") == "deg"})
        
        # stop and print error message to screen
        if(length(which(tmp01)) > 0) {
          stop(base::paste0("def.stat.sta.diff(): ", base::names(base::which(tmp01)), 
                            " units are not matching internal units (radians), please check."))}
        
        # clean up
        base::rm(tmp01)
      
      # determine whether there are any variables in unit "rad"
      tmp02 <- base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") == "rad"})
      
      # continue only if there is at least one variable in unit "rad"
      if(length(which(tmp02)) > 0) {
        
        # variables to re-calculate / re-assign
        tmp03 <- base::names(base::which(tmp02))
        
        # re-calculate / re-assign
        for(idx in tmp03) {
          
          # re-assign reference value if provided
          if(!is.null(refe$mean) & idx %in% base::names(refe$mean)) {
            
            # check that refe is of class list
            if(base::class(refe) != "list") {
              stop(base::paste0("def.stat.sta.diff(): refe is not of class list, please check."))  
            }
            
            # test for correct units
            if(attributes(refe$mean[[idx]])$unit != attributes(inp[[idx]])$unit) {
              stop(base::paste0("def.stat.sta.diff(): refe$mean$", idx, 
                                " unit is not matching inp$", idx, " unit, please check."))}
            
            # re-assign
            mean[[idx]] <- refe$mean[[idx]]
          
          # re-calculate otherwise
          # need to change from degree to internal units radians: eddy4R.base::def.pol.cart() and 
          # eddy4R.base::def.cart.pol(); inconsistent results when calculating mean wind direction
          # 1) directly from mean horizontal wind vector [m s-1] via single call to eddy4R.base::def.pol.cart() vs.
          # 2) re-calculating from high-frequency wind direction -> unit vector via call to 
          # eddy4R.base::def.pol.cart(eddy4R.base::def.cart.pol()); implemented above look-back as a
          # workaround specifically for wind direction to be consistently defined as the direction that
          # minimizes the mean cross-wind; example:
          # 124.8 deg from 1) wind vector: eddy4R.base::def.pol.cart(cart = base::matrix(c(0.9941063, -1.429056), ncol=2))
          # 123.6 deg from 2) unit vector: eddy4R.base::def.pol.cart(cart = base::matrix(c(0.5394431, -0.8106568), ncol=2))
          } else {
          
            mean[[idx]] <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(
              cart = base::matrix(
                base::colMeans(
                  eddy4R.base::def.cart.pol(
                    az = eddy4R.base::def.conv.poly(data = data[[idx]], coefPoly = eddy4R.base::IntlConv$RadDeg)
                    ),
                  na.rm=TRUE),
                ncol=2)
              ), unitFrom = "deg", unitTo = "rad")

          } 
          
        }; base::rm(idx, tmp03)
        
      }; base::rm(tmp02)

      
  # base states; AlgBase <- c("mean", "trnd", "ord03")[1]
      
      # re-assign mean data if AlgBase == "mean"
      if(AlgBase == "mean") {
        
        base <- mean
        
      # compute base state otherwise
      } else {
        
        # calculate base states      
        base <- base::sapply(base::names(inp), function(x) eddy4R.base::def.base.ec(
          idxTime = inp$idep,
          var = inp[[x]],
          AlgBase = AlgBase),
          simplify = FALSE)
        
        # reshape results
        base <- base::as.data.frame(base::matrix(base::unlist(base), ncol = base::ncol(inp)))
        base::attributes(base)$names <- base::attributes(inp)$names
        
        # apply units from inp to base
        base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") <<- 
          base::attr(inp[[x]], which = "unit")})
        
        # determine whether there are any variables in unit "rad"
        tmp01 <- base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") == "rad"})
        
        # continue only if there is at least one variable in unit "rad"
        if(length(which(tmp01)) > 0) {
          
          # variables to re-calculate / re-assign
          tmp02 <- base::names(base::which(tmp01))
          
          # assign NAs - current computation of de-trending or 3rd order polynomial not defined for circular quantities
          for(idx in tmp02) {
            
            base[[idx]] <- base::rep(x = NaN, length.out = base::length(base[[idx]]))
            attributes(base[[idx]])$unit <- attributes(inp[[idx]])$unit
            
          }
          base::rm(idx)
          
        }
        # clean up
        base::rm(tmp01, tmp02)
      
      }

      
  # instantaneous differences (corresponding to chosen base state treatment)
      
    # calculate
    diff <- sapply(base::names(inp), function(x) inp[[x]] - base[[x]])
    
    # reshape data
    diff <- base::as.data.frame(base::matrix(diff, ncol = base::ncol(inp)))
    base::attributes(diff)$names <- base::attributes(inp)$names
    
    # apply units from inp to diff
    base::sapply(base::names(diff), function(x) {base::attr(diff[[x]], which = "unit") <<- 
      base::attr(inp[[x]], which = "unit")})

      
  # standard deviations (corresponding to chosen base state treatment)
    
    # calculate
    sd <- plyr::colwise("sd")(diff, na.rm=TRUE)
    
    # apply units from inp to sd
    base::sapply(base::names(sd), function(x) {base::attr(sd[[x]], which = "unit") <<- 
      base::attr(inp[[x]], which = "unit")})
    
  # create object for export
    
    # create list
    rpt <- base::list()
    
    # populate list
    rpt$min <- min
    rpt$max <- max
    rpt$mean <- mean
    rpt$base <- base
    rpt$diff <- diff
    rpt$sd <- sd
    
    # clean up
    rm(base, diff, max, mean, min, sd)
    
    # return results
    return(rpt)

}
