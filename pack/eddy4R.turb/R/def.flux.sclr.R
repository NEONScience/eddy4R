##############################################################################################
#' @title Definition function: Eddy-covariance turbulent flux calculation for scalar quantities

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. This function calculates eddy-covariance turbulent flux for scalar quantities, such as temperature, moisture, CO2, CH4, NOx, VOCs etc.

#' @param inp A data frame with the variables vect and sclr that each contain the instantaneous differences reported by ?def.stat.sta.diff. In a typical eddy-covariance application, vect would be the vertical wind speed in streamwise ENU convention (positive from below), e.g.  veloZaxs derived from ?def.rot.ang.zaxs.erth and further processed in ?def.stat.sta.diff, of class "numeric" and with unit attribute [m s-1]. scal would be any scalar quantity in SI base units that does not require WPL density correction (Webb et al., 1980), i.e. dry air temperature in unit [K] and gas concentration in dry mole fraction [mol m-3], of class "numeric" and with unit attribute. These inputs can be viewed as a specific example that can be generalized through replacement by other variables that share the same coordinate conventions and consistent units among inp and Unit.
#' @param conv An optional vector of class "numeric" with unit attribute to permit conversion of the results, e.g. to output units that are different from the product of the inp$vect unit and the inp$sclr unit. conv must be either of length = 1 or have the same length as number of observations in inp. If conv is of length = 1, then the same conversion factor is applied to all observations supplied in inp (e.g., unit conversion). On the other hand, if conv is of the same length as number of observations in inp, then a point-by-point conversion is performed individually for each observation supplied in inp (e.g., different weights for each observation).
#' @param Unit A data frame with the entries InpVect, InpSclr, Conv, Out, of class "character". To ensure consistent units of the returned object, Unit needs to be specified with the constraint that Unit$Out = Unit$InpVect * Unit$InpSclr * Unit$Conv. If the function call argument conv is not specified, then Unit$Conv should be supplied as = "-".
#' @param AlgBase A vector of length 1 that defines the base state with respect to which the element-dataframe base in the returned object is calculated, of class "character" and no unit attribute. Is set to one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details.
#' @param idep An optional vector of class "numeric" with unit attribute. idep is only required to be specified if argument AlgBase is set to "trnd" or "ord03", in which case idep provides the independent variable for interpolation.

#' @return 
#' The returned object is a list containing the element vectors base, conv, corr, diff, max, mean, min, sd, each of class "numeric" and with unit attribute.
#' All elements with the exception of conv and corr are calculated from the instantaneous products inp$vect * inp$scal * conv and are assigned the Unit$Out unit attribute. diff provides the instantaneous products themselves, with the same number of observations as inp. base provides the base state of the instantaneous products as specified per argument AlgBase, with a single observation for AlgBase = "mean", and the same number of observations as inp for AlgBase %in% c("trnd", "ord03"). max, mean, min and sd provide the maximum, mean, minimum and standard deviation of the instantaneous products, respectively, each with a single observation.
#' The element conv reports the conversion vector utilized during the fuction call, with the same number of observations as inp and unit attribute.
#' The element corr is calculated directly from inp$vect and inp$scal without invoking conv, and provides the correlation between vector and scalar quantity with unit attribute "-".

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Webb, E. K., Pearman, G. I., and Leuning, R.: Correction of flux measurements for density effects due to heat and water vapour transfer, Q. J. R. Meteorolog. Soc., 106, 85-100, doi:10.1002/qj.49710644707, 1980.

#' @keywords correlation, flux, temperature, moisture, water, humidity, H2O, CO2, CH4, NOx, VOC, standard deviation, scalar

#' @examples
#' Sensible heat flux in units of energy [kg s-3] = [W m-2]
#' make sure to assign all variables and units, the function should run ok.
#' input data: vertical wind speed and temperature instantaneous differences from base state, see ?def.stat.sta.diff for details
#' inp <- base::data.frame(
#'   vect = c(0.2259224, 0.2342562, 0.3936403, 0.2395388, 0.2885017),
#'   sclr = c(0.1067013, 0.1015043, 0.1324425, 0.1732023, 0.1262345)
#' )
#' attr(inp$vect,"unit") <- "m s-1"; attr(inp$sclr,"unit") <- "K"
#' volumetric heat capacity for conversion from kinematic units [K m s-1] to units of energy [W m-2], see ?def.heat.air.wet for details
#' conv <- 1220.079
#' attr(conv,"unit") <- "kg m-1 s2 K-1"
#' function call
#' out <- def.flux.sclr(
#'   inp = inp,
#'   conv = conv,
#'   Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "kg m-1 s2 K-1", Out = "W m-2")
#' )
#' utils::str(out)
#' base::rm(inp, conv, out)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2022-03-24)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# Eddy-covariance flux calculation for scalar quantities
def.flux.sclr <- function(
  inp,
  conv = NULL,
  Unit,
  AlgBase = c("mean", "trnd", "ord03")[1],
  idep = NULL
) {
    
  # check presence of input arguments, consistent lengths and units
      
    # conv check 1 of 2
      
      # if conv defaults to NULL, convert to 1 and assign unit attribute. Also specify Unit$Conv if not already present (failsafe)
      if(is.null(conv)) {
        conv <- 1
        base::attr(conv, which = "unit") <- "-"
        if(!("Conv" %in% base::names(Unit))) Unit$Conv <- "-"
      }
      
      # check that conv is either of length 1 or of the same length as inp
      if(!(base::length(conv) %in% c(1, base::nrow(inp)))) {
        stop(base::paste0("def.flux.sclr(): conv needs to be either of length 1 or have the same number of observations as inp, please check."))}  
  
    # Unit
    
      # check that Unit is of class data.frame
      if(base::class(Unit) != "data.frame") {
        stop(base::paste0("def.flux.sclr(): Unit is not of class data.frame, please check."))  
      }

      # test unit variables
      for(idx in c("InpVect", "InpSclr", "Conv", "Out")){
        # idx <- "InpVect"
        
        # test for presence/absence of variables
        if(!(idx %in% base::names(Unit))) {
          stop(base::paste0("def.flux.sclr(): Unit$", idx, " is missing."))}
        
        # test for character type
        if(base::typeof(Unit[[idx]]) != "character") {
          stop(base::paste0("def.flux.sclr(): Unit$", idx, 
                            " is not of type character, please check."))}
        
      }; base::rm(idx)
  
    # inp
  
      # check that input is of class data.frame
      if(base::class(inp) != "data.frame") {
        stop(base::paste0("def.flux.sclr(): inp is not of class data.frame, please check."))  
      }
      
      # test for presence/absence of input variables and unit attributes
      for(idx in c("vect", "sclr")){
        # idx <- "vect"
        
        # test for presence/absence of variables
        if(!(idx %in% base::names(inp))) {
          stop(base::paste0("def.flux.sclr(): inp$", idx, " is missing."))}
        
        # test for presence/absence of unit attribute
        if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
          stop(base::paste0("def.flux.sclr(): inp$", idx, " is missing unit attribute."))}
        
      }; base::rm(idx)
  
      # test for consistent units
        
        # inp$vect
        if(attributes(inp$vect)$unit != Unit$InpVect) {
          stop(base::paste0("def.flux.sclr(): inp$vect unit attribute does not match Unit$InpVect, please check."))}
  
        # inp$sclr
        if(attributes(inp$sclr)$unit != Unit$InpSclr) {
          stop(base::paste0("def.flux.sclr(): inp$sclr unit attribute does not match Unit$InpSclr, please check."))}
  
    # conv check 2 of 2

      # test for consistent units - do this after the Unit dataframe has been tested
      if(attributes(conv)$unit != Unit$Conv) {
        stop(base::paste0("def.flux.sclr(): conv unit attribute does not match Unit$Conv, please check."))}

    # AlgBase and idep
    if(AlgBase != "mean" & is.null(idep)) {
      stop(base::paste0("def.flux.sclr(): please specify function argument idep when AlgBase != 'mean'."))}

  
  # convert conv to the same number of observations as inp
  if(base::length(conv) == 1){
    convTmp <- base::rep(x = conv, length.out = base::nrow(inp))
    base::attr(convTmp, which = "unit") <- base::attr(conv, which = "unit")
    conv <- convTmp
    base::rm(convTmp)
  }
  
  
  # perform calculations
    
    # instantaneous fluxes from instantaneous vector and scalar differences in input (typically kinematic) units
  
      # calculation
      diff <- inp$vect * inp$sclr * conv

      # assign units
      base::attr(diff, which = "unit") <- Unit$Out
    
    # descriptive statistics incl. mean fluxes
    
      # calculation
      min <- base::min(diff, na.rm = TRUE)
      max <- base::max(diff, na.rm = TRUE)
      mean <- base::mean(diff, na.rm = TRUE)
      sd <- stats::sd(diff, na.rm = TRUE)
      if(AlgBase == "mean") base <- mean else base <- eddy4R.base::def.base.ec(
        idxTime = idep, var = diff, AlgBase = AlgBase)

      # assign units
      base::attr(base, which = "unit") <- Unit$Out
      base::attr(min, which = "unit") <- Unit$Out
      base::attr(max, which = "unit") <- Unit$Out
      base::attr(mean, which = "unit") <- Unit$Out
      base::attr(sd, which = "unit") <- Unit$Out
    
    # correlation
      
      # calculation
      corr <- stats::cor(inp$vect, inp$sclr, use = "pairwise.complete.obs")
      
      # assign units
      base::attr(corr, which = "unit") <- "-"
  
      
  # create object for export
    
    # create list
    rpt <- base::list()
    
    # populate list
    rpt$base <- base
    rpt$conv <- conv
    rpt$corr <- corr
    rpt$diff <- diff
    rpt$max <- max
    rpt$mean <- mean
    rpt$min <- min
    rpt$sd <- sd
    
    # clean up
    base::rm(base, conv, corr, diff, max, mean, min, sd)
    
    
  # return results
  return(rpt)
    
}
