##############################################################################################
#' @title Definition function: Unit conversion 

#' @author 
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Convert data between units using unit character strings 
#' or numerical conversion factors

#' @param \code{data} Required. A named data frame of type numeric, containing the data to be converted. 
#' A numeric vector input is also allowed if only 1 variable is to be converted.
#' @param \code{unitFrom} Optional. The current units of \code{data}. Either: \cr 
#' (1) the case-sensitive character string "intl", signifying the current units are in the internal 
#' unit base (see documentation for eddy4R.base internal data IntlUnit$Intl). \cr
#' (2) a named list or vector of type character containing the existing units of the corresponding variable 
#' in \code{data} (see rules below for formatting unit character strings). Note, if a character vector 
#' containing only one unit string is specified, it will be used for all variables in \code{data}.\cr
#' (3) the case-sensitive character string "arb" (default), signifying the current units are arbitrarily 
#' defined and numerical scale factors for conversion are given within input parameter \code{coefPoly}.
#' @param \code{unitTo} Optional if \code{unitFrom = "arb"}. The units to convert to.
#' Either: \cr 
#' (1) the character string "intl", signifying to convert to the internal unit base. In this case, 
#' input parameter \code{unitFrom} must be a named list of units to convert from). \cr
#' (2) a named list or vector of type character containing the desired output units of the corresponding variable 
#' in \code{data} (see rules below for formatting unit character strings). Note, if a character vector 
#' containing only one unit string is specified, it will be used for all variables in \code{data}.\cr
#' (3) the character string "arb" (default), signifying the current units are arbitrarily defined and numerical
#' scale factors for conversion are given within input parameter \code{coefPoly}.
#' \cr
#' Use the following rules to construct unit character strings: \cr
#' Unit character strings must be constructed from the base unit symbols (case-sensitive) listed 
#' in eddy4R.base internal data IntlUnit$Base$Symb (e.g. the symbol for meter is "m"). \cr
#' \cr
#' Unit base symbols can be directly preceded (no space) by the case-sensitive unit prefixes listed in 
#' eddy4R.base internal data IntlUnit$Prfx (e.g. kilometers = "km") . \cr
#'  \cr
#' Unit base symbols can be directly followed (no spaces) by the suffix \code{n}, where \code{n} is 
#' an integer (...-2,-1,0,1,2...), indicating the unit is raised to the power of 
#' \code{n} (e.g. per square kilometer = "km-2").  \cr
#' \cr
#' In the case of chemical species (i.e. converting between mass and molar units), specify
#' the full unit (including prefix and suffix) followed immediately (no spaces inbetween) by one of 
#' the chemical species characters listed in eddy4R.base internal data IntlUnit$Spcs 
#' (eg. per gram of carbon dioxide = "g-1CO2"): \cr
#' \cr
#' Compound units can be formed by inserting spaces between the individual unit components 
#' (ex.milligrams carbon per meter squared per day = "mgC m-2 d-1").\cr
#' 
#' \cr
#' Additional rules for unit conversion: \cr
#' (1) Number of input & output base units must be the same. \cr
#' (2) Input-output units must be mapped 1-to-1, position-wise in the compound unit string. 
#' Thus, it is recommended to express units in the highest level available (i.e. if wanting to 
#' convert pressure units from pounds per square inch to Pascals, use "psi" and "Pa" rather 
#' than "lbf in2" and "Pa". The latter will result in an error. \cr
#' (3) Except for mass to mole conversions, you can only convert within the same unit type  
#' (i.e. you can't go from pressure to force/area, even tho these are technically compatible.  
#' Check documentation for eddy4R.base::IntlUnit to see unit types) \cr
#' (4) Suffixes (powers) between corresponding input & output terms must be the same.
#' (5) If a conversion polynomial has more than the a1 term (a0,a2,... i.e more than a 
#' linear multiplier), there must be only one base term in each the input and output (i.e. a 
#' single-unit conversion, ex. degrees F to C) and the unit suffix must be 1 (no powers). 
#' This is because internally-defined conversion polynomials reflect the conversion 
#' between actual values of the units, not how a change in one unit results in a change 
#' in the other (i.e. the derivative). \cr
#' (6) If converting to/from the eddy4R internal unit base, mass to mol conversions are only allowed
#' for single gas species (i.e. CO2, H2O, CH4). No gas mixtures other than dry air are allowed. This 
#' is relevant for moist air, which must be converted to molar units outside of this unit conversion 
#' function.
#' \cr
#' @param \code{coefPoly} Optional. A named list of type numeric, containing the polynomial 
#' coefficients (in increasing order) to apply to the corresponding variable in \code{data}. 
#' Each entry in the named list contains a numerical vector of [a0 a1 a2 a3 ...] signifying the 
#' coeficients of the equation a0 + a1*x + a2*x^2 + a3*x^3 ... , where x is the input data.
#' Default is c(0,1) for each variable in \code{data}. Note: If unitFrom and unitTo are input along
#' with coefPoly, coefPoly is applied first. Then the unit transformation from \code{unitFrom} to
#' \code{unitTo} is performed.
#'  
#' @return Function output format depends on input parameter \code{vrbs}. \cr
#' If \code{vrbs} is set to \code{FALSE} (default), the function returns a vector or named data frame 
#' (matching the input format of \code{data}) with unit conversion 
#' applied. Attribute \code{unit} (queried by base::attributes()) specifies the output units. All 
#' attributes attached to input \code{data} are retained. \cr
#' If \code{vrbs} is set to \code{TRUE}, the function returns a named list containing the following:\cr
#' data = a named data frame (matching the input variables of \code{data}) with unit conversion applied \cr
#' coefPoly = a named list containing the numerical scaling coefficients applied for each variable \cr
#' unitFrom = a named list containing the unit character strings of each input variable \cr
#' unitTo = a named list containing the unit character strings of each output variable
#' 
#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords unit conversion, input units, output units

#' @examples Currently none

#' @seealso \code{\link{IntlUnit}}, \code{\link{def.unit.info}}, \code{\link{def.unit.intl}}

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-03-30)
#     original creation 
#   Cove Sturtevant (2016-04-20)
#     first full working & documented version
#   Cove Sturtevant (2016-04-29)
#     update all function calls to use double-colon operator
#     added acceptance of vector input for coefPoly if data has only 1 variable
#   Cove Sturtevant (2016-05-04)
#     function now accepts vector input for input data, and returns same
#     added vrbs option to control whether output is a list explicitly calling out data,
#        unitFrom,unitTo, and coefPoly OR as a vector or data frame (matching input data) with 
#        output units specified as an attribute (attribute name: unit)
#   Cove Sturtevant (2016-08-11)
#     assigned names to unit attribute output
#     reduced memory usage by re-using variables & applying garbage collection
#     adjusted error checking to allow data & unit throughput if units are not recognized
#   Cove Sturtevant (2016-08-23)
#     fixed bug causing incorrect application of unit suffix to unit prefix conversion
#   Cove Sturtevant (2016-08-25)
#     fixed potential hazard when applying polynomial conversion coefficients AND unit string 
#        conversion. If unit string wasn't recognized, the polynomial was still applied.
#   Cove Sturtevant (2016-11-03)
#     added catch for when input 'data' is NULL
#   Natchaya P-Durden (2016-12-02)
#     rename function to def.unit.conv()
##############################################################################################

def.unit.conv <- function(
  data,
  unitFrom="arb",
  unitTo="arb",
  coefPoly=base::lapply(base::as.data.frame(data),function(x) c(0,1)),
  vrbs=FALSE
  ) {
  
  # Make sure data is not NULL
  if(is.null(data)) {
    base::stop("Required input variable \'data\' cannot be NULL. Check inputs.",call. = FALSE)
  }
  
  # Check whether we have a vector input for data. If so, we will 
  # return the converted output in the same format
  flagVect <- FALSE
  if(is.vector(data) && !is.list(data)) {
    flagVect <- TRUE
  }
  
  # Pre-processing
  data <- base::as.data.frame(data)
  nameVars <- base::names(data)  
  
  # Allow a single vector of polynomial scaling coefficients if only one variable is specified
  if (!base::is.list(coefPoly) && base::is.vector(coefPoly) && base::length(data) == 1) {
    coefPoly <- base::list(data=coefPoly)
    base::names(coefPoly) <- nameVars
  }  
  
  # Check for proper input format of polynomial scaling coefficients
  if(!base::is.list(coefPoly) || base::Reduce("+",lapply(coefPoly,base::is.numeric))!=base::length(coefPoly)) {
    base::stop("Polynomial scaling coefficients must be specified in a numeric list",call. = FALSE)
  }
  
  # Check for proper use of arbitrary units
  if(((base::length(unitFrom) != 1) && (base::sum(unitFrom == "arb") > 0)) || 
     ((base::length(unitTo) != 1) && (base::sum(unitTo == "arb") > 0)) ||
     (base::sum(base::is.na(base::pmatch(c(unitFrom[[1]],unitTo[[1]]),c("arb","arb")))) == 1)) {
    base::stop(base::paste("Arbitrary units can only be specified when the entire dataset is converted",
               "via polynomial scaling. To use arbitrary units, set both unitFrom = \"arb\" and",
                "unitTo == \"arb\", and specify polynomial scaling coefficients in a list."),call. = FALSE)
  }
  
  # If internal unit base being used, make a list assigning to each variable.
  # Also allow single inputs of input and output units, to be farmed out to all variables.
  if((base::length(unitFrom) == 1) && is.character(unitFrom) && (unitFrom != "arb")){
    unitFrom <- base::as.list(rep(unitFrom,length=length(nameVars)))
    base::names(unitFrom) <- nameVars
  }
  if((length(unitTo) == 1) && is.character(unitTo) && (unitTo != "arb")){
    unitTo <- base::as.list(rep(unitTo,length=base::length(nameVars)))
    base::names(unitTo) <- nameVars
  }
  
  # Allow non-list unit inputs if inputs consist of vectors that match
  nameUnitFrom <- base::names(unitFrom)
  nameUnitTo <- base::names(unitTo)
  if(base::is.null(nameUnitFrom) && (base::length(data) == base::length(unitFrom))) {
    unitFrom <- base::as.list(unitFrom)
    base::names(unitFrom) <- nameVars
  }
  if(base::is.null(nameUnitTo) && (base::length(data) == base::length(unitTo))) {
    unitTo <- base::as.list(unitTo)
    base::names(unitTo) <- nameVars
  }
  
  # Check for naming consistency
  nameUnitFrom <- base::names(unitFrom)
  nameUnitTo <- base::names(unitTo)
  nameCoefPoly <- base::names(coefPoly)
  if(base::is.null(nameVars) || (base::is.na(base::sum(base::pmatch(nameVars,nameUnitFrom))) && (unitFrom != "arb")) || 
     base::is.na(base::sum(base::pmatch(nameVars,nameCoefPoly))) ) {
    base::stop("Variable names are not consistent among inputs: data, coefPoly, unitFrom, and unitTo. Check inputs.",call. = FALSE)
  }
  
  # Check for proper use of internal units
  if (base::length(base::which((unitTo == "intl") & (unitFrom == "intl"))) != 0) {
    # There is at least one instance where both unitFrom & unitTo are listed as "intl"
    base::stop("Both unitFrom and unitTo cannot be set as \"intl\". Check inputs.",call. = FALSE)
  }
    
  # Apply polynomial transformation
  for(idxVar in nameVars) {
    # If no transformation, skip
    if((length(coefPoly[[idxVar]]) == 2) && (coefPoly[[idxVar]][1] == 0) && (coefPoly[[idxVar]][2] == 1)) {
      next
    }
    data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=coefPoly[[idxVar]])
    base::gc(verbose=FALSE) # Clean up memory
  }

  # If we aren't using unit character strings, we are done
  if((base::length(unitFrom) == 1) && (unitFrom == "arb")) {
    base::return(base::list(data=data,coefPoly=coefPoly,unitFrom=unitFrom,unitTo=unitTo))
  }
  
  # Apply unit conversion using character strings
  for(idxVar in nameVars) {
    
    # Check for character unit inputs 
    if(!base::is.character(unitFrom[[idxVar]]) || !base::is.character(unitTo[[idxVar]]))  {
      base::stop("Input and output units for each variable must be character strings. Check inputs.",call. = FALSE)
    }
    
    # If we have the exact same input and output unit character strings, move on
    if (unitFrom[[idxVar]] == unitTo[[idxVar]]) {
      next
    }
    
    # Intpret "From" units
    if(unitFrom[[idxVar]] == "intl"){
      # Using the internal unit base. Find internal units corresponding to "To" units
      unitFrom[[idxVar]] <- eddy4R.base::def.unit.intl(unitTo[[idxVar]])
    }
    infoUnitFrom <- eddy4R.base::def.unit.info(unitFrom[[idxVar]])
    # Check to make sure they were interpreted correctly
    if(base::sum(base::is.na(infoUnitFrom$posBase)) > 0) {
      
      # Did we already apply a polynomial conversion? If so, we need to NA everything
      if(!base::isTRUE(base::all.equal(coefPoly[[idxVar]],c(0,1)))){
        base::warning(base::paste("Cannot interpret unitFrom for variable: \"",idxVar,
                                  "\". Output for this variable will be NA. ",
                                  "Check unit terms or only use polynomial scaling coefficients."))
        data[[idxVar]][] <- NA
        next
      }
      
      # Otherwise, we can simpy pipe thru the input units
      base::warning(base::paste("Cannot interpret unitFrom for variable: \"",idxVar,
                    "\". No unit coversion will be performed for this variable. ",
                    "Check unit terms or use polynomial scaling coefficients instead."))
      unitTo[[idxVar]] <- unitFrom[[idxVar]]
      next
    }

    # Intpret To" units
    if(unitTo[[idxVar]] == "intl"){
      # Using the internal unit base. Find internal units corresponding to "From" units
      unitTo[[idxVar]] <- eddy4R.base::def.unit.intl(unitFrom[[idxVar]])
    }
    infoUnitTo <- eddy4R.base::def.unit.info(unitTo[[idxVar]])
    # Check to make sure they were interpreted correctly
    if(base::sum(base::is.na(infoUnitTo$posBase)) > 0) {
      
      # Did we already apply a polynomial conversion? If so, we need to NA everything
      if(!base::isTRUE(base::all.equal(coefPoly[[idxVar]],c(0,1)))){
        base::warning(base::paste("Cannot interpret unitTo for variable: \"",idxVar,
                                  "\". Output for this variable will be NA. ",
                                  "Check unit terms or only use polynomial scaling coefficients."))
        data[[idxVar]][] <- NA
        next
      }
      
      # Otherwise, we can simpy pipe thru the input units
      base::warning(base::paste("Cannot interpret unitTo for variable: \"",idxVar,
                                "\". No unit coversion will be performed for this variable. ",
                                "Check unit terms or use polynomial scaling coefficients instead."))
      unitTo[[idxVar]] <- unitFrom[[idxVar]]
      next
    }
    
    # Do some checking to make sure we are following the rules:
    
    # Check to make sure the "From" and "To" units have the same number of base unit terms
    if (base::length(infoUnitTo$posBase) != base::length(infoUnitFrom$posBase)) {
      
      # Did we already apply a polynomial conversion? If so, we need to NA everything
      if(!base::isTRUE(base::all.equal(coefPoly[[idxVar]],c(0,1)))){
        base::warning(base::paste("Number of terms in unitTo and unitFrom must be the same. Unit conversion ", 
                                  "for variable \"",idxVar, "\" not possible. Output for this variable set to NA. ",
                                  "Check unit terms or use polynomial scaling coefficients only."))
        data[[idxVar]][] <- NA
        next
      }
      
      # Otherwise, just pipe thru input inits
      base::warning(base::paste("Number of terms in unitTo and unitFrom must be the same. Unit conversion ", 
                    "for variable \"",idxVar, "\" not possible. Input units will be retained. ",
                     "Check unit terms or use polynomial scaling coefficients instead."))
      unitTo[[idxVar]] <- unitFrom[[idxVar]]
      next
    }
    
    # Check suffixes are the same between corresponding input & output units
    if(base::sum(infoUnitFrom$sufx != infoUnitTo$sufx) > 0 || base::is.na(infoUnitFrom$sufx != infoUnitTo$sufx)) {
      
      if(!base::isTRUE(base::all.equal(coefPoly[[idxVar]],c(0,1)))){
        base::warning(base::paste("Unit suffixes for corresponding terms in unitTo and unitFrom must be the same. ", 
                                  "Unit conversion for variable: \"",idxVar, "\" not possible. Output for this variable set to NA. ",
                                  "Check unit terms or use polynomial scaling coefficients only."))
        data[[idxVar]][] <- NA
        next
      }
      
      base::warning(base::paste("Unit suffixes for corresponding terms in unitTo and unitFrom must be the same. ", 
                    "Unit conversion for variable: \"",idxVar, "\" not possible. Input units will be retained. ",
                    "Check unit terms or use polynomial scaling coefficients instead."))
      unitTo[[idxVar]] <- unitFrom[[idxVar]]
      next
      
    }
    
  # Go through each base term, applying conversion along the way
    for (idxBase in 1:base::length(infoUnitFrom$posBase)) {

      # Convert "from" unit prefix to no-prefix 
      if(!base::is.na(infoUnitFrom$posPrfx[idxBase])) {
        coefPolyPrfxFrom <- eddy4R.base::IntlConv[[paste0(names(eddy4R.base::IntlUnit$Prfx[infoUnitFrom$posPrfx[idxBase]]),"None")]]
        data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,coefPolyPrfxFrom[2]^infoUnitFrom$sufx[idxBase])) # Convert data using polynomial function
        base::gc(verbose=FALSE) # Clean up memory
      }
      
      # Do we have a chemical species? If so, we will likely need the molar mass, so do it here
      if(!base::is.na(infoUnitFrom$posSpcs[idxBase])) {
        # Get molar mass of "From" species
        if (nchar(eddy4R.base::IntlUnit$Spcs[infoUnitFrom$posSpcs[idxBase]]) > 1) {
          spcsSplt <- base::strsplit(eddy4R.base::IntlUnit$Spcs[infoUnitFrom$posSpcs[idxBase]],"")[[1]]
          nameSpcs <- base::paste0(base::toupper(spcsSplt[1]),base::paste0(base::tolower(spcsSplt[2:base::nchar(eddy4R.base::IntlUnit$Spcs[infoUnitFrom$posSpcs[idxBase]])]),collapse=""))
          molmSpcsFrom <- eddy4R.base::IntlNatu[[base::paste0("Molm",nameSpcs)]]*1000; # molar mass [g/mol]
        } 
        else {
          molmSpcsFrom <- eddy4R.base::IntlNatu[[base::paste0("Molm",base::toupper(eddy4R.base::IntlUnit$Spcs[infoUnitFrom$posSpcs[idxBase]]))]]*1000 # molar mass [g/mol]
        }
      }
      if(!base::is.na(infoUnitTo$posSpcs[idxBase])) {
        # Get molar mass of "To" species
        if (base::nchar(eddy4R.base::IntlUnit$Spcs[infoUnitTo$posSpcs[idxBase]]) > 1) {
          spcsSplt <- base::strsplit(eddy4R.base::IntlUnit$Spcs[infoUnitTo$posSpcs[idxBase]],"")[[1]]
          nameSpcs <- base::paste0(base::toupper(spcsSplt[1]),base::paste0(base::tolower(spcsSplt[2:base::nchar(
            eddy4R.base::IntlUnit$Spcs[infoUnitTo$posSpcs[idxBase]])]),collapse=""))
          molmSpcsTo <- eddy4R.base::IntlNatu[[base::paste0("Molm",nameSpcs)]]*1000 # molar mass [g/mol]
        } 
        else {
          molmSpcsTo <- eddy4R.base::IntlNatu[[base::paste0("Molm",base::toupper(eddy4R.base::IntlUnit$Spcs[infoUnitTo$posSpcs[idxBase]]))]]*1000 # molar mass [g/mol]
        }
      }
      
      
      # Base unit conversion necessary only if base units differ (an exception below)
      if(infoUnitFrom$posBase[idxBase] != infoUnitTo$posBase[idxBase]) {
      
        # Check to make sure the corresponding "To" unit is of the same type
        if(infoUnitFrom$type[idxBase] != infoUnitTo$type[idxBase]){
          
          # Check if we are converting between mass and mole. If so, need chemical species for both
          if((infoUnitFrom$type[idxBase] == "Mass" && infoUnitTo$type[idxBase] == "Num") || 
            (infoUnitFrom$type[idxBase] == "Num" && infoUnitTo$type[idxBase] == "Mass")) {
            
            if(base::is.na(infoUnitFrom$posSpcs[idxBase]) || base::is.na(infoUnitTo$posSpcs[idxBase])) {
              base::warning(base::paste("Cannot interpret chemical species needed for conversion between mass and ", 
                      "molar units for variable \"",idxVar,"\". Output data for this variable will be NA. ",
                      "Check unit terms or use polynomial scaling coefficients instead."))
              data[[idxVar]][] <- NA
              base::gc(verbose=FALSE) # Clean up memory
              break
            } else {
              
              # Convert between mass & molar units

              if (infoUnitFrom$type[idxBase] == "Mass") {
                # We are going from mass to mole
                
                # If we have an input unit other than g, convert to g first
                if(eddy4R.base::IntlUnit$Base$Symb[[infoUnitFrom$posBase[idxBase]]] != "g"){
                  
                  nameBaseFrom <- base::names(eddy4R.base::IntlUnit$Base$Symb[infoUnitFrom$posBase[idxBase]])
                  coefPolyBase <- eddy4R.base::IntlConv[[base::paste0(nameBaseFrom,"Gram")]]
                  data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=coefPolyBase) # Convert data using polynomial function
                  base::gc(verbose=FALSE) # Clean up memory
                  
                }
                
                # Now do g to mol conversion
                data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,molmSpcsFrom^(-infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                base::gc(verbose=FALSE) # Clean up memory
                
              } 
              else {
                # We are going from mol to mass
                
                # Convert to g
                data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,molmSpcsTo^(infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                base::gc(verbose=FALSE) # Clean up memory
                
                # If we are outputting to a base unit other than g, convert that here
                if(eddy4R.base::IntlUnit$Base$Symb[[infoUnitTo$posBase[idxBase]]] != "g"){
                  
                  nameBaseTo <- base::names(eddy4R.base::IntlUnit$Base$Symb[infoUnitTo$posBase[idxBase]])
                  coefPolyBase <- eddy4R.base::IntlConv[[paste0("Gram",nameBaseTo)]]
                  data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,coefPolyBase[2]^(infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                  base::gc(verbose=FALSE) # Clean up memory
                  
                }
                
              } 
              
            }
          } # End mass-mole conversion 
          else {
            
            # Otherwise, can't do conversion
            base::warning(base::paste("Input-output units must be mapped 1-to-1 position-wise, within the",
                    "unit strings. Corresponding input-output units for variable \"",idxVar,
                    "\" are not of the same unit type. Output data for this variable will be NA.",
                    "Check unit terms or use polynomial scaling coefficients instead."))
            data[[idxVar]][] <- NA
            base::gc(verbose=FALSE) # Clean up memory
            break
          }         
                       
        } 
        else {
          # We have same-type unit conversion between different base units
          
          # Find base unit conversion polynomial
          nameBaseFrom <- base::names(eddy4R.base::IntlUnit$Base$Symb[infoUnitFrom$posBase[idxBase]])
          nameBaseTo <- base::names(eddy4R.base::IntlUnit$Base$Symb[infoUnitTo$posBase[idxBase]])
          coefPolyBase <- eddy4R.base::IntlConv[[base::paste0(nameBaseFrom,nameBaseTo)]]
          # Make sure we have conversion coefficients that follow the rules
          if(is.null(coefPolyBase)){
            # Can't find conversion
            base::warning(base::paste("Conversion between input-output units",
                          base::paste0("eddy4R.base::IntlConv$",nameBaseFrom,nameBaseTo),"not found for variable \"",idxVar,
                          "\". Check unit terms or use polynomial scaling coefficients instead."))
            data[[idxVar]][] <- NA
            base::gc(verbose=FALSE) # Clean up memory
            break
          } 
          else if (((coefPolyBase[1] != 0) || 
                      ((base::length(coefPolyBase) > 2) && (base::sum(base::abs(coefPolyBase[3:base::length(coefPolyBase)]),na.rm=TRUE) > 0))) &&
                     ((base::length(infoUnitFrom$posBase) > 1) || (infoUnitFrom$sufx[idxBase] != 1))) {
            # We broke our rule of only 1 term and no suffix for conversion polynomial <> a1)
            base::warning(base::paste("For conversions which have polynomial coefficients other than a1 (",
                          "in this case", base::paste0("eddy4R.base::IntlConv$",nameBaseFrom,nameBaseTo),")",
                          "the input/output units must have only 1 base term and no unit suffixes.",
                          "Cannot perform unit conversion for variable \"",idxVar,"\". Output data",
                          "for this variable will be NA.",
                          "Check unit terms or use polynomial scaling coefficients instead."))
            base::gc(verbose=FALSE) # Clean up memory
            data[[idxVar]][] <- NA
            break
            
          } 
          
          # Great, we have good conversion polynomial. Apply.
          if(infoUnitTo$sufx[idxBase] == 1) {
            data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=coefPolyBase^infoUnitTo$sufx[idxBase]) # Convert data using polynomial function
            base::gc(verbose=FALSE) # Clean up memory
          } 
          else {
            data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,coefPolyBase[2]^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
            base::gc(verbose=FALSE) # Clean up memory
          }

          # If we are converting between different mass units of different chemical species
          if (((infoUnitFrom$type[idxBase] == "Mass") && (infoUnitTo$type[idxBase] == "Mass")) && 
              !base::is.na(infoUnitFrom$posSpcs[idxBase]+infoUnitTo$posSpcs[idxBase]) &&
              (infoUnitFrom$posSpcs[idxBase] != infoUnitTo$posSpcs[idxBase])) {
            
            # Convert different mass units between different chemical species
            # (no need to account for base unit other than gram, since did it already)
            data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,(molmSpcsTo/molmSpcsFrom)^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
            base::gc(verbose=FALSE) # Clean up memory
            
          }
          
        }
      
      } 
      else if (((infoUnitFrom$type[idxBase] == "Mass") && (infoUnitTo$type[idxBase] == "Mass")) && 
                 !base::is.na(infoUnitFrom$posSpcs[idxBase]+infoUnitTo$posSpcs[idxBase]) &&
                 (infoUnitFrom$posSpcs[idxBase] != infoUnitTo$posSpcs[idxBase])) {
        # Convert same mass units between different chemical species
        # (no need to account for base unit other than gram, since it will cancel)
        data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,(molmSpcsTo/molmSpcsFrom)^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
        base::gc(verbose=FALSE) # Clean up memory
        
      }# End base-unit conversion
      
    
      # Convert to output unit prefix 
      if(!base::is.na(infoUnitTo$posPrfx[idxBase])) {
        coefPolyPrfxTo <- eddy4R.base::IntlConv[[base::paste0("None",base::names(eddy4R.base::IntlUnit$Prfx[infoUnitTo$posPrfx[idxBase]]))]]
        data[[idxVar]] <- eddy4R.base::def.conv.poly(data=data[[idxVar]],coefPoly=c(0,coefPolyPrfxTo[2]^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
        base::gc(verbose=FALSE) # Clean up memory
      }
          
      
    } # End loop around unit terms
    
  } # End loop around variables

  # If data was entered as single vector inputs, return the converted output as such
  if(flagVect) {
    data <- data[[1]]
    unitFrom <- unitFrom[[1]]
    unitTo <- unitTo[[1]]
    coefPoly <- coefPoly[[1]]
    base::gc(verbose=FALSE) # Clean up memory
  }
  
  # As default, assign units as attribute attached to output data frame. 
  # But if vrbs is set to TRUE, return output as a list explicitly identifying 
  # input & output units and conversion polynomial
  if(vrbs == FALSE) {
    
    # Assign units as attributes to output data (data frame or vector)
    attr <- base::attributes(data) # Get current attributes
    attr$unit <- base::unlist(unitTo) # Add/modify units
    base::attributes(data) <- attr # Re-load the attributes to the output
    
    # Output
    base::return(data)
    
  } else {
      
    # Output as list
    base::return(base::list(data=data,coefPoly=coefPoly,unitFrom=unitFrom,unitTo=unitTo))

  }

}

