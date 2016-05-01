##############################################################################################
#' @title Unit conversion 

#' @author 
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Convert data between units using unit character strings 
#' or numerical conversion factors

#' @param \code{data} Required. Named data frame of type numeric, containing the data to be converted
#' @param \code{coefPoly} Optional. A named list of type numeric, containing the polynomial 
#' coefficients (in increasing order) to apply to the corresponding variable in \code{data}. 
#' Each entry in the named list contains a numerical vector of [a0 a1 a2 a3 ...] signifying the 
#' coeficients of the equation a0 + a1*x + a2*x^2 + a3*x^3 ... , where x is the input data.
#' Default is c(0,1) for each variable in \code{data}. Note: If unitFrom and unitTo are input along
#' with coefPoly, coefPoly is applied first. Then the unit transformation from \code{unitFrom} to
#' \code{unitTo} is performed.
#' @param \code{unitFrom} Optional. The current units of \code{data}. Either: \cr 
#' (1) the case-sensitive character string "intl", signifying the current units are in the internal 
#' unit base (see documentation for eddy4R.base internal data Unit$Intl). \cr
#' (2) a named list of type character containing the existing units of the corresponding variable 
#' in \code{data} (see rules below for formatting unit character strings). \cr
#' (3) the case-sensitive character string "arb" (default), signifying the current units are arbitrarily 
#' defined and numerical scale factors for conversion are given within input parameter \code{coefPoly}.
#' @param \code{unitTo} The units to convert to. Either: \cr
#' (1) the character string "intl", signifying to convert to the internal unit base. In this case, 
#' input parameter \code{unitFrom} must be a named list of units to convert from). \cr
#' (2) a named list of type character containing the desired output units of the corresponding variable 
#' in \code{data} (see rules below for formatting unit character strings). \cr
#' (3) the character string "arb" (default), signifying the current units are arbitrarily defined and numerical
#' scale factors for conversion are given within input parameter \code{coefPoly}.
#' \cr
#' Use the following rules to construct unit character strings: \cr
#' Unit character strings must be constructed from the base unit symbols (case-sensitive) listed 
#' in eddy4R.base internal data Unit$Base$Symb (e.g. the symbol for meter is "m"). \cr
#' \cr
#' Unit base symbols can be directly preceded (no space) by the case-sensitive unit prefixes listed in 
#' eddy4R.base internal data Unit$Prfx (e.g. kilometers = "km") . \cr
#'  \cr
#' Unit base symbols can be directly followed (no spaces) by the suffix \code{n}, where \code{n} is 
#' an integer (...-2,-1,0,1,2...), indicating the unit is raised to the power of 
#' \code{n} (e.g. per square kilometer = "km-2").  \cr
#' \cr
#' In the case of chemical species (i.e. converting between mass and molar units), specify
#' the full unit (including prefix and suffix) followed immediately (no spaces inbetween) by one of 
#' the chemical species characters listed in eddy4R.base internal data Unit$Spcs 
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
#' Check documentation for eddy4R.base::Unit to see unit types) \cr
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
#'  
#' @return A named list containing the following:
#' dataConv = a named data frame (matching the input variables of \code{data}) with unit conversion applied
#' coefPoly = a named list containing the numerical scaling coefficients applied for each variable
#' unitFrom = a named list containing the unit character strings of each input variable
#' unitTo = a named list containing the unit character strings of each output variable
#' 
#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords unit conversion, input units, output units

#' @examples Currently none

#' @seealso \code{\link{Unit}}, \code{\link{def.intp.unit}}, \code{\link{def.find.unit.intl}}

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-03-30)
#     original creation 
#   Cove Sturtevant (2016-04-20)
#     first full working & documented version
##############################################################################################

def.conv.unit <- function(
  data,
  coefPoly=lapply(as.data.frame(data),function(x) c(0,1)),
  unitFrom="arb",
  unitTo="arb") {
 
  # Pre-processing
  data <- as.data.frame(data)
  nameVars <- names(data)  
  
  # Check for proper input format of polynomial scaling coefficients
  if(!is.list(coefPoly) || base::Reduce("+",lapply(coefPoly,is.numeric))!=length(coefPoly)) {
    stop("Polynomial scaling coefficients must be specified in a numeric list",call. = FALSE)
  }
  
  # Check for proper use of arbitrary units
  if(((length(unitFrom) != 1) && (sum(unitFrom == "arb") > 0)) || 
     ((length(unitTo) != 1) && (sum(unitTo == "arb") > 0)) ||
     (sum(is.na(pmatch(c(unitFrom[[1]],unitTo[[1]]),c("arb","arb")))) == 1)) {
    stop(paste("Arbitrary units can only be specified when the entire dataset is converted",
               "via polynomial scaling. To use arbitrary units, set both unitFrom = \"arb\" and",
                "unitTo == \"arb\", and specify polynomial scaling coefficients in a list."),call. = FALSE)
  }
  
  # If internal unit base being used, make a list assigning to each variable
  if((length(unitFrom) == 1) && (unitFrom == "intl")){
    unitFrom <- as.list(rep("intl",length=length(nameVars)))
    names(unitFrom) <- nameVars
  }
  if((length(unitTo) == 1) && (unitTo == "intl")){
    unitTo <- as.list(rep("intl",length=length(nameVars)))
    names(unitTo) <- nameVars
  }
  
  # Allow non-list input if inputs consist of vectors that match
  nameUnitFrom <- names(unitFrom)
  nameUnitTo <- names(unitTo)
  if(is.null(nameUnitFrom) && (length(data) == length(unitFrom))) {
    unitFrom <- as.list(unitFrom)
    names(unitFrom) <- nameVars
  }
  if(is.null(nameUnitTo) && (length(data) == length(unitTo))) {
    unitTo <- as.list(unitTo)
    names(unitTo) <- nameVars
  }
  
  # Check for naming consistency
  nameUnitFrom <- names(unitFrom)
  nameUnitTo <- names(unitTo)
  nameCoefPoly <- names(coefPoly)
  if(is.null(nameVars) || (is.na(sum(pmatch(nameVars,nameUnitFrom))) && (unitFrom != "arb")) || 
     is.na(sum(pmatch(nameVars,nameCoefPoly))) ) {
    stop("Variable names are not consistent among inputs: data, coefPoly, unitFrom, and unitTo. Check inputs.",call. = FALSE)
  }
  
  # Check for proper use of internal units
  if (length(which((unitTo == "intl") & (unitFrom == "intl"))) != 0) {
    # There is at least one instance where both unitFrom & unitTo are listed as "intl"
    stop("Both unitFrom and unitTo cannot be set as \"intl\". Check inputs.",call. = FALSE)
  }
    
  # Initialize output data
  dataConv <- data
  dataConv[] <- NA
  
  # Pre-processing
  # If internal unit base being used, make a list assigning to each variable
  if((length(unitFrom) == 1) && (unitFrom == "intl")){
    unitFrom <- as.list(rep("intl",length=length(nameVars)))
    names(unitFrom) <- nameVars
  }
  if((length(unitTo) == 1) && (unitTo == "intl")){
    unitTo <- as.list(rep("intl",length=length(nameVars)))
    names(unitTo) <- nameVars
  }
  
  # Apply polynomial transformation
  for(idxVar in nameVars) {
    dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=data[[idxVar]],coefPoly=coefPoly[[idxVar]])
  }

  # If we aren't using unit character strings, we are done
  if((length(unitFrom) == 1) && (unitFrom == "arb")) {
    rpt <- list(dataConv=dataConv,coefPoly=coefPoly,unitFrom=unitFrom,unitTo=unitTo)
    return(rpt)
  }
  
  # Apply unit conversion using character strings
  for(idxVar in nameVars) {
    
    # Intpret "From" units
    if(unitFrom[[idxVar]] == "intl"){
      # Using the internal unit base. Find internal units corresponding to "To" units
      unitFrom[[idxVar]] <- eddy4R.base::def.find.unit.intl(unitTo[[idxVar]])
    }
    infoUnitFrom <- eddy4R.base::def.intp.unit(unitFrom[[idxVar]])
    # Check to make sure they were interpreted correctly
    if(sum(is.na(infoUnitFrom$posBase)) > 0) {
      warning(paste("Cannot interpret unitFrom for variable:",idxVar,
                    "\n Output data for this variable will be NA.",
                    "Check unit terms or use polynomial scaling coefficients instead."))
      dataConv[[idxVar]][] <- NA
      next
    }

    # Intpret To" units
    if(unitTo[[idxVar]] == "intl"){
      # Using the internal unit base. Find internal units corresponding to "From" units
      unitTo[[idxVar]] <- eddy4R.base::def.find.unit.intl(unitFrom[[idxVar]])
    }
    infoUnitTo <- eddy4R.base::def.intp.unit(unitTo[[idxVar]])
    # Check to make sure they were interpreted correctly
    if(sum(is.na(infoUnitTo$posBase)) > 0) {
      warning(paste("Cannot interpret unitTo for variable:",idxVar,
                    "\n Output data for this variable will be NA.",
                    "Check unit terms or use polynomial scaling coefficients instead."))
      dataConv[[idxVar]][] <- NA
      next
    }
    
    # Do some checking to make sure we are following the rules:
    
    # Check to make sure the "From" and "To" units have the same number of base unit terms
    if (length(infoUnitTo$posBase) != length(infoUnitFrom$posBase)) {
      warning(paste("Number of terms in unitTo and unitFrom must be the same. Unit conversion 
                    for variable",idxVar, "not possible. Output data for this variable will be NA.
                     Check unit terms or use polynomial scaling coefficients instead."))
      dataConv[[idxVar]][] <- NA
      next
    }
    
    # Check suffixes are the same between corresponding input & output units
    if(sum(infoUnitFrom$sufx != infoUnitTo$sufx) > 0 || is.na(infoUnitFrom$sufx != infoUnitTo$sufx)) {
       warning(paste("Unit suffixes for corresponding terms in unitTo and unitFrom must be the same.", 
                    "Unit conversion for variable:",idxVar, "not possible. Output data for this",
                    "variable will be NA.",
                    "Check unit terms or use polynomial scaling coefficients instead."))
      dataConv[[idxVar]][] <- NA
      next
      
    }
    
  # Go through each base term, applying conversion along the way
    for (idxBase in 1:length(infoUnitFrom$posBase)) {

      # Convert "from" unit prefix to no-prefix 
      if(!is.na(infoUnitFrom$posPrfx[idxBase])) {
        coefPolyPrfxFrom <- eddy4R.base::Conv[[paste0(names(eddy4R.base::Unit$Prfx[infoUnitFrom$posPrfx[idxBase]]),"None")]]
        dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=coefPolyPrfxFrom) # Convert data using polynomial function
      }
      
      # Do we have a chemical species? If so, we will likely need the molar mass, so do it here
      if(!is.na(infoUnitFrom$posSpcs[idxBase])) {
        # Get molar mass of "From" species
        if (nchar(eddy4R.base::Unit$Spcs[infoUnitFrom$posSpcs[idxBase]]) > 1) {
          spcsSplt <- strsplit(eddy4R.base::Unit$Spcs[infoUnitFrom$posSpcs[idxBase]],"")[[1]]
          nameSpcs <- paste0(toupper(spcsSplt[1]),paste0(tolower(spcsSplt[2:nchar(eddy4R.base::Unit$Spcs[infoUnitFrom$posSpcs[idxBase]])]),collapse=""))
          molmSpcsFrom <- Natu[[paste0("Molm",nameSpcs)]]*1000; # molar mass [g/mol]
        } 
        else {
          molmSpcsFrom <- Natu[[paste0("Molm",toupper(eddy4R.base::Unit$Spcs[infoUnitFrom$posSpcs[idxBase]]))]]*1000 # molar mass [g/mol]
        }
      }
      if(!is.na(infoUnitTo$posSpcs[idxBase])) {
        # Get molar mass of "To" species
        if (nchar(eddy4R.base::Unit$Spcs[infoUnitTo$posSpcs[idxBase]]) > 1) {
          spcsSplt <- strsplit(eddy4R.base::Unit$Spcs[infoUnitTo$posSpcs[idxBase]],"")[[1]]
          nameSpcs <- paste0(toupper(spcsSplt[1]),paste0(tolower(spcsSplt[2:nchar(
            eddy4R.base::Unit$Spcs[infoUnitTo$posSpcs[idxBase]])]),collapse=""))
          molmSpcsTo <- Natu[[paste0("Molm",nameSpcs)]]*1000 # molar mass [g/mol]
        } 
        else {
          molmSpcsTo <- Natu[[paste0("Molm",toupper(eddy4R.base::Unit$Spcs[infoUnitTo$posSpcs[idxBase]]))]]*1000 # molar mass [g/mol]
        }
      }
      
      
      # Base unit conversion necessary only if base units differ (an exception below)
      if(infoUnitFrom$posBase[idxBase] != infoUnitTo$posBase[idxBase]) {
      
        # Check to make sure the corresponding "To" unit is of the same type
        if(infoUnitFrom$type[idxBase] != infoUnitTo$type[idxBase]){
          
          # Check if we are converting between mass and mole. If so, need chemical species for both
          if((infoUnitFrom$type[idxBase] == "Mass" && infoUnitTo$type[idxBase] == "Num") || 
            (infoUnitFrom$type[idxBase] == "Num" && infoUnitTo$type[idxBase] == "Mass")) {
            
            if(is.na(infoUnitFrom$posSpcs[idxBase]) || is.na(infoUnitTo$posSpcs[idxBase])) {
              warning(paste("Cannot interpret chemical species needed for conversion between mass and", 
                      "molar units for variable",idxVar,". Output data for this variable will be NA.",
                      "Check unit terms or use polynomial scaling coefficients instead."))
              dataConv[[idxVar]][] <- NA
              break
            } else {
              
              # Convert between mass & molar units

              if (infoUnitFrom$type[idxBase] == "Mass") {
                # We are going from mass to mole
                
                # If we have an input unit other than g, convert to g first
                if(eddy4R.base::Unit$Base$Symb[[infoUnitFrom$posBase[idxBase]]] != "g"){
                  
                  nameBaseFrom <- names(eddy4R.base::Unit$Base$Symb[infoUnitFrom$posBase[idxBase]])
                  coefPolyBase <- eddy4R.base::Conv[[paste0(nameBaseFrom,"Gram")]]
                  dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=coefPolyBase) # Convert data using polynomial function
                  
                }
                
                # Now do g to mol conversion
                dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,molmSpcsFrom^(-infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                
              } 
              else {
                # We are going from mol to mass
                
                # Convert to g
                dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,molmSpcsTo^(infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                
                # If we are outputting to a base unit other than g, convert that here
                if(eddy4R.base::Unit$Base$Symb[[infoUnitTo$posBase[idxBase]]] != "g"){
                  
                  nameBaseTo <- names(eddy4R.base::Unit$Base$Symb[infoUnitTo$posBase[idxBase]])
                  coefPolyBase <- eddy4R.base::Conv[[paste0("Gram",nameBaseTo)]]
                  dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,coefPolyBase[2]^(infoUnitTo$sufx[idxBase]))) # Convert data using polynomial function
                  
                }
                
              }
              
              next
              
            }
          } 
          else {
            
            # Otherwise, can't do conversion
            warning(paste("Input-output units must be mapped 1-to-1 position-wise, within the 
                    unit strings. Corresponding input-output units for variable ",idxVar,
                    "are not of the same unit type. Output data for this variable will be NA.
                    Check unit terms or use polynomial scaling coefficients instead."))
            dataConv[[idxVar]][] <- NA
            break
          }         
                       
        } 
        else {
          # We have same-type unit conversion between different base units
          
          # Find base unit conversion polynomial
          nameBaseFrom <- names(eddy4R.base::Unit$Base$Symb[infoUnitFrom$posBase[idxBase]])
          nameBaseTo <- names(eddy4R.base::Unit$Base$Symb[infoUnitTo$posBase[idxBase]])
          coefPolyBase <- eddy4R.base::Conv[[paste0(nameBaseFrom,nameBaseTo)]]
          # Make sure we have conversion coefficients that follow the rules
          if(is.null(coefPolyBase)){
            # Can't find conversion
            warning(paste("Conversion between input-output units",
                          paste0("eddy4R.base::Conv$",nameBaseFrom,nameBaseTo),"not found for variable ",idxVar,
                          ". Check unit terms or use polynomial scaling coefficients instead."))
            dataConv[[idxVar]][] <- NA
            break
          } 
          else if (((coefPolyBase[1] != 0) || 
                      ((length(coefPolyBase) > 2) && (sum(abs(coefPolyBase[3:length(coefPolyBase)]),na.rm=TRUE) > 0))) &&
                     ((length(infoUnitFrom$posBase) > 1) || (infoUnitFrom$sufx[idxBase] != 1))) {
            # We broke our rule of only 1 term and no suffix for conversion polynomial <> a1)
            warning(paste("For conversions which have polynomial coefficients other than a1 (",
                          "in this case", paste0("eddy4R.base::Conv$",nameBaseFrom,nameBaseTo),")",
                          "the input/output units must have only 1 base term and no unit suffixes.",
                          "Cannot perform unit conversion for variable ",idxVar,". Output data",
                          "for this variable will be NA.",
                          "Check unit terms or use polynomial scaling coefficients instead."))
            dataConv[[idxVar]][] <- NA
            break
            
          } 
          
          # Great, we have good conversion polynomial. Apply.
          if(infoUnitTo$sufx[idxBase] == 1) {
            dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=coefPolyBase^infoUnitTo$sufx[idxBase]) # Convert data using polynomial function
          } 
          else {
            dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,coefPolyBase[2]^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
          }

          # If we are converting between different mass units of different chemical species
          if (((infoUnitFrom$type[idxBase] == "Mass") && (infoUnitTo$type[idxBase] == "Mass")) && 
              !is.na(infoUnitFrom$posSpcs[idxBase]+infoUnitTo$posSpcs[idxBase]) &&
              (infoUnitFrom$posSpcs[idxBase] != infoUnitTo$posSpcs[idxBase])) {
            
            # Convert different mass units between different chemical species
            # (no need to account for base unit other than gram, since did it already)
            dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,(molmSpcsTo/molmSpcsFrom)^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
            
          }
          
        }
      
      } 
      else if (((infoUnitFrom$type[idxBase] == "Mass") && (infoUnitTo$type[idxBase] == "Mass")) && 
                 !is.na(infoUnitFrom$posSpcs[idxBase]+infoUnitTo$posSpcs[idxBase]) &&
                 (infoUnitFrom$posSpcs[idxBase] != infoUnitTo$posSpcs[idxBase])) {
        # Convert same mass units between different chemical species
        # (no need to account for base unit other than gram, since it will cancel)
        dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,(molmSpcsTo/molmSpcsFrom)^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
        
      }# End base-unit conversion
      
    
      # Convert to output unit prefix 
      if(!is.na(infoUnitTo$posPrfx[idxBase])) {
        coefPolyPrfxTo <- eddy4R.base::Conv[[paste0("None",names(eddy4R.base::Unit$Prfx[infoUnitTo$posPrfx[idxBase]]))]]
        dataConv[[idxVar]] <- eddy4R.base::def.aply.conv.poly(data=dataConv[[idxVar]],coefPoly=c(0,coefPolyPrfxTo[2]^infoUnitTo$sufx[idxBase])) # Convert data using polynomial function
      }
          
      
    } # End loop around unit terms
    
  } # End loop around variables

  
  # Apply 
  rpt <- list(dataConv=dataConv,coefPoly=coefPoly,unitFrom=unitFrom,unitTo=unitTo)
  return(rpt)
  
}

