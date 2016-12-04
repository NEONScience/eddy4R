##############################################################################################
#' @title Find internal unit

#' @author 
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Given an arbitraty input unit string, find the corresponding 
#' internal unit (string) used within the eddy4R family of functions. 

#' @param \code{unit} Required. A single character string providing the compount unit, constructed
#' with the rules specified in eddy4R.base::def.unit.info.R. \cr
#' #' 
#' @return A single character string specifying the corresponding eddy4R internal unit.

#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16
#' eddy4R.base::def.unit.info.R
#' 

#' @keywords unit conversion, internal unit base, input units, output units

#' @examples Currently none

#' @seealso \code{\link{def.unit.info}}, \code{\link{Unit$Intl}}, \code{\link{def.unit.conv}}

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-04-19)
#     original creation 
#   Cove Sturtevant (2016-04-29)
#     update all function calls to use double-colon operator
#   Natchaya P-Durden
#     rename function to def.unit.intl()
##############################################################################################

def.unit.intl <- function(unit) {
    
  # Parse input unit string
  infoUnit <- eddy4R.base::def.unit.info(unit)
  if(base::sum(base::is.na(infoUnit$posBase)) > 0) {
    base::warning(paste("Cannot interpret input unit string. Check unit terms"))
    rpt <- NA
    base::return(rpt)
  }
  
  
  # We have to know what compound units represent, let's see if it matches predefined special cases
  flagIntl <- 0 # Flag for whether the compound unit was recognized as a special case
  
  # Mass Density
  chkType <- base::pmatch(infoUnit$type,base::c("Mass","Dist"))
  if((base::length(chkType) == 2) && !base::is.na(base::sum(chkType)) && (infoUnit$sufx[base::which(chkType == 1)] == 1) &&
     (infoUnit$sufx[base::which(chkType == 2)] == -3)) {
    
    # Check to make sure we have a gas species. Internal density is always in number density, and
    # we need a single chemical species to convert from mass to number density
    if (base::is.na(infoUnit$spcs[base::which(chkType == 1)])) {
      base::warning(base::paste("Unable to convert",unit,"to internal units. Internal units always use",
                    "number density. In order to convert mass density to number density",
                    "a chemical species must be attached to the mass unit in the unit string.",
                    "Check unit terms."))
      rpt <- "NA"
      base::return(rpt)
    }
    
    # We have the info we need to convert mass density to number density, 
    # let's create the output unit string
    unitNum <- base::paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$spcs[base::which(chkType == 1)],sep="") # mol portion
    unitDist <- base::paste0(eddy4R.base::Unit$Intl[["Dist"]],"-3",sep="") # dist portion
    unitIntl <- base::paste(base::c(unitNum,unitDist)[chkType],collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.unit.info(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass density
  
  # Mass Flux Density
  chkType <- base::pmatch(infoUnit$type,base::c("Mass","Dist","Time"))
  if((base::length(chkType) == 3) && !base::is.na(sum(chkType)) && (infoUnit$sufx[base::which(chkType == 1)] == 1) &&
     (infoUnit$sufx[base::which(chkType == 2)] == -2) && (infoUnit$sufx[base::which(chkType == 3)] == -1)) {
    
    # Check to make sure we have a gas species. Internal flux is always in molar units, and
    # we need a single chemical species to convert from mass to moles
    if (base::is.na(infoUnit$spcs[base::which(chkType == 1)])) {
      base::warning(base::paste("Unable to convert",unit,"to internal units. Internal units always specify",
                    "flux density in molar units. In order to convert mass to moles",
                    "a chemical species must be attached to the mass unit in the unit string.",
                    "Check unit terms."))
      rpt <- "NA"
      base::return(rpt)
    }
    
    # We have the info we need to convert mass flux density to mole flux density, 
    # let's create the output unit string
    unitNum <- base::paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$spcs[base::which(chkType == 1)],sep="") # mol portion
    unitDist <- base::paste0(eddy4R.base::Unit$Intl[["Dist"]],"-2",sep="") # dist portion
    unitTime <- base::paste0(eddy4R.base::Unit$Intl[["Time"]],"-1",sep="") # time portion
    unitIntl <- base::paste(base::c(unitNum,unitDist,unitTime)[chkType],collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.unit.info(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass flux density
  
  
  # Mass ratio
  chkType <- base::pmatch(infoUnit$type,c("Mass","Mass"))
  if((base::length(chkType) == 2) && !base::is.na(sum(chkType)) && (base::length(base::which(infoUnit$sufx == 1)) == 1) &&
     (base::length(base::which(infoUnit$sufx == -1)) == 1)) {
    
    # Check to make sure we have gas species. Internal substance ratio is always in molar units, and
    # we need a single chemical species attached to each base unit to convert from mass to moles
    if (base::is.na(infoUnit$spcs[base::which(chkType == 1)]) || base::is.na(infoUnit$spcs[base::which(chkType == 2)])) {
      base::warning(base::paste("Unable to convert",unit,"to internal units. Internal units always specify",
                    "chemical species ratios in molar units. In order to convert mass to moles",
                    "a chemical species must be attached to each mass unit. Check unit terms."))
      rpt <- "NA"
      base::return(rpt)
    }
    
    # We have the info we need to convert mass ratio to mole ratio, 
    # let's create the output unit string
    unitNum01 <- base::paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$sufx[1],infoUnit$spcs[1],sep="") # mol portion 1
    unitNum02 <- base::paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$sufx[2],infoUnit$spcs[2],sep="") # mol portion 2
    unitIntl <- base::paste(base::c(unitNum01,unitNum02),collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.unit.info(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass ratio
  
  # If we don't have any special cases, go through each base unit, converting to internal (SI) unit
  if (flagIntl == 0) {
    infoUnitIntl <- infoUnit # Intialize internal unit info
    unitIntlTmp <- base::character(length=length(infoUnit$posBase))
    for (idxBase in 1:base::length(infoUnit$posBase)) {
      infoUnitIntlIdx <- eddy4R.base::def.unit.info(eddy4R.base::Unit$Intl[[infoUnit$type[idxBase]]])
      
      # Replace the unit base and prefix with the internal unit
      infoUnitIntl$base[idxBase] <- infoUnitIntlIdx$base
      infoUnitIntl$posBase[idxBase] <- infoUnitIntlIdx$posBase
      infoUnitIntl$prfx[idxBase] <- infoUnitIntlIdx$prfx
      infoUnitIntl$posPrfx[idxBase] <- infoUnitIntlIdx$posPrfx
      
      # Create unit character string
      if (infoUnitIntl$sufx[idxBase] == 1) {
        # If unit suffix is equal to 1, don't need to print in unit string
        unitIntlTmp[idxBase] <- base::paste0(infoUnitIntl$prfx[idxBase],infoUnitIntl$base[idxBase],
                                       infoUnitIntl$spcs[idxBase],sep="")
      } 
      else {
        unitIntlTmp[idxBase] <- base::paste0(infoUnitIntl$prfx[idxBase],infoUnitIntl$base[idxBase],
                                       infoUnitIntl$sufx[idxBase],infoUnitIntl$spcs[idxBase],sep="")
      }
      unitIntlTmp[idxBase] <- base::gsub("NA","",unitIntlTmp[idxBase]) # remove NA values
    }
    # Create compound unitTo string
    unitIntl <- base::paste(unitIntlTmp,collapse=" ") # full output unit string
  }
  
  base::return(unitIntl)
  
} # End function
