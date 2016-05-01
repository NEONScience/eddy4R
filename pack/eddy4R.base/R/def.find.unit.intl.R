##############################################################################################
#' @title Find internal unit

#' @author 
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Given an arbitraty input unit string, find the corresponding 
#' internal unit (string) used within the eddy4R family of functions. 

#' @param \code{unit} Required. A single character string providing the compount unit, constructed
#' with the rules specified in eddy4R.base::def.intp.unit.R. \cr
#' #' 
#' @return A single character string specifying the corresponding eddy4R internal unit.

#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16
#' eddy4R.base::def.intp.unit.R
#' 

#' @keywords unit conversion, internal unit base, input units, output units

#' @examples Currently none

#' @seealso \code{\link{def.intp.unit}}, \code{\link{Unit$Intl}}, \code{\link{def.conv.unit}}

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-04-19)
#     original creation 
##############################################################################################

def.find.unit.intl <- function(unit) {
    
  # Parse input unit string
  infoUnit <- eddy4R.base::def.intp.unit(unit)
  if(sum(is.na(infoUnit$posBase)) > 0) {
    warning(paste("Cannot interpret input unit string. Check unit terms"))
    rpt <- NA
    return(rpt)
  }
  
  
  # We have to know what compound units represent, let's see if it matches predefined special cases
  flagIntl <- 0 # Flag for whether the compound unit was recognized as a special case
  
  # Mass Density
  chkType <- pmatch(infoUnit$type,c("Mass","Dist"))
  if((length(chkType) == 2) && !is.na(sum(chkType)) && (infoUnit$sufx[which(chkType == 1)] == 1) &&
     (infoUnit$sufx[which(chkType == 2)] == -3)) {
    
    # Check to make sure we have a gas species. Internal density is always in number density, and
    # we need a single chemical species to convert from mass to number density
    if (is.na(infoUnit$spcs[which(chkType == 1)])) {
      warning(paste("Unable to convert",unit,"to internal units. Internal units always use",
                    "number density. In order to convert mass density to number density",
                    "a chemical species must be attached to the mass unit in the unit string.",
                    "Check unit terms."))
      rpt <- "NA"
      return(rpt)
    }
    
    # We have the info we need to convert mass density to number density, 
    # let's create the output unit string
    unitNum <- paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$spcs[which(chkType == 1)],sep="") # mol portion
    unitDist <- paste0(eddy4R.base::Unit$Intl[["Dist"]],"-3",sep="") # dist portion
    unitIntl <- paste(c(unitNum,unitDist)[chkType],collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.intp.unit(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass density
  
  # Mass Flux Density
  chkType <- pmatch(infoUnit$type,c("Mass","Dist","Time"))
  if((length(chkType) == 3) && !is.na(sum(chkType)) && (infoUnit$sufx[which(chkType == 1)] == 1) &&
     (infoUnit$sufx[which(chkType == 2)] == -2) && (infoUnit$sufx[which(chkType == 3)] == -1)) {
    
    # Check to make sure we have a gas species. Internal flux is always in molar units, and
    # we need a single chemical species to convert from mass to moles
    if (is.na(infoUnit$spcs[which(chkType == 1)])) {
      warning(paste("Unable to convert",unit,"to internal units. Internal units always specify",
                    "flux density in molar units. In order to convert mass to moles",
                    "a chemical species must be attached to the mass unit in the unit string.",
                    "Check unit terms."))
      rpt <- "NA"
      return(rpt)
    }
    
    # We have the info we need to convert mass flux density to mole flux density, 
    # let's create the output unit string
    unitNum <- paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$spcs[which(chkType == 1)],sep="") # mol portion
    unitDist <- paste0(eddy4R.base::Unit$Intl[["Dist"]],"-2",sep="") # dist portion
    unitTime <- paste0(eddy4R.base::Unit$Intl[["Time"]],"-1",sep="") # time portion
    unitIntl <- paste(c(unitNum,unitDist,unitTime)[chkType],collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.intp.unit(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass flux density
  
  
  # Mass ratio
  chkType <- pmatch(infoUnit$type,c("Mass","Mass"))
  if((length(chkType) == 2) && !is.na(sum(chkType)) && (length(which(infoUnit$sufx == 1)) == 1) &&
     (length(which(infoUnit$sufx == -1)) == 1)) {
    
    # Check to make sure we have gas species. Internal substance ratio is always in molar units, and
    # we need a single chemical species attached to each base unit to convert from mass to moles
    if (is.na(infoUnit$spcs[which(chkType == 1)]) || is.na(infoUnit$spcs[which(chkType == 2)])) {
      warning(paste("Unable to convert",unit,"to internal units. Internal units always specify",
                    "chemical species ratios in molar units. In order to convert mass to moles",
                    "a chemical species must be attached to each mass unit. Check unit terms."))
      rpt <- "NA"
      return(rpt)
    }
    
    # We have the info we need to convert mass ratio to mole ratio, 
    # let's create the output unit string
    unitNum01 <- paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$sufx[1],infoUnit$spcs[1],sep="") # mol portion 1
    unitNum02 <- paste0(eddy4R.base::Unit$Intl[["Num"]],infoUnit$sufx[2],infoUnit$spcs[2],sep="") # mol portion 2
    unitIntl <- paste(c(unitNum01,unitNum02),collapse=" ") # full output unit string
    infoUnitIntl <- eddy4R.base::def.intp.unit(unitIntl) # Interpret unit
    flagIntl <- 1
    
  } # End check on mass ratio
  
  # If we don't have any special cases, go through each base unit, converting to internal (SI) unit
  if (flagIntl == 0) {
    infoUnitIntl <- infoUnit # Intialize internal unit info
    unitIntlTmp <- character(length=length(infoUnit$posBase))
    for (idxBase in 1:length(infoUnit$posBase)) {
      infoUnitIntlIdx <- eddy4R.base::def.intp.unit(eddy4R.base::Unit$Intl[[infoUnit$type[idxBase]]])
      
      # Replace the unit base and prefix with the internal unit
      infoUnitIntl$base[idxBase] <- infoUnitIntlIdx$base
      infoUnitIntl$posBase[idxBase] <- infoUnitIntlIdx$posBase
      infoUnitIntl$prfx[idxBase] <- infoUnitIntlIdx$prfx
      infoUnitIntl$posPrfx[idxBase] <- infoUnitIntlIdx$posPrfx
      
      # Create unit character string
      unitIntlTmp[idxBase] <- paste0(infoUnitIntl$prfx[idxBase],infoUnitIntl$base[idxBase],
                                   infoUnitIntl$sufx[idxBase],infoUnitIntl$spcs[idxBase],sep="")
      unitIntlTmp[idxBase] <- gsub("NA","",unitIntlTmp[idxBase]) # remove NA values
    }
    # Create compound unitTo string
    unitIntl <- paste(unitIntlTmp,collapse=" ") # full output unit string
  }
  
  return(unitIntl)
  
} # End function
