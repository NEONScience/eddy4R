##############################################################################################
#' @title Interpret unit string

#' @author 
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Interpret a compound unit string, identifying the matching entries
#' within eddy4R internal data \code{eddy4R.base::Unit} for the base unit symbol, unit prefix, chemical   
#' species, and unit suffix (raise to the power of)

#' @param \code{unit} Required. A single character string providing the compount unit, constructed
#' with the following rules. \cr
#' \cr
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
#' @return A named list of vectors, each of length N, where N is the number of base units 
#' making up the compound unit. For example, umolCO2 m-2 s-1 has 3 base units (mol, m, s): \cr
#' type: the abbreviated character string denoting unit type (e.g. "dist" for distance)
#' base: the base unit character symbol as listed in eddy4R.base::Unit$Base$Symb
#' posBase: the integer list position in eddy4R.base::Unit$Base$Symb corresponding to the base unit
#' prfx: the unit prefix character(s) as listed in eddy4R.base::Unit$Prfx
#' posPrfx: the integer list position in eddy4R.base::Unit$Prfx corresponding to the unit prefix
#' sufx: the numerical unit suffix (i.e. the power to which the unit is raised)
#' spcs: a character string of the chemical species as listed in eddy4R.base::Unit$Spcs
#' posSpcs: the integer list position in eddy4R.base::Unit$Spcs corresponding to the chemical species

#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords unit conversion, input units, output units

#' @examples Currently none

#' @seealso \code{\link{def.conv.unit}}, \code{\link{def.unit.intl}}, \code{\link{Unit$Intl}}

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-04-08)
#     original creation 
#   Cove Sturtevant (2016-04-20)
#     first full working & documented version
#   Cove Sturtevant (2016-04-29)
#     update all function calls to use double-colon operator
#   Natchaya P-Durden (2016-11-27)
#     rename function to def.unit.info()
##############################################################################################

def.unit.info <- function(unit) {

  # Check input
  if(!base::is.character(unit)) {
    base::stop("Input unit must be of type character",call. = FALSE)
  }
  if(base::length(unit) != 1) {
    base::stop("Input unit must be a character array of length 1",call. = FALSE)
  }
  
  # Parse the unit string into component units (separated by spaces)
  unitSplt <- base::strsplit(unit," ")[[1]]
  numBase <- base::length(unitSplt) # number of base units
  
  # Initialize output
  rpt <- base::list(
    type = rep(NA,times=numBase),
    base = rep(NA,times=numBase),
    posBase = rep(NA,times=numBase),
    prfx = rep(NA,times=numBase),
    posPrfx = rep(NA,times=numBase),
    sufx = rep(1,times=numBase),
    spcs = rep(NA,times=numBase),
    posSpcs = rep(NA,times=numBase)
  )
  
  # Parse the unit base symbol, prefix, suffix, and chemical species (if applicable)
  for(idxSplt in 1:numBase) {
    
    # Get number of characters in this unit string
    numCharSplt <- base::nchar(unitSplt[idxSplt])
    
    # See which base unit symbols have a match
    posUnitMtch <- base::which(base::as.data.frame(base::lapply(eddy4R.base::Unit$Base$Symb,grepl,x=unitSplt[idxSplt],
                                              ignore.case=FALSE)) == TRUE)
    
    # For each base unit, which positions within the split character string match?
    posChar <- base::lapply(eddy4R.base::Unit$Base$Symb,function(val) {base::gregexpr(pattern=val,text=unitSplt[idxSplt],
                                                        ignore.case=FALSE,fixed=TRUE)[[1]]})
    
    # Check to see if any possibilities exist. If not, move on
    if(base::length(posUnitMtch) == 0) {next}
    
    # For each potential base unit match, assign the base unit symbol, prefix, and suffix to see if 
    # they make sense
    for(idxMtch in posUnitMtch) {
      
      # (Re)initialize the chosen components
      type <- NA # Initialize unit type string
      base <- NA # Initialize base unit string
      posBase <- NA # Initialize base unit (position within eddy4R.base::Unit$Base$Symb list)
      prfx <- NA  # Initialize unit prefix string
      posPrfx <- NA # Initialize unit prefix (position within eddy4R.base::Unit$Prfx list)
      sufx <- 1 # Initialize unit suffix 
      spcs <- NA # Initialize chemical species string
      posSpcs <- NA # Initialize chemical species (position within eddy4R.base::Unit$Spcs list)        
      
      # Run through each potential starting position of a base unit match in the unit character string
      for(idxPosChar in 1:base::length(posChar[[idxMtch]])) {
        
        # Positions of the base unit symbol within the unit character string
        posCharBase <- posChar[[idxMtch]][idxPosChar]:
          (posChar[[idxMtch]][idxPosChar]+base::nchar(eddy4R.base::Unit$Base$Symb[[idxMtch]])-1) 
        
        # If the base unit is the entire character string, we're done
        if (base::length(posCharBase) == numCharSplt) {
          posBase <- idxMtch
          base <- eddy4R.base::Unit$Base$Symb[[idxMtch]]
          type <- eddy4R.base::Unit$Base$Type[[base::names(eddy4R.base::Unit$Base$Symb[posBase])]]
          break
        }
        
        # Positions of the unit prefix within the unit character string
        if(posCharBase[1] != 1) {
          
          posCharPrfx <- 1:(posCharBase[1]-1)
          
          # Do we have a match for the prefix?
          posPrfx <- base::which(base::as.data.frame(base::lapply(eddy4R.base::Unit$Prfx,function(val){
            val==base::paste0(base::strsplit(unitSplt[idxSplt],"")[[1]][posCharPrfx],collapse="")})) == TRUE)
          if(base::length(posPrfx) == 0) {
            posPrfx <- NA
            next
          } else {
            prfx <- eddy4R.base::Unit$Prfx[[posPrfx]]
          }
        }
        
        # Positions of the  unit suffix within the unit character string
        exstNum <- TRUE # initialize the existence of numbers in the suffix
        idxCharSufx <- base::max(posCharBase)+1 # Start with character following base unit symbol
        posCharSufx <- base::numeric(0) # intialize
        while((exstNum == TRUE) & (idxCharSufx <= numCharSplt)) {
          
          # Check if the next character is a number
          if(!base::is.na(base::pmatch(base::strsplit(unitSplt[idxSplt],"")[[1]][idxCharSufx],
                                       base::c("0","1","2","3","4","5","6","7","8","9","-",".")))) {
            
            # We have part of a numeric sequence, add it to the suffix
            posCharSufx <- base::c(posCharSufx,idxCharSufx) # Add this character to the suffix
            idxCharSufx <- idxCharSufx+1
            
          } else {
            exstNum <- FALSE
          }
          
        }
        # If we have marked any suffix characters, see if they make an actual number
        if (base::length(posCharSufx) > 0) {
          sufx = tryCatch({
            base::as.numeric(base::paste0(base::strsplit(unitSplt[idxSplt],"")[[1]][posCharSufx],collapse=""))
          }, warning = function(var){
            NA
          }, error = function(var){
            NA
          })
          # If our "number" is uninterpretable, move on
          if(base::is.na(sufx)) {next}
        }
        
        
        # Is there a chemical species indicated?
        if (idxCharSufx <= numCharSplt) {
          # Positions of the the chemical species within the unit character string
          posCharSpcs <- idxCharSufx:numCharSplt
          
          # Find the chemical species within eddy4R.base::Unit$Spcs
          posSpcs <- base::pmatch(base::tolower(base::paste0(base::strsplit(unitSplt[idxSplt],"")[[1]][posCharSpcs],
                                  collapse="")),base::tolower(eddy4R.base::Unit$Spcs))
          
          # If we have no match or more than one match, move on
          if (base::is.na(posSpcs)) {
            next
          } else {
            spcs <- eddy4R.base::Unit$Spcs[[posSpcs]]
          }
          
        }
        
        
        # If we got this far, we have a fully parsed and recognizable unit string
        posBase <- idxMtch
        base <- eddy4R.base::Unit$Base$Symb[[idxMtch]]
        type <- eddy4R.base::Unit$Base$Type[[base::names(eddy4R.base::Unit$Base$Symb[posBase])]]
        break
        
        
      }
      
      # If we were able to recognize the unit string, break out of the search
      if (!base::is.na(posBase)) {
        
        # Assign output
        rpt$type[idxSplt] <- type
        rpt$base[idxSplt] <- base
        rpt$posBase[idxSplt] <- posBase
        rpt$prfx[idxSplt] <- prfx
        rpt$posPrfx[idxSplt] <- posPrfx
        rpt$sufx[idxSplt] <- sufx
        rpt$spcs[idxSplt] <- spcs
        rpt$posSpcs[idxSplt] <- posSpcs
        
        break

      }
      
    }
    
  }
  
  base::return(rpt)
}
