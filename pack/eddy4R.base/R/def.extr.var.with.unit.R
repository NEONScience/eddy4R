##############################################################################################
#' @title Extract variable with unit attribute from data frame

#' @author 
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description Function definition. Extracts a variable from a data frame (with units attached as attributes
#' at the data frame level), and attaches the variable's unit directly to the extracted variable. 

#' @param \code{data} Required. A data frame with units of each variables attached as a named attribute "unit"
#' to the data frame.
#' @param \code{nameVar} Required. A string containing the name of the variable within \code{data} to extract,
#' along with its units
#' @param \code{AllwPos} Optional. Logical, defaulting to FALSE. Allow positional determination of the unit within
#' the unit attribute vector? If FALSE, an error will result if the variable name does not match an entry with 
#' the named unit vector. If TRUE, the unit attribute vector does not need to be named, and the unit will be 
#' pulled based on the same position of the variable within the data frame \code{data}.
#'  
#' @return The desired variable with units attached as attribute. 
#' 
#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords Fulcrum

#' @examples Currently none

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-09-08)
#     original creation 
#     
##############################################################################################

def.extr.var.with.unit <- function(
  data,
  nameVar,
  AllwPos=FALSE
  ) {
  
  # Grab the variable
  var <- data[[nameVar]]

  # Get position of variable within data frame in case unit attribute is not names
  posVar <- which(names(data) == nameVar)
  
  # Assign the attributes
  if (nameVar %in% names(attr(data,"unit"))) {
    # If unit attribute is named
    attr(var,"unit") <- attr(data,"unit")[[nameVar]]
  } else {
    # If we don't allow positional determination within the unit attribute vector, stop
    if(!AllwPos){
      stop("Cannot find variable in unit attribute list. Make sure unit attribute is a named vector corresponding to the variable names.")
    }
    # Otherwise use same position as variable within data frame
    attr(var,"unit") <- attr(data,"unit")[posVar]
  }
    
  # Assign output
  return(var)
}