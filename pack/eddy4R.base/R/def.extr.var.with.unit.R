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

def.extr.var.with.unit <- function(data,nameVar) {
  
  # Grab the variable
  var <- data[[nameVar]]

  # Get position of variable within data frame in case unit attribute is not names
  posVar <- which(names(data) == nameVar)
  
  # Assign the attributes
  if (nameVar %in% names(attr(data,"unit"))) {
    # If unit attribute is named
    attr(var,"unit") <- attr(data,"unit")[[nameVar]]
  } else {
    # Otherwise use same position as variable within data frame
    attr(var,"unit") <- attr(data,"unit")[posVar]
  }
    
  # Assign output
  return(var)
}