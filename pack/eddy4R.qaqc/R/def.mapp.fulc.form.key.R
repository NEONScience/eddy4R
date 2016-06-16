##############################################################################################
#' @title Map data names to internal keys in Fulcrum form schema

#' @author 
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description Function definition. Parses Fulcrum form schema to map all data names to internal keys 

#' @param \code{schmForm} Required. The Fulcrum form schema as extracted from the Fulcrum API GET request (using the use httr::content function) 
#'  
#' @return A data frame of \code{name} and \code{key} mappings. \cr
#' 
#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords Fulcrum

#' @examples 
#' urlApiForm <- paste0("https://api.fulcrumapp.com/api/v2/forms/",idForm,".json") # URL for specific form id, where idForm is a string with your form id
#' rspnForm <- httr::GET(urlApiForm,add_headers("X-ApiToken" = keyApi, Accept = "application/json")) # where keyApi is a string with your API key
#' schmForm <- httr::content(rspnForm) # The overall form schema
#' fldMapp <- def.mapp.fulc.form.key(schmForm) # Grab the mapping between the field names and keys

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-06-16)
#     original creation 
#     
##############################################################################################

def.mapp.fulc.form.key <- function(schmForm){
  
  # Initialize output
  key <- character(length=0)
  name = character(length=0)
  
  # Grab the form "elements", consisting of nested list of characteristics incl. key & data name
  elmtForm <- schmForm$form$elements 
  numElmt <- length(elmtForm)
  
  # If there is at least one element, set and set current level 
  if(numElmt > 0){
    flag <- 1  # Set flag to remain within schema 
    levl <- 1 # Set current level to 1
    idxElmt <- 1 # At each level, which element index are we in? column dimension corresponds to level
    idxOut <- 1 # initialize output index
  }
  
  while(flag == 1) {
    
    # Get to current level
    elmtCurr <- elmtForm
    if(levl > 1) {
      for (idxLevl in 1:(levl-1)) {
        elmtCurr <- elmtCurr[[idxElmt[idxLevl]]]$elements
      }
    } 
    if (is.null(names(elmtCurr))) {
      if ((idxElmt[levl]+1) <= length(elmtCurr)) {
        # Get element
        elmtCurr <- elmtCurr[[idxElmt[levl]]]
      } else {
        # Back out one level
        if(levl == 1) {
          flag <- 0 # stop
          next
        } else {
          levl <- levl - 1
          idxElmt[levl] <- idxElmt[levl]+1
          next
        }
      }
    }
    
    # Are there name-key pairs here?
    if ("key" %in% names(elmtCurr)) {
      key[idxOut] <- elmtCurr$key
      name[idxOut] <- elmtCurr$data_name
      idxOut <- idxOut+1
    }
    
    # If there are nested elements here, go a level further, 
    # If not, go to next element at current level
    # If at end of elements at current level, back out one level
    if ("elements" %in% names(elmtCurr)) {
      # Go a level deeper
      levl <- levl + 1
      idxElmt[levl] <- 1
    } else {
      # Get to current level
      elmtCurr <- elmtForm
      if(levl > 1) {
        for (idxLevl in 1:(levl-1)) {
          elmtCurr <- elmtCurr[[idxElmt[idxLevl]]]$elements
        }
      } 
      if (base::is.null(base::names(elmtCurr))) {
        
        if ((idxElmt[levl]+1) <= length(elmtCurr)) {
          # go to next element at current level
          idxElmt[levl] <- idxElmt[levl]+1
        } else {
          # Back out one level
          if(levl == 1) {
            flag <- 0 # stop 
          } else {
            levl <- levl - 1
            idxElmt[levl] <- idxElmt[levl]+1
          }
        }
      }
    }
  }

rpt <- data.frame(name=name,key=key)

}