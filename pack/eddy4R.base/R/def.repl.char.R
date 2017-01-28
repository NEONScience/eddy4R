##############################################################################################
#' @title Definition function: Replace character vector elements with entries from a lookup table

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. If any of the values in \code{ReplFrom} exist in \code{data}, these values are replaced with the corresponding values in \code{ReplTo}. If none of the values in \code{ReplFrom} exist in \code{data}, then \code{data} is returned unchanged.

#' @param \code{data} Named character vector, can contain values that are to be replaced as specified via \code{ReplFrom} and \code{ReplTo}.
#' @param \code{ReplFrom} Character vector of the same length as \code{ReplTo}, specifying the values in \code{data} that are to be replaced with the corrresponding values in \code{ReplTo}.
#' @param \code{ReplTo} Character vector of the same length as \code{ReplFrom}, specifying the replacement values for \code{ReplFrom} in \code{data}.

#' @return 
#' Named character vector of the same length as \code{data}, with values specified in \code{ReplFrom} replaced by the corresponding values in \code{ReplTo}. If none of the entries in \code{ReplFrom} exist in \code{data}, then \code{data} is returned unchanged.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords replacement, character, lookup table

#' @examples
#' def.repl.char(
#' data = structure(.Data = c("Celsius", "Pa", "percent"), names = c("temperature", "pressure", "humidity")),
#' ReplFrom = c("Celsius", "percent"),
#' ReplTo = c("C", "%")
#' )

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-08-07)
#     original creation
##############################################################################################


# kind of klunky but works for now
# idea: turn around arguments of which() or use match()
def.repl.char <- function(
  data,
  ReplFrom,
  ReplTo
) {
  
  # replacement values from lookup table
  
  # index of lookup table elements that correspond with elements in data
  tmp01 <- unlist(sapply(data, function(x) which(ReplFrom == x)))
  
  # lookup table "from" entries that correspond with elements in data
  tmp02 <- ReplTo[tmp01]; names(tmp02) <- names(tmp01)
  
  # indices of data for replacement values
  tmp03 <- unlist(sapply(names(tmp02), function(x) which(names(data) == x)))
  
  # actual replacement
  rpt <- unlist(replace(x = data, list = tmp03, values = tmp02))
  
  #return result
  return(rpt)
  
}
