##############################################################################################
#' @title Definition function: Compare output against reference

#' @author
#' David Durden \email{ddurden@battelleecology.org} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description
#' Function definition. A function to compare the file output from a workflow to a reference output file
#' to ensure that changes during development to functions called by the workflow have not impacted the
#' results.\cr
#' The function reads in the new data produced in the tmp directory and compares the first 10 lines to the reference output data.

#' @param fileOut String. The relative or absolute path and filename of the numerical data output.
#' @param fileRefe String. The relative or absolute path and filename of the numerical reference data.
#' @param Head Logical. TRUE if there is 1 header row in both the output and reference files. Defaults to FALSE.
#' @param NumLine The number of output rows to compare (starting from the first row). Defaults to 10.

#' @return Currently none

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr


#' @keywords function test, gold file

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-07-11)
#     original creation
#   Cove Sturtevant (2016-07-14)
#     turned original workflow into a general function to be called at the end of each test script
#   Natchaya P-Durden (2016-11-26)
#     rename to def.mtch.out.refe()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.mtch.out.refe <- function(
  fileOut,
  fileRefe,
  Head = FALSE,
  NumLine = 10) {

  log <- eddy4R.log::def.log.init()
  log$debug("in def.mtch.out.refe.R")

  # Read in the output file
  dataOut <- utils::read.csv(fileOut)

  # Read in the reference file
  dataRefe <- utils::read.csv(fileRefe)

  #Compare the first numLine lines of the data between the output and reference
  if(!isTRUE(base::all.equal(dataOut[1:NumLine,],dataRefe[1:NumLine,]))){
      base::stop("Bummer! The current output DOES NOT MATCH the reference output :(")
  } else {
    log$debug("Yay! The current output MATCHES the reference output :)")
  }

}
