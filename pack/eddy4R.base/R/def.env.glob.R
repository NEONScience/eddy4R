##############################################################################################
#' @title Global R-environment settings for use with the eddy4R family of R-packages

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. To avoid user-location-specific dependencies, the system locale is set to the C language standard. For R in Windows and Mac the 'times' font is defined. Under Unix OS, it is not neccesary to assign font "times". 

#' @param Currently none

#' @return
#' Sys.setlocale('LC_ALL','C') \cr
#' if(.Platform$OS.type == "windows") windowsFonts(times=windowsFont("TT Times New Roman"))
#' if(Sys.info()["sysname"] == "Darwin") quartzFonts(times = quartzFont(rep("Times-Roman", 4)))

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' \url{http://www.inside-r.org/r-doc/base/Sys.setlocale}

#' @keywords global environment

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-26)
#     original creation
#   Stefan Metzger (2015-11-29)
#     re-formualtion as function() to allow packaging
#   Ke Xu (2016-08-03)
#     define font "times" for Mac
#   Stefan Metzger (2016-12-18)
#     print POSIX date/timestamps with fractional seconds
##############################################################################################

def.env.glob <- function() {
  
  # set system local to avoid errors related to special characters  
  Sys.setlocale('LC_ALL','C')
  
  # define 'times' language in Windows OS
  if(.Platform$OS.type == "windows") windowsFonts(times=windowsFont("TT Times New Roman"))  
  # define 'times' language in Mac OS
  if(Sys.info()["sysname"] == "Darwin") quartzFonts(times = quartzFont(rep("Times-Roman", 4)))
  
  # print POSIX date/timestamps with fractional seconds
  options(digits.secs=3)
  
}
