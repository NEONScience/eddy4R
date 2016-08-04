##############################################################################################
#' @title Global R-environment settings for use with the the eddy4R family of R-packages

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. To avoid user-location-specific dependencies, the system locale is set to the C language standard. For R in Windows the 'times' font is defined. 

#' @param Currently none

#' @return
#' Sys.setlocale('LC_ALL','C') \cr
#' if(.Platform$OS.type == "windows") windowsFonts(times=windowsFont("TT Times New Roman"))

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
##############################################################################################

def.env.glob <- function() {

# set system local to avoid errors related to special characters  
Sys.setlocale('LC_ALL','C')

# define 'times' language in Windows OS
if(.Platform$OS.type == "windows") windowsFonts(times=windowsFont("TT Times New Roman"))  
# define 'times' language in unix OS
if(.Platform$OS.type == "unix") quartzFonts(times = quartzFont(rep("Times-Roman", 4)))
  #c(“Avenir Book”, “Avenir Black”, “Avenir Book Oblique”, “Avenir Black Oblique”))
  #quartzFonts(sans = quartzFont(rep("AppleGothic", 4)))

}
