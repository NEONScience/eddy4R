##############################################################################################
#' @title Definition function: Copy metadata from HDF5 input file to another HDF5 output file

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function recreates the metadata from an HDF5 file to another HDF5 file with the same group heirarchy structure.

#' @param \code{FileIn} is the input file where the parameters are being read from.
#' @param \code{FileOut} is the output file where the parameters are being written to.


#' @return A NEON formatted HDF5 file that has parameters from the input file written to the output HDF5 file. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823)
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 


#'#Set directory for example output to working directory
#'DirOut <- getwd()
#'
#'fileOut <- "ECTE_dp01_SERC_2016-04-24_new_format.h5"

#'#Running example
#'def.para.hdf5.dp01(fileIn = , fileOut = )

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-02-12)
#     original creation

##############################################################################################################
#Start of function call to read metadata from one file and write to another
##############################################################################################################

def.para.hdf5.dp01 <- function(
  FileIn,
  FileOut
){

  #list of everything written within the input file
listPara <- h5ls(FileIn, datasetinfo = FALSE)

#Grabbing just the HDF5 groups
listPara <- listPara[listPara$otype == "H5I_GROUP",]
listGrup <- paste(listPara$group, listPara$name, sep = "/") # Combining output


# read attributes from input file
listAttr <- lapply(listGrup, h5readAttributes, file = FileIn)

#Apply group names to the attributes list
names(listAttr) <- listGrup

#Remove all empty lists
listAttr <- listAttr[!sapply(listAttr, function(x) length(x) == 0)]


#Open the output file HDF5 link
fid <- H5Fopen()
  
#Write the attributes to the new file
lapply(names(listAttr), function(x){
  print(x)
  gid <- H5Gopen(fid, x)
 lapply(names(listAttr[[x]]), function(y){
   print(y)
   h5writeAttribute(attr = listAttr[[x]][[y]], h5obj = gid, name = y)})
})

}
