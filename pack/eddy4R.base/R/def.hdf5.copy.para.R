##############################################################################################
#' @title Definition function: Copy metadata from HDF5 input file to another HDF5 output file

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function recreates the metadata from an HDF5 file to another HDF5 file with the same group heirarchy structure.

#' @param FileInp is the input file where the parameters are being read from.
#' @param FileOut is the output file where the parameters are being written to.


#' @return A NEON formatted HDF5 file that has parameters from the input file written to the output HDF5 file. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-02-12)
#     original creation
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Natchaya P-Durden (2018-04-12)
#    applied eddy4R term name convention; replaced fid by idFile
#    replaced gid by idData
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced FileIn by FileInp
#    Natchaya P-Durden (2018-05-22)
#     rename function from def.para.hdf5.dp01() to def.hdf5.copy.para()
##############################################################################################################
#Start of function call to read metadata from one file and write to another
##############################################################################################################

def.hdf5.copy.para <- function(
  FileInp,
  FileOut
){

  if(!base::file.exists(FileInp)) {
    stop("Input file does not exist")
  } 
  
  if(!base::file.exists(FileOut)) {
    stop("Output file does not exist")
  } 
  
  #list of everything written within the input file
listPara <- rhdf5::h5ls(FileInp, datasetinfo = FALSE)

#Grabbing just the HDF5 groups
#listPara <- listPara[listPara$otype == "H5I_GROUP",] #Used to grab metadata if it is only attached to the group level
listGrp <- base::paste(listPara$group, listPara$name, sep = "/") # Combining output


# read attributes from input file
listAttr <- base::lapply(listGrp, rhdf5::h5readAttributes, file = FileInp)


#Apply group names to the attributes list
base::names(listAttr) <- listGrp

#Remove all empty lists
listAttr <- listAttr[!base::sapply(listAttr, function(x) base::length(x) == 0)]


#Open the output file HDF5 link
idFile <- rhdf5::H5Fopen(FileOut)
  
#Write the attributes to the new file
lapply(names(listAttr), function(x){
  
  #x <- "/CPER/dp0p/data/irga_001/000_020"
  idData <- rhdf5::H5Oopen(idFile, x)
 base::lapply(names(listAttr[[x]]), function(y){
   #y <- names(listAttr[[x]])[1]
   rhdf5::h5writeAttribute(attr = listAttr[[x]][[y]], h5obj = idData, name = y)})
})

h5closeAll()
}
