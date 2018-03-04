##############################################################################################
#' @title Definition function: Extract data and attributes from HDF5 input file to another HDF5 output file 

#' @author 
#' David Durden \email{eddy4R.info@gmail.com}
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Definition function. Function extracts group sturcture, data, and metadata attributes from input, examples include ecte (turbulent/turb) and ecse (storage/stor) HDF5 files to a another HDF5 file, used for nsae (net surface atmosphere exchange), with the same group heirarchy structure from each file. Either \code{FileIn} or \code{rpt} must be specified for \code{eddy4R.base::def.extr.hdf5()} to work. If \code{FileIn} is specified, the contents of the specified file are read (and optionally written to an output file). If \code{rpt} is specified, its contents are being written to an output file. This enables to read hdf5 files (call \code{eddy4R.base::def.extr.hdf5()} and specify \code{FileIn}), then modify the resulting \code{rpt} object as needed, and lastly to write the modified \code{rpt} object to file (call \code{eddy4R.base::def.extr.hdf5()} and specify \code{rpt}).

#' @param FileIn is the input HDF5 file (turb or stor) the data and metadata are being read from. It is ignored if \code{rpt} is specified.
#' @param rpt is the list returned from a previous call of \code{eddy4R.base::def.extr.hdf5()} with \code{FileIn} specified. At a minimum the entries \code{rpt$listGrpName}, \code{rpt$listData} and \code{rpt$listAttr} are required to write the contents to an output hdf5 file.
#' @param FileOut is the output file nsae HDF5 written to.
#' @param MethExtrData logical parameter that decides if data from the input file should be extracted and written to the output file.
#' @param MethExtrAttr logical parameter that decides if attributes (metadata) from the input file should be extracted and written to the output file.
#' @param dp01 A vector of class "character" containing the name of NEON dp01 which data from the input file are not extracted and written to the output file

#' @return A NEON formatted HDF5 file that has parameters from the input file written to the output HDF5 file. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE, ECSE, turb, stor

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-08-08)
#     original creation
#   Natchaya Pingintha-Durden
#     added functionality not to include defined dp01 data
#   Dave Durden (2017-11-08)
#     Update function to report data and attributes
#   Dave Durden (2017-11-08)
#     Update function to prevent output to another file if FileOut = NULL
#   Stefan Metzger (2018-03-04)
#     enable writing of modified data objects by adding rpt to arguments
##############################################################################################################
#Start of function call to extract data from one file and write to another
##############################################################################################################

def.extr.hdf5 <- function(
  FileIn = NULL,
  rpt = NULL,
  FileOut = NULL,
  MethExtrData = TRUE,
  MethExtrAttr = TRUE,
  dp01 = NULL
) {

  
    
# read data only if rpt is not specified
if(base::is.null(rpt)) {

  
  # test whether input file present
  if(!base::file.exists(FileIn)) {
    stop("Input file does not exist")
  } 

    
  # list file contents
    
    #Create a list to report data and attributes
    rpt <- list()
    
    #list of everything written within the input file
    listObj <- rhdf5::h5ls(FileIn, datasetinfo = FALSE)
    
    #List of all object names
    listObjName <- base::paste(listObj$group, listObj$name, sep = "/")
  
    
  # Groups for HDF5 group structure
    
    #Grabbing just the HDF5 groups
    listGrp <- listObj[listObj$otype == "H5I_GROUP",] 
    
    rpt$listGrpName <- base::paste(listGrp$group, listGrp$name, sep = "/") # Combining group names for writing output
  
    
  # Dataset for HDF5 dataset output
    
    #Grab just the data objects
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    
    #Combining names for grabbing datasets
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/") # Combining output
    
    #Ignoring dp0p data
    listDataName <- listDataName[grep(pattern = "dp0p", x = listDataName, invert = TRUE)]
    
    #Ignoring dp01 data as define in dp01GrpName
    if (!is.null(dp01)){
      for (idx in dp01){
      listDataName <- listDataName[grep(paste0("dp01", "/", "data", "/", idx), x = listDataName, invert = TRUE)] 
      listDataName <- listDataName[grep(paste0("dp01", "/", "qfqm", "/", idx), x = listDataName, invert = TRUE)]
      listDataName <- listDataName[grep(paste0("dp01", "/", "ucrt", "/", idx), x = listDataName, invert = TRUE)]
      }
    } 
   
    # Read data from the input file
    rpt$listData <- base::lapply(listDataName, rhdf5::h5read, file = FileIn)
    
    #Apply group names to the attributes list
    base::names(rpt$listData) <- listDataName
  
    
  # Attributes for writing to the output HDF5 file
    
    # read attributes from input file
    rpt$listAttr <- base::lapply(listObjName, rhdf5::h5readAttributes, file = FileIn)
    
    #Apply group names to the attributes list
    base::names(rpt$listAttr) <- listObjName
    
    #Remove all empty lists
    rpt$listAttr <- rpt$listAttr[!base::sapply(rpt$listAttr, function(x) base::length(x) == 0)]

    
}
  

    
# perform some tests if rpt is provided
if(base::is.null(rpt)) {
  
  # rpt of type list?
  if(!(base::typeof(rpt) == "list")) base::stop("rpt needs to be of type 'list'.")
  
  # are all mandatory list entries specified?
  if(base::is.null(rpt$listGrpName)) base::stop("please provide rpt$listGrpName.")
  if(base::is.null(rpt$listData)) base::stop("please provide rpt$listData.")
  if(base::is.null(rpt$listAttr)) base::stop("please provide rpt$listAttr.")

}

  
  
# Write to the output HDF5 file, if FileOut provided
if(!is.null(FileOut)) {
  
    
  # create connection to HDF5 file if FileOut already exists, or create new file
  if(file.exists(FileOut) == TRUE) {
    fid <- rhdf5::H5Fopen(name = FileOut) #Open connection
  } else {
    fid <- rhdf5::H5Fcreate(name = FileOut) #Create file and open connection
  }

    
  # create the group structure in the output file
  lapply(rpt$listGrpName, function(x){
    rhdf5::h5createGroup(fid, x)
    })
  
  
  # determine if data should be written to output HDF5
  if(MethExtrData == TRUE){
  #Write the data to the output file
  lapply(names(rpt$listData), function(x){
   rhdf5::h5write(obj = rpt$listData[[x]], file = fid, name = x)
  })
  }  
  
  
  # determine if attributes should be written to output HDF5
  if(MethExtrAttr == TRUE){
    #Write attributes to the output HDF5 file
    lapply(names(rpt$listAttr), function(x){
      gid <- rhdf5::H5Oopen(fid, x)
      base::lapply(names(rpt$listAttr[[x]]), function(y){
        #y <- names(rpt$listAttr[[x]])[1]
        rhdf5::h5writeAttribute(attr = rpt$listAttr[[x]][[y]], h5obj = gid, name = y)})
    })
  }
  
  
# close the HDF5 file connection
rhdf5::H5close()
}
  
  
  
# return the data and attributes
return(rpt)

    
}
