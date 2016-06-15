##############################################################################################
#' @title Download and extract .zip archives from a web address

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. Downloads .zip archives from a web \code{Url}, and saves and extracts the archive into the directory \code{Dir}.

#' @param \code{Inp} The input parameter list containing \code{Url} and \code{Dir}.
#' @param \code{Url} The internet address, of class "character". For use with Dropbox, Google Drive, OneNote... the \href{https://www.google.com/search?q=Direct+Link+Generator}{direct download link} needs to be used. For example in case of Dropbox the web address needs to end on ...dl=1.
#' @param \code{Dir} The target directory, of class "character".

#' @return The function saves the extracted .zip archive into the directory \code{Dir}. Currently no values are returned to the environment it is called from.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' \url{http://hydroecology.net/downloading-extracting-and-reading-files-in-r/}. \cr

#' @keywords Dropbox, zip, archive, download

#' @examples
#' def.dld.zip(Inp = list(
#' Url = "https://www.dropbox.com/s/1yv1ais1wdvoq1l/outRefe_20160410.zip?dl=1",
#' Dir = tempdir()))

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-04-10)
#     original creation
#   Stefan Metzger (2016-04-11)
#     adjusted function name and description
##############################################################################################


# start function def.dld.zip()
def.dld.zip <- function(Inp = list(
  Url,
  Dir = tempdir()
)) {
  
  # create temporary filename for archive download
  NameFileTmp <- paste0(Inp$Dir, "/tmp.zip")
  
  # download into the temporary file
  # -Google Drive direct download URL not working for large files: http://stackoverflow.com/questions/25010369/wget-curl-large-file-from-google-drive
  # -Dropbox direct download URL works for large files via: http://www.syncwithtech.org/2014/09/direct-download-link.html
  # -set mode = "wb" for archives: http://stackoverflow.com/questions/23899525/using-r-to-download-zipped-data-file-extract-and-import-csv
  # -set method = "wget" for unix systems
  #  https://orajavasolutions.wordpress.com/2014/06/03/unsupported-url-scheme-error-when-getting-data-from-https-sites-in-r/
  if(.Platform$OS.type == "windows") suppressWarnings(download.file(url = Inp$Url, destfile = NameFileTmp, mode="wb"))
  if(.Platform$OS.type == "unix") download.file(url = Inp$Url, destfile = NameFileTmp, mode="wb", method = "wget", quiet = TRUE)
  
  # unzip the archive to the target directory
  unzip(zipfile = NameFileTmp, exdir = Inp$Dir, overwrite = TRUE)
  
  # remove obsolete zip archive
  tmpOut01 <- file.remove(NameFileTmp)
  
# end function def.dld.zip()  
}
