##############################################################################################
#' @title Workflow for downloading dp04 data from unpublished file list in GCS

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Downloading unpublished SAE data from GCS.

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords eddy-covariance, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David (2024-09-23)
#     original creation
##############################################################################################

#Site for analysis
Site <- "KONA"

#Date begin
dateBgn <- as.Date("2024-09-01")
#Date end
dateEnd <- as.Date("2024-09-10")

#Download directory
DirDnld <- tempdir()

#Unpublished SAE file list
listFile <- read.csv("https://storage.googleapis.com/neon-sae-files/ods/sae_files_unpublished/sae_file_url_unpublished.csv")

#Date interval
setDate <- lubridate::interval(start = dateBgn, end = dateEnd)

#Subset file list by dates and site
listFileSub <- listFile[as.Date(listFile$date) %within% setDate & listFile$site == Site,]

#Download filename (full path)
fileDnld <-  paste0(DirDnld,"/", str_extract(string = listFileSub$url,pattern = "NEON.*.h5$"))

#Download data
lapply(seq_along(listFileSub$url), function(x){
  download.file(url = listFileSub$url[x], destfile = fileDnld[x])
})

#Read in data
dp04 <- neonUtilities::stackEddy(DirDnld, level = "dp04")       
