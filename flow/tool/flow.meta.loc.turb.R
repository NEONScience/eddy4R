##############################################################################################
#' @title Workflow for to grab ECTE sensor locations and calculate UTM coordinates

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Downloading dp0p data from S3 and converting to ICOS format.

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords eddy-covariance, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David (2024-09-12)
#     original creation
##############################################################################################


#Install and load libraries
library(devtools)
install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)

#Set the NEON site code
Site <- "UNDE"

#Grab site location metadata for all configure locations for the current time with history = F
locSite <- geoNEON::getLocBySite(Site, type="TIS", history=F)


#Grab tower location metadata
locTowr <- locSite[grep(pattern = "TOWER", x = locSite$namedLocation),]

#Grab a subset of ECTE sensor location metadata
locSens <- locSite[grep(pattern = "ECTE IRGA L|3D Wind L", x = locSite$locationDescription),]

#Grab a subset of ECTE sensor location metadata
angRot <- as.numeric(locSite[grep(pattern = "3D Wind L", x = locSite$locationDescription),"gammaOrientation"])

#perform actual rotation
xOfst <- as.numeric(locSens$xOffset)*cos(angRot) - as.numeric(locSens$yOffset) * sin(angRot)  
yOfst <- as.numeric(locSens$xOffset) * sin(angRot) + as.numeric(locSens$yOffset) * cos(angRot)

#Apply offsets to get UTM coordinates for sensors
locSens$easting <- as.numeric(locTowr$easting) + xOfst
locSens$northing <-  as.numeric(locTowr$northing) + yOfst
locSens$utmZone <- locTowr$utmZone

#Calculate Lat and Lon from UTM coordinates (needs terra installed)
#latLon <- geoNEON::calcLatLong(locSens$easting, locSens$northing, utmZone = locSens$utmZone)
