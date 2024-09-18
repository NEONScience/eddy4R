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
Site <- "KONZ"

#Angle of local tower coordinate reference system (x aligned with C face from CD corner)
angRefeTowr <- c("BARR" = 255,"CLBJ" = 270,"MLBS" = 180,"DSNY" = 270,"NIWO" = 225,"ORNL" = 270,"OSBS" = 225,
                    "SCBI" = 300,"LENO" = 315,"TALL" = 270,"CPER" = 270,"BART" = 270,"HARV" = 270,"BLAN" = 240,
                    "SERC" = 230,"JERC" = 135,"GUAN" = 270,"LAJA" = 270,"STEI" = 225,"TREE" = 225,"UNDE" = 200,
                    "KONA" = 290,"KONZ" = 290,"UKFS" = 230,"GRSM" = 230,"DELA" = 280,"DCFS" = 245,"NOGP" = 220,
                    "WOOD" = 245,"RMNP" = 303,"OAES" = 270,"YELL" = 180,"MOAB" = 270,"STER" = 270,"JORN" = 220,
                    "SRER" = 290,"ONAQ" = 180,"ABBY" = 270,"WREF" = 225,"SJER" = 270,"SOAP" = 180,"TEAK" = 270,
                    "TOOL" = 270,"BONA" = 270,"DEJU" = 290,"HEAL" = 270,"PUUM" = 295)[Site]

#Grab site location metadata for all configure locations for the current time with history = F
locSite <- geoNEON::getLocBySite(Site, type="TIS", history=F)


#Grab tower location metadata
locTowr <- locSite[grep(pattern = "TOWER", x = locSite$namedLocation),]

#Grab a subset of Tower sensor location metadata
locSensTowr <- locSite[grep(pattern = "BOOM|LEVEL", x = locSite$locationParent),]
locSensTowr <- locSensTowr[grep(pattern = "CFGLOC[0-9]{6}$", x = locSensTowr$namedLocation),]


#perform actual rotation
xOfst <- as.numeric(locSensTowr$xOffset)*cos(angRefeTowr) - as.numeric(locSensTowr$yOffset) * sin(angRefeTowr)  
yOfst <- as.numeric(locSensTowr$xOffset) * sin(angRefeTowr) + as.numeric(locSensTowr$yOffset) * cos(angRefeTowr)

#Apply offsets to get UTM coordinates for sensors
locSensTowr$easting <- as.numeric(locTowr$easting) + xOfst
locSensTowr$northing <-  as.numeric(locTowr$northing) + yOfst
locSensTowr$utmZone <- locTowr$utmZone

#Grab a subset of ECTE sensor location metadata
locEcte <- locSensTowr[grep(pattern = "ECTE IRGA L|3D Wind L", x = locSensTowr$locationDescription),]

#Calculate Lat and Lon from UTM coordinates (needs terra installed)
#latLon <- geoNEON::calcLatLong(locSens$easting, locSens$northing, utmZone = locSens$utmZone)
