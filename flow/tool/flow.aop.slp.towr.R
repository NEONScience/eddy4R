# install.packages(c("neonUtilities","terra","sf","dplyr","stringr"))
library(neonUtilities)
library(terra)
library(sf)
library(dplyr)
library(stringr)

library(remotes)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)


#############################################################
#Workflow parameters
#############################################################

# -----------------------------
# 1) SITE SETTINGS
# -----------------------------
#Which NEON site are we grabbing data from (4-letter ID)
setSite <- c("BARR","CLBJ","MLBS","DSNY","NIWO","ORNL","OSBS",
             "SCBI","LENO","TALL","CPER","BART","HARV","BLAN",
             "SERC","JERC","GUAN","LAJA","STEI","TREE","UNDE",
             "KONA","KONZ","UKFS","GRSM","DELA","DCFS","NOGP",
             "WOOD","RMNP","OAES","YELL","MOAB","STER","JORN",
             "SRER","ONAQ","ABBY","WREF","SJER","SOAP","TEAK",
             "TOOL","BONA","DEJU","HEAL","PUUM") [1:47]



setSite  <- "KONA"         # NEON site if just running a single site (comment out if running all sites)
AngZaxsSoniInst <- 110          #Azimuth angle of sonic anemometer deployment

#Loop around setSite
for(site in setSite){

#Get site level geolocation metadata
locSite <- geoNEON::getLocBySite(site = site, type = "site")
if(is.null(AngZaxsSoniInst)){
  locSens <- geoNEON::getLocBySite(site = site, type = "TIS", history = F,token = Sys.getenv("NEON_API_TOKEN"))
  AngZaxsSoniInst <- as.numeric(locSens[grep("3D Wind L",locSens$locationDescription),]$gammaOrientation)
}

tower_lat   <- as.numeric(locSite$decimalLatitude)        # tower latitude (WGS84)
tower_lon   <- as.numeric(locSite$decimalLongitude)     # tower longitude (WGS84)
utm_easting <- as.numeric(locSite$easting)
utm_northing <- as.numeric(locSite$northing)
utm_zone <- as.numeric(locSite$utmZoneNumber)

dpID        <- "DP3.30025.001"   # Slope & Aspect – LiDAR (1‑km tiles)
years       <- 2013:as.integer(format(Sys.Date(), "%Y"))

dirBase <- paste0("/home/root/eddy/data/neon_slope_aspect/reorientation")
dirDnld <- paste0(dirBase,"/dnld")
dirOut <- paste0(dirBase,"/out")
dir.create(dirBase, showWarnings = FALSE, recursive = TRUE)
dir.create(dirDnld, showWarnings = FALSE, recursive = TRUE)
dir.create(dirOut, showWarnings = FALSE, recursive = TRUE)
#api_token   <- Sys.getenv("NEON_TOKEN")   # optional but recommended, now using internal API

# 1‑km AOI centered on tower (±500 m)
box_half_m  <- 500
check_size  <- FALSE

# -----------------------------
# 2) Lat/Lon -> UTM for HARV
# -----------------------------
# utm_zone <- floor((tower_lon + 180) / 6) + 1
epsg <- if (tower_lat >= 0) 32600 + utm_zone else 32700 + utm_zone
# 
# pt_ll  <- st_as_sf(data.frame(lon=tower_lon, lat=tower_lat), coords=c("lon","lat"), crs=4326)
# pt_utm <- st_transform(pt_ll, crs = sf::st_crs(paste0("EPSG:", epsg)))
# coords <- st_coordinates(pt_utm)[1, ]
# utm_easting  <- coords["X"]
# utm_northing <- coords["Y"]

# Build 1‑km AOI polygon in UTM
aoi_ext <- ext(utm_easting - box_half_m, utm_easting + box_half_m,
               utm_northing - box_half_m, utm_northing + box_half_m)
aoi_poly <- as.polygons(aoi_ext, crs = paste0("EPSG:", epsg))

# -----------------------------
# 3) Download Slope/Aspect tiles overlapping the AOI (per year)
# -----------------------------


for (yr in years) {
  message(sprintf("Querying %s %s for %s ...", dpID, site, yr))
  try({
    byTileAOP(dpID = dpID,
              site = site,
              year = yr,
              easting = utm_easting,
              northing = utm_northing,
              buffer = box_half_m,              # 500 m -> 1‑km square
              include.provisional = FALSE,
              check.size = check_size,
              savepath = dirDnld,
              # token = if (nzchar(api_token)) api_token else NA_character_,
              progress = TRUE)
  }, silent = TRUE)
}

# -----------------------------
# 4) Locate slope & aspect GeoTIFFs
# -----------------------------
tif_files <- list.files(dirDnld, pattern = "\\.tif(f)?$", recursive = TRUE, full.names = TRUE)
tif_files <- tif_files[grepl("DP3\\.30025", tif_files, ignore.case = TRUE)]

if (length(tif_files) == 0) stop("No DP3.30025 GeoTIFFs found. Check years and availability.")

# Identify slope vs aspect by filename (NEON uses consistent naming)
is_slope  <- grepl("slope",  basename(tif_files), ignore.case = TRUE)
is_aspect <- grepl("aspect", basename(tif_files), ignore.case = TRUE)

files_slope  <- tif_files[is_slope]
files_aspect <- tif_files[is_aspect]

# Extract acquisition year from folder path (/YYYY/)
extract_year <- function(path) {
  y <- str_extract(path, "/(20[0-9]{2})/")
  as.integer(gsub("/", "", y))
}
years_slope  <- sapply(files_slope,  extract_year)
years_aspect <- sapply(files_aspect, extract_year)

# -----------------------------
# 5) Helper functions
# -----------------------------
# Mosaic list of rasters (only those intersecting the AOI), align CRS, crop to AOI
mosaic_crop <- function(flist, aoi_poly, aoi_ext) {
  rlist <- lapply(flist, function(f) {
    r <- try(rast(f), silent = TRUE)
    # if (inherits(r, "try-error")) return(NULL)
    # quick intersect test
    # if (!relate(r, vect(aoi_poly), "intersects")[1]) return(NULL)
    r
  })
  rlist <- Filter(Negate(is.null), rlist)
  if (length(rlist) == 0) return(NULL)
  rmos <- if (length(rlist) > 1) do.call(mosaic, c(rlist, list(fun = "mean"))) else rlist[[1]]
  # if (!compareGeom(rmos, vect(aoi_poly), stopOnError = FALSE)) {
  #  rmos <- project(rmos, crs(vect(aoi_poly)))
  #}
  crop(rmos, aoi_ext)
}

# Circular mean of aspect (degrees 0–360); returns mean aspect (deg) and mean resultant length R (0–1)
circ_mean_aspect <- function(aspect_vals_deg) {
  rad <- aspect_vals_deg * pi / 180
  # Remove NA
  rad <- rad[is.finite(rad)]
  if (length(rad) == 0) return(c(mean_aspect_deg = NA_real_, R = NA_real_))
  C <- mean(cos(rad))
  S <- mean(sin(rad))
  R <- sqrt(C^2 + S^2)
  mean_rad <- atan2(S, C)  # -pi .. pi
  mean_deg <- (mean_rad * 180 / pi) %% 360
  c(mean_aspect_deg = mean_deg, R = R)
}


#Function to calculate the sonic pitch and roll
compute_sonic_pitch_roll <- function(slope_deg, aspect_deg, sonic_azimuth_deg) {
  
  # Convert degrees to radians
  deg2rad <- function(d) d * pi / 180
  rad2deg <- function(r) r * 180 / pi
  
  slope  <- deg2rad(slope_deg)
  aspect <- deg2rad(aspect_deg)
  az     <- deg2rad(sonic_azimuth_deg)
  
  # --- 1. Terrain normal vector (GIS convention: aspect is downslope direction) ---
  nx <- -sin(slope) * sin(aspect)
  ny <-  sin(slope) * cos(aspect)
  nz <-  cos(slope)
  
  n <- c(nx, ny, nz)
  
  # --- 2. Sonic coordinate system ---
  # Sonic x-axis points toward azimuth (sin = east component, cos = north component)
  x0 <- c(sin(az),  cos(az), 0)
  y0 <- c(cos(az), -sin(az), 0)   # Orthogonal horizontal axis
  z0 <- c(0, 0, 1)
  
  # --- 3. Project terrain normal into sonic coordinates ---
  nxp <- sum(n * x0)
  nyp <- sum(n * y0)
  nzp <- sum(n * z0)
  
  # --- 4. Compute pitch (φ) and roll (ρ) ---
  # pitch: rotation about sonic x-axis
  pitch_rad <- atan2(nyp, nzp)
  
  # roll: rotation about sonic y-axis
  roll_rad  <- atan2(-nxp, nzp)
  
  # Return degrees
  list(
    pitch_deg = rad2deg(pitch_rad),
    roll_deg  = rad2deg(roll_rad),
    sonic_azimuth_deg = sonic_azimuth_deg
  )
}

# -----------------------------
# 6) Summaries by year
# -----------------------------
summaries <- list()

for (yr in sort(unique(na.omit(c(years_slope, years_aspect))))) {
  
  # Slope
  slope_files_y  <- files_slope[years_slope == yr]
  slope_crop     <- mosaic_crop(flist = slope_files_y, aoi_poly, aoi_ext)
  
  # Aspect
  aspect_files_y <- files_aspect[years_aspect == yr]
  aspect_crop    <- mosaic_crop(aspect_files_y, aoi_poly, aoi_ext)
  
  plot(aspect_crop)
  if (is.null(slope_crop) & is.null(aspect_crop)) next
  
  # Mean slope (degrees)
  mean_slope <- if (!is.null(slope_crop)) {
    as.numeric(global(slope_crop, fun = "mean", na.rm = TRUE)[1,1])
  } else NA_real_
  
  # Circular mean aspect (degrees) & R
  aspect_stats <- if (!is.null(aspect_crop)) {
    vals <- values(aspect_crop, na.rm = TRUE)
    # terra::values returns a matrix for multi-layer; ensure vector
    vals <- as.numeric(vals)
    circ_mean_aspect(vals)
  } else c(mean_aspect_deg = NA_real_, R = NA_real_)
  
  summaries[[as.character(yr)]] <- data.frame(
    site = site,
    year = yr,
    mean_slope_deg = mean_slope,
    mean_aspect_deg = aspect_stats["mean_aspect_deg"],
    aspect_resultant_R = aspect_stats["R"]
  )
}

slope_aspect_summary <- bind_rows(summaries) %>% arrange(year)
print(slope_aspect_summary)

#Calculate the deployment angles for pitch and roll based on new azimuth
outAng <- compute_sonic_pitch_roll(slope_deg = mean(slope_aspect_summary$mean_slope_deg),aspect_deg = mean(slope_aspect_summary$mean_aspect_deg),sonic_azimuth_deg = AngZaxsSoniInst)

# Optional: save CSV
write.csv(slope_aspect_summary,
          file.path(dirOut, paste0(site,"_slope_aspect_mean_by_year_1km_box.csv")),
          row.names = FALSE)
write.csv(outAng,
          file.path(dirOut, paste0(site,"_soni_pitch_roll_azimuth.csv")),
          row.names = FALSE)


# Optional: quick plot of mean slope
# if (nrow(slope_aspect_summary) > 0) {
#   plot(as.Date(paste0(slope_aspect_summary$year,"-01-01")), slope_aspect_summary$mean_slope_deg,
#        xlab = "Year", ylab = "Mean slope (degrees)",
#        main = "NEON AOP Slope at ",site," — 1-km box around tower")
# }

#Clean up files
unlink(list.files(dirDnld, full.names = TRUE), recursive = TRUE)

} #End for loop around site set