# install.packages(c("neonUtilities","terra","sf","dplyr","stringr","plotly", "openxlsx"))
library(neonUtilities)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(plotly)

library(remotes)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(geoNEON)
library(openxlsx)


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
             "TOOL","BONA","DEJU","HEAL","PUUM") [3:47]



setSite  <- "SCBI"         # NEON site if just running a single site (comment out if running all sites)
#AngZaxsSoniInst <- NULL         #Azimuth angle of sonic anemometer deployment

#Loop around setSite
for(site in setSite){

AngZaxsSoniInst <- 210         #Azimuth angle of sonic anemometer deployment  
  
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

#dirBase <- paste0("/home/root/eddy/data/neon_slope_aspect")
dirBase <- paste0("/home/root/eddy/data/neon_slope_aspect/reorientation")
dirDnld <- paste0(dirBase,"/dnld")
dirOut <- paste0(dirBase,"/out")
dirPlot <- paste0(dirBase,"/plot")
dir.create(dirBase, showWarnings = FALSE, recursive = TRUE)
dir.create(dirDnld, showWarnings = FALSE, recursive = TRUE)
dir.create(dirOut, showWarnings = FALSE, recursive = TRUE)
dir.create(dirPlot, showWarnings = FALSE, recursive = TRUE)
#api_token   <- Sys.getenv("NEON_TOKEN")   # optional but recommended, now using internal API

#Load excel workbook
wb <- loadWorkbook("/home/root/eddy/data/neon_slope_aspect/revisit pitch and roll_2026.xlsx")


#Name of worksheet
sheet_name <- "Analysis"
#Add worksheet if needed
if (!(sheet_name %in% names(wb))) {
  addWorksheet(wb, sheet_name)
  # Read existing data
  dfAng <- read.xlsx(wb, sheet = 1)
}else{
  # Read existing data
  dfAng <- read.xlsx(wb, sheet = sheet_name)}

#Check if data exists and angles match from worksheet and database
stopifnot(
  nrow(dfAng) > 0,
  site %in% dfAng$SITE,
  dfAng[which(dfAng$SITE == site),]$AngNedZaxs == AngZaxsSoniInst|dfAng[which(dfAng$SITE == site),]$Reorientation.AngNedZaxs == AngZaxsSoniInst
  )

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
  nx <-  sin(slope) * sin(aspect)
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
  # pitch: rotation about sonic y-axis, positive when pointing up
  pitch_rad <- atan2(-nxp, nzp)

  # roll: rotation about sonic x-axis, positive when tilted right (when looking from behind)
  roll_rad  <- atan2(nyp, nzp)
  
  # Return degrees
  list(
    pitch_deg = rad2deg(pitch_rad),
    roll_deg  = rad2deg(roll_rad),
    sonic_azimuth_deg = sonic_azimuth_deg
  )
}

# Plot terrain plane vs. deployed/rotated sonic plane in 3-D (ENU, x=East y=North z=Up)
plot_sonic_terrain <- function(slope_deg, aspect_deg, sonic_azimuth_deg, fileOut = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Install plotly: install.packages('plotly')")

  deg2rad <- function(d) d * pi / 180
  rad2deg <- function(r) r * 180 / pi

  slope  <- deg2rad(slope_deg)
  aspect <- deg2rad(aspect_deg)
  az     <- deg2rad(sonic_azimuth_deg)

  # Terrain normal (ENU)
  n  <- c( sin(slope)*sin(aspect), sin(slope)*cos(aspect), cos(slope))

  # Sonic axes as deployed (horizontal, x toward azimuth)
  x0 <- c(sin(az),  cos(az), 0)
  y0 <- c(cos(az), -sin(az), 0)
  z0 <- c(0, 0, 1)

  # Sonic axes terrain-aligned: z -> terrain normal, x -> project x0 onto terrain plane
  z1 <- n
  x1 <- x0 - sum(x0 * n) * n;  x1 <- x1 / sqrt(sum(x1^2))
  y1 <- y0 - sum(y0 * n) * n;  y1 <- y1 / sqrt(sum(y1^2))

  # Pitch and roll (matches compute_sonic_pitch_roll)
  nxp <- sum(n * x0); nyp <- sum(n * y0); nzp <- sum(n * z0)
  pitch_rad <- atan2(-nxp, nzp)
  roll_rad  <- atan2(nyp, nzp)
  pitch_deg <- rad2deg(pitch_rad)
  roll_deg  <- rad2deg(roll_rad)

  # Build a square patch in the plane of a given normal vector
  plane_patch <- function(normal, size = 1.2) {
    u <- if (abs(normal[1]) < 0.9) c(1,0,0) else c(0,1,0)
    u <- u - sum(u * normal) * normal;  u <- u / sqrt(sum(u^2))
    v <- c(normal[2]*u[3]-normal[3]*u[2],
           normal[3]*u[1]-normal[1]*u[3],
           normal[1]*u[2]-normal[2]*u[1])
    corners <- rbind(-size*u - size*v,
                      size*u - size*v,
                      size*u + size*v,
                     -size*u + size*v,
                     -size*u - size*v)   # closed loop
    corners
  }

  tp <- plane_patch(n)    # terrain plane
  sp <- plane_patch(z0)   # sonic plane (deployed horizontal)

  # Arc sweeping clockwise from North to azimuth in the horizontal plane
  arc_r <- 0.45
  arc_t <- seq(0, az, length.out = max(60L, abs(round(sonic_azimuth_deg))))
  arc_x <- arc_r * sin(arc_t)
  arc_y <- arc_r * cos(arc_t)
  arc_z <- rep(0, length(arc_t))
  mid   <- ceiling(length(arc_t) / 2)

  # Pitch arc: rotation about y0 from z0, sweeping by pitch_rad (positive = z toward -x0)
  arc_ang <- 60L
  arc_rs  <- 0.65
  pt      <- seq(0, pitch_rad, length.out = arc_ang)
  p_arc   <- arc_rs * (outer(cos(pt), z0) - outer(sin(pt), x0))
  p_tang  <- -sin(pitch_rad)*z0 - cos(pitch_rad)*x0          # tangent at arc endpoint
  p_tang  <- p_tang / sqrt(sum(p_tang^2))

  # Roll arc: rotation about x0 from z0, sweeping by roll_rad (positive = z toward +y0)
  rt     <- seq(0, roll_rad, length.out = arc_ang)
  r_arc  <- arc_rs * (outer(cos(rt), z0) + outer(sin(rt), y0))
  r_tang <- -sin(roll_rad)*z0 + cos(roll_rad)*y0              # tangent at arc endpoint
  r_tang <- r_tang / sqrt(sum(r_tang^2))

  fig <- plotly::plot_ly() |>
    # --- planes ---
    plotly::add_trace(x=tp[,1], y=tp[,2], z=tp[,3], type="scatter3d", mode="lines",
              line=list(color="saddlebrown", width=3), name="Terrain plane") |>
    plotly::add_trace(x=sp[,1], y=sp[,2], z=sp[,3], type="scatter3d", mode="lines",
              line=list(color="steelblue",   width=3), name="Sonic plane (deployed)") |>
    # --- terrain normal ---
    plotly::add_trace(x=c(0,n[1]),  y=c(0,n[2]),  z=c(0,n[3]),  type="scatter3d", mode="lines+markers",
              line=list(color="saddlebrown", width=5),
              marker=list(color="saddlebrown", size=c(0,7)), name="Terrain normal") |>
    # --- deployed sonic axes ---
    plotly::add_trace(x=c(0,x0[1]), y=c(0,x0[2]), z=c(0,x0[3]), type="scatter3d", mode="lines+markers",
              line=list(color="red",       width=4),
              marker=list(color="red",       size=c(0,6)), name="Sonic x (azimuth, deployed)") |>
    plotly::add_trace(x=c(0,y0[1]), y=c(0,y0[2]), z=c(0,y0[3]), type="scatter3d", mode="lines+markers",
              line=list(color="steelblue",  width=4),
              marker=list(color="steelblue",  size=c(0,6)), name="Sonic y (deployed)") |>
    plotly::add_trace(x=c(0,z0[1]), y=c(0,z0[2]), z=c(0,z0[3]), type="scatter3d", mode="lines+markers",
              line=list(color="royalblue",  width=4),
              marker=list(color="royalblue",  size=c(0,6)), name="Sonic z (deployed, vertical)") |>
    # --- azimuth arc ---
    plotly::add_trace(x=c(0,0), y=c(0,0.6), z=c(0,0), type="scatter3d", mode="lines",
              line=list(color="gray50", width=2, dash="dot"), name="North") |>
    plotly::add_trace(x=arc_x, y=arc_y, z=arc_z, type="scatter3d", mode="lines",
              line=list(color="orange", width=3),
              name=sprintf("Azimuth %.1f\u00b0", sonic_azimuth_deg)) |>
    plotly::add_trace(x=arc_x[mid], y=arc_y[mid], z=0.05, type="scatter3d", mode="text",
              text=sprintf("%.1f\u00b0", sonic_azimuth_deg),
              textfont=list(color="orange", size=13),
              showlegend=FALSE, name="az label") |>
    # --- terrain-aligned sonic axes ---
    plotly::add_trace(x=c(0,x1[1]), y=c(0,x1[2]), z=c(0,x1[3]), type="scatter3d", mode="lines+markers",
              line=list(color="darkred",    width=4, dash="dash"),
              marker=list(color="darkred",    size=c(0,6)), name="Sonic x (terrain-aligned)") |>
    plotly::add_trace(x=c(0,y1[1]), y=c(0,y1[2]), z=c(0,y1[3]), type="scatter3d", mode="lines+markers",
              line=list(color="darkgreen",  width=4, dash="dash"),
              marker=list(color="darkgreen",  size=c(0,6)), name="Sonic y (terrain-aligned)") |>
    plotly::add_trace(x=c(0,z1[1]), y=c(0,z1[2]), z=c(0,z1[3]), type="scatter3d", mode="lines+markers",
              line=list(color="forestgreen",width=4, dash="dash"),
              marker=list(color="forestgreen",size=c(0,6)), name="Sonic z (terrain-aligned)") |>
    # --- pitch rotation arc ---
    plotly::add_trace(x=p_arc[,1], y=p_arc[,2], z=p_arc[,3], type="scatter3d", mode="lines",
              line=list(color="tomato", width=4),
              name=sprintf("Pitch %.2f\u00b0", pitch_deg)) |>
    plotly::add_trace(x=p_arc[arc_ang,1], y=p_arc[arc_ang,2], z=p_arc[arc_ang,3],
              u=-p_tang[1]*0.07, v=-p_tang[2]*0.07, w=-p_tang[3]*0.07,
              type="cone", anchor="tail", sizemode="absolute", sizeref=0.07,
              colorscale=list(list(0,"tomato"),list(1,"tomato")),
              showscale=FALSE, showlegend=FALSE, name="pitch arrow") |>
    plotly::add_trace(x=p_arc[ceiling(arc_ang/2),1], y=p_arc[ceiling(arc_ang/2),2],
              z=p_arc[ceiling(arc_ang/2),3]+0.05, type="scatter3d", mode="text",
              text=sprintf("P=%.2f\u00b0", pitch_deg),
              textfont=list(color="tomato", size=12),
              showlegend=FALSE, name="pitch label") |>
    # --- roll rotation arc ---
    plotly::add_trace(x=r_arc[,1], y=r_arc[,2], z=r_arc[,3], type="scatter3d", mode="lines",
              line=list(color="mediumvioletred", width=4),
              name=sprintf("Roll %.2f\u00b0", roll_deg)) |>
    plotly::add_trace(x=r_arc[arc_ang,1], y=r_arc[arc_ang,2], z=r_arc[arc_ang,3],
              u=-r_tang[1]*0.07, v=-r_tang[2]*0.07, w=-r_tang[3]*0.07,
              type="cone", anchor="tail", sizemode="absolute", sizeref=0.07,
              colorscale=list(list(0,"mediumvioletred"),list(1,"mediumvioletred")),
              showscale=FALSE, showlegend=FALSE, name="roll arrow") |>
    plotly::add_trace(x=r_arc[ceiling(arc_ang/2),1], y=r_arc[ceiling(arc_ang/2),2],
              z=r_arc[ceiling(arc_ang/2),3]+0.05, type="scatter3d", mode="text",
              text=sprintf("R=%.2f\u00b0", roll_deg),
              textfont=list(color="mediumvioletred", size=12),
              showlegend=FALSE, name="roll label") |>
    plotly::layout(
      title = sprintf("Slope %.1f\u00b0 | Aspect %.1f\u00b0 | Azimuth %.1f\u00b0 || Pitch %.2f\u00b0 | Roll %.2f\u00b0",
                      slope_deg, aspect_deg, sonic_azimuth_deg, pitch_deg, roll_deg),
      scene = list(
        xaxis = list(title="East",  range=c(-1.5,1.5)),
        yaxis = list(title="North", range=c(-1.5,1.5)),
        zaxis = list(title="Up",    range=c(-1.5,1.5)),
        aspectmode = "cube",
        camera = list(eye = list(x=1.5, y=-1.5, z=1.0))
      )
    )

  print(fig)
  if (!is.null(fileOut)) {
    htmlwidgets::saveWidget(fig, file = fileOut, selfcontained = TRUE)
    message("Plot saved to: ", fileOut)
  }
  invisible(fig)
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
outAng <- compute_sonic_pitch_roll(slope_deg = mean(slope_aspect_summary$mean_slope_deg, na.rm = T),aspect_deg = mean(slope_aspect_summary$mean_aspect_deg, na.rm = T),sonic_azimuth_deg = AngZaxsSoniInst)

# Plot terrain plane vs. deployed/rotated sonic plane
plot_sonic_terrain(slope_deg = mean(slope_aspect_summary$mean_slope_deg, na.rm = T),
                   aspect_deg = mean(slope_aspect_summary$mean_aspect_deg, na.rm = T),
                   sonic_azimuth_deg = AngZaxsSoniInst,
                   fileOut = file.path(dirPlot, paste0(site, "_soni_terrain_orientation.html")))

# Optional: save CSV
write.csv(slope_aspect_summary,
          file.path(dirOut, paste0(site,"_slope_aspect_mean_by_year_1km_box.csv")),
          row.names = FALSE)
write.csv(outAng,
          file.path(dirOut, paste0(site,"_soni_pitch_roll_azimuth.csv")),
          row.names = FALSE)

ifelse(grepl("reorientation", dirOut),dfAng[which(dfAng$SITE == site),]$Derived.Pitch.from.AOP.after.reorientation <- outAng$pitch_deg, dfAng[which(dfAng$SITE == site),]$Derived.Pitch.from.AOP <- outAng$pitch_deg)
ifelse(grepl("reorientation", dirOut),dfAng[which(dfAng$SITE == site),]$Derived.roll.from.AOP.after.reorientation <- outAng$roll_deg, dfAng[which(dfAng$SITE == site),]$Derived.roll.from.AOP <- outAng$roll_deg)

#Write output data
writeData(
  wb, sheet = sheet_name, x = dfAng
)

#Save output
saveWorkbook(wb,"/home/root/eddy/data/neon_slope_aspect/revisit pitch and roll_2026.xlsx",overwrite = TRUE)

# Optional: quick plot of mean slope
# if (nrow(slope_aspect_summary) > 0) {
#   plot(as.Date(paste0(slope_aspect_summary$year,"-01-01")), slope_aspect_summary$mean_slope_deg,
#        xlab = "Year", ylab = "Mean slope (degrees)",
#        main = "NEON AOP Slope at ",site," — 1-km box around tower")
# }

#Clean up files
unlink(list.files(dirDnld, full.names = TRUE), recursive = TRUE)

} #End for loop around site set