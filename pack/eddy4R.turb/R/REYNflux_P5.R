##############################################################################################
#' @title Calculate turbulent vertical flux and auxiliary variables

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Calculate turbulent vertical flux and auxiliary variables.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords eddy-covariance, turbulent flux

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-06-12)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Cove Sturtevant (2016-02-16)
#     updated reference to base.state.r --> def.ec.state.base.R and associated input arguments
#   Ke Xu (2016-09-07)
#     change sapply parameter "simplify" to FALSE, and transfer the resulted list to dataframe data type
##############################################################################################



# attributes(data)$names
# [1] "t_utc"        "d_x_utm"      "d_y_utm"      "d_z_m"        "d_xy_travel"  "d_xy_flow"    "PSI_aircraft" "uvw"         
# [9] "u_met"        "v_met"        "w_met"        "p_air"        "T_air"        "FD_mole_H2O"  "FD_mole_CH4"  "d_z_terrain" 
# [17] "d_z_ABL"      "T_surface"    "R_SW_down"


REYNflux_FD_mole_dry <- function(
  data=eddy.data,
  AlgBase=c("mean", "trnd", "ord03")[1],
  FcorPOT=TRUE,
  FcorPOTl=NULL,
  PltfEc="airc",
  flagCh4 = TRUE
) {


  
############################################################
#THERMODYNAMICS: MOISTURE, DENSITIES AND TEMPERATURES
############################################################

  #-----------------------------------------------------------
  #GENERAL CONVERSIONS
  
    #partial pressure of water vapor
      data$p_H2O <- data$p_air * data$FD_mole_H2O / (1 + data$FD_mole_H2O)
    #partial density of H2O
      data$rho_H2O <- data$p_H2O / eddy4R.base::Natu$Rg / data$T_air
    #total (wet) air density [mol m-3]
      data$rho_air <- data$p_air / eddy4R.base::Natu$Rg / data$T_air
    #dry air density [mol m-3]
      data$rho_dry <- data$rho_air - data$rho_H2O
    #virtual temperature -> the temperature of a moist air parcel at which a theoretical 
    #dry air parcel would have a total pressure and density equal to the moist parcel of air [K]
    #http://en.wikipedia.org/wiki/Virtual_temperature
      #    data$T_v <- data$Temp * (1 + 0.61 * data$q)
      data$T_v <- data$T_air / (1 - ((data$p_H2O / data$p_air) * (1 - eddy4R.base::Natu$RtioMolmH2oDry)) )
    #latent heat of vaporization (Eq 2.55 Foken 2008) [J kg-1] == [m2 s-2]
      data$Lv <- 2500827 - 2360 * eddy4R.base::def.conv.unit(data=data$T_air,unitFrom="K",unitTo="C")

  #-----------------------------------------------------------
  #CONSIDER HUMIDITY IN DRY ADIABATIC CONSTANT

    #cp as function of humidity (Webb 1980 Eq 40) [J kg-1 K-1] == [m2 s-2 K-1]
    #dry mole fraction fomulation identical to within 1e-13
      #data$cph <- (eddy4R.base::Natu$CpDry * data$rho_dry + eddy4R.base::Natu$CpH2o * data$rho_H2O) / data$rho_air
      data$cph <- (eddy4R.base::Natu$CpDry * 1 + eddy4R.base::Natu$CpH2o * data$FD_mole_H2O) / (1 + data$FD_mole_H2O)
   #cv as function of humidity  [J kg-1 K-1] == [m2 s-2 K-1]
      #data$cvh <- (eddy4R.base::Natu$CvDry * data$rho_dry + eddy4R.base::Natu$CvH2o * data$rho_H2O) / data$rho_air
      data$cvh <- (eddy4R.base::Natu$CvDry * 1 + eddy4R.base::Natu$CvH2o * data$FD_mole_H2O) / (1 + data$FD_mole_H2O)
    #specific gas constant for humid air  [J kg-1 K-1] == [m2 s-2 K-1]
      data$Rh <- data$cph - data$cvh
    #Kappa exponent for ideal gas law (Poisson) [-]
      data$Kah <- data$Rh / data$cph
      mn <- data.frame(Kah=mean(data$Kah, na.rm=TRUE))


  #-----------------------------------------------------------
  #POTENTIAL TEMPERATURE AND DENSITIES

    #potential temperature at NIST standard pressure (1013.15 hPa) [K]
      data$T_air_0 <- def.pois.temp.pres(temp01=data$T_air, pres01=data$p_air, pres02=eddy4R.base::Natu$Pres00, Kppa=mn$Kah)
    #virtual potential temperature at NIST standard pressure (1013.15 hPa) [K]
      data$T_v_0 <- def.pois.temp.pres(temp01=data$T_v, pres01=data$p_air, pres02=eddy4R.base::Natu$Pres00, Kppa=mn$Kah)

    #use potential temperature and densities?
      if(FcorPOT == TRUE) {

      	#define pressure level
          plevel <- ifelse(!is.null(FcorPOTl), FcorPOTl, mean(data$p_air, na.rm=TRUE) )
      	#potential temperature at mean pressure level
      	  data$T_air <- def.pois.temp.pres(temp01=data$T_air, pres01=data$p_air, pres02=plevel, Kppa=mn$Kah)      
      	#potential densities at mean pressure level      
      	  #dry air
      	    data$rho_dry <- def.pois.dens.pres(dens01=data$rho_dry, pres01=data$p_air, pres02=plevel, Kppa=mn$Kah)
      	  #H2O
      	    data$rho_H2O <- def.pois.dens.pres(dens01=data$rho_H2O, pres01=data$p_air, pres02=plevel, Kppa=mn$Kah)
      	  #wet air
      	    data$rho_air <- def.pois.dens.pres(dens01=data$rho_air, pres01=data$p_air, pres02=plevel, Kppa=mn$Kah)
    	  #clean up
    	    rm(plevel)
    
      }



############################################################
#TIME SERIES AVERAGES
############################################################

  #data frame
    mn <- as.data.frame( matrix(colMeans(data, na.rm=TRUE), ncol=ncol(data)) )
    attributes(mn)$names <- attributes(data)$names

    
  #aircraft heading as vector average
    if(PltfEc == "airc") mn$PSI_aircraft <- eddy4R.base::def.aply.conv.poly(data=def.conv.cart.az(matrix(colMeans(def.conv.az.cart(eddy4R.base::def.aply.conv.poly(data=data$PSI_aircraft,coefPoly=eddy4R.base::Conv$DegRad)), na.rm=TRUE), ncol=2)),coefPoly=eddy4R.base::Conv$RadDeg)

  #wind direction as vector average
    data$PSI_uv <- def.conv.cart.az(matrix(c(data$v_met, data$u_met), ncol=2))
    mn$PSI_uv <- def.conv.cart.az(matrix(c(mn$v_met, mn$u_met), ncol=2))



############################################################
#ROTATION INTO THE MEAN WIND
############################################################

  #rotation angle
    rotang <- (eddy4R.base::def.conv.unit(data=(mn$PSI_uv+180),unitFrom="deg",unitTo="rad")) %% (2*pi)

  #transformation matrix
    B <- matrix(nrow=3, ncol=3)
      B[1,1] <- cos(rotang)
      B[1,2] <- sin(rotang)
      B[1,3] <- 0.
      B[2,1] <- -sin(rotang)
      B[2,2] <- cos(rotang)
      B[2,3] <- 0.
      B[3,1] <- 0.
      B[3,2] <- 0.
      B[3,3] <- 1.
      BT <- t(B)

  #wind in (horizontal) geodetic coordinates
    U <- rbind(data$v_met, data$u_met, data$w_met)

  #actual rotation
    Urot <- B %*% U
    data$u_hor <- Urot[1,]
    data$v_hor <- -Urot[2,]     #requires mirroring as output is still in geodetic axes order, downstream inpact on imfl$u_star2_y
    data$w_hor <- Urot[3,]
    mn$u_hor <- mean(Urot[1,], na.rm=TRUE)
    mn$v_hor <- mean(Urot[2,], na.rm=TRUE)
    mn$w_hor <- mean(Urot[3,], na.rm=TRUE)
    rm(rotang, U, Urot)



############################################################
#BASE STATE AND DEVIATIONS
############################################################


  #base state
    #AlgBase <- c("mean", "trnd", "ord03")[2]
    if(PltfEc == "airc") base <- sapply(1:ncol(data), function(x) def.ec.sta.base(data$d_xy_travel, data[,x], AlgBase), simplify = FALSE)
    if(PltfEc == "towr") base <- sapply(1:ncol(data), function(x) def.ec.sta.base(data$t_utc, data[,x], AlgBase), simplify = FALSE)
       
    base <- as.data.frame(matrix(unlist(base), ncol=ncol(data)))
    attributes(base)$names <- attributes(data)$names
    #vector averages for azimuth angles if AlgBase == "mean"
      if(AlgBase == "mean") {  
        if(PltfEc == "airc") base$PSI_aircraft <- mn$PSI_aircraft
        base$PSI_uv <- mn$PSI_uv
      }
    #str(base)

  #immidiate fluctuations
    imfl <- sapply(1:ncol(data), function(x) data[,x] - base[,x])
    imfl <- as.data.frame(matrix(imfl, ncol=ncol(data)))
    attributes(imfl)$names <- attributes(data)$names

    #correct wind direction from (detrended) wind components
      PSI_uv_dum <- def.conv.cart.az(matrix(c(imfl$v_met + mn$v_met, imfl$u_met + mn$u_met), ncol=2))
      imfl$PSI_uv <- (PSI_uv_dum - mn$PSI_uv)
      rm(PSI_uv_dum)
    #same should be done for PSI_aircraft

    #variances (corresponding to base state treatment)
      sd <- as.data.frame( matrix(sqrt(splus2R::colVars(imfl, na.rm=TRUE)), ncol=ncol(imfl)) )
      attributes(sd)$names <- attributes(imfl)$names



############################################################
#ROTATION OF STRESS TENSOR
############################################################

  #wind deviations in MET coordinates
    dx <- imfl$v_met
    dy <- imfl$u_met
    dz <- imfl$w_met

  #stress tensor
    M <- rbind(
      c(mean(dx * dx, na.rm = TRUE), mean(dx * dy, na.rm = TRUE), mean(dx * dz, na.rm = TRUE)),
      c(mean(dy * dx, na.rm = TRUE), mean(dy * dy, na.rm = TRUE), mean(dy * dz, na.rm = TRUE)),
      c(mean(dz * dx, na.rm = TRUE), mean(dz * dy, na.rm = TRUE), mean(dz * dz, na.rm = TRUE))
    )

  #rotation into mean wind coordinate system
    Mrot1 <- B %*% M
    Mrot2 <- Mrot1 %*% BT

  #clean up
    rm(BT, dx, dy, dz, M, Mrot1)



############################################################
#MOMENTUM FLUX AND FRICTION VELOCITY
############################################################


  #-----------------------------------------------------------
  #FROM INITIAL COMPONENTS

    #immidiate fluxes from deviations in mean wind coordinates
      imfl$u_star2_x <- -(imfl$u_hor * imfl$w_hor)
      imfl$u_star2_y <- -(imfl$v_hor * imfl$w_hor)
      imfl$u_star <- NaN

  #-----------------------------------------------------------
  #FROM STRESS TENSOR

    #u_star [m s-1]; optionally only considers the along wind stress u_star_x; Foken (2008) Eq.(2.23)
      mn$u_star2_x <- -Mrot2[1,3]
      mn$u_star2_y <- -Mrot2[2,3]
      mn$u_star <- (mn$u_star2_x^2 + mn$u_star2_y^2)^(1/4)	#TK3 style, necessary if flight path not with wind

    #correlations
      cor <- data.frame(
      	u_star2_x= -Mrot2[1,3] / sd$u_hor / sd$w_hor,
      	u_star2_y= -Mrot2[2,3] / sd$v_hor / sd$w_hor
      )

    #wind variance; deviations from initial sd are < 2%
      sd_dum <- sqrt(abs(diag(Mrot2)))
      sd$u_hor <- sd_dum[1]
      sd$v_hor <- sd_dum[2]
      sd$w_hor <- sd_dum[3]

  #-----------------------------------------------------------
  #CLEAN UP
    rm(Mrot2, sd_dum)



############################################################
#SENSIBLE HEAT FLUX
############################################################


  #SENSIBLE HEAT FLUX 
    #flux in kinematic units [K m s-1]
      imfl$F_H_kin <- imfl$w_hor * imfl$T_air
      mn$F_H_kin <- mean(imfl$F_H_kin, na.rm=TRUE)

    #conversion to units of energy [W m-2] == [kg s-3]
      imfl$F_H_en <- (eddy4R.base::Natu$CpDry * base$rho_dry * eddy4R.base::Natu$MolmDry + eddy4R.base::Natu$CpH2o * base$rho_H2O * eddy4R.base::Natu$MolmH2o) * imfl$F_H_kin
      mn$F_H_en <- mean(imfl$F_H_en, na.rm=TRUE)

  #BUOYANCY FLUX considering water vapor buoyancy and NIST standard pressure (1013.15 hPa) reference (virt. pot. temp.) -> z/L
    #flux in kinematic units  [K m s-1]
      imfl$F_H_kin_v_0 <- imfl$w_hor * imfl$T_v_0
      mn$F_H_kin_v_0 <- mean(imfl$F_H_kin_v_0, na.rm=TRUE)

  #CORRELATIONS
    cor$F_H_kin <- stats::cor(imfl$w_hor, imfl$T_air, use="pairwise.complete.obs")
    cor$F_H_en <- cor$F_H_kin
    cor$F_H_kin_v_0 <- stats::cor(imfl$w_hor, imfl$T_v_0, use="pairwise.complete.obs")



############################################################
#LATENT HEAT FLUX
############################################################

  #latent heat flux in kinematic units [mol m-2 s-1]
    imfl$F_LE_kin <- base$rho_dry * imfl$w_hor * imfl$FD_mole_H2O
    mn$F_LE_kin <- mean(imfl$F_LE_kin, na.rm=TRUE)
  #latent heat flux in units of energy  [W m-2] == [kg s-3]
    imfl$F_LE_en <- base$Lv * eddy4R.base::Natu$MolmH2o * imfl$F_LE_kin
    mn$F_LE_en <- mean(imfl$F_LE_en, na.rm=TRUE)
  #correlation
    cor$F_LE_kin <- stats::cor(imfl$w_hor, imfl$FD_mole_H2O, use="pairwise.complete.obs")
    cor$F_LE_en <- cor$F_LE_kin



############################################################
#CH4 FLUX
############################################################
if(flagCh4 == TRUE){
  #CH4 flux in kinematic units [mol m-2 s-1]
  imfl$F_CH4_kin <- base$rho_dry * imfl$w_hor * imfl$FD_mole_CH4
  mn$F_CH4_kin <- mean(imfl$F_CH4_kin, na.rm=TRUE)
  #CH4 flux in mass units [mg m-2 h-1]
  imfl$F_CH4_mass <- imfl$F_CH4_kin * eddy4R.base::Natu$MolmCh4 * 1e6 * 3600
  mn$F_CH4_mass <- mean(imfl$F_CH4_mass, na.rm=TRUE)
  #correlation
  cor$F_CH4_kin <- stats::cor(imfl$w_hor, imfl$FD_mole_CH4, use="pairwise.complete.obs")
  cor$F_CH4_mass <- cor$F_CH4_kin
}
 



############################################################
#AUXILARY PARAMETERS
############################################################

  #turbulence intensity from total wind vector (Stull, Eq. 1.4d)
    #total wind vector
      tot <- sqrt(data$u_met^2 + data$v_met^2 + data$w_met^2)
    #turbulence intensity should be <0.5 to allow for Taylors hypothesis
      mn$I <- sd(tot, na.rm=TRUE) / mean(tot, na.rm=TRUE)

  #Obukhov length (used positive g!)
    mn$d_L_v_0 <- (-(
      ( (mn$u_star)^3 / 
	      ( eddy4R.base::Natu$VonkFokn * eddy4R.base::Natu$Grav / mn$T_v_0 * mn$F_H_kin_v_0 )
      )
    ))

  #stability
    mn$sigma <- mn$d_z_m / mn$d_L_v_0

  #convective (Deardorff) velocity [m s-1]
  #missing values in Deardorff velocity and resulting variables when buoyancy flux is negative!
    mn$w_star <- ( eddy4R.base::Natu$Grav * mn$d_z_ABL / mn$T_v_0 * mn$F_H_kin_v_0 )^(1/3)
  
  #(free) convective time scale [s], often in the order of 5-15 min
    mn$t_star <- mn$d_z_ABL / mn$w_star

  #temperature scale (eddy temperature fluctuations) [K]
    #surface layer
      #mn$T_star_SL <- - mn$F_H_kin_v_0 / mn$u_star	#according to Stull (1988) p. 356
      mn$T_star_SL <- - mn$F_H_kin / mn$u_star	#according to Foken (2008) p.42, fits with ITC assessment
    #mixed layer
      mn$T_star_ML <-   mn$F_H_kin / mn$w_star #according to Stull (1988) p. 356

  #humidity scale (eddy moisture fluctuations) [mol mol-1 dry air]
    #surface layer
      mn$FD_mole_H2O_star_SL <- - mean(mn$F_LE_kin / base$rho_dry, na.rm=TRUE) / mn$u_star
    #mixed layer layer
      mn$FD_mole_H2O_star_ML <-   mean(mn$F_LE_kin / base$rho_dry, na.rm=TRUE) / mn$w_star

  #clean up
    rm(tot)



############################################################
#EXPORT RESULTS
############################################################


  #PREPARE DATA
    #mins
      mi <- as.data.frame( matrix(suppressWarnings(splus2R::colMins(data, na.rm=TRUE)), ncol=ncol(data)) )
    	attributes(mi)$names <- attributes(data)$names
    #maxs
      ma <- as.data.frame( matrix(suppressWarnings(splus2R::colMaxs(data, na.rm=TRUE)), ncol=ncol(data)) )
    	attributes(ma)$names <- attributes(data)$names

  #assemble export list
    export <- list(
      data=data,	#data including internal calculations
      base=base, #base state
      min=mi,		#min
      max=ma,		#max
      mn=mn,		#mean
      sd=sd,		#standard deviation
      imfl=imfl,	#immidiate fluctuations
      cor=cor,		#correlation coefficient
      B=B       #transformation matrix for stress tensor
    )

  #clean up
    rm(B, base, data, mi, ma, mn, sd, imfl, cor)

  #return result
    return(export)
}
