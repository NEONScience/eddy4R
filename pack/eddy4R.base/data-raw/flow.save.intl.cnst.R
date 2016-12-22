##############################################################################################
#' @title Save natural constants and conversions for internal use by the eddy4R family of R-packages

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Workflow. Define and save natural constants and conversion factors for internal use in eddy4R family of R-packages. 

#' @param Currently none

#' @return Saves three lists to sysdata.rda for use within package functions\cr
#' Intl.Natu is a list of natural constants 
#' Intl.Conv is a list of unit conversions
#' Intl.Unit is a nested list of unit symbols, types, prefixes, and the eddy4R internal unit base

#' @references
#' NEON Algorithm Theoretical Basis Document Atmospheric Properties and Units (NEON.DOC.000651) \cr

#' @keywords natural constants, unit conversions, unit symbols, internal units

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2013-08-26)
#     original creation of a file with global constants that is called via source()
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Cove Sturtevant (2016-02-10)
#     changed from function definition to workflow
#     conformed names to EC TES coding convention (prev func name: conNat.r): 
#     Old name        New name
#     g               Intl.Natu$Grav        
#     Tearth          Intl.Natu$PrdErth
#     Oearth          Intl.Natu$AvelErth
#     coriolis        Moved to it's own function def.coef.corl.R
#     r2d             Intl.Conv$RadDeg
#     d2r             Intl.Conv$DegRad
#     p0              Intl.Natu$Pres00
#     T0              Intl.Conv$CelsKelv
#     R_igl           Intl.Natu$Rg
#     kap             Intl.Natu$VonkFokn
#     Md              Intl.Natu$MolmDry
#     Mv              Intl.Natu$MolmH2o
#     Mc              Intl.Natu$MolmCo2
#     M_CH4           Intl.Natu$MolmCh4
#     mvmd            Intl.Natu$RtioMolmH2oDry
#     mdmv            Intl.Natu$RtioMolmDryH2o
#     cpd             Intl.Natu$CpDry
#     cvd             Intl.Natu$CvDry
#     Rd              Intl.Natu$RsDry
#     gammad          Intl.Natu$GmmaDry
#     Kad             Intl.Natu$KppaDry
#     cpv             Intl.Natu$CpH2o
#     cvv             Intl.Natu$CvH2o
#     Rv              Intl.Natu$RsH2o
#     gammav          Intl.Natu$GmmaH2o
#     Kav             Intl.Natu$KppaH2o
#   Cove Sturtevant (2016-04-06)
#     Updated Conversion factors to be n-element vectors of polynomial coefficients [a0 a1 a2 ...]
#     Updated molar masses to conform to internal SI unit base [kg mol-1]
#   Cove Sturtevant (2016-04-19)
#     Added Unit lists and associated conversion factors
#     Updated molar masses to kg mol-1
#   Cove Sturtevant (2016-04-25)
#     Changed standard pressure (Pres00) from 1e5 [Pa] to NIST standard pressure 101325 [Pa]
#     Added NIST standard temperature 293.15 [K]
#   Cove Sturtevant (2016-05-12)
#     Added percent to dimensionless unit conversion
#   Cove Sturtevant (2016-11-28)
#     Added units to natural constants
#   Cove Sturtevant (2016-12-21)
#     Added prefix "Intl." to internal data, for logical grouping in file directories and environment
##############################################################################################

library(devtools)

# Package directory
dirPack <- "C:/Users/csturtevant/OneDrive - Neon, Inc/R/NEON-FIU-algorithm-covesturtevant/ext/eddy4R/pack"
namePack <- "eddy4R.base"

# WARNING: Do not change/delete constants here without updating the package functions that depend on them

# Initialize constants lists
Intl.Unit <- list() # Unit symbols
Intl.Conv <- list() # Conversions
Intl.Natu <- list() # Natural constants


# UNITS
# Base unit symbols
    Intl.Unit$Base$Symb = list(
      Metr = "m",
      Feet = "ft",
      Inch = "inch",
      Mile = "mi", # international mile
      MileNaut = "NM", # nautical mile
      Gram = "g",
      Pnd  = "lb", # pound-mass
      Ton  = "t", # metric ton
      TonUs = "ST", # US/short ton
      Scnd = "s",
      Mint = "min",
      Hour = "h",
      Day  = "d",
      Year = "y",
      Kelv = "K",
      Cels = "C",
      Frht = "F",
      Mole = "mol",
      Rad  = "rad", # (geometry)
      Deg  = "deg", # (geometry)
      Perc = "%",
      Rtio = "-",
      Ampr = "A",
      Volt = "V",
      Ohm  = "ohm",
      Pasc = "Pa",
      Bar  = "bar", 
      Atm  = "atm", # standard atmospheres
      Torr = "Torr",
      Psi  = "psi",
      Watt = "W",
      Joul = "J",
      Newt = "N", 
      Pndf = "lbf", # Pound-force
      Hrtz = "Hz"
      )

# Base unit types (corresponding to entries in Intl.Unit$Base$Symb). These types are used to constrain 
    # conversions between variables (except for mol to/from mass units, conversion is only allowed
    # within the same unit type). Unit types are also used to find the eddy4R internal units
    # (listed in Intl.Unit$Intl).
# Make sure string entries are Title case.
    Intl.Unit$Base$Type = list(
      Metr = "Dist",
      Feet = "Dist",
      Inch = "Dist",
      Mile = "Dist",
      MileNaut = "Dist",
      Gram = "Mass",
      Pnd  = "Mass",
      Ton  = "Mass",
      TonUs = "Mass",
      Scnd = "Time",
      Mint = "Time",
      Hour = "Time",
      Day  = "Time",
      Year = "Time",
      Kelv = "Temp",
      Cels = "Temp",
      Frht = "Temp",
      Mole = "Num",
      Rad  = "Ang",
      Deg  = "Ang",
      Perc = "Dmls",
      Rtio = "Dmls",
      Ampr = "Curr",
      Volt = "Epot",
      Ohm  = "Eres",
      Pasc = "Pres",
      Bar  = "Pres", 
      Atm  = "Pres",
      Torr = "Pres",
      Psi  = "Pres",
      Watt = "Powr",
      Joul = "Engy",
      Newt = "Forc",
      Pndf = "Forc",
      Hrtz = "Freq"
    )

# Unit prefixes
    Intl.Unit$Prfx = list(
      Exa  = "E",
      Peta = "P",
      Tera = "T",
      Giga = "G",
      Mega = "M",
      Kilo = "k",
      Hect = "h",
      Deca = "da",
      Deci = "d",
      Cnti = "c",
      Mili = "m",
      Micr = "u",
      Nano = "n",
      Pico = "p",
      Femt = "f",
      Atto = "a"
    )

# Unit chemical species (a character vector, not a list)
    Intl.Unit$Spcs = c(
      "C",
      "Co2",
      "H2o",
      "Ch4",
      "Dry"
      )
      
# Define internal units. List names should be selected from the entries in Intl.Unit$Base$Type.
# List values should be symbols selected from the values in Intl.Unit$Base$Symb, with applicable 
# prefixes selected from the entries in Intl.Unit$Prfx
    Intl.Unit$Intl = list(
      Dist = "m",
      Mass = "kg",
      Time = "s",
      Temp = "K",
      Num  = "mol",
      Ang  = "rad",
      Dmls = "-",
      Curr = "A",
      Epot = "V",
      Eres = "ohm",
      Pres = "Pa",
      Powr = "W",
      Engy = "J",
      Forc = "N",
      Freq = "Hz"
    )


# CONVERSIONS - n-element vectors of polynomial coefficients for conversion ([a0 a1 a2 ...])
# Unit prefixes
    Intl.Conv$ExaNone <- c(0,1e18) 
    Intl.Conv$NoneExa <- c(0,1e-18) 
    Intl.Conv$PetaNone <- c(0,1e15) 
    Intl.Conv$NonePeta <- c(0,1e-15) 
    Intl.Conv$TeraNone <- c(0,1e12) 
    Intl.Conv$NoneTera <- c(0,1e-12) 
    Intl.Conv$GigaNone <- c(0,1e9) 
    Intl.Conv$NoneGiga <- c(0,1e-9) 
    Intl.Conv$MegaNone <- c(0,1e6) 
    Intl.Conv$NoneMega <- c(0,1e-6) 
    Intl.Conv$KiloNone <- c(0,1e3) 
    Intl.Conv$NoneKilo <- c(0,1e-3) 
    Intl.Conv$HectNone <- c(0,1e2) 
    Intl.Conv$NoneHect <- c(0,1e-2) 
    Intl.Conv$DecaNone <- c(0,1e1) 
    Intl.Conv$NoneDeca <- c(0,1e-1) 
    Intl.Conv$DeciNone <- c(0,1e-1) 
    Intl.Conv$NoneDeci <- c(0,1e1) 
    Intl.Conv$CntiNone <- c(0,1e-2) 
    Intl.Conv$NoneCnti <- c(0,1e2) 
    Intl.Conv$MiliNone <- c(0,1e-3) 
    Intl.Conv$NoneMili <- c(0,1e3) 
    Intl.Conv$MicrNone <- c(0,1e-6) 
    Intl.Conv$NoneMicr <- c(0,1e6) 
    Intl.Conv$NanoNone <- c(0,1e-9) 
    Intl.Conv$NoneNano <- c(0,1e9) 
    Intl.Conv$PicoNone <- c(0,1e-12) 
    Intl.Conv$NonePico <- c(0,1e12) 
    Intl.Conv$FemtNone <- c(0,1e-15) 
    Intl.Conv$NoneFemt <- c(0,1e15) 
    Intl.Conv$AttoNone <- c(0,1e-18) 
    Intl.Conv$NoneAtto <- c(0,1e18) 
    
    
# Time
    Intl.Conv$ScndMint = c(0,1/60) # [a0 a1] convert seconds to minutes [min s-1]
    Intl.Conv$ScndHour = c(0,1/60/60) # [a0 a1] convert seconds to hours [h s-1]
    Intl.Conv$ScndDay = c(0,1/60/60/24) # [a0 a1] convert seconds to days [d s-1]
    Intl.Conv$ScndYear = c(0,1/60/60/24/365.25) # [a0 a1] convert seconds to years [y s-1]
    Intl.Conv$MintScnd = c(0,60) # [a0 a1] convert minutes to seconds [s min-1]
    Intl.Conv$MintHour = c(0,1/60) # [a0 a1] convert minutes to hours [hr min-1]
    Intl.Conv$MintDay = c(0,1/60/24) # [a0 a1] convert minutes to days [d min-1]
    Intl.Conv$MintYear = c(0,1/60/24/365.25) # [a0 a1] convert minutes to years [y min-1]
    Intl.Conv$HourScnd = c(0,60*60) # [a0 a1] convert hours to seconds [s h-1]
    Intl.Conv$HourMint = c(0,60) # [a0 a1] convert hours to minutes [min h-1]
    Intl.Conv$HourDay = c(0,1/24) # [a0 a1] convert hours to days [d h-1]
    Intl.Conv$HourYear = c(0,1/24/365.25) # [a0 a1] convert hours to years [y h-1]
    Intl.Conv$DayScnd = c(0,60*60*24) # [a0 a1] convert days to seconds [s d-1]
    Intl.Conv$DayMint = c(0,60*24) # [a0 a1] convert days to minutes [min d-1]
    Intl.Conv$DayHour = c(0,24) # [a0 a1] convert days to hours [h d-1]
    Intl.Conv$DayYear = c(0,1/365.25) # [a0 a1] convert days to years [y d-1]
    Intl.Conv$YearScnd = c(0,60*60*24*365.25) # [a0 a1] convert years to seconds [s y-1]
    Intl.Conv$YearMint = c(0,60*24*365.25) # [a0 a1] convert years to minutes [min y-1]
    Intl.Conv$YearHour = c(0,24*365.25) # [a0 a1] convert years to hours [h y-1]
    Intl.Conv$YearDay = c(0,365.25) # [a0 a1] convert years to days [d y-1]
    
# Distance

    Intl.Conv$MetrInch = c(0,39.3701) # [a0 a1] convert meters to inches [inch m-1]
    Intl.Conv$InchMetr = c(0,1/39.3701) # [a0 a1] convert inches to meters [m inch-1]
    Intl.Conv$MetrFeet = c(0,3.28084) # [a0 a1] convert meters to feet [ft m-1]
    Intl.Conv$FeetMetr = c(0,0.3048) # [a0 a1] convert feet to meters [m ft-1]
    Intl.Conv$MetrMile = c(0,1/1609.34) # [a0 a1] convert meters to statute miles [mi m-1]
    Intl.Conv$MileMetr = c(0,1609.34) # [a0 a1] convert statute miles to meters [m mi-1]
    Intl.Conv$MetrMileNaut = c(0,1/1852) # [a0 a1] convert meters to nautical miles [NM m-1]
    Intl.Conv$MileNautMetr = c(0,1852) # [a0 a1] convert nautical miles to meters [m NM-1]
    Intl.Conv$InchFeet = c(0,1/12) # [a0 a1] convert inches to feet [ft inch-1]
    Intl.Conv$FeetInch = c(0,12) # [a0 a1] convert feet to inches [inch ft-1]
    Intl.Conv$InchMile = c(0,1/12/5280) # [a0 a1] convert inches to statute miles [mi inch-1]
    Intl.Conv$MileInch = c(0,12*5280) # [a0 a1] convert statute miles to inches [inch mi-1]
    Intl.Conv$InchMileNaut = c(0,1/12/6076.11549) # [a0 a1] convert inches to nautical miles [NM inch-1]
    Intl.Conv$MileNautInch = c(0,12*6076.11549) # [a0 a1] convert nautical miles to inches [inch NM-1]
    Intl.Conv$FeetMile = c(0,1/5280) # [a0 a1] convert feet to statute miles [mi ft-1]
    Intl.Conv$MileFeet = c(0,5280) # [a0 a1] convert statute miles to feet [ft mi-1]
    Intl.Conv$FeetMileNaut = c(0,1/6076.11549) # [a0 a1] convert feet to nautical miles [NM ft-1]
    Intl.Conv$MileNautFeet = c(0,6076.11549) # [a0 a1] convert nautical miles to feet [ft NM-1]
    Intl.Conv$MileMileNaut = c(0,1/1.15077945) # [a0 a1] convert statute miles to nautical miles [NM mi-1]
    Intl.Conv$MileNautMile = c(0,1.15077945) # [a0 a1] convert nautical miles to statute miles [mi NM-1]
    
# Mass
    Intl.Conv$GramPnd <- c(0,1/453.592) # [a0 a1] convert grams to pounds [lb g-1]
    Intl.Conv$PndGram <- c(0,453.592) # [a0 a1] convert pounds to grams [g lb-1]
    Intl.Conv$GramTonUs <- c(0,1/453.592/2000) # [a0 a1] convert grams to US tons [ST g-1]
    Intl.Conv$TonUsGram <- c(0,453.592*2000) # [a0 a1] convert US tons to grams [g ST-1]
    Intl.Conv$GramTon <- c(0,1e-6) # [a0 a1] convert grams to metric tons [t g-1]
    Intl.Conv$TonGram <- c(0,1e6) # [a0 a1] convert metric tons to grams [g t-1]
    Intl.Conv$PndTonUs <- c(0,1/2000) # [a0 a1] convert pounds to US tons [ST lb-1]
    Intl.Conv$TonUsPnd <- c(0,2000) # [a0 a1] convert US tons to pounds [lb ST-1]
    Intl.Conv$PndTon <- c(0,4.53592e-4) # [a0 a1] convert pounds to metric tons [t lb-1]
    Intl.Conv$TonPnd <- c(0,2204.624) # [a0 a1] convert metric tons to pounds [lb t-1]
    Intl.Conv$TonUsTon <- c(0,0.907185) # [a0 a1] convert US tons to metric tons [t ST-1]
    Intl.Conv$TonTonUs <- c(0,1.10231) # [a0 a1] convert metric tons to US tons [ST t-1]
    
# Pressure
    Intl.Conv$PascBar <- c(0,1e-5) # [a0 a1] convert Pascal to bar [bar Pa-1]
    Intl.Conv$BarPasc <- c(0,1e5) # [a0 a1] convert bar to Pascal [Pa bar-1]
    Intl.Conv$PascAtm <- c(0,9.8692e-6) # [a0 a1] convert Pascal to atm [atm Pa-1]
    Intl.Conv$AtmPasc <- c(0,101324.6646142) # [a0 a1] convert atm to Pascal [Pa atm-1]
    Intl.Conv$PascTorr <- c(0,0.0075005920011807008727) # [a0 a1] convert Pascal to Torr [Torr Pa-1]
    Intl.Conv$TorrPasc <- c(0,133.3219271028947901) # [a0 a1] convert torr to Pascal [Pa Torr-1]
    Intl.Conv$PascPsi <- c(0,0.00014503725765876738303) # [a0 a1] convert Pascal to psi [psi Pa-1]
    Intl.Conv$PsiPasc <- c(0,6894.734471349956948) # [a0 a1] convert psi to Pascal [Pa psi-1]
    Intl.Conv$BarAtm <- c(0,0.98692) # [a0 a1] convert bar to atm [atm bar-1]
    Intl.Conv$AtmBar <- c(0,1.013246646142) # [a0 a1] convert atm to bar [bar atm-1]
    Intl.Conv$BarTorr <- c(0,750.05920011807006631) # [a0 a1] convert bar to Torr [Torr bar-1]
    Intl.Conv$TorrBar <- c(0,0.0013332192710289478364) # [a0 a1] convert Torr to bar [bar Torr-1]
    Intl.Conv$BarPsi <- c(0,14.503725765876739118) # [a0 a1] convert bar to psi [psi bar-1]
    Intl.Conv$PsiBar <- c(0,0.06894734471349957261) # [a0 a1] convert psi to bar [bar psi-1]
    Intl.Conv$AtmTorr <- c(0,759.99748451963432672) # [a0 a1] convert atm to Torr [Torr atm-1]
    Intl.Conv$TorrAtm <- c(0,0.001315785118212630311) # [a0 a1] convert Torr to atm [atm Torr-1]
    Intl.Conv$AtmPsi <- c(0,14.695900132274603678) # [a0 a1] convert atm to psi [psi atm-1]
    Intl.Conv$PsiAtm <- c(0,0.068045738676042008541) # [a0 a1] convert psi to atm [atm psi-1]
    Intl.Conv$TorrPsi <- c(0,0.019336710697307905871) # [a0 a1] convert Torr to psi [psi Torr-1]
    Intl.Conv$PsiTorr <- c(0,51.714761401958099896) # [a0 a1] convert psi to Torr [Torr psi-1]

# Angles
    Intl.Conv$RadDeg <- c(0,180/pi)		# [a0 a1] convert radians to degree [deg rad-1]
    Intl.Conv$DegRad <- c(0,pi/180)		# [a0 a1] convert degree to radians [rad deg-1]
    
# Percent/dimensionless
    Intl.Conv$PercRtio <- c(0,0.01)		# [a0 a1] convert percent to dimensionless ratio [[]  %-1]
    Intl.Conv$RtioPerc <- c(0,100)		  # [a0 a1] convert dimensionless ratio to percent [%  []-1]
    
# Temperature
    Intl.Conv$KelvCels <- c(-273.15,1)			# [a0 a1] conversion from °C to K
    Intl.Conv$CelsKelv <- c(273.15,1)	      # [a0 a1] conversion from K to °C
    Intl.Conv$KelvFrht <- c(-459.67,9/5)			# [a0 a1] conversion from F to K
    Intl.Conv$FrhtKelv <- c(-32*5/9+273.15,5/9)	      # [a0 a1] conversion from K to F
    Intl.Conv$CelsFrht <- c(32,9/5)      # [a0 a1] convert Celcius to Fahrenheit
    Intl.Conv$FrhtCels <- c(-32*5/9,5/9) # [a0 a1] convert Fahrenheit to Celcius  

    
# Force
    Intl.Conv$NewtPndf <- c(0,0.224809) # [a0 a1] convert Newton to pound-force [lbf N-1]
    Intl.Conv$PndfNewt <- c(0,4.44822)  # [a0 a1] convert pound-force to Newton [N lbf-1]
    
# NATURAL CONSTANTS
#EARTH BODY
    Intl.Natu$Grav <-	9.81				#gravitational acceleration [m s-2]
      attr(Intl.Natu$Grav,"unit") <- "m s-2"
    Intl.Natu$PrdErth <- 86164.1	        #rotation period of the Earth (one sidereal day) [s]
      attr(Intl.Natu$PrdErth,"unit") <- "s"
    Intl.Natu$AvelErth <- 2 * pi / Intl.Natu$PrdErth   #angular speed of earth [rad s-1]
      attr(Intl.Natu$AvelErth,"unit") <- "rad s-1"
    
#THERMODYNAMICS
  #standard atmosphere
    Intl.Natu$Pres00 <- 101325			# NIST standard pressure [Pa == kg m-1 s-2]
      attr(Intl.Natu$Pres00,"unit") <- "Pa"
    Intl.Natu$Temp00 <- 293.15     # NIST standard temperature [K] 
      attr(Intl.Natu$Temp00,"unit") <- "K"
    Intl.Natu$Rg <- 8.314462175	# ideal gas constant [J mol-1 K-1 == kg m2 s-2 mol-1 K-1]
      attr(Intl.Natu$Rg,"unit") <- "J mol-1 K-1"
    Intl.Natu$VonkFokn <- 0.40				#von-Karman constant accordig to Foken (2008) [-]
      attr(Intl.Natu$VonkFokn,"unit") <- "-"
    
  #molar masses
    Intl.Natu$MolmDry <- 28.97e-3			#dry air [kg mol-1]
      attr(Intl.Natu$MolmDry,"unit") <- "kgDry mol-1"
    Intl.Natu$MolmH2o <- 18.02e-3			#water vapor [kg mol-1]
      attr(Intl.Natu$MolmH2o,"unit") <- "kgH2o mol-1"
    Intl.Natu$MolmCo2 <- 44.01e-3			#co2 [kg mol-1]
      attr(Intl.Natu$MolmCo2,"unit") <- "kgCo2 mol-1"
    Intl.Natu$MolmCh4 <- 16.04e-3		#CH4 [kg mol-1]
      attr(Intl.Natu$MolmCh4,"unit") <- "kgCh4 mol-1"
    Intl.Natu$MolmC <- 12e-3          # C [kg mol-1]
      attr(Intl.Natu$MolmC,"unit") <- "kgC mol-1"
    Intl.Natu$RtioMolmH2oDry <- Intl.Natu$MolmH2o / Intl.Natu$MolmDry		#molar mass ratio water vapour / dry air
      attr(Intl.Natu$RtioMolmH2oDry,"unit") <- "kgH2o kgDry-1"
    Intl.Natu$RtioMolmDryH2o <- Intl.Natu$MolmDry / Intl.Natu$MolmH2o		#molar mass ratio dry air / water vapour
      attr(Intl.Natu$RtioMolmDryH2o,"unit") <- "kgDry kgH2o-1"
    
  #dry air (Dry)
    Intl.Natu$CpDry <- 1004.64		#dry air specific heat at constant pressure 	[J kg-1 K-1]
      attr(Intl.Natu$CpDry,"unit") <- "J kg-1 K-1"
    Intl.Natu$CvDry <- 717.6		#dry air specific heat at constant volume	[J kg-1 K-1]
      attr(Intl.Natu$CvDry,"unit") <- "J kg-1 K-1"
    Intl.Natu$RsDry <- Intl.Natu$CpDry - Intl.Natu$CvDry		#specific gas constant for dry air		[J kg-1 K-1]
      attr(Intl.Natu$RsDry,"unit") <- "J kg-1 K-1"
    Intl.Natu$GmmaDry <- Intl.Natu$CpDry / Intl.Natu$CvDry	# Ratio of specific heat of dry air at contant pressure to specific heat of dry air at contant volume [-]
      attr(Intl.Natu$GmmaDry,"unit") <- "-"
    Intl.Natu$KppaDry <- Intl.Natu$RsDry / Intl.Natu$CpDry		#Kappa exponent for ideal gas law (Poisson)	[-]
      attr(Intl.Natu$KppaDry,"unit") <- "-"
    
  #water vapor (H2o)
    Intl.Natu$CpH2o <- 1846			#water vapor specific heat at constant pressure [m2 K-1 s-2]
      attr(Intl.Natu$CpH2o,"unit") <- "m2 K-1 s-2"
    Intl.Natu$CvH2o <- 1384.04		#water vapor specific heat at constant volume	[m2 K-1 s-2]
      attr(Intl.Natu$CvH2o,"unit") <- "m2 K-1 s-2"
    Intl.Natu$RsH2o <- Intl.Natu$CpH2o - Intl.Natu$CvH2o		#specific gas constant for water vapor		[m2 K-1 s-2]
      attr(Intl.Natu$RsH2o,"unit") <- "m2 K-1 s-2"
    Intl.Natu$GmmaH2o <- Intl.Natu$CpH2o / Intl.Natu$CvH2o # Ratio of specific heat of water vapour at contant pressure to specific heat of water vapour at contant volume [-]
      attr(Intl.Natu$GmmaH2o,"unit") <- "-"
    Intl.Natu$KppaH2o <- Intl.Natu$RsH2o / Intl.Natu$CpH2o		#Kappa exponent for ideal gas law (Poisson)	[-]
      attr(Intl.Natu$KppaH2o,"unit") <- "-"
    
# Save internal constants and conversion factors as .rda files within /data
devtools::use_data(Intl.Unit,Intl.Natu,Intl.Conv,pkg=paste0(dirPack,"/",namePack),overwrite=TRUE) 
