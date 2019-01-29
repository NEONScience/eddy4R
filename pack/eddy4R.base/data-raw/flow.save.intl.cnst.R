##############################################################################################
#' @title Save natural constants and conversions for internal use by the eddy4R family of R-packages

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Workflow. Define and save natural constants and conversion factors for internal use in eddy4R family of R-packages. 

#' @param Currently none

#' @return Saves three lists to sysdata.rda for use within package functions\cr
#' IntlNatu is a list of natural constants 
#' IntlConv is a list of unit conversions
#' IntlUnit is a nested list of unit symbols, types, prefixes, and the eddy4R internal unit base

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
#     g               IntlNatu$Grav        
#     Tearth          IntlNatu$PrdErth
#     Oearth          IntlNatu$AvelErth
#     coriolis        Moved to it's own function def.coef.corl.R
#     r2d             IntlConv$RadDeg
#     d2r             IntlConv$DegRad
#     p0              IntlNatu$Pres00
#     T0              IntlConv$CelsKelv
#     R_igl           IntlNatu$Rg
#     kap             IntlNatu$VonkFokn
#     Md              IntlNatu$MolmDry
#     Mv              IntlNatu$MolmH2o
#     Mc              IntlNatu$MolmCo2
#     M_CH4           IntlNatu$MolmCh4
#     mvmd            IntlNatu$RtioMolmH2oDry
#     mdmv            IntlNatu$RtioMolmDryH2o
#     cpd             IntlNatu$CpDry
#     cvd             IntlNatu$CvDry
#     Rd              IntlNatu$RsDry
#     gammad          IntlNatu$GmmaDry
#     Kad             IntlNatu$KppaDry
#     cpv             IntlNatu$CpH2o
#     cvv             IntlNatu$CvH2o
#     Rv              IntlNatu$RsH2o
#     gammav          IntlNatu$GmmaH2o
#     Kav             IntlNatu$KppaH2o
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
#   Cove Sturtevant (2018-01-15)
#     Fixed some minor differences in pressure conversion factors to match the National Physical 
#        Laboratory (http://www.npl.co.uk/reference/faqs/pressure-units)
#   Adam Vaughan (2019-01-29)
#     Added extended chemical species molecular masses
#        IntlNatu$MolmNO      -   Nitrogen monoxide
#        IntlNatu$MolmNO2     -   Nitrogen dioxide
#        IntlNatu$MolmHONO    -   Nitrous acid
#        IntlNatu$MolmO3      -   Ozone
#        IntlNatu$MolmCO      -   Carbon monoxide
#        IntlNatu$MolmCH3OH   -   Methanol
#        IntlNatu$MolmC2H4O   -   Acetaldehyde
#        IntlNatu$MolmC2H5OH  -   Ethanol
#        IntlNatu$MolmC2H6    -   Ethane
#        IntlNatu$MolmC3H6O   -   Propanal
#        IntlNatu$MolmC3H8    -   Propane
#        IntlNatu$MolmC4H6O   -   Methyl vinyl ketone
#        IntlNatu$MolmC4H8O   -   Butanone
#        IntlNatu$MolmC4H10   -   Butane
#        IntlNatu$MolmC5H8    -   Isoprene
#        IntlNatu$MolmC5H10O  -   Methyl isopropyl ketone
#        IntlNatu$MolmC5H12O  -   Pentanol
#        IntlNatu$MolmC6H12O  -   Hexanal
#        IntlNatu$MolmC6H6    -   Benzene
#        IntlNatu$MolmC7H8    -   Toluene
#        IntlNatu$MolmC8H10   -   Xylene
#        IntlNatu$MolmC9H12   -   1,2,4-Trimethyl benzene

##############################################################################################

library(devtools)

# Package directory
dirPack <- "~/eddy/Github/Docker/NEON-FIU-algorithm/NEON-FIU-algorithm/ext/eddy4R/pack"
namePack <- "eddy4R.base"

# WARNING: Do not change/delete constants here without updating the package functions that depend on them

# Initialize constants lists
IntlUnit <- list() # Unit symbols
IntlConv <- list() # Conversions
IntlNatu <- list() # Natural constants


# UNITS
# Base unit symbols
    IntlUnit$Base$Symb = list(
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

# Base unit types (corresponding to entries in IntlUnit$Base$Symb). These types are used to constrain 
    # conversions between variables (except for mol to/from mass units, conversion is only allowed
    # within the same unit type). Unit types are also used to find the eddy4R internal units
    # (listed in IntlUnit$Intl).
# Make sure string entries are Title case.
    IntlUnit$Base$Type = list(
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
    IntlUnit$Prfx = list(
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
    IntlUnit$Spcs = c(
      "C",
      "Co2",
      "H2o",
      "Ch4",
      "Dry",
      "NO",
      "NO2",
      "HONO",
      "O3",
      "CO",
      "CH3OH",
      "C2H4O",
      "C2H5OH",
      "C2H6",
      "C3H6O",
      "C3H8",
      "C4H6O",
      "C4H8O",
      "C4H10",
      "C5H8",
      "C5H10O",
      "C5H12O",
      "C6H12O",
      "C6H6",
      "C7H8",
      "C8H10",
      "C9H12"
      )
      
# Define internal units. List names should be selected from the entries in IntlUnit$Base$Type.
# List values should be symbols selected from the values in IntlUnit$Base$Symb, with applicable 
# prefixes selected from the entries in IntlUnit$Prfx
    IntlUnit$Intl = list(
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
    IntlConv$ExaNone <- c(0,1e18) 
    IntlConv$NoneExa <- c(0,1e-18) 
    IntlConv$PetaNone <- c(0,1e15) 
    IntlConv$NonePeta <- c(0,1e-15) 
    IntlConv$TeraNone <- c(0,1e12) 
    IntlConv$NoneTera <- c(0,1e-12) 
    IntlConv$GigaNone <- c(0,1e9) 
    IntlConv$NoneGiga <- c(0,1e-9) 
    IntlConv$MegaNone <- c(0,1e6) 
    IntlConv$NoneMega <- c(0,1e-6) 
    IntlConv$KiloNone <- c(0,1e3) 
    IntlConv$NoneKilo <- c(0,1e-3) 
    IntlConv$HectNone <- c(0,1e2) 
    IntlConv$NoneHect <- c(0,1e-2) 
    IntlConv$DecaNone <- c(0,1e1) 
    IntlConv$NoneDeca <- c(0,1e-1) 
    IntlConv$DeciNone <- c(0,1e-1) 
    IntlConv$NoneDeci <- c(0,1e1) 
    IntlConv$CntiNone <- c(0,1e-2) 
    IntlConv$NoneCnti <- c(0,1e2) 
    IntlConv$MiliNone <- c(0,1e-3) 
    IntlConv$NoneMili <- c(0,1e3) 
    IntlConv$MicrNone <- c(0,1e-6) 
    IntlConv$NoneMicr <- c(0,1e6) 
    IntlConv$NanoNone <- c(0,1e-9) 
    IntlConv$NoneNano <- c(0,1e9) 
    IntlConv$PicoNone <- c(0,1e-12) 
    IntlConv$NonePico <- c(0,1e12) 
    IntlConv$FemtNone <- c(0,1e-15) 
    IntlConv$NoneFemt <- c(0,1e15) 
    IntlConv$AttoNone <- c(0,1e-18) 
    IntlConv$NoneAtto <- c(0,1e18) 
    
    
# Time
    IntlConv$ScndMint = c(0,1/60) # [a0 a1] convert seconds to minutes [min s-1]
    IntlConv$ScndHour = c(0,1/60/60) # [a0 a1] convert seconds to hours [h s-1]
    IntlConv$ScndDay = c(0,1/60/60/24) # [a0 a1] convert seconds to days [d s-1]
    IntlConv$ScndYear = c(0,1/60/60/24/365.25) # [a0 a1] convert seconds to years [y s-1]
    IntlConv$MintScnd = c(0,60) # [a0 a1] convert minutes to seconds [s min-1]
    IntlConv$MintHour = c(0,1/60) # [a0 a1] convert minutes to hours [hr min-1]
    IntlConv$MintDay = c(0,1/60/24) # [a0 a1] convert minutes to days [d min-1]
    IntlConv$MintYear = c(0,1/60/24/365.25) # [a0 a1] convert minutes to years [y min-1]
    IntlConv$HourScnd = c(0,60*60) # [a0 a1] convert hours to seconds [s h-1]
    IntlConv$HourMint = c(0,60) # [a0 a1] convert hours to minutes [min h-1]
    IntlConv$HourDay = c(0,1/24) # [a0 a1] convert hours to days [d h-1]
    IntlConv$HourYear = c(0,1/24/365.25) # [a0 a1] convert hours to years [y h-1]
    IntlConv$DayScnd = c(0,60*60*24) # [a0 a1] convert days to seconds [s d-1]
    IntlConv$DayMint = c(0,60*24) # [a0 a1] convert days to minutes [min d-1]
    IntlConv$DayHour = c(0,24) # [a0 a1] convert days to hours [h d-1]
    IntlConv$DayYear = c(0,1/365.25) # [a0 a1] convert days to years [y d-1]
    IntlConv$YearScnd = c(0,60*60*24*365.25) # [a0 a1] convert years to seconds [s y-1]
    IntlConv$YearMint = c(0,60*24*365.25) # [a0 a1] convert years to minutes [min y-1]
    IntlConv$YearHour = c(0,24*365.25) # [a0 a1] convert years to hours [h y-1]
    IntlConv$YearDay = c(0,365.25) # [a0 a1] convert years to days [d y-1]
    
# Distance

    IntlConv$MetrInch = c(0,39.3701) # [a0 a1] convert meters to inches [inch m-1]
    IntlConv$InchMetr = c(0,1/39.3701) # [a0 a1] convert inches to meters [m inch-1]
    IntlConv$MetrFeet = c(0,3.28084) # [a0 a1] convert meters to feet [ft m-1]
    IntlConv$FeetMetr = c(0,0.3048) # [a0 a1] convert feet to meters [m ft-1]
    IntlConv$MetrMile = c(0,1/1609.34) # [a0 a1] convert meters to statute miles [mi m-1]
    IntlConv$MileMetr = c(0,1609.34) # [a0 a1] convert statute miles to meters [m mi-1]
    IntlConv$MetrMileNaut = c(0,1/1852) # [a0 a1] convert meters to nautical miles [NM m-1]
    IntlConv$MileNautMetr = c(0,1852) # [a0 a1] convert nautical miles to meters [m NM-1]
    IntlConv$InchFeet = c(0,1/12) # [a0 a1] convert inches to feet [ft inch-1]
    IntlConv$FeetInch = c(0,12) # [a0 a1] convert feet to inches [inch ft-1]
    IntlConv$InchMile = c(0,1/12/5280) # [a0 a1] convert inches to statute miles [mi inch-1]
    IntlConv$MileInch = c(0,12*5280) # [a0 a1] convert statute miles to inches [inch mi-1]
    IntlConv$InchMileNaut = c(0,1/12/6076.11549) # [a0 a1] convert inches to nautical miles [NM inch-1]
    IntlConv$MileNautInch = c(0,12*6076.11549) # [a0 a1] convert nautical miles to inches [inch NM-1]
    IntlConv$FeetMile = c(0,1/5280) # [a0 a1] convert feet to statute miles [mi ft-1]
    IntlConv$MileFeet = c(0,5280) # [a0 a1] convert statute miles to feet [ft mi-1]
    IntlConv$FeetMileNaut = c(0,1/6076.11549) # [a0 a1] convert feet to nautical miles [NM ft-1]
    IntlConv$MileNautFeet = c(0,6076.11549) # [a0 a1] convert nautical miles to feet [ft NM-1]
    IntlConv$MileMileNaut = c(0,1/1.15077945) # [a0 a1] convert statute miles to nautical miles [NM mi-1]
    IntlConv$MileNautMile = c(0,1.15077945) # [a0 a1] convert nautical miles to statute miles [mi NM-1]
    
# Mass
    IntlConv$GramPnd <- c(0,1/453.592) # [a0 a1] convert grams to pounds [lb g-1]
    IntlConv$PndGram <- c(0,453.592) # [a0 a1] convert pounds to grams [g lb-1]
    IntlConv$GramTonUs <- c(0,1/453.592/2000) # [a0 a1] convert grams to US tons [ST g-1]
    IntlConv$TonUsGram <- c(0,453.592*2000) # [a0 a1] convert US tons to grams [g ST-1]
    IntlConv$GramTon <- c(0,1e-6) # [a0 a1] convert grams to metric tons [t g-1]
    IntlConv$TonGram <- c(0,1e6) # [a0 a1] convert metric tons to grams [g t-1]
    IntlConv$PndTonUs <- c(0,1/2000) # [a0 a1] convert pounds to US tons [ST lb-1]
    IntlConv$TonUsPnd <- c(0,2000) # [a0 a1] convert US tons to pounds [lb ST-1]
    IntlConv$PndTon <- c(0,4.53592e-4) # [a0 a1] convert pounds to metric tons [t lb-1]
    IntlConv$TonPnd <- c(0,2204.624) # [a0 a1] convert metric tons to pounds [lb t-1]
    IntlConv$TonUsTon <- c(0,0.907185) # [a0 a1] convert US tons to metric tons [t ST-1]
    IntlConv$TonTonUs <- c(0,1.10231) # [a0 a1] convert metric tons to US tons [ST t-1]
    
# Pressure
    IntlConv$PascBar <- c(0,1e-5) # [a0 a1] convert Pascal to bar [bar Pa-1]
    IntlConv$BarPasc <- c(0,1e5) # [a0 a1] convert bar to Pascal [Pa bar-1]
    IntlConv$PascAtm <- c(0,9.8692e-6) # [a0 a1] convert Pascal to atm [atm Pa-1]
    IntlConv$AtmPasc <- c(0,101325) # [a0 a1] convert atm to Pascal [Pa atm-1]
    IntlConv$PascTorr <- c(0,760/101325) # [a0 a1] convert Pascal to Torr [Torr Pa-1]
    IntlConv$TorrPasc <- c(0,101325/760) # [a0 a1] convert torr to Pascal [Pa Torr-1]
    IntlConv$PascPsi <- c(0,0.0001450377) # [a0 a1] convert Pascal to psi [psi Pa-1]
    IntlConv$PsiPasc <- c(0,6894.8) # [a0 a1] convert psi to Pascal [Pa psi-1]
    IntlConv$BarAtm <- c(0,0.98692) # [a0 a1] convert bar to atm [atm bar-1]
    IntlConv$AtmBar <- c(0,1.01325) # [a0 a1] convert atm to bar [bar atm-1]
    IntlConv$BarTorr <- c(0,750.06) # [a0 a1] convert bar to Torr [Torr bar-1]
    IntlConv$TorrBar <- c(0,0.001333224) # [a0 a1] convert Torr to bar [bar Torr-1]
    IntlConv$BarPsi <- c(0,14.50377) # [a0 a1] convert bar to psi [psi bar-1]
    IntlConv$PsiBar <- c(0,0.068948) # [a0 a1] convert psi to bar [bar psi-1]
    IntlConv$AtmTorr <- c(0,760) # [a0 a1] convert atm to Torr [Torr atm-1]
    IntlConv$TorrAtm <- c(0,1/760) # [a0 a1] convert Torr to atm [atm Torr-1]
    IntlConv$AtmPsi <- c(0,14.69595) # [a0 a1] convert atm to psi [psi atm-1]
    IntlConv$PsiAtm <- c(0,0.068046) # [a0 a1] convert psi to atm [atm psi-1]
    IntlConv$TorrPsi <- c(0,0.01933678) # [a0 a1] convert Torr to psi [psi Torr-1]
    IntlConv$PsiTorr <- c(0,51.71493) # [a0 a1] convert psi to Torr [Torr psi-1]

# Angles
    IntlConv$RadDeg <- c(0,180/pi)		# [a0 a1] convert radians to degree [deg rad-1]
    IntlConv$DegRad <- c(0,pi/180)		# [a0 a1] convert degree to radians [rad deg-1]
    
# Percent/dimensionless
    IntlConv$PercRtio <- c(0,0.01)		# [a0 a1] convert percent to dimensionless ratio [[]  %-1]
    IntlConv$RtioPerc <- c(0,100)		  # [a0 a1] convert dimensionless ratio to percent [%  []-1]
    
# Temperature
    IntlConv$KelvCels <- c(-273.15,1)			# [a0 a1] conversion from °C to K
    IntlConv$CelsKelv <- c(273.15,1)	      # [a0 a1] conversion from K to °C
    IntlConv$KelvFrht <- c(-459.67,9/5)			# [a0 a1] conversion from F to K
    IntlConv$FrhtKelv <- c(-32*5/9+273.15,5/9)	      # [a0 a1] conversion from K to F
    IntlConv$CelsFrht <- c(32,9/5)      # [a0 a1] convert Celcius to Fahrenheit
    IntlConv$FrhtCels <- c(-32*5/9,5/9) # [a0 a1] convert Fahrenheit to Celcius  

    
# Force
    IntlConv$NewtPndf <- c(0,0.224809) # [a0 a1] convert Newton to pound-force [lbf N-1]
    IntlConv$PndfNewt <- c(0,4.44822)  # [a0 a1] convert pound-force to Newton [N lbf-1]
    
# NATURAL CONSTANTS
#EARTH BODY
    IntlNatu$Grav <-	9.81				#gravitational acceleration [m s-2]
      attr(IntlNatu$Grav,"unit") <- "m s-2"
    IntlNatu$PrdErth <- 86164.1	        #rotation period of the Earth (one sidereal day) [s]
      attr(IntlNatu$PrdErth,"unit") <- "s"
    IntlNatu$AvelErth <- 2 * pi / IntlNatu$PrdErth   #angular speed of earth [rad s-1]
      attr(IntlNatu$AvelErth,"unit") <- "rad s-1"
    
#THERMODYNAMICS
  #standard atmosphere
    IntlNatu$Pres00 <- 101325			# NIST standard pressure [Pa == kg m-1 s-2]
      attr(IntlNatu$Pres00,"unit") <- "Pa"
    IntlNatu$Temp00 <- 293.15     # NIST standard temperature [K] 
      attr(IntlNatu$Temp00,"unit") <- "K"
    IntlNatu$Rg <- 8.314462175	# ideal gas constant [J mol-1 K-1 == kg m2 s-2 mol-1 K-1]
      attr(IntlNatu$Rg,"unit") <- "J mol-1 K-1"
    IntlNatu$VonkFokn <- 0.40				#von-Karman constant accordig to Foken (2008) [-]
      attr(IntlNatu$VonkFokn,"unit") <- "-"
    
  #molar masses
    IntlNatu$MolmDry <- 28.97e-3			#dry air [kg mol-1]
      attr(IntlNatu$MolmDry,"unit") <- "kgDry mol-1"
    IntlNatu$MolmH2o <- 18.02e-3			#water vapor [kg mol-1]
      attr(IntlNatu$MolmH2o,"unit") <- "kgH2o mol-1"
    IntlNatu$MolmCo2 <- 44.01e-3			#co2 [kg mol-1]
      attr(IntlNatu$MolmCo2,"unit") <- "kgCo2 mol-1"
    IntlNatu$MolmCh4 <- 16.04e-3		#CH4 [kg mol-1]
      attr(IntlNatu$MolmCh4,"unit") <- "kgCh4 mol-1"
    IntlNatu$MolmC <- 12e-3          # C [kg mol-1]
      attr(IntlNatu$MolmC,"unit") <- "kgC mol-1"
    IntlNatu$RtioMolmH2oDry <- IntlNatu$MolmH2o / IntlNatu$MolmDry		#molar mass ratio water vapour / dry air
      attr(IntlNatu$RtioMolmH2oDry,"unit") <- "kgH2o kgDry-1"
    IntlNatu$RtioMolmDryH2o <- IntlNatu$MolmDry / IntlNatu$MolmH2o		#molar mass ratio dry air / water vapour
      attr(IntlNatu$RtioMolmDryH2o,"unit") <- "kgDry kgH2o-1"
    
    #University of York Chemistry molar masses
    IntlNatu$MolmNO <- 30.01e-3			#NO [kg mol-1]
      attr(IntlNatu$MolmNO,"unit") <- "kgNO mol-1"
    IntlNatu$MolmNO2 <- 46.01e-3			#NO2 [kg mol-1]
      attr(IntlNatu$MolmNO2,"unit") <- "kgNO2 mol-1"
    IntlNatu$MolmHONO <- 47.01e-3			#HONO [kg mol-1]
      attr(IntlNatu$MolmHONO,"unit") <- "kgHONO mol-1"
    IntlNatu$MolmO3 <- 48.00e-3			#O3 [kg mol-1]
      attr(IntlNatu$MolmO3,"unit") <- "kgO3 mol-1"
    IntlNatu$MolmCO <- 28.01e-3			#CO [kg mol-1]
      attr(IntlNatu$MolmCO,"unit") <- "kgCO mol-1"
    IntlNatu$MolmCH3OH <- 32.04e-3			#CH3OH [kg mol-1]
      attr(IntlNatu$MolmCH3OH,"unit") <- "kgCH3OH mol-1"
    IntlNatu$MolmC2H4O <- 44.05e-3			#C2H4O [kg mol-1]
      attr(IntlNatu$MolmC2H4O,"unit") <- "kgC2H4O mol-1"
    IntlNatu$MolmC2H5OH <- 46.07e-3			#C2H5OH [kg mol-1]
      attr(IntlNatu$MolmC2H5OH,"unit") <- "kgC2H5OH mol-1"
    IntlNatu$MolmC2H6 <- 30.07e-3			#C2H6 [kg mol-1]
      attr(IntlNatu$MolmC2H6,"unit") <- "kgC2H6 mol-1"
    IntlNatu$MolmC3H6O <- 58.08e-3			#C3H6O [kg mol-1]
      attr(IntlNatu$MolmC3H6O,"unit") <- "kgC3H6O mol-1"
    IntlNatu$MolmC3H8 <- 44.10e-3			#C3H8 [kg mol-1]
      attr(IntlNatu$MolmC3H8,"unit") <- "kgC3H8 mol-1"
    IntlNatu$MolmC4H6O <- 70.09e-3			#C4H6O [kg mol-1]
      attr(IntlNatu$MolmC4H6O,"unit") <- "kgC4H6O mol-1"
    IntlNatu$MolmC4H8O <- 71.22e-3			#C4H8O [kg mol-1]
      attr(IntlNatu$MolmC4H8O,"unit") <- "kgC4H8O mol-1"
    IntlNatu$MolmC4H10 <- 58.12e-3			#C4H10 [kg mol-1]
      attr(IntlNatu$MolmC4H10,"unit") <- "kgC4H10 mol-1"
    IntlNatu$MolmC5H8 <- 68.12e-3			#C5H8 [kg mol-1]
      attr(IntlNatu$MolmC5H8,"unit") <- "kgC5H8 mol-1"
    IntlNatu$MolmC5H10O <- 86.13e-3			#C5H10O [kg mol-1]
      attr(IntlNatu$MolmC5H10O,"unit") <- "kgC5H10O mol-1"
    IntlNatu$MolmC5H12O <- 88.15e-3			#C5H12O [kg mol-1]
      attr(IntlNatu$MolmC5H12O,"unit") <- "kgC5H12O mol-1"
    IntlNatu$MolmC6H12O <- 100.16e-3			#C6H12O [kg mol-1]
      attr(IntlNatu$MolmC6H12O,"unit") <- "kgC6H12O mol-1"
    IntlNatu$MolmC6H6 <- 78.11e-3			#C6H6 [kg mol-1]
      attr(IntlNatu$MolmC6H6,"unit") <- "kgC6H6 mol-1"
    IntlNatu$MolmC7H8 <- 92.14e-3			#C7H8 [kg mol-1]
      attr(IntlNatu$MolmC7H8,"unit") <- "kgC7H8 mol-1"
    IntlNatu$MolmC8H10 <- 106.16e-3			#C8H10 [kg mol-1]
      attr(IntlNatu$MolmC8H10,"unit") <- "kgC8H10 mol-1"
    IntlNatu$MolmC9H12 <- 120.19e-3			#C9H12 [kg mol-1]
      attr(IntlNatu$MolmC9H12,"unit") <- "kgC9H12 mol-1"
      
    
  #dry air (Dry)
    IntlNatu$CpDry <- 1004.64		#dry air specific heat at constant pressure 	[J kg-1 K-1]
      attr(IntlNatu$CpDry,"unit") <- "J kg-1 K-1"
    IntlNatu$CvDry <- 717.6		#dry air specific heat at constant volume	[J kg-1 K-1]
      attr(IntlNatu$CvDry,"unit") <- "J kg-1 K-1"
    IntlNatu$RsDry <- IntlNatu$CpDry - IntlNatu$CvDry		#specific gas constant for dry air		[J kg-1 K-1]
      attr(IntlNatu$RsDry,"unit") <- "J kg-1 K-1"
    IntlNatu$GmmaDry <- IntlNatu$CpDry / IntlNatu$CvDry	# Ratio of specific heat of dry air at contant pressure to specific heat of dry air at contant volume [-]
      attr(IntlNatu$GmmaDry,"unit") <- "-"
    IntlNatu$KppaDry <- IntlNatu$RsDry / IntlNatu$CpDry		#Kappa exponent for ideal gas law (Poisson)	[-]
      attr(IntlNatu$KppaDry,"unit") <- "-"
    
  #water vapor (H2o)
    IntlNatu$CpH2o <- 1846			#water vapor specific heat at constant pressure [m2 K-1 s-2]
      attr(IntlNatu$CpH2o,"unit") <- "m2 K-1 s-2"
    IntlNatu$CvH2o <- 1384.04		#water vapor specific heat at constant volume	[m2 K-1 s-2]
      attr(IntlNatu$CvH2o,"unit") <- "m2 K-1 s-2"
    IntlNatu$RsH2o <- IntlNatu$CpH2o - IntlNatu$CvH2o		#specific gas constant for water vapor		[m2 K-1 s-2]
      attr(IntlNatu$RsH2o,"unit") <- "m2 K-1 s-2"
    IntlNatu$GmmaH2o <- IntlNatu$CpH2o / IntlNatu$CvH2o # Ratio of specific heat of water vapour at contant pressure to specific heat of water vapour at contant volume [-]
      attr(IntlNatu$GmmaH2o,"unit") <- "-"
    IntlNatu$KppaH2o <- IntlNatu$RsH2o / IntlNatu$CpH2o		#Kappa exponent for ideal gas law (Poisson)	[-]
      attr(IntlNatu$KppaH2o,"unit") <- "-"
    
# Save internal constants and conversion factors as .rda files within /data
devtools::use_data(IntlUnit,IntlNatu,IntlConv,pkg=paste0(dirPack,"/",namePack),overwrite=TRUE) 
