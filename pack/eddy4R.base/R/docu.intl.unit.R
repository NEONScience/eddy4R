#' Internal data: Parameter set: Unit representations
#'
#' Unit base symbols, types, prefixes, associated chemical species, and definition of the 
#' internal units used in the eddy4R family of packages. \cr
#' \cr
#' The unit representations below are used to construct character strings describing variable units.
#' Unit character strings are constructed following these rules: \cr
#' \cr
#' The (case-sensitive) unit base symbol is selected from those listed 
#' in IntlUnit$Base$Symb (e.g. the base symbol for meter is "m"). \cr
#' \cr
#' Unit base symbols can be directly preceded (no space) by the (case-sensitive) unit prefixes listed in 
#' IntlUnit$Prfx (e.g. kilometers = "km") . \cr
#'  \cr
#' Unit base symbols can be directly followed (no spaces) by the suffix \code{n}, where \code{n} is 
#' an integer (...-2,-1,0,1,2...), indicating the unit is raised to the power of 
#' \code{n} (e.g. per square kilometer = "km-2").  \cr
#' \cr
#' In the case of chemical species attached to a unit, specify
#' the full unit (including prefix and suffix) followed immediately (no spaces) by one of 
#' the chemical species character strings in IntlUnit$Spcs 
#' (eg. per gram of carbon dioxide = "g-1Co2"): \cr
#' \cr
#' Compound units can be formed by inserting spaces between the individual unit components 
#' (ex.milligrams carbon per meter squared per day = "mgC m-2 d-1").\cr
#' 
#' 
#'
#' @format A nested list of named unit categories:
#' \describe{
#'   \item{Base}{
#'    \describe{
#'      \item{Symb}{A named list of base unit (case-sensitive) character designations:\cr\cr
#'        \code{Metr = "m"}  (meter)\cr
#'        \code{Feet = "ft"}  (foot)\cr
#'        \code{Inch = "inch"}  (inch)\cr
#'        \code{Mile = "mi"}  (statute mile)\cr
#'        \code{MileNaut = "NM"} (nautical mile)\cr
#'        \code{Gram = "g"}  (gram)\cr
#'        \code{Pnd  = "lb"}  (pound-mass)\cr
#'        \code{Ton  = "t"}  (metric ton)\cr
#'        \code{TonUs = "ST"}  (US/short ton)\cr
#'        \code{Scnd = "s"}  (second)\cr
#'        \code{Mint = "min"}  (minute)\cr
#'        \code{Hour = "h"}  (hour)\cr
#'        \code{Day  = "d"}  (day)\cr
#'        \code{Year = "y"}  (year)\cr
#'        \code{Kelv = "K"}  (Kelvin)\cr
#'        \code{Cels = "C"}  (degrees Celcius)\cr
#'        \code{Frht = "F"}  (degrees Fahrenheit)\cr
#'        \code{Mole = "mol"}  (mole)\cr
#'        \code{Rad  = "rad"}  (radian)\cr
#'        \code{Deg  = "deg"}  (degree - geometry)\cr
#'        \code{Ampr = "A"}  (ampere)\cr
#'        \code{Volt = "V"}  (volt)\cr
#'        \code{Ohm  = "ohm"}  (ohm)\cr
#'        \code{Pasc = "Pa"}  (Pascal)\cr
#'        \code{Bar  = "bar"}  (bar)\cr
#'        \code{Atm  = "atm"}  (standard atmosphere)\cr
#'        \code{Torr = "Torr"}  (torr)\cr
#'        \code{Psi  = "psi"}  (pound-force per square inch)\cr
#'        \code{Watt = "W"}  (watt)\cr
#'        \code{Joul = "J"}  (joule)\cr
#'        \code{Newt = "N"}  (newton)\cr
#'        \code{Pndf = "lbf"}  (pound-force)\cr
#'        \code{Hrtz = "Hz"}  (hertz)\cr
#'        }
#'      \item{Type}{A named list of the unit type corresponding to entries in IntlUnit$Base$Symb.
#'      Unit types are used to constrain conversions between variables (except for mol to/from 
#'      mass units, conversion is only allowed within the same unit type). Unit types are also 
#'      used to find the eddy4R internal units (listed in IntlUnit$Intl):\cr\cr
#'        \code{Metr = "Dist"}  (distance)\cr
#'        \code{Feet = "Dist"}  (distance)\cr
#'        \code{Inch = "Dist"}  (distance)\cr
#'        \code{Mile = "Dist"}  (distance)\cr
#'        \code{MileNaut = "Dist"} (distance)\cr
#'        \code{Gram = "Mass"}  (mass)\cr
#'        \code{Pnd  = "Mass"}  (mass)\cr
#'        \code{Ton  = "Mass"}  (mass)\cr
#'        \code{TonUs = "Mass"}  (mass)\cr
#'        \code{Scnd = "Time"}  (time)\cr
#'        \code{Mint = "Time"}  (time)\cr
#'        \code{Hour = "Time"}  (time)\cr
#'        \code{Day  = "Time"}  (time)\cr
#'        \code{Year = "Time"}  (time)\cr
#'        \code{Kelv = "Temp"}  (temperature)\cr
#'        \code{Cels = "Temp"}  (temperature)\cr
#'        \code{Frht = "Temp"}  (temperature)\cr
#'        \code{Mole = "Num"}  (number)\cr
#'        \code{Rad  = "Ang"}  (angle)\cr
#'        \code{Deg  = "Ang"}  (angle)\cr
#'        \code{Ampr = "Curr"}  (electrical current)\cr
#'        \code{Volt = "Epot"}  (electrical potential)\cr
#'        \code{Ohm  = "Eres"}  (electrical resistance)\cr
#'        \code{Pasc = "Pres"}  (pressure)\cr
#'        \code{Bar  = "Pres"}  (pressure)\cr
#'        \code{Atm  = "Pres"}  (pressure)\cr
#'        \code{Torr = "Pres"}  (pressure)\cr
#'        \code{Psi  = "Pres"}  (pressure)\cr
#'        \code{Watt = "Powr"}  (power)\cr
#'        \code{Joul = "Engy"}  (energy)\cr
#'        \code{Newt = "Forc"}  (force)\cr
#'        \code{Pndf = "Forc"}  (force)\cr
#'        \code{Hrtz = "Freq"}  (frequency)\cr
#'        }
#'      }
#'    }
#'   \item{Prfx}{A named list of unit prefix (case-sensitive) character designations:\cr\cr
#'    \code{Exa  = "E"}  (1e18)\cr
#'    \code{Peta = "P"}  (1e15)\cr
#'    \code{Tera = "T"}  (1e12)\cr
#'    \code{Giga = "G"}  (1e9)\cr
#'    \code{Mega = "M"}  (1e6)\cr
#'    \code{Kilo = "k"}  (1e3)\cr
#'    \code{Hect = "h"}  (1e2)\cr
#'    \code{Deca = "da"}  (1e1)\cr
#'    \code{Deci = "d"}  (1e-1)\cr
#'    \code{Cnti = "c"}  (1e-2)\cr
#'    \code{Mili = "m"}  (1e-3)\cr
#'    \code{Micr = "u"}  (1e-6)\cr
#'    \code{Nano = "n"}  (1e-9)\cr
#'    \code{Pico = "p"}  (1e-12)\cr
#'    \code{Femt = "f"}  (1e-15)\cr
#'    \code{Atto = "a"}  (1e-18)\cr
#'    }
#'  \item{Spcs}{A character vector of chemical species used in unit designations (not case-sensitive):\cr\cr
#'    \code{"C" (carbon)}\cr
#'    \code{"Co2" (carbon dioxide)}\cr
#'    \code{"H2o" (water vapor)}\cr
#'    \code{"Ch4" (methane)}\cr
#'    \code{"Dry" (dry air)}\cr
#'    }
#'   \item{Intl}{A named list of the units (including prefixes) used internally in the 
#'   eddy4R family of functions. List names are unit types selected from IntlUnit$Base$Type.
#'   List values are unit base symbol and prefix character strings selected from 
#'   IntlUnit$Base$Symb and IntlUnit$Prfx, respectively. Only one unit per type is used internally:\cr\cr
#'    \code{Dist = "m"}\cr
#'    \code{Mass = "kg"}\cr
#'    \code{Time = "s"}\cr
#'    \code{Temp = "K"}\cr
#'    \code{Num  = "mol"}\cr
#'    \code{Ang  = "rad"}\cr
#'    \code{Curr = "A"}\cr
#'    \code{Epot = "V"}\cr
#'    \code{Eres = "ohm"}\cr
#'    \code{Pres = "Pa"}\cr
#'    \code{Powr = "W"}\cr
#'    \code{Engy = "J"}\cr
#'    \code{Forc = "N"}\cr
#'    \code{Freq = "Hz"}\cr
#'    }
#' }
#' @source Units are defined within flow.save.intl.cnst.R,
#'  available in the data-raw/ folder of the source version of the package
"IntlUnit"
