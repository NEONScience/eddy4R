#####
# Written by John Frank, USDA Forest Service, 240 W. Prospect Rd., Fort Collins, CO 80526, 970-498-1319, john.frank@usda.gov
# This code reads in a 10/20 Hz time series file and applies both the Wyngaard and Zhang (1985) shadowing correction and the "Self/Cross/Top" shadowing correction.
# The Wyngaard and Zhang correction is applied to the CSAT3 as described by Horst, Semmer, and Maclean (2015)
# The "Self/Cross/Top" shadowing correction was created by John Frank (2025) by reevaluating the data and analysis of Frank, Massman, and Ewers (2016) to account for CSAT3 transducer self-shadowing, transducer cross-shadowing, and top/bottom shadowing (the original formulation also included supporting arm shadowing, but it is omitted here due to its lack of significance).

#------------------------------------------------------------------------------
rm(list=ls()) #clear all
library(abind)
options(scipen = 8)

#####
# Define matrices to convert transducer <-> sonic coordinates for CSAT3
H_StoT = matrix(c(1/4,0.433012701892219,0.866025403784439,-1/2,0,0.866025403784439,1/4,-0.433012701892219,0.866025403784439),nrow=3,ncol=3)
G_TtoS = matrix(c(2/3,-4/3,2/3,1.154700538379252,0,-1.154700538379252,0.384900179459751,0.384900179459751,0.384900179459751),nrow=3,ncol=3)

V_top = c(0,0,1) # unit vector for the top of the CSAT3

# Model parameters for the self/cross/top shadowing correction
d0 = c(0.00159718301425092,0.0113301152734556,0.0633033802230689)
d1 = c(29.2463049972615,24.8602835257800,18.5970206839229)
d2 = c(0.836978194375597,0.874138513424717,0.809209610043660)
d3 = 1.06606236917833

#####
# Read in time series data from each 30-minute data file
fname_in = sprintf('g:\\Studies\\Manuscripts\\2015\\Sonic Shadowing\\Analysis\\Beltsville, MD, OPE3\\Time series data\\TOA5_1580.ts_corn_2014_07_16_0000.dat')
TSData <- list()
TSData[[1]] <-  as.data.frame(read.table(fname_in,sep = ",",skip=4,na.strings = c(".","NAN")))
N_TSData = nrow(TSData[[1]]) # Number of rows of time series data in each file

u <- abind(lapply(TSData,"[",3),along=1)
v <- abind(lapply(TSData,"[",4),along=1)
w <- abind(lapply(TSData,"[",5),along=1)

Us = matrix(c(u,v,w),nrow = length(u)) # Matrix of time series data in sonic coordinates
Ut = Us %*% H_StoT # Matrix of time series data in transducer coordinates

#####
# Calculate the angle (theta) between the 3-D wind and the 3 transducers plus the top/bottom (i.e. support structure)
Us_uv = sqrt(Us[,1]^2 + Us[,2]^2)
Us_uvw = sqrt(Us[,1]^2 + Us[,2]^2 + Us[,3]^2)

theta_transducer = matrix(0,N_TSData,3)
theta_top = matrix(0,N_TSData,1)
for (i in 1:N_TSData) {
  theta_transducer[i,1] = acos(abs(( H_StoT[,1] %*% Us[i,]) / Us_uvw[i]))*180/pi
  theta_transducer[i,2] = acos(abs(( H_StoT[,2] %*% Us[i,]) / Us_uvw[i]))*180/pi
  theta_transducer[i,3] = acos(abs(( H_StoT[,3] %*% Us[i,]) / Us_uvw[i]))*180/pi
  theta_top[i,1] = acos(abs(( V_top %*% Us[i,]) / Us_uvw[i]))*180/pi
}

#####
# Apply the Wyngaard and Zhang sinusoidal correction
c_Wyngaard = 1./(0.84 + 0.16*sin(theta_transducer*pi/180))
Ut_Wyngaard = Ut*c_Wyngaard
Us_Wyngaard = Ut_Wyngaard %*% G_TtoS # Matrix of time series data in sonic coordinates with Wyngaard and Zhang sinusoidal correction

#####
# Apply the self/cross/top shadowing correction

# define self/cross/top shadowing correction function
f_c_self_cross_top = function(d0,d1,d2,d3){
  err_transducer_self = (1-d2[1])*tanh(d0[1]*(theta_transducer-d1[1]))/tanh(d0[1]*(90-d1[1]))+d2[1]
  c_transducer_self = 1/err_transducer_self
  
  err_transducer_cross = (1-d2[2])*tanh(d0[2]*(theta_transducer-d1[2]))/tanh(d0[2]*(90-d1[2]))+d2[2]
  c_transducer_cross = 1/err_transducer_cross
  
  err_top = (1-d2[3])*tanh(d0[3]*(theta_top-d1[3]))/tanh(d0[3]*(90-d1[3]))+d2[3]
  c_top = 1/err_top
  
  c_self_cross_top = matrix(0,N_TSData,3)
  c_self_cross_top[,1] = ((c_transducer_self[,1] - 1)^8 + (c_transducer_cross[,2] - 1)^8 + (c_transducer_cross[,3] - 1)^8 + (c_top - 1)^8)^(1/8) + 1
  c_self_cross_top[,2] = ((c_transducer_cross[,1] - 1)^8 + (c_transducer_self[,2] - 1)^8 + (c_transducer_cross[,3] - 1)^8 + (c_top - 1)^8)^(1/8) + 1
  c_self_cross_top[,3] = ((c_transducer_cross[,1] - 1)^8 + (c_transducer_cross[,2] - 1)^8 + (c_transducer_self[,3] - 1)^8 + (c_top - 1)^8)^(1/8) + 1
  c_self_cross_top = c_self_cross_top/d3 # scale this correction to minimize change in horizontal wind relative to the Wyngaard correction
  
  return(c_self_cross_top)
}

c_self_cross_top = f_c_self_cross_top(d0,d1,d2,d3)
Ut_self_cross_top = Ut*c_self_cross_top
Us_self_cross_top = Ut_self_cross_top %*% G_TtoS # Matrix of time series data in sonic coordinates with self/cross/top shadowing correction
