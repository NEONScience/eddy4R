##############################################################################################
#' @title Fast Fourier transform

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Fast Fourier transform.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

##the transform can be inverted (use for sensor fusion?)
#  spec.inv <- Re(mvfft(spec.fft, inverse = T))

########################################################
#FAST FOURIER TRANSFORM
########################################################
SPEC.fwd <- function(
  tstamp=POST$UT,
  data,
  relmot=POST$veloTAmagn,
  height=POST$lengMCzLS,
  fre=freq,
  demean=F,
  detrend=T,
  taper=0.05)
{
#in:
  #tstamp: continuous timestamp in any float format; only for gapfilling
  #data: continuous matrix of variables to be transformed with same length as tstamp; NAs are interpolated
  #relmot: relative motion between observation platform and atmosphere with same length as tstamp; NAs are interpolated. E.g. |wind vector| for tower observation [m s-1]
  #height: measurement height, either single number or vector of same length as tstamp
  #fre: observation frequency: single integer [Hz]
  #demean: demean the data?
  #detrend: detrend the data?
  #taper: tapter the data for a fraction (<=0.5) at both ends, or F
#out:
  #transformation of variables in frequency space and calculation of various independent variables
  #sorted from lowest to highest frequency

#combine variables
  datadum <- cbind(data, height, relmot)

#gap filling
  for(i in 1:ncol(datadum)) {
    dummy <- approx(tstamp, datadum[,i], xout = tstamp)[[2]]	#linearly interpolate gaps
#    dummy <- dummy[2:(length(dummy)-1)]				#cut out potentially remaining NAs at the end points
    if (i == 1) {
      spec.in <- as.ts(dummy, frequency = fre)
    } else {
      spec.in <- ts.union(spec.in, as.ts(dummy, frequency = fre))
    }
  }

#cut out potentially remaining NAs at the end points
  spec.in <- ts.union(as.ts(tstamp, frequency = fre), spec.in)
  spec.in <- na.omit(spec.in)
  tstamp <- spec.in[,1]
  spec.in <- spec.in[,-1]
  
#separate variables
  height <- spec.in[,(ncol(spec.in)-1)]
  relmot <- spec.in[,ncol(spec.in)]
  spec.in <- as.matrix(spec.in[,1:(ncol(spec.in)-2)])
  dimnames(spec.in)[[2]] <- dimnames(data)[[2]]

#demeaning
if(demean == T & detrend == F) {
  for(i in 1:ncol(spec.in)) {
    spec.in[,i] <- spec.in[,i] - mean(spec.in[,i], na.rm=T)
  }
}

#detrending
#http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Before you estimate a power spectrum, trends should be removed. Rapid falloff in the spectrum makes the estimates very unreliable (bias is a problem). The spectrum() command removes linear trends by default if you omit detrend=F.
if(detrend == T) {
  for(i in 1:ncol(spec.in)) {
    spec.in[,i] <- spec.in[,i] - lm(spec.in[,i]~tstamp)$fitted.values
    #plot(spec.in[,1]~I(tstamp[2:(length(tstamp)-1)]), type="l")
    #abline(h=0, col=4, lwd=3)
    #lines(test$fitted.values~I(tstamp[2:(length(tstamp)-1)]), col=2, lwd=3)
  }
}

#create time series
spec.in <- as.ts(spec.in, frequency = fre)

#tapering (omit for EC)
#http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Tapering reduces bias and makes peaks look sharper. By tapering you avoid having far away frequency peaks leak into other frequencies. The taper number has to be between 0 and 0.5. This is the fraction of data points that get down-weighted in the spectrum computation. Tapering reduces bias, increases variance, decreases the correlation between variables.
if(is.numeric(taper)) {
  spec.in <- spec.taper(x=spec.in, p=taper)
}

#independent variables                                                                                                                    
  #define relative frequency according to Stull
    fr <- (as.ts(1:attributes(spec.in)$dim[1], frequency = fre) - 1) / attributes(spec.in)$dim[1]
  #only 0 < frequency < Nyqvist
    fr_whr <- which(fr > 0 & fr <= 0.5)
  #corresponding 'observation' frequency
    fo <- fr * fre
  #corresponding wavelength according to taylor's hypothesis (mean relative motion, Foken, 2008 Eq. 2.106)
    la <- mean(relmot, na.rm=T) / fo
  #corresponding normalized frequency
    fn <- mean(height, na.rm=T) / la
  #corresponding wavenumber
    ka <- (2 * pi) / la

#dependent variables
  #fft returns unnormalized univariate Fourier transform of the sequence of values in z
  #therefore divide by length of time series
  #Note that for frequencies greater than .5 the Fourier transform is just the complex conjugate of the frequencies less than .5
    spec.fft <- mvfft(spec.in) / attributes(spec.in)$dim[1]

#unfolded spectral energy
  G <- Re(spec.fft * Conj(spec.fft))

#combine results
  export <- list(
    fr_whr,	#valid subset (0 < frequency < Nyqvist)
    fr,		#relative frequency (0...1) [-]
    fo,		#observation frequency (0 < frequency < fmax) [s-1]
    la,		#wavelength [m]
    fn,		#normalized frequency (fo * height / U) [-]
    ka,		#wavenumber (2 * pi * fo / U) [m-1]
    spec.in,	#continuous input data in time space (no NAs)
    spec.fft,	#variables in frequency space; complex numbers [input units]
    G		#unfolded spectral energy in frequency space[(input units)^2]
  )
  attributes(export)$names <- c("fr_whr", "fr_rel", "fr_obs", "wl_obs", "fr_nor", "wn_obs", "TScont", "FScomplex", "FSunfold")

#export results
  return(export)

########################################################
}
########################################################



##############################################################################################
#' @title Plot power spectra of up to three variables

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Plot power spectra of up to three variables.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT POWER SPECTRA OF UP TO THREE VARIABLES
########################################################
SPEC.plot <- function(name, IDE, DEP, legtext, labx=NA, laby=NA, colvec=NULL, limx=NULL, limy=NULL, setoff=NULL, powerc=NULL, inisub=c(0.05,Inf)) {
#name: (no default) output location and filename for plot
#IDE: (no default) one independent variable, e.g. frequencey, wavenumber...
#DEP: (no default) spectra of up to three dependent variables, same length as IDE
#legtext: (no default) one discription (character or expression) for each variable for the legend
#labx: (NA) description for abscissa
#laby: (NA) description for ordinate
#colvec: (NULL) colors for plotting, as many colors as dependent variables
#limx: (NULL) limits for abscissa
#limy: (NULL) limits for ordinate
#setoff: (NULL) offsets different variables from each other to improve legibility (only of powerc is set)
#powerc: (NULL) coefficient of the power law c("-5/3", "-2/3") for unweighted / weighted spectra
#inisub: (c(0.05,Inf)) frequency range of initial subrange, used to fit the power law

#defaults for color vector
  coln <- ncol(as.matrix(DEP))
  if(is.null(colvec)) colvec <- c(2:(coln+1))
  if(is.null(setoff)) setoff <- rep(1,coln)

#call graphics device
  png(filename=name, width=830*2, height=1170,pointsize=2000/50,bg = "white")
    cexvar=3; par(mfrow=c(1,3),las=0,cex.axis=cexvar*0.4,cex.lab=cexvar*0.4,font.lab=2, mar=c(4,4,2,2),mgp=c(2.8,0.8,0), family="times",lwd=cexvar*1,cex.main=cexvar*0.6)

  #plotting
    #pick variables
      ide <- IDE
      if(is.null(powerc)) dep <- as.matrix(DEP)[,1] else dep <- setoff[1] * as.matrix(DEP)[,1]

    #estimate slope for reference line
      if(!is.null(powerc)) {
	powercn <- as.integer(strsplit(powerc,"/")[[1]])
	powercn <- powercn[1] / powercn[2]
	whr <- which(ide >= inisub[1] & ide < inisub[2])
	lm_slope <- robustbase::lmrob(I(dep[whr]^(1/powercn)) ~ 0 + ide[whr])
	slope <- lm_slope$coefficients
	powerl <- (slope * ide)^powercn
      }

    #actual plotting
      plot(dep ~ ide, type="l", log="xy", xaxt="n", yaxt="n", xlim=limx, ylim=limy, xlab=labx, ylab=laby, col=colvec[1])
      if(!is.null(powerc)) lines(powerl ~ ide, col=1, lty=2) #, lwd=5

    #max. two additional variables
    if(coln > 1 & coln <= 3) {
      for (i in c(2:coln)) {
	#pick next dependend variable
	  if(is.null(powerc)) dep <- as.matrix(DEP)[,i] else dep <- setoff[i] * as.matrix(DEP)[,i]

	#estimate slope for reference line
	if(!is.null(powerc)) {
	  lm_slope <- robustbase::lmrob(I(dep[whr]^(1/powercn)) ~ 0 + ide[whr])
	  slope <- lm_slope$coefficients
	  powerl <- (slope * ide)^powercn
	}

	#actual plotting
	  lines(dep ~ ide, col=colvec[i])
	  if(!is.null(powerc)) lines(powerl ~ ide, col=1, lty=2) #, lwd=5
      }
    }
    sfsmisc::eaxis(side=1,labels=NA,las=0)
    sfsmisc::eaxis(side=2,labels=NA,las=0)
    if(is.null(powerc)) {
      legend(x="bottomleft", lty=rep(1,coln), bty="n", col=colvec, cex=cexvar*0.4, xjust = 0, yjust = 0, lwd=rep(par()$lwd,coln), legend=legtext)
    } else {
      legend(x="bottomleft", lty=c(rep(1,coln),2), bty="n", col=c(colvec, 1), cex=cexvar*0.4, xjust = 0, yjust = 0, lwd=c(rep(par()$lwd,coln),par()$lwd), legend=c(legtext, substitute(paste("f"^{powerc}, " law", sep=""), list(powerc=powerc))))
    }
    box()

#close graphics device
  dev.off()
########################################################
}
########################################################



##############################################################################################
#' @title Generate cospectra

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Generate cospectra.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#GENERATE COSPECTRA
########################################################
COSP.fwd <- function(FS, TS, cparho=1) {
#FS: frequency space data, raw spectra as complex numbers
#TS: time space data, identical variables and length as FS
#cparho: (1) single factor (specific heat at constant pressure * air density) for conversion from kinematic to energetic units; only used for comparison with Reynolds-Covariance, not for output

#calculate cospectra
  for(i in 2:ncol(FS)) {
    if(i == 2) {
      COSPdata <- Re(FS[,1]) * Re(FS[,i]) + Im(FS[,1]) * Im(FS[,i])
    } else {
      COSPdata <- cbind(COSPdata, Re(FS[,1]) * Re(FS[,i]) + Im(FS[,1]) * Im(FS[,i]))
    }
  }
  COSPdata <- as.matrix(COSPdata)
  if(ncol(COSPdata) == 1) {
    dimnames(COSPdata)[[2]] <- list(paste("Co(w'", dimnames(FS)[[2]][2:ncol(FS)], "')", sep=""))
  } else {
    dimnames(COSPdata)[[2]] <- paste("Co(w'", dimnames(FS)[[2]][2:ncol(FS)], "')", sep="")
  }

#the sum should be the same as the covariance:
  COSPsum <- sapply(1:ncol(COSPdata), function(x) sum(COSPdata[2:nrow(COSPdata),x], na.rm=T)) * cparho
  REYsum <- stats:::cov(TS)[1,c(2:ncol(TS))] * cparho
    names(COSPsum) <- names(REYsum) <- dimnames(COSPdata)[[2]]
  #relative difference
    DIFFperc <- (COSPsum - REYsum) / REYsum * 100

#generate output
  OUTsum <- rbind(COSPsum, REYsum, DIFFperc)
  export <- list(COSPdata, OUTsum)
    attributes(export)$names <- c("FScosp", "sum")

#return results
  return(export)
########################################################
}
########################################################



##############################################################################################
#' @title Plot cospectra

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Plot cospectra.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT COSPECTRA
########################################################
COSP.plot <- function(
  name,
  IDE,
  DEP,
  title,
  labx=NA,
  laby=NA,
  colvec=c(1,2,1),
  limx=NULL,
  limy=NULL,
  weight=T,
  wl=F,
  norm=NULL,
  part_cov=1,
  modspec=T,
  zeta,
  FX=NULL,
  devran=NULL,
  plotting=T
  ) {
#name: (no default) output location and filename for plot
#IDE: (no default) one independent variable, e.g. frequencey, wavenumber...
#DEP: (no default) spectra of up to three dependent variables, same length as IDE
#title: (no default) one discription (character or expression) for each variable for the title
#labx: (NA) description for abscissa
#laby: (NA) description for ordinate
#colvec: (c(1,2,1)) colors for plotting of measurement, model and evaluation range
#limx: (NULL) limits for abscissa
#limy: (NULL) limits for ordinate
#weight (T) weight the spectra for independent variable (wavelength / frequency)?
#wl: (F) is wavelength used as independent variable?
#norm: (NULL) normalizes sum(cospectrum) to a given quantity, e.g. unity or the total flux per variable, else NULL
#part_cov: (1) allows to plot measured cospectrum with scaling factor that is different from norm
#modspec: (T) shall model cospetra be calculated and plotted?
#zeta: (no default) atmospheric stability to choose the reference cospectrum
#FX: (NULL) frequency at which fCO(f) reaches its maximum value; will be determined from measured data if NULL
#devran: (NULL) range to assess sum((cospectrum - model)[whr]) / sum(model) [%]; only valid for equidistant data

#prepare variables
  ide <- IDE
  fs <- as.matrix(DEP)

#weighting of the cospectrum: divide for wavelenght, multiply for frequencies
  if(weight == T) {
    if(wl==T) fs <- as.matrix(fs / ide) else fs <- as.matrix(fs * ide)
  }

#normalize each cospectrum
  if(!is.null(norm)) {
    if(length(norm) == 1) norm <- rep(norm, ncol(fs))
    FSsum <- sapply(1:ncol(fs), function(x) sum(fs[,x], na.rm=TRUE))
    fs <- sapply(1:length(FSsum), function(x) as.matrix(fs[,x] / FSsum[x] * norm[x]))
  }

#call graphics device
  if(plotting == T) {
    
    png(filename=name, width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
    cexvar=3; par(mfrow=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
      mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
    
  }
    
##loop around variables
for(i in 1:ncol(fs)) {
##

  #Model cospectrum after Massman, 2005 (in Lee, 2005); continuous approximation of the Kaimal (1972) cospectra
    if(modspec == T) {
      #(inertial subrange) slope parameter; 3/4 for -7/3 (cospectra), and 3/2 for -5/3 (spectra) power law
      	m=3/4
      #broadness parameter; 1/2 for unstable, 7/6 for stable stratification 
      	if(zeta <= 0) mue=1/2  else mue=7/6
      #frequency at which fCO(f) reaches its maximum value
      	if(!is.null(FX)) fx=FX else fx=ide[which(fs[,i] == max(fs[,i]))]
      #calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
      	COmM <- (ide / fx) / (
      	  ( 1 + m * (ide / fx)^(2 * mue) )^( (1/(2*mue)) * ((m+1)/m) )
      	)
		if(weight == F) COmM <- COmM / ide
      #individual normalisation parameter for each variable
      	A0 <- sum(fs[,i], na.rm=T) / sum(COmM, na.rm=T)
      	COmM <- A0 * COmM
    }

  #Assessment of deviation(cospectrum - model) in given frequency range
    #only valid for equidistantly binned data; else multiply with bin width
    if(modspec == T & !is.null(devran)) {
      #range to assess    
        whr <- which(ide >= devran[1] & ide <= devran[2])
      #weighted or unweighted?
        if(weight == T) {
          
          if(wl==T) {
            fac <- 1 - ( sum(((fs[,i] - COmM) * ide)[whr], na.rm=T) / abs( sum(fs[,i] * ide, na.rm=T) ) )
          } else {
            fac <- 1 - ( sum(((fs[,i] - COmM) / ide)[whr], na.rm=T) / abs( sum(fs[,i] / ide, na.rm=T) ) )
          }
        } else {

          crit <- 1
          crit_run <- 0
           fac_0 <- 1
          
          while(crit > 1e-2) {
            crit_run <- crit_run + 1
            
            #fac_dum <- 1 - ( sum((fs[,i] - COmM * fac_0)[whr], na.rm=T) / sum(fs[,i], na.rm=T) )
			fac_dum <- 1 - ( sum((fs[,i] / fac_0 - COmM)[whr], na.rm=T) / sum(fs[,i], na.rm=T) )
                      
            crit <- fac_dum - fac_0
            fac_0 <- fac_dum
                        
          }
            fac <- fac_dum
          
        }
      #store      
        if(i == 1) corfac <- fac else corfac <- c(corfac, fac)
    }

  #plot cospectrum  
    
    if(plotting == T) {
      
      plot(fs[,i] * part_cov ~ ide, type="l", log="x", xaxt="n", yaxt="n", xlim=limx, ylim=limy, main=title[i], xlab=labx,
        ylab=laby, col=colvec[1])
      
        abline(h=0, col="grey")
      
        if(modspec == T) lines(COmM ~ ide, lty=2, col=colvec[2], lwd=5)
        if(modspec == T & !is.null(devran)) abline(v=c(devran), lty=2, col=colvec[3])
      
        sfsmisc::eaxis(side=1,labels=NA,las=0)
        sfsmisc::eaxis(side=2,labels=NA,las=0)
      
        legend(x="topleft", lty=c(1,2,2,1,1), bty="n", col=c(colvec,NA,NA), xjust = 0, yjust = 0,
          lwd=c(par()$lwd,5,par()$lwd,1,1), legend = c(
          	"measured",
          	"Massman (2005)",
          	"evaluated range",
          	paste("stability", " = ", round(zeta,1), sep=""),
          	paste("N = ", length(ide), sep="")
        ))
      
        box()
      
    }

##end loop around variables
}
##


#close graphics device
  if(plotting == T) {
    dev.off()
  }

#export results
  if(!is.null(devran)) return(corfac)

########################################################
}
########################################################



##############################################################################################
#' @title Plot ogives

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Plot ogives.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT OGIVES
##cumulation only valid with raw data or equidistant binned data!
########################################################
OGIV.plot <- function(name, IDR, RAW, IDS, SMO, thresh, Fperi, title=NULL, labx=NULL, laby=NULL, colvec=c("grey",1,1), LIMX=NULL, LIMY=NULL, weight=F, wl=T, norm=1) {
#name: (no default) output location and filename for plot
#IDR: (no default) one independent variable for the raw cospectra, e.g. frequencey, wavenumber...
#RAW: (no default) raw cospectra of up to four dependent variables, same length as IDR
#IDS: (no default) one independent variable for the smoothed cospectra, e.g. frequencey, wavenumber...
#SMO: (no default) smoothed cospectra of up to four dependent variables, same length as IDR
#thresh: (no default) flux contribution level to evaluate e.g. corresponding wavelenght
#Fperi: (no default) flux averaging period to evaluate corresponding contribution level
#title: (no default) one discription (character or expression) for each variable for the title
#labx: (NA) description for abscissa
#laby: (NA) description for ordinate
#colvec: (c(1,2,1)) colors for plotting of measurement, model and evaluation range
#LIMX: (NULL) limits for abscissa
#LIMY: (NULL) limits for ordinate
#weight (F) weight the cospectra for independent variable (wavelength / frequency)?
#wl: (T) is wavelength used as independent variable?
#norm: (1) normalizes max(cospectrum) to a given quantity, e.g. total flux per variable, else unity

#prepare local variables
  idr <- IDR
  raw <- as.matrix(RAW)
  ids <- IDS
  smo <- as.matrix(SMO)

#sort for increasing wavelenght (a) or decreasing frequency (b) -> always from short- to longer wavelength contributions
  if(wl == T) {
    raw <- as.matrix(raw[sort.list(idr, decreasing = F),])	#first dependent variable
    idr <- idr[sort.list(idr, decreasing = F)]			#then independent variable
    smo <- as.matrix(smo[sort.list(ids, decreasing = F),])	#first dependent variable
    ids <- ids[sort.list(ids, decreasing = F)]			#then independent variable
  } else {
    raw <- as.matrix(raw[sort.list(idr, decreasing = T),])
    idr <- idr[sort.list(idr, decreasing = T)]
    smo <- as.matrix(smo[sort.list(ids, decreasing = T),])
    ids <- ids[sort.list(ids, decreasing = T)]
  }

#weighting of the cospectrum: divide for wavelength, multiply for frequencies
#only valid for cospectrum, not for Ogive!
  if(weight == T) {
    if(wl==T) {
#      raw <- as.matrix(raw / idr)
      smo <- as.matrix(smo / ids)
    } else {
#      raw <- as.matrix(raw * idr)
      smo <- as.matrix(smo * ids)
  }}

#make sure that all fluxes are cumulating towards positive
    signum <- sign(colSums(raw))	#check the sign(tendency) of the different Cospectra
    smo <- t(signum * t(smo))
    raw <- t(signum * t(raw))

#normalize the smoothed cospectrum
  if(!is.null(norm)) {
    if(length(norm) == 1) norm <- rep(norm, ncol(raw))
    smo <- sapply(1:length(norm), function(x) as.matrix(smo[,x] / max(smo[,x]) * norm[x]))
  }

#call graphics device
  png(filename=name, width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
  cexvar=3; par(mfrow=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.lab=2, mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.6)


##loop around variables
for(i in 1:ncol(raw)) {
##

  #Ogive: cumulated flux contribution
    OGIVE <- cumsum(raw[,i])
    ogive <- OGIVE / max(OGIVE) * norm[i]

  #determine wavelength for given flux contribution level
    #find first value exceeding given contribution level
      zcross <- zeroCross(ogive - thresh, slope="positive") + 1
    #flag whether zero crossing is singular (s) or multiple (m)
      if(length(zcross == 1)) Og.flag <- "s" else Og.flag <- "m"
    #wavelength corresponding to given contribution level
      wathr <- idr[(zcross[1]-1)]

  #determine flux contribution level for given wavelength
    Fnear <- GenKern::nearest(idr, Fperi)
    Fleve <- ogive[Fnear]

  #store ogive results
    OGdum <- c(wathr, Fleve)
    if(i == 1) OGout <- OGdum else OGout <- cbind(OGout, OGdum)

  #plotting range
    if(!is.null(LIMX)) limx <- LIMX else limx <- range(idr)
    if(!is.null(LIMY)) limy <- LIMY else limy <- range(c(smo[,i], ogive))

  #plotting
    plot(smo[,i] ~ ids, col=colvec[1], type="l", xlim=limx, ylim=limy, main=title[i], xlab=labx, ylab=laby, las=1, log="x", xaxt = "n")
      lines(ogive ~ idr, col=colvec[2])
      abline(h=0)
      abline(h=thresh, lty=2, col=colvec[3])
      abline(v=wathr, lty=2, col=colvec[3])
      legend(x="topleft", lty=c(1,1,1,1,2), bty="n", col=c(NA,NA,col=colvec,NA,NA,NA,NA), cex=cexvar*0.4, pt.cex=cexvar/2, pt.lwd=cexvar*2/3, xjust = 0, yjust = 0, legend = c(
	"","",
	"Cospectrum",
	"Ogive (Og)",
	paste(thresh, " max(Og)", sep=""),
	paste("== ", round(wathr,2)," X", sep="")
      ))
      legend(x="topright", bty="n", cex=cexvar*0.4, pt.cex=cexvar/2, xjust = 0, yjust = 0, legend = c(
	"","","","",
	paste(Fperi, " X ==", sep=""),
	paste(round(Fleve,2), " max(Og)", sep="")
      ))
      sfsmisc::eaxis(side=1,labels=NA,las=0)
      box()

##end loop around variables
}
##


#close graphics device
  dev.off()

#export Ogive results
  dimnames(OGout) <- list(c(paste(thresh, "(Og)==", sep=""), paste(Fperi, " X==", sep="")), dimnames(raw)[[2]])
  return(OGout)

########################################################
}
########################################################



##############################################################################################
#' @title Model (co)spectrum after Massman, 2005 (in Lee, 2005)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Model (co)spectrum after Massman, 2005 (in Lee, 2005).

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to generate model (co)spectrum after Massman, 2005 (in Lee, 2005)
#continuous approximation of the Kaimal (1972) cospectra
########################################################
  SPEmod <- function(
	#independent variable, preferabley f, but n is possible
	  ide = SPEout$fr_obs[SPEout$fr_whr],
	#spectrum or cospectrum?
	  sc = c("spe", "cos")[2],
	#stability parameter
	  si = OUT$REYN$mn$sigma[FILE],
	#frequency f at which fCO(f) reaches its maximum value
	  fx = 0.1,
	#output frequency-weighted (co)spectrum?
	  weight = TRUE
	){
  
	#(inertial subrange) slope parameter
	  #3/2 for -5/3 (spectra) power law
		if(sc == "spe") m <- 3/2
	  #3/4 for -7/3 (cospectra), 
		if(sc == "cos") m <- 3/4
  
	#broadness parameter
	  #1/2 for unstable
		if(si <= 0) mue=1/2  
	  #7/6 for stable stratification 
		if(si > 0)  mue=7/6
	  
	#calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
	  COmM <- (ide / fx) / (
		( 1 + m * (ide / fx)^(2 * mue) )^( (1/(2*mue)) * ((m+1)/m) )
	  )
	
	#(un)weight the (co)spectrum if necessary
	  if(weight == FALSE) COmM <- COmM / ide
  
	#normalize to sum of 1
	  COmM <- COmM / sum(COmM, na.rm=TRUE)

########################################################	  
  }
########################################################


  
##############################################################################################
#' @title Determine spectral peak using an Ogive method

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Determine spectral peak using an Ogive method.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################
  
########################################################
#function to determine spectral peak using an Ogive method
########################################################
	find_FX_og <- function(
	  #frequency f at which fCO(f) reaches its maximum value
		FX,
	  #independent variable, preferabley f, but n is possible
		IDE = SPEout$fr_obs[SPEout$fr_whr],
	  #dependent variable, spectra or cospectra
		DEP = SPEout$FScosp[SPEout$fr_whr,4],
	  #spectrum or cospectrum?
		SC = c("spe", "cos")[2],
	  #stability parameter
		SI = OUT$REYN$mn$sigma[FILE],
	  #use frequency-weighted (co)spectrum?
		WEIGHT =TRUE,
	  #frequency range for determining optimiality criterion
		WHR_CRIT = c(0.01, 1),
	  #cumulative flux contribution for which measured (co)-spectrum is scaled to model (co)-spectrum
		crit_cum = 0.6,
	  #generate plot?
		plot_path = NULL,
	  #determine peak frequency or output spectral correction factor?
		meth = c("peak", "corfac")[1]
	  ) {

	  #generate spectral model for range of frequencies
		spemod <- SPEmod(
		  #independent variable, preferabley f, but n is possible
			ide = IDE,
		  #spectrum or cospectrum?
			sc = SC,
		  #stability parameter
			si = SI,
		  #frequency f at which fCO(f) reaches its maximum value
			fx=FX,
		  #output frequency-weighted (co)spectrum?
			weight=WEIGHT
		)
		  #cumulate to Ogive from lowest to highest frequency
			spemod_cum <- cumsum(spemod)
	
	  #assign measured variables
		#independent variable
		  ide <- IDE
	
		#dependent variable
		  #frequency-weighted
			if(WEIGHT == TRUE) {
			  dep <- ide * DEP
		  #not frequency weighted  
			} else {
			  dep <- DEP  
			}
		  #normalize to sum of 1
			dep <- dep / sum(dep, na.rm=TRUE)
		  #cumulate to Ogive from lowest to highest frequency
			dep_cum <- cumsum(dep)
		  #scaling factor to intersect with modelled Ogive at pre-determined level
			whr_fac_scal <- GenKern::nearest(dep_cum, crit_cum)
			fac_scal <- spemod_cum[whr_fac_scal] / crit_cum
		  #scale to intersect with modelled Ogive at pre-determined level
			dep_cum_scal <- dep_cum * fac_scal  
	  #indices of observations in the frequency range for determining optimiality criterion
		whr_crit <- which(ide > WHR_CRIT[1] & ide < WHR_CRIT[2])
	
	  #optimiality criterion
		#crit <- sd((spemod_cum - dep_cum_scal)[whr_crit])  
		crit <- RBPCdet2(refe = spemod_cum[whr_crit], test = dep_cum_scal[whr_crit])[1,1]
		#crit <- cor(x = spemod_cum[whr_crit], y = dep_cum_scal[whr_crit], use = "pairwise.complete.obs")
		#crit <- sqrt(def.med.mad((spemod_cum - dep_cum_scal)[whr_crit])[1,1]^2 + def.med.mad((spemod_cum - dep_cum_scal)[whr_crit])[1,2]^2)
	
	  #plotting
		if(!is.null(plot_path)) {
	  
		#graphics device
		  png(filename=plot_path, width = 1000, 
			  height = 1000, units = "px", pointsize = 20, bg = "white")
		  cexvar=3; par(mfcol=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
						mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
	
		#actual plotting
		  #Ogive
			plot(spemod_cum ~ ide, log="x", type="l", main=paste("peak = ", round(FX,2), sep=""), 
				 xlab="frequency", ylab="relative contribution")
			  lines(dep_cum ~ ide, col=2)
			  lines(dep_cum_scal ~ ide, col=4)
			  abline(h=c(crit_cum, spemod_cum[whr_fac_scal]), lty=2)
			  abline(v=ide[whr_fac_scal], lty=2)
			  abline(v=WHR_CRIT, lty=5)
			  points(I(ide[whr_fac_scal]), crit_cum, col=2, cex=2)
			  points(I(ide[whr_fac_scal]), I(spemod_cum[whr_fac_scal]), col=4, cex=2)
	
		#close graphics device
		  dev.off()
		
		}
	  
	  #return result
		if(meth == "peak") return(crit)
		if(meth == "corfac") return(list(ide_fac_scal=ide[whr_fac_scal], fac_cor=1/max(dep_cum_scal, na.rm=TRUE)))

########################################################
	}
########################################################



##############################################################################################
#' @title Sigmoidal transfer function (Lorentzian)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
########################################################
fun_TSIG <- function(freq_0, freq) 1 / (1 + (freq / freq_0)^2)



##############################################################################################
#' @title Determine cutoff frequency empirically 

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Determine cutoff frequency empirically .

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#determine cutoff frequency empirically 
########################################################
find_F0 <- function(
  #cutoff frequency
  f0,
  #independent variable, preferabley f, but n is possible
  ide = SPEout$fr_obs[SPEout$fr_whr][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
  #dependent variable, spectra or cospectra
  dep = SPEout$FSunfold[SPEout$fr_whr,fpo][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
  #reference correction factor
  corfac_ref = corfac_out
) {
  
  #calculate transfer function
  fun_tsig <- fun_TSIG(freq_0=f0, freq=ide)
  
  #plotting
  #       plot(fun_tsig ~ ide)
  
  #calculate resulting correction factor over all frequencies
  corfac <- sum(dep / fun_tsig, na.rm=TRUE) / sum(dep, na.rm=TRUE)
  
  #calculate optimality criterion
  crit <- abs(corfac_ref - corfac)
  
  #return results
  return(crit)

########################################################  
}
########################################################



##############################################################################################
#' @title Determine half-power / cut-off frequency

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Determine half-power / cut-off frequency.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to determine half-power / cut-off frequency
########################################################
	find_TF <- function(
	  #half-power / cut-off frequency
		F0 = 0.5,
	  #frequency f at which fCO(f) reaches its maximum value
		FX = fx_out$par * mean(SPEout$fr_obs / SPEout$fr_nor, na.rm=TRUE),
	  #independent variable, preferabley f, but n is possible
		IDE = SPEout$fr_obs[SPEout$fr_whr][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
	  #dependent variable, spectra or cospectra
		DEP = SPEout$FScosp[SPEout$fr_whr,fpo][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
	  #spectrum or cospectrum?
		SC = c("spe", "cos")[2],
	  #stability parameter
		SI = OUT$REYN$mn$sigma[FILE],
	  #use frequency-weighted (co)spectrum?
		WEIGHT =FALSE,
	  #frequency range for determining optimiality criterion
		WHR_CRIT = c(0.01, 10),
	  #correction factor for spectral attenuation
		corfac = corfac_out,
	  #number of bins if binning shall be performed
		BINS = 1000,
	  #generate plot?
		plot_path = NULL
	) {

	  #assign indipendent value
		ide <- IDE
	  
	  #correct measured coefficients for spectral attenuation
  #flaw: corrects coefficients at all frequencies by the same amount
		dep <- DEP / sum(DEP, na.rm=TRUE) / corfac
	  
	  #generate spectral model for range of frequencies
		spemod <- SPEmod(
		  #independent variable, preferabley f, but n is possible
		  ide = ide,
		  #spectrum or cospectrum?
		  sc = SC,
		  #stability parameter
		  si = SI,
		  #frequency f at which fCO(f) reaches its maximum value
		  fx=FX,
		  #output frequency-weighted (co)spectrum?
		  weight=WEIGHT
		)      
		#plot(dep ~ ide, log = "x", ylim = limy, col=2)
		#lines(spemod ~ ide, col=1)
		#abline(h=0, lty=2)

	  #calculate empirical transfer function
		trans_dat <- dep / spemod
		#plot(trans_dat ~ ide, log="x")

	  #binning
		if(!is.null(BINS)) {
		  
		  dummy_bin <- def.bin(
			idep=ide,
			depe=trans_dat,
			RngMinMax=NULL,
			NumBin=BINS,
			widtBin=c("lin", "log10", "exp10", "logExp", "expLog")[1],
			meanFunc=c("mean", "median")[2]
		  )
		  ide <- dummy_bin$idep
		  trans_dat <- dummy_bin$depe
		  rm(dummy_bin)          
	  
		}
	  
	  #sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
		tfumod <- fun_TSIG(freq_0=F0, freq=ide)
	  
	  #indices of observations in the frequency range for determining optimiality criterion
		whr_crit <- which(ide > WHR_CRIT[1] & ide < WHR_CRIT[2])
	  
	  #optimiality criterion
		#crit <- sd((tfumod - trans_dat)[whr_crit])
		crit <- RBPCdet2(refe = trans_dat[whr_crit], test = tfumod[whr_crit])[1,1]
		#crit <- cor(x = spemod_cum[whr_crit], y = dep_cum_scal[whr_crit], use = "pairwise.complete.obs")
		#dum_MEDmad <- def.med.mad((tfumod - trans_dat)[whr_crit])
		  #crit <- sqrt(dum_MEDmad[1,1]^2 + dum_MEDmad[1,2]^2)
		  #rm(dum_MED)
	  
	  #plotting
	  if(!is.null(plot_path)) {
		
		#graphics device
		png(filename=plot_path, width = 1000, 
			height = 1000, units = "px", pointsize = 20, bg = "white")
		cexvar=3; par(mfcol=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
					  mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
		
		#actual plotting
		  plot(trans_dat ~ ide, log="x", type="p", main=paste("half power frequency = ", round(F0,2), " Hz", sep=""), 
			   xlab="frequency", ylab="transfer function")
		  lines(tfumod ~ ide, col=4)
		  abline(h=0, lty=2)
		  abline(v=F0, lty=3)
		  #points(tfumod[GenKern::nearest(ide, F0)] ~ ide[GenKern::nearest(ide, F0)], col=4)
		
		#close graphics device
		  dev.off()
		
	  }
	  
	  #return result
		if(is.null(plot_path)) return(crit)

########################################################
	}
########################################################


##############################################################################################
#' @title 63\% frequency constant after Aubinet (2012) Eq. 4.22

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description 63\% frequency constant after Aubinet (2012) Eq. 4.22.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################
	
	const_f <- function(t0) 1 / (2 * pi * t0)



##############################################################################################
#' @title 63\% time constant after Aubinet (2012) Eq. 4.22

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description 63\% time constant after Aubinet (2012) Eq. 4.22.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

const_t <- function(f0) 1 / (2 * pi * f0)
