##############################################################################################
#' @title Definition function: Plot ogives

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
