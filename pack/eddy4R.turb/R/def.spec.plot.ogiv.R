##############################################################################################
#' @title Definition function: Plot ogives

# type (one of function defintion, function wrapper, workflow, demo): function definition

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-12)
#     terms update

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
OGIV.plot <- function(
  name, #name: (no default) output location and filename for plot
  IDR, #IDR: (no default) one independent variable for the raw cospectra, e.g. frequencey, wavenumber...
  RAW, #RAW: (no default) raw cospectra of up to four dependent variables, same length as IDR
  IDS, #IDS: (no default) one independent variable for the smoothed cospectra, e.g. frequencey, wavenumber...
  SMO, #SMO: (no default) smoothed cospectra of up to four dependent variables, same length as IDR
  thresh, #thresh: (no default) flux contribution level to evaluate e.g. corresponding wavelenght
  Fperi, #Fperi: (no default) flux averaging period to evaluate corresponding contribution level
  title=NULL, #title: (no default) one discription (character or expression) for each variable for the title
  labx=NULL, #labx: (NA) description for abscissa
  laby=NULL, #laby: (NA) description for ordinate
  colvec=c("grey",1,1), #colvec: (c(1,2,1)) colors for plotting of measurement, model and evaluation range
  LIMX=NULL, #LIMX: (NULL) limits for abscissa
  LIMY=NULL, #LIMY: (NULL) limits for ordinate
  weight=F, #weight (F) weight the cospectra for independent variable (wavelength / frequency)?
  wl=TRUE, #wl: (T) is wavelength used as independent variable?
  norm=1 #norm: (1) normalizes max(cospectrum) to a given quantity, e.g. total flux per variable, else unity
  ) {
  
  #prepare local variables
  idr <- IDR
  raw <- base::as.matrix(RAW)
  ids <- IDS
  smo <- base::as.matrix(SMO)
  
  #sort for increasing wavelenght (a) or decreasing frequency (b) -> always from short- to longer wavelength contributions
  if(wl == TRUE) {
    raw <- base::as.matrix(raw[base::sort.list(idr, decreasing = FALSE),])	#first dependent variable
    idr <- idr[base::sort.list(idr, decreasing = FALSE)]			#then independent variable
    smo <- base::as.matrix(smo[base::sort.list(ids, decreasing = FALSE),])	#first dependent variable
    ids <- ids[base::sort.list(ids, decreasing = FALSE)]			#then independent variable
  } else {
    raw <- base::as.matrix(raw[sort.list(idr, decreasing = TRUE),])
    idr <- idr[base::sort.list(idr, decreasing = TRUE)]
    smo <- base::as.matrix(smo[base::sort.list(ids, decreasing = TRUE),])
    ids <- ids[base::sort.list(ids, decreasing = TRUE)]
  }
  
  #weighting of the cospectrum: divide for wavelength, multiply for frequencies
  #only valid for cospectrum, not for Ogive!
  if(weight == TRUE) {
    if(wl==TRUE) {
      #      raw <- as.matrix(raw / idr)
      smo <- base::as.matrix(smo / ids)
    } else {
      #      raw <- as.matrix(raw * idr)
      smo <- base::as.matrix(smo * ids)
    }}
  
  #make sure that all fluxes are cumulating towards positive
  signum <- base::sign(base::colSums(raw))	#check the sign(tendency) of the different Cospectra
  smo <- base::t(signum * base::t(smo))
  raw <- base::t(signum * base::t(raw))
  
  #normalize the smoothed cospectrum
  if(!base::is.null(norm)) {
    if(base::length(norm) == 1) norm <- base::rep(norm, base::ncol(raw))
    smo <- base::sapply(1:base::length(norm), function(x) base::as.matrix(smo[,x] / base::max(smo[,x]) * norm[x]))
  }
  
  #call graphics device
  grDevices::png(filename=name, width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
  cexvar=3; graphics::par(mfrow=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.lab=2, mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.6)
  
  
  ##loop around variables
  for(i in 1:base::ncol(raw)) {
    ##
    
    #Ogive: cumulated flux contribution
    OGIVE <- base::cumsum(raw[,i])
    ogive <- OGIVE / base::max(OGIVE) * norm[i]
    
    #determine wavelength for given flux contribution level
    #find first value exceeding given contribution level
    zcross <- zeroCross(ogive - thresh, slope="positive") + 1
    #flag whether zero crossing is singular (s) or multiple (m)
    if(base::length(zcross == 1)) Og.flag <- "s" else Og.flag <- "m"
    #wavelength corresponding to given contribution level
    wathr <- idr[(zcross[1]-1)]
    
    #determine flux contribution level for given wavelength
    Fnear <- GenKern::nearest(idr, Fperi)
    Fleve <- ogive[Fnear]
    
    #store ogive results
    OGdum <- c(wathr, Fleve)
    if(i == 1) OGout <- OGdum else OGout <- base::cbind(OGout, OGdum)
    
    #plotting range
    if(!base::is.null(LIMX)) limx <- LIMX else limx <- base::range(idr)
    if(!base::is.null(LIMY)) limy <- LIMY else limy <- base::range(c(smo[,i], ogive))
    
    #plotting
   graphics::plot(smo[,i] ~ ids, col=colvec[1], type="l", xlim=limx, ylim=limy, main=title[i], xlab=labx, ylab=laby, las=1, log="x", xaxt = "n")
   graphics::lines(ogive ~ idr, col=colvec[2])
   graphics::abline(h=0)
   graphics::abline(h=thresh, lty=2, col=colvec[3])
   graphics::abline(v=wathr, lty=2, col=colvec[3])
   graphics::legend(x="topleft", lty=c(1,1,1,1,2), bty="n", col=c(NA,NA,col=colvec,NA,NA,NA,NA), cex=cexvar*0.4, pt.cex=cexvar/2, pt.lwd=cexvar*2/3, xjust = 0, yjust = 0, legend = c(
      "","",
      "Cospectrum",
      "Ogive (Og)",
      base::paste(thresh, " max(Og)", sep=""),
      base::paste("== ", base::round(wathr,2)," X", sep="")
    ))
   graphics::legend(x="topright", bty="n", cex=cexvar*0.4, pt.cex=cexvar/2, xjust = 0, yjust = 0, legend = c(
      "","","","",
      base::paste(Fperi, " X ==", sep=""),
      base::paste(base::round(Fleve,2), " max(Og)", sep="")
    ))
    sfsmisc::eaxis(side=1,labels=NA,las=0)
    graphics::box()
    
    ##end loop around variables
  }
  ##
  
  
  #close graphics device
  grDevices::dev.off()
  
  #export Ogive results
  base::dimnames(OGout) <- base::list(c(paste(thresh, "(Og)==", sep=""), base::paste(Fperi, " X==", sep="")), base::dimnames(raw)[[2]])
  
  #Return output
  return(OGout)
  
  ########################################################
} #End of function
########################################################
