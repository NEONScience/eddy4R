##############################################################################################
#' @title Definition function: Plot cospectra

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


