##############################################################################################
#' @title Definition function: Plot power spectra of up to three variables

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-12)
#     terms update

#' @description Plot power spectra of up to three variables.

#' @param FileOut character, output location and filename for plot
#' @param idep one independent variable, e.g. frequencey, wavenumber...
#' @param depe spectra of up to three dependent variables, same length as idep
#' @param DscrVar one description (character or expression) for each variable for the legend
#' @param Labx character (NA), description for abscissa
#' @param Laby character (NA), description for ordinate
#' @param Colr (NULL) colors for plotting, as many colors as dependent variables
#' @param Limx (NULL) limits for abscissa
#' @param Limy (NULL) limits for ordinate
#' @param Ofst (NULL) offsets different variables from each other to improve legibility (only if CoefPowrSlp is set)
#' @param CoefPowrSlp (NULL) coefficient of the power law c("-5/3", "-2/3") for unweighted / weighted spectra
#' @param FreqSub frequency range of initial subrange, used to fit the power law which defaults to (0.05,Inf) 

#' @return Plot output to FileOut

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT POWER SPECTRA OF UP TO THREE VARIABLES
########################################################
def.spec.plot.powr <- function( 
  FileOut,   #FileOut: (no default) output location and filename for plot
  idep,   #idep: (no default) one independent variable, e.g. frequencey, wavenumber...
  depe,  #depe: (no default) spectra of up to three dependent variables, same length as idep
  DscrVar, #DscrVar: (no default) one description (character or expression) for each variable for the legend
  Labx=NA, #Labx: (NA) description for abscissa
  Laby=NA, #Laby: (NA) description for ordinate
  Colr=NULL, #Colr: (NULL) colors for plotting, as many colors as dependent variables
  Limx=NULL, #Limx: (NULL) limits for abscissa
  Limy=NULL, #Limy: (NULL) limits for ordinate
  Ofst=NULL, #Ofst: (NULL) offsets different variables from each other to improve legibility (only if CoefPowrSlp is set)
  CoefPowrSlp=NULL, #CoefPowrSlp: (NULL) coefficient of the power law c("-5/3", "-2/3") for unweighted / weighted spectra
  FreqSub=c(0.05,Inf)   #FreqSub: (c(0.05,Inf)) frequency range of initial subrange, used to fit the power law
){

  #defaults for color vector
  numCol <- base::ncol(base::as.matrix(depe))
  if(base::is.null(Colr)) Colr <- c(2:(numCol+1))
  if(base::is.null(Ofst)) Ofst <- base::rep(1,numCol)
  
  #call graphics device
  grDevinces::png(filename=FileOut, width=830*2, height=1170,pointsize=2000/50,bg = "white")
  cexvar=3; par(mfrow=c(1,3),las=0,cex.axis=cexvar*0.4,cex.lab=cexvar*0.4,font.lab=2, mar=c(4,4,2,2),mgp=c(2.8,0.8,0), family="times",lwd=cexvar*1,cex.main=cexvar*0.6)
  
  #plotting
  #pick variables
  if(base::is.null(CoefPowrSlp)) depeLoca <- base::as.matrix(depe)[,1] else depeLoca <- Ofst[1] * base::as.matrix(depe)[,1]
  
  #estimate slope for reference line
  if(!base::is.null(CoefPowrSlp)) {
    numCoefPowrSlp <- base::as.integer(strsplit(CoefPowrSlp,"/")[[1]])
    numCoefPowrSlp <- numCoefPowrSlp[1] / numCoefPowrSlp[2]
    setFreq <- base::which(idep >= FreqSub[1] & idep < FreqSub[2])
    modlLin <- robustbase::lmrob(I(depeLoca[setFreq]^(1/numCoefPowrSlp)) ~ 0 + idep[setFreq])
    slp <- modlLin$coefficients
    linePowrSlpRefe <- (slp * idep)^numCoefPowrSlp
  }
  
  #actual plotting
  graphics::plot(depeLoca ~ idep, type="l", log="xy", xaxt="n", yaxt="n", xlim=Limx, ylim=Limy, xlab=Labx, ylab=Laby, col=Colr[1])
  if(!is.null(CoefPowrSlp)) lines(linePowrSlpRefe ~ idep, col=1, lty=2) #, lwd=5
  
  #max. two additional variables
  if(numCol > 1 & numCol <= 3) {
    for (idxCol in c(2:numCol)) {
      #pick next dependend variable
      if(base::is.null(CoefPowrSlp)) depeLoca <- as.matrix(depe)[,idxCol] else depeLoca <- Ofst[idxCol] * as.matrix(depe)[,idxCol]
      
      #estimate slope for reference line
      if(!base::is.null(CoefPowrSlp)) {
        modlLin <- robustbase::lmrob(I(depeLoca[setFreq]^(1/numCoefPowrSlp)) ~ 0 + idep[setFreq])
        slp <- modlLin$coefficients
        linePowrSlpRefe <- (slp * idep)^numCoefPowrSlp
      }
      
      #actual plotting
      graphics::lines(depeLoca ~ idep, col=Colr[idxCol])
      if(!is.null(CoefPowrSlp)) graphics::lines(linePowrSlpRefe ~ idep, col=1, lty=2) #, lwd=5
    }
  }
  sfsmisc::eaxis(side=1,labels=NA,las=0)
  sfsmisc::eaxis(side=2,labels=NA,las=0)
  if(base::is.null(CoefPowrSlp)) {
    graphics::legend(x="bottomleft", lty=base::rep(1,numCol), bty="n", col=Colr, cex=cexvar*0.4, xjust = 0, yjust = 0, lwd=base::rep(graphics::par()$lwd,numCol), legend=DscrVar)
  } else {
    graphics::legend(x="bottomleft", lty=c(rep(1,numCol),2), bty="n", col=c(Colr, 1), cex=cexvar*0.4, xjust = 0, yjust = 0, lwd=c(base::rep(graphics::par()$lwd,numCol),par()$lwd), legend=c(DscrVar, base::substitute(paste("f"^{CoefPowrSlp}, " law", sep=""), base::list(CoefPowrSlp=CoefPowrSlp))))
  }
  graphics::box()
  
  #close graphics device
  grDevices::dev.off()
  ########################################################
}
########################################################