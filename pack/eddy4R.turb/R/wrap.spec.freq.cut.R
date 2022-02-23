##############################################################################################
#' @title Definition function: Determine half-power / cut-off frequency

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Determine half-power / cut-off frequency.

#' @param FreqCut half-power / cut-off frequency
#' @param FreqPeak frequency f at which fCO(f) reaches its maximum value
#' @param idep independent variable, preferabley f, but n is possible
#' @param depe dependent variable, spectra or cospectra
#' @param MethSpec spectrum or cospectrum?
#' @param paraStbl stability parameter
#' @param MethWght use frequency-weighted (co)spectrum?
#' @param ThshFreqRng frequency range for determining optimiality criterion
#' @param CoefCor correction factor for spectral attenuation
#' @param NumBin number of bins if binning shall be performed
#' @param FilePlot generate plot?

#' @return Optimality criterion for transfer function to correct the data to model specta

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Aubinet et al. (2012)
#' Eugster and Senn (1995)

#' @keywords Fast Fourier Transform, FFT, spectral, cut-off frequency, half-power

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to determine half-power / cut-off frequency
########################################################
def.spec.freq.cut <- function(
  #half-power / cut-off frequency
  FreqCut = 0.5,
  #frequency f at which fCO(f) reaches its maximum value
  FreqPeak,
  #independent variable, preferabley f, but n is possible
  idep,
  #dependent variable, spectra or cospectra
  depe,
  #spectrum or cospectrum?
  MethSpec = c("spec", "cosp")[2],
  #stability parameter
  paraStbl,
  #use frequency-weighted (co)spectrum?
  MethWght =FALSE,
  #frequency range for determining optimiality criterion
  ThshFreqRng = c(0.01, 10),
  #correction factor for spectral attenuation
  CoefCor,
  #number of bins if binning shall be performed
  NumBin = 1000,
  #generate plot?
  FilePlot = NULL
) {
  
  #correct measured coefficients for spectral attenuation
  #flaw: corrects coefficients at all frequencies by the same amount
  depe <- depe / base::sum(depe, na.rm=TRUE) / CoefCor
  
  #generate spectral model for range of frequencies
  modlSpec <- eddy4R.turb::def.spec.modl(
    #independent variable, preferabley f, but n is possible
    idep = idep,
    #spectrum or cospectrum?
    MethSpec = MethSpec,
    #stability parameter
    paraStbl = paraStbl,
    #frequency f at which fCO(f) reaches its maximum value
    FreqPeak= FreqPeak,
    #output frequency-weighted (co)spectrum?
    MethWght= MethWght
  )      
  #plot(dep ~ idep, log = "x", ylim = limy, col=2)
  #lines(spemod ~ idep, col=1)
  #abline(h=0, lty=2)
  
  #calculate empirical transfer function
  dataTfun <- depe / modlSpec
  #plot(trans_dat ~ idep, log="x")
  
  #binning
  if(!base::is.null(NumBin)) {
    
    tmpBin <- eddy4R.base::def.bin(
      idep= idep,
      depe= dataTfun,
      RngMinMax= NULL,
      NumBin= NumBin,
      widtBin= c("lin", "log10", "exp10", "logExp", "expLog")[1],
      meanFunc= c("mean", "median")[2]
    )
    idep <- tmpBin$idep
    dataTfun <- tmpBin$depe
    rm(tmpBin)          
    
  } #End if statement for bins
  
  #sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
  tfunSigm <- eddy4R.turb::def.spec.tfun.sigm(FreqCut=FreqCut, Freq=idep)
  
  #indices of observations in the frequency range for determining optimiality criterion
  setFreqCritOptm <- base::which(idep > ThshFreqRng[1] & idep < ThshFreqRng[2])
  
  #optimiality criterion
  #crit <- sd((tfumod - trans_dat)[whr_crit])
  critOptm <- eddy4R.base::def.rmsd.diff.prcs.rsq(refe = dataTfun[setFreqCritOptm], test = tfunSigm[setFreqCritOptm])[1,1]
  #crit <- cor(x = spemod_cum[whr_crit], y = dep_cum_scal[whr_crit], use = "pairwise.complete.obs")
  #dum_MEDmad <- def.med.mad((tfumod - trans_dat)[whr_crit])
  #crit <- sqrt(dum_MEDmad[1,1]^2 + dum_MEDmad[1,2]^2)
  #rm(dum_MED)
  
  
  #plotting
  if(!base::is.null(FilePlot)) {
    
    #graphics device
    grDevices::png(filename=FilePlot, width = 1000, 
        height = 1000, units = "px", pointsize = 20, bg = "white")
    cexvar=3; par(mfcol=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
                  mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
    
    #actual plotting
    graphics::plot(dataTfun ~ idep, log="x", type="p", main=paste("half power frequency = ", round(FreqCut,2), " Hz", sep=""), 
         xlab="frequency", ylab="transfer function")
    graphics::lines(tfunSigm ~ idep, col=4)
    graphics::abline(h=0, lty=2)
    graphics::abline(v=FreqCut, lty=3)
    #points(tfunSigm[GenKern::nearest(idep, FreqCut)] ~ idep[GenKern::nearest(idep, FreqCut)], col=4)
    
    #close graphics device
    grDevices::dev.off()
    
  }
  
  #return result
  return(critOptm)
  
  ########################################################
}
########################################################
