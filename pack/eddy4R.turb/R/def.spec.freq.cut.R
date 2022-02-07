##############################################################################################
#' @title Definition function: Determine half-power / cut-off frequency

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
  crit <- def.rmsd.diff.prcs.rsq(refe = trans_dat[whr_crit], test = tfumod[whr_crit])[1,1]
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
