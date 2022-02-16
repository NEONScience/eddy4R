##############################################################################################
#' @title Definition function: Determine spectral peak using an Ogive method

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

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
def.spec.peak.ogiv <- function(
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
  MethWght =TRUE,
  #frequency range for determining optimiality criterion
  ThshFreqRng = c(0.01, 1),
  #cumulative flux contribution for which measured (co)-spectrum is scaled to model (co)-spectrum
  crit_cum = 0.6,
  #generate plot?
  FilePlot = NULL,
  #determine peak frequency or output spectral correction factor?
  Meth = c("peak", "corfac")[1]
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
  crit <- def.rmsd.diff.prcs.rsq(refe = spemod_cum[whr_crit], test = dep_cum_scal[whr_crit])[1,1]
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