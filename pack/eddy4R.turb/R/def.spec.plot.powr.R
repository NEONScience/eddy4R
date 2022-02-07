



##############################################################################################
#' @title Definition function: Plot power spectra of up to three variables

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