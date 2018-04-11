##############################################################################################
#' @title Definition function: Plot quality flags and quality metrics (basic L1 data products) 

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com }

#' @description 
#' Function definition. Plots the aggregated quality flags, quality metrics and final quality flag for basic L1 (time window averaged) data products as output from wrap.dp01.qfqm.R.

#' @param dataDp01 Required input. A list output from wrap.dp01.qfqm.R of: \cr
#' timeAgrBgn - the starting time stamp of aggregated L1 data and quality metrics \cr
#' timeAgrEnd - the ending time stamp (non-inclusive) of aggregated L1 data and quality metrics \cr
#' dataAgr - a list of named variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, number of points going into the average, standard error of the mean, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in flgs, as well as the alpha & beta quality metrics and final quality flag. It is important that the column names of this data frame are indistinguishable from those that would be created from wrap.dp01.qfqm.R
#' @param WndwTime Optional. A 2-element POSIXlt vector of the minimum and maximum time range to plot. Default is the entire data range.
#' @param NameQmPlot Optional. A character vector listing the individual quality metrics to plot. The strings in this vector can be partial names, i.e. a partial match will result in the quality metric being plotted (ex. NameQmIndiv <- c("Step") will result in the quality metrics "qmStepPass","qmStepFail" and "qmStepNa" to be plotted). Default is all QMs. 

#' @return Running this function will output 3 plots per data variable: 1) basic L1 statistics (mean, min, max, etc). 2) Pass, Fail, and NA quality metrics for ever flag, 3) the final Alpha and Beta quality metrics and the final quality flag.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113)

#' @keywords NEON QAQC, quality flags and metrics, L1 average, final quality flag

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-02-09)
#     original creation
#   Cove Sturtevant (2016-11-14)
#     fix error in multi-plot grobs
#     update variable naming according to eddy4R coding style
#   Cove Sturtevant (2016-11-28)
#     adjusted for changes to upstream dependencies
#     changed function name from def.plot.qfqm.dp01 to def.plot.dp01.qfqm
#   Natchaya P-Durden (2018-04-11)
#    applied eddy4R term name convention; replaced pos by set
##############################################################################################


def.plot.dp01.qfqm <- function (
  dataDp01,             # the list output from wrap.dp01.qfqm.R. Required input.
  WndwTime = c(min(dataDp01$timeAgrBgn),max(dataDp01$timeAgrBgn)), # a 2-element POSIXlt vector of the minimum and maximum time range to plot. Default is entire range
  NameQmPlot = sub("Pass","",names(dataDp01$dataAgr[[1]][grep("Pass",names(dataDp01$dataAgr[[1]]))])) # a character vector listing the individual quality metrics to plot. The strings in this vector can be partial names, i.e. a partial match will result in the quality metric being plotted (ex. NameQmIndiv <- "Step" will result in the quality metrics "qmStepPass","qmStepFail" and "qmStepNa" to be plotted). Default is all QMs. 
  ) {
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(base::missing("dataDp01") | !base::is.list(dataDp01)) {
    stop("Required input 'data' must be a list output from wrap.dp01.qfqm.R")
  }
  
  # Check WndwTime
  WndwTime <- base::try(as.POSIXlt(WndwTime),silent=TRUE)
  if(base::class(WndwTime)[1] == "try-error"){
    stop("Input parameter WndwTime must be of class POSIXlt")
  } else if(length(WndwTime) != 2) {
    stop("Input parameter WndwTime must be a POSIXlt vector of length 2")
  } else if ((min(WndwTime) > max(dataDp01$timeAgrBgn)) | (max(WndwTime) < min(dataDp01$timeAgrBgn))) {
    stop("Check input parameter WndwTime. The selected date range does not cover any portion of the data.")
  }
  
  
# Do Plotting -------------------------------------------------------------
  
  # Plot the L1 basic stats
  numVar <- base::length(dataDp01$dataAgr)
  nameVar <- base::names(dataDp01$dataAgr)
    
  for(idxVar in nameVar) {
    
    # Pull out data to plot
    dataIdx <- dataDp01$dataAgr[[idxVar]]
    dataIdx$time <- dataDp01$timeAgrBgn

    # Set of plots for basic stats: Mean, min, max, var, num,se 
    dataIdxQf <- base::data.frame(time=dataIdx$time,val=dataIdx$mean,valQfFinlFail=dataIdx$mean)
    dataIdxQf$valQfFinlFail[dataIdx$qfFinl==0] <- NA
    dataIdxQf <- reshape2::melt(dataIdxQf,id="time")
    if(base::sum(dataIdx$mean,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotMean <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank()
    } else {
      # Data to plot!
      plotMean <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time,y=mean)) + ggplot2::geom_line() +
        ggplot2::geom_point() + 
        ggplot2::geom_point(data=dataIdxQf,ggplot2::aes(x=time,y=value,color=factor(variable))) 
    }
    plotMean <- plotMean +
      ggplot2::theme_bw()+
      ggplot2::scale_colour_manual(name="",values=c("black","red"),labels=c("Final QF=0","Final QF=1")) +
      ggplot2::theme(legend.title=ggplot2::element_blank(),legend.position=c(1,1),legend.justification=c(1,1)) +
      ggplot2::theme(legend.background=ggplot2::element_blank(),legend.key=ggplot2::element_blank()) + 
      ggplot2::labs(title=" ",x="Date/Time",y="Mean")
    
    if(base::sum(dataIdx$min,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotMin <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank() + 
        ggplot2::labs(title=idxVar,x="Date/Time",y="Minimum") + ggplot2::theme_bw()
    } else {
      # Data to plot!
      plotMin <- ggplot2::ggplot() + ggplot2::geom_line(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$min)) +
        ggplot2::geom_point(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$min)) +
        ggplot2::labs(title=idxVar,x="Date/Time",y="Minimum") + ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=dataIdx$time[dataIdx[,"qfFinl"]==1],y=dataIdx$min[dataIdx[,"qfFinl"]==1]),color="red") 
    }

    
    if(base::sum(dataIdx$max,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotMax <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank() + 
        ggplot2::labs(title=" ",x="Date/Time",y="Maximum") + ggplot2::theme_bw()
    } else {
      # Data to plot!
      plotMax <- ggplot2::ggplot() + ggplot2::geom_line(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$max)) +
        ggplot2::geom_point(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$max)) + 
        ggplot2::labs(title=" ",x="Date/Time",y="Maximum") + ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=dataIdx$time[dataIdx[,"qfFinl"]==1],y=dataIdx$max[dataIdx[,"qfFinl"]==1]),color="red") 
    }
    
    
    if(base::sum(dataIdx$var,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotVar <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank() + 
        ggplot2::labs(title=" ",x="Date/Time",y="Variance") + ggplot2::theme_bw()
    } else {
      # Data to plot!
      plotVar <- ggplot2::ggplot() + ggplot2::geom_line(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$var)) +
        ggplot2::geom_point(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$var)) + 
        ggplot2::labs(title=" ",x="Date/Time",y="Variance") + ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=dataIdx$time[dataIdx[,"qfFinl"]==1],y=dataIdx$var[dataIdx[,"qfFinl"]==1]),color="red") 
    }

    if(base::sum(dataIdx$se,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotSe <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank() + 
        ggplot2::labs(title=" ",x="Date/Time",y="Standard error") + ggplot2::theme_bw()
    } else {
      # Data to plot!
      plotSe <- ggplot2::ggplot() + ggplot2::geom_line(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$se)) +
        ggplot2::geom_point(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$se)) + 
        ggplot2::labs(title=" ",x="Date/Time",y="Standard error") + ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=dataIdx$time[dataIdx[,"qfFinl"]==1],y=dataIdx$se[dataIdx[,"qfFinl"]==1]),color="red") 
    }
    

    if(base::sum(dataIdx$numSamp,na.rm=TRUE) == 0) {
      # No non-NA data, generate empty plot
      plotNumPts <- ggplot2::ggplot(data=dataIdx,ggplot2::aes(x=time)) + ggplot2::geom_blank() + 
        ggplot2::labs(title=" ",x="Date/Time",y="Number of points") + ggplot2::theme_bw()
    } else {
      # Data to plot!
      plotNumPts <- ggplot2::ggplot() + ggplot2::geom_line(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$numSamp)) +
        ggplot2::geom_point(data=dataIdx,ggplot2::aes(x=dataIdx$time,y=dataIdx$numSamp)) + 
        ggplot2::labs(title=" ",x="Date/Time",y="Number of points") + ggplot2::theme_bw() +
        ggplot2::geom_point(ggplot2::aes(x=dataIdx$time[dataIdx[,"qfFinl"]==1],y=dataIdx$numSamp[dataIdx[,"qfFinl"]==1]),color="red")
    }
    
      
    
    Rmisc::multiplot(plotMean,plotVar,plotMin,plotSe,plotMax,plotNumPts,cols=3)
    
    
    # Set up the quality metrics to plot
    nameColsIdx <- base::names(dataIdx) # get column names for this variable
    setQm <- base::numeric(0) # initialize the individual QMs we want to plot
    for (idxNameQmPlot in 1:base::length(NameQmPlot)) {
      setQm <- base::union(setQm,base::grep(NameQmPlot[idxNameQmPlot],nameColsIdx)) # Find the QMs in the list of those to plot
    }
    setQmPass <- base::intersect(setQm,base::grep("Pass",nameColsIdx)) # get column positions of pass quality metrics
    setQmFail <- base::intersect(setQm,base::grep("Fail",nameColsIdx)) # get column positions of fail quality metrics
    setQmNa <- base::intersect(setQm,base::grep("Na",nameColsIdx)) # get column positions of NA quality metrics
    setQmNa <- base::setdiff(setQmNa,base::union(setQmPass,setQmFail)) # Ensure QMs aren't chosen by accident due to "Na" being elsewhere in the name
    
    # Subset out the "pass", "fail", and "NA" quality metrics in long format
    dataQmPass <- base::data.frame(time=dataIdx$time, dataIdx[,nameColsIdx[setQmPass]])
    names(dataQmPass) <- base::c("time",nameColsIdx[setQmPass])
    dataQmPass <- reshape2::melt(dataQmPass,id="time")
    dataQmPass$variable <- base::sub("Pass","",dataQmPass$variable) # get rid of the "Pass" in the QM name
    dataQmPass$variable <- base::sub("Pos","",dataQmPass$variable) # get rid of the "Pos" in the QM name
    
    dataQmFail <- base::data.frame(time=dataIdx$time, dataIdx[,nameColsIdx[setQmFail]])
    names(dataQmFail) <- base::c("time",nameColsIdx[setQmFail])
    dataQmFail <- reshape2::melt(dataQmFail,id="time")
    dataQmFail$variable <- base::sub("Fail","",dataQmFail$variable) # get rid of the "Fail" in the QM name
    dataQmFail$variable <- base::sub("Pos","",dataQmFail$variable) # get rid of the "Pos" in the QM name
    
    dataQmNa <- base::data.frame(time=dataIdx$time, dataIdx[,nameColsIdx[setQmNa]])
    names(dataQmNa) <- base::c("time",nameColsIdx[setQmNa])
    dataQmNa <- reshape2::melt(dataQmNa,id="time")
    dataQmNa$variable <- base::sub("Na","",dataQmNa$variable) # get rid of the "Na" in the QM name
    dataQmNa$variable <- base::sub("Pos","",dataQmNa$variable) # get rid of the "Pos" in the QM name
    
    
    # Plot the passing, failing, and na quality metrics
    plotPass <- ggplot2::ggplot(dataQmPass,ggplot2::aes(x=time,y=value,color=factor(variable)))+ggplot2::geom_line() + 
      ggplot2::geom_point() + ggplot2::scale_color_discrete(name="QM") + ggplot2::theme_bw() + ggplot2::theme(legend.position="none") +
      ggplot2::labs(title=paste("Indiv. quality metrics:",idxVar),y="% Pass",x=" ")
    grobPass <- ggplot2::ggplotGrob(plotPass) # grab the grob for this plot for later manipulation
    
    plotFail <- ggplot2::ggplot(dataQmFail,ggplot2::aes(x=time,y=value,color=factor(variable)))+ggplot2::geom_line() +
      ggplot2::geom_point() + ggplot2::scale_color_discrete(name="QM") + ggplot2::theme_bw() + ggplot2::labs(y="% Fail",x=" ") +
      ggplot2::theme(legend.key=ggplot2::element_blank())
    grobFail <- ggplot2::ggplotGrob(plotFail) # grab the grob for this plot for later manipulation
    
    plotNa <- ggplot2::ggplot(dataQmNa,ggplot2::aes(x=time,y=value,color=factor(variable)))+ggplot2::geom_line() +
      ggplot2::geom_point() + ggplot2::scale_color_discrete(name="QM") + ggplot2::theme_bw() + ggplot2::labs(x="Date/Time",y="% NA") + 
      ggplot2::theme(legend.position="none")
    grobNa <- ggplot2::ggplotGrob(plotNa) # grab the grob for this plot for later manipulation
    
    # Adjust grobs and combine to align plots (since we want the legend only for the middle plot)
    colGrobLeg <- which(!(grobFail$widths %in% grobPass$widths)) # where the legend width is located in grobFail
    distWdthLeg <- grobFail$widths[colGrobLeg] # Get legend width
    grobPass <- gtable::gtable_add_cols(grobPass, distWdthLeg, colGrobLeg[1]-1) # Add an entry for the legend width matching grobFail
    grobNa <- gtable::gtable_add_cols(grobNa, distWdthLeg, colGrobLeg[1]-1) # Add an entry for the legend width matching grobFail
    
    grobComb <- gtable:::rbind_gtable(grobPass,grobFail,"first") # Combine 1st 2 grobs
    grobComb <- gtable:::rbind_gtable(grobComb,grobNa,"first") # Add last grob
    
    grobComb <- gtable::gtable_add_rows(grobComb,grid::unit(-.5,"cm"), pos=base::nrow(grobPass)) # Reduce vertical space between plots 1 & 2
    grobComb <- gtable::gtable_add_rows(grobComb,grid::unit(-.5,"cm"), pos=base::nrow(grobPass)+base::nrow(grobFail)) # Reduce vertical space between plots 2 & 3
    gridExtra::grid.arrange(grobComb,nrow=1) # plot it
   

    # Set up the alpha and beta metrics to plot
    # Subset out the alpha and beta quality metrics in long format
    dataQmAlphBeta <- reshape2::melt(data.frame(time=dataIdx$time, dataIdx[,c("qmAlph","qmBeta")]),id="time")

    # Plot the passing, failing, and na quality metrics
    plotAlphBeta <- ggplot2::ggplot(dataQmAlphBeta,ggplot2::aes(x=time,y=value,color=factor(variable)))+ ggplot2::geom_line() +
      ggplot2::geom_point() + ggplot2::scale_color_discrete(name="QM") + ggplot2::theme_bw() + ggplot2::theme(legend.key=ggplot2::element_blank()) +
      ggplot2::labs(ggplot2::labs(title=paste("Final quality metrics:",idxVar),y="%",x=" "))
    grobAlphBeta <- ggplot2::ggplotGrob(plotAlphBeta) # grab the grob for this plot for later manipulation
    
    plotFinl <- ggplot2::ggplot(dataIdx,ggplot2::aes(x=time,y=qfFinl)) + ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::theme_bw() + ggplot2::labs(y="Final QF",x="Date/Time") + ggplot2::theme(legend.key=ggplot2::element_blank()) +
      ggplot2::coord_cartesian(ylim=c(0,1)) + ggplot2::scale_y_continuous(breaks=c(0,1))
    grobFinl <- ggplot2::ggplotGrob(plotFinl) # grab the grob for this plot for later manipulation
    
    # Adjust grobs and combine to align plots (since we want the legend only for the top plot)
    colGrobLeg <- which(!(grobAlphBeta$widths %in% grobFinl$widths)) # where the legend width is located in grobFail
    distWdthLeg <- grobAlphBeta$widths[colGrobLeg] # Get legend width
    grobFinl <- gtable::gtable_add_cols(grobFinl, distWdthLeg, colGrobLeg[1]-1) # Add an entry for the legend width matching grobAlphBeta

    grobCombFinl <- gtable:::rbind_gtable(grobAlphBeta,grobFinl,"first") # Combine grobs

    grobCombFinl <- gtable::gtable_add_rows(grobCombFinl,grid::unit(-.5,"cm"), pos=nrow(grobAlphBeta)) # Reduce vertical space between plots 1 & 2
    gridExtra::grid.arrange(grobCombFinl,nrow=1) # plot it
     
  }
  
}