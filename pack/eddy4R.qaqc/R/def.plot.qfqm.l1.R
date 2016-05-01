##############################################################################################
#' @title Plot quality flags and quality metrics (basic L1 data products) 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Function definition. Plots the aggregated quality flags, quality metrics and final quality flag for basic L1 (time window averaged) data products as output from def.qfqm.l1.R.

#' @param dataL1 Required input. A list output from def.qfqm.l1.R of: \cr
#' tsAggrStrt - the starting time stamp of aggregated L1 data and quality metrics \cr
#' qfqm - a list of named variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, standard deviation of the mean, number of points going into the average, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in flgs, as well as the alpha & beta quality metrics and final quality flag. It is important that the column names of this data frame are indistinguishable from those that would be created from def.qfqm.l1.R
#' @param DateRng Optional. A 2-element POSIXlt vector of the minimum and maximum time range to plot. Default is the entire data range.
#' @param NameQmPlot Optional. A character vector listing the individual quality metrics to plot. The strings in this vector can be partial names, i.e. a partial match will result in the quality metric being plotted (ex. NameQmIndiv <- c("Step") will result in the quality metrics "qmPosFlagStepPass","qmPosFlagStepFail" and "qmPosFlagStepNa" to be plotted). Default is all QMs. 

#' @return Running this function will output 3 plots per data variable: 1) basic L1 statistics (mean, min, max, etc). 2) Pass, Fail, and NA quality metrics for ever flag, 3) the final Alpha and Beta quality metrics and the final quality flag.

#' @references 
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113)

#' @keywords NEON QAQC, quality flags and metrics, L1 average, final quality flag

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-02-09)
#     original creation
##############################################################################################


def.plot.qfqm.l1 <- function (
  dataL1,             # the list output from def.qfqm.l1.R. Required input.
  DateRng = c(min(dataL1$tsAggrStrt),max(dataL1$tsAggrStrt)), # a 2-element POSIXlt vector of the minimum and maximum time range to plot. Default is entire range
  NameQmPlot = sub("Pass","",names(dataL1$qfqm[[1]][grep("Pass",names(dataL1$qfqm[[1]]))])) # a character vector listing the individual quality metrics to plot. The strings in this vector can be partial names, i.e. a partial match will result in the quality metric being plotted (ex. NameQmIndiv <- "Step" will result in the quality metrics "qmPosFlagStepPass","qmPosFlagStepFail" and "qmPosFlagStepNa" to be plotted). Default is all QMs. 
  ) {
  

# Load Libraries ----------------------------------------------------------
  
  if (!require(ggplot2)) {
    stop("Please install package 'ggplot2' before continuing")
  }
  if (!require(Rmisc)) {
    stop("Please install package 'Rmisc' before continuing")
  }
  if (!require(reshape2)) {
    stop("Please install package 'reshape2' before continuing")
  }
  if (!require(gridExtra)) {
    stop("Please install package 'gridExtra' before continuing")
  }
  if (!require(gtable)) {
    stop("Please install package 'gtable' before continuing")
  }
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(missing("dataL1") | !is.list(dataL1)) {
    stop("Required input 'data' must be a list output from def.qfqm.l1.R")
  }
  
  # Check DateRng
  DateRng <- try(as.POSIXlt(DateRng),silent=TRUE)
  if(class(DateRng)[1] == "try-error"){
    stop("Input parameter DateRng must be of class POSIXlt")
  } else if(length(DateRng) != 2) {
    stop("Input parameter DateRng must be a POSIXlt vector of length 2")
  } else if ((min(DateRng) > max(dataL1$tsAggrStrt)) | (max(DateRng) < min(dataL1$tsAggrStrt))) {
    stop("Check input parameter DateRng. The selected date range does not cover any portion of the data.")
  }
  
  
# Do Plotting -------------------------------------------------------------
  
  # Plot the L1 basic stats
  numVars <- length(dataL1$qfqm)
  nameVars <- names(dataL1$qfqm)
    
  for(idxVar in 1:numVars) {
    
    # Pull out data to plot
    dataIdx <- dataL1$qfqm[[idxVar]]
    dataIdx$ts <- dataL1$tsAggrStrt
    nameVarsIdx <- nameVars[idxVar]
    
    # Set of plots for basic stats: Mean, min, max, var, ste, and numPts
    dataIdxQf <- data.frame(ts=dataIdx$ts,val=dataIdx$mean,valQfFinlFail=dataIdx$mean)
    dataIdxQf$valQfFinlFail[dataIdx$qfFinl==0] <- NA
    dataIdxQf <- reshape2::melt(dataIdxQf,id="ts")
    plotMean <- ggplot(data=dataIdx,aes(x=ts,y=mean)) + geom_line() +
      geom_point() + 
      geom_point(data=dataIdxQf,aes(x=ts,y=value,color=factor(variable))) + 
      theme_bw()+
      scale_colour_manual(name="",values=c("black","red"),labels=c("Final QF=0","Final QF=1")) +
      theme(legend.title=element_blank(),legend.position=c(1,1),legend.justification=c(1,1)) +
      theme(legend.background=element_blank(),legend.key=element_blank()) + 
      labs(title=" ",x="Date/Time",y="Mean")
    
    plotMin <- ggplot() + geom_line(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$min)) +
      geom_point(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$min)) + 
      labs(title=nameVarsIdx,x="Date/Time",y="Minimum") + theme_bw() +
      geom_point(aes(x=dataIdx$ts[dataIdx[,"qfFinl"]==1],y=dataIdx$min[dataIdx[,"qfFinl"]==1]),color="red")
    
    plotMax <- ggplot() + geom_line(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$max)) +
      geom_point(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$max)) + 
      labs(title=" ",x="Date/Time",y="Maximum") + theme_bw() +
      geom_point(aes(x=dataIdx$ts[dataIdx[,"qfFinl"]==1],y=dataIdx$max[dataIdx[,"qfFinl"]==1]),color="red")
    
    
    plotVar <- ggplot() + geom_line(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$var)) +
      geom_point(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$var)) + 
      labs(title=" ",x="Date/Time",y="Variance") + theme_bw() +
      geom_point(aes(x=dataIdx$ts[dataIdx[,"qfFinl"]==1],y=dataIdx$var[dataIdx[,"qfFinl"]==1]),color="red")
    
    
    plotSte <- ggplot() + geom_line(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$ste)) +
      geom_point(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$ste)) + 
      labs(title=" ",x="Date/Time",y="Standard error") + theme_bw() +
      geom_point(aes(x=dataIdx$ts[dataIdx[,"qfFinl"]==1],y=dataIdx$ste[dataIdx[,"qfFinl"]==1]),color="red")
    
 
    plotNumPts <- ggplot() + geom_line(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$numPts)) +
      geom_point(data=dataIdx,aes(x=dataIdx$ts,y=dataIdx$numPts)) + 
      labs(title=" ",x="Date/Time",y="Number of points") + theme_bw() +
      geom_point(aes(x=dataIdx$ts[dataIdx[,"qfFinl"]==1],y=dataIdx$numPts[dataIdx[,"qfFinl"]==1]),color="red")
      
    
    Rmisc::multiplot(plotMean,plotVar,plotMin,plotSte,plotMax,plotNumPts,cols=3)
    
    
    # Set up the quality metrics to plot
    nameColsIdx <- names(dataIdx) # get column names for this variable
    posQm <- numeric(0) # initialize the individual QMs we want to plot
    for (idxNameQmPlot in 1:length(NameQmPlot)) {
      posQm <- union(posQm,grep(NameQmPlot[idxNameQmPlot],nameColsIdx)) # Find the QMs in the list of those to plot
    }
    posQmPass <- intersect(posQm,grep("Pass",nameColsIdx)) # get column positions of pass quality metrics
    posQmFail <- intersect(posQm,grep("Fail",nameColsIdx)) # get column positions of fail quality metrics
    posQmNa <- intersect(posQm,grep("Na",nameColsIdx)) # get column positions of NA quality metrics
    posQmNa <- setdiff(posQmNa,union(posQmPass,posQmFail)) # Ensure QMs aren't chosen by accident due to "Na" being elsewhere in the name
    
    # Subset out the "pass", "fail", and "NA" quality metrics in long format
    dataQmPass <- data.frame(ts=dataIdx$ts, dataIdx[,nameColsIdx[posQmPass]])
    names(dataQmPass) <- c("ts",nameColsIdx[posQmPass])
    dataQmPass <- reshape2::melt(dataQmPass,id="ts")
    dataQmPass$variable <- sub("Pass","",dataQmPass$variable) # get rid of the "Pass" in the QM name
    dataQmPass$variable <- sub("Pos","",dataQmPass$variable) # get rid of the "Pos" in the QM name
    
    dataQmFail <- data.frame(ts=dataIdx$ts, dataIdx[,nameColsIdx[posQmFail]])
    names(dataQmFail) <- c("ts",nameColsIdx[posQmFail])
    dataQmFail <- reshape2::melt(dataQmFail,id="ts")
    dataQmFail$variable <- sub("Fail","",dataQmFail$variable) # get rid of the "Fail" in the QM name
    dataQmFail$variable <- sub("Pos","",dataQmFail$variable) # get rid of the "Pos" in the QM name
    
    dataQmNa <- data.frame(ts=dataIdx$ts, dataIdx[,nameColsIdx[posQmNa]])
    names(dataQmNa) <- c("ts",nameColsIdx[posQmNa])
    dataQmNa <- reshape2::melt(dataQmNa,id="ts")
    dataQmNa$variable <- sub("Na","",dataQmNa$variable) # get rid of the "Na" in the QM name
    dataQmNa$variable <- sub("Pos","",dataQmNa$variable) # get rid of the "Pos" in the QM name
    
    
    # Plot the passing, failing, and na quality metrics
    plotPass <- ggplot(dataQmPass,aes(x=ts,y=value,color=factor(variable)))+geom_line() + 
      geom_point() + scale_color_discrete(name="QM") + theme_bw() + theme(legend.position="none") +
      labs(title=paste("Indiv. quality metrics:",nameVarsIdx),y="% Pass",x=" ")
    grobPass <- ggplotGrob(plotPass) # grab the grob for this plot for later manipulation
    
    plotFail <- ggplot(dataQmFail,aes(x=ts,y=value,color=factor(variable)))+geom_line() +
      geom_point() + scale_color_discrete(name="QM") + theme_bw() + labs(y="% Fail",x=" ") +
      theme(legend.key=element_blank())
    grobFail <- ggplotGrob(plotFail) # grab the grob for this plot for later manipulation
    
    plotNa <- ggplot(dataQmNa,aes(x=ts,y=value,color=factor(variable)))+geom_line() +
      geom_point() + scale_color_discrete(name="QM") + theme_bw() + labs(x="Date/Time",y="% NA") + 
      theme(legend.position="none")
    grobNa <- ggplotGrob(plotNa) # grab the grob for this plot for later manipulation
    
    # Adjust grobs and combine to align plots (since we want the legend only for the middle plot)
    colGrobLeg <- 5 # where the legend width is located in grobFail
    distWdthLeg <- grobFail$widths[colGrobLeg] # Get legend width
    grobPass <- gtable::gtable_add_cols(grobPass, distWdthLeg, colGrobLeg-1) # Add an entry for the legend width matching grobFail
    grobNa <- gtable::gtable_add_cols(grobNa, distWdthLeg, colGrobLeg-1) # Add an entry for the legend width matching grobFail
    
    grobComb <- gtable:::rbind_gtable(grobPass,grobFail,"first") # Combine 1st 2 grobs
    grobComb <- gtable:::rbind_gtable(grobComb,grobNa,"first") # Add last grob
    
    grobComb <- gtable::gtable_add_rows(grobComb,unit(-.5,"cm"), pos=nrow(grobPass)) # Reduce vertical space between plots 1 & 2
    grobComb <- gtable::gtable_add_rows(grobComb,unit(-.5,"cm"), pos=nrow(grobPass)+nrow(grobFail)) # Reduce vertical space between plots 2 & 3
    gridExtra::grid.arrange(grobComb,nrow=1) # plot it
   

    # Set up the alpha and beta metrics to plot
    # Subset out the alpha and beta quality metrics in long format
    dataQmAlphBeta <- reshape2::melt(data.frame(ts=dataIdx$ts, dataIdx[,c("qmAlpha","qmBeta")]),id="ts")

    # Plot the passing, failing, and na quality metrics
    plotAlphBeta <- ggplot(dataQmAlphBeta,aes(x=ts,y=value,color=factor(variable)))+ geom_line() +
      geom_point() + scale_color_discrete(name="QM") + theme_bw() + theme(legend.key=element_blank()) +
      labs(labs(title=paste("Final quality metrics:",nameVarsIdx),y="%",x=" "))
    grobAlphBeta <- ggplotGrob(plotAlphBeta) # grab the grob for this plot for later manipulation
    
    plotFinl <- ggplot(dataIdx,aes(x=ts,y=qfFinl)) + geom_line() + geom_point() +
      theme_bw() + labs(y="Final QF",x="Date/Time") + theme(legend.key=element_blank()) +
      coord_cartesian(ylim=c(0,1)) + scale_y_continuous(breaks=c(0,1))
    grobFinl <- ggplotGrob(plotFinl) # grab the grob for this plot for later manipulation
    
    # Adjust grobs and combine to align plots (since we want the legend only for the top plot)
    colGrobLeg <- 5 # where the legend width is located in grobAlphBeta
    distWdthLeg <- grobFail$widths[colGrobLeg] # Get legend width
    grobFinl <- gtable::gtable_add_cols(grobFinl, distWdthLeg, colGrobLeg-1) # Add an entry for the legend width matching grobAlphBeta

    grobCombFinl <- gtable:::rbind_gtable(grobAlphBeta,grobFinl,"first") # Combine grobs

    grobCombFinl <- gtable::gtable_add_rows(grobCombFinl,unit(-.5,"cm"), pos=nrow(grobAlphBeta)) # Reduce vertical space between plots 1 & 2
    gridExtra::grid.arrange(grobCombFinl,nrow=1) # plot it
     
  }
  
}