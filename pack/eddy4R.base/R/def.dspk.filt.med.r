##############################################################################################
#' @title Median filter de-spiking

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Median filter de-spiking\cr
#' After Brock (1986), Starkenburg et al. (2014); \cr
#' Order N = 3 - 4 (i.e., a window of 7 - 9 points); \cr
#' Such a filter will be sensitive to spikes of up to 3 - 4 consecutive points (Brock, 1986);
#' this is the maximum number of points typically allowed to be considered spikes (Vickers and Mahrt, 1997; Mauder and Foken, 2011);
#' Raw signal is normalized so that its mean is zero and standard deviation is 1; \cr
#' Initial number of bins is 2, and doubles iteratively until minima are found; \cr
#' Distribution  of the differences (D1) between the filtered signal and the raw signal is assessed;
#' good data points will be centered within the histogram near zero;
#' spikes will lie farthest from the center, resulting in subpopulations that make the distribution multi-modal; \cr
#' Determine the the range of accepted values, DT by searching the histogram for the first minima from the center;
#' Points where |D1| > DT are considered spikes, provided the difference is >= 10 x the measurement resolution (smallest detected change);
#' Error escapes:\cr
#' -1: no non-NAs in dataset \cr
#' -2: singular or constant value \cr
#' -3: measurement changes so slow that time-series and filter value are identical \cr

#' @param \code{dataIn} Required. A univariate vector of integers or numerics of Input data
#' @param \code{WidtFilt} Optional. A single integer value of filter width. Default = 9
#' @param \code{NumBin} Optional. A single integer value of the initial number/step size of histogram bins. Default = 2
#' @param \code{ThshReso} Optional. A single integer value of the resolution threshold. Default = 10

#' @return A list of: \cr
#' \code{dataIn} Same as input.
#' \code{WidtFilt} Same as input.
#' \code{NumBin} Same as input.
#' \code{ThshReso} Same as input.
#' \code{numBinFinl} Integer. The final number of histogram bins used.
#' \code{numSpk} Integer. The number of spikes identified.
#' \code{dataOut} Numeric vector of input data with spikes removed.
#' \code{dataNorm} Numeric vector of input data normalized to mean 0 and standard deviation of 1
#' \code{dataNormDiff} Numeric vector of differences between subsequent values of \code{dataNorm}. Length = 1-length(dataIn)
#' \code{resoDataNorm} Numeric value. Measurement resolution (assumed to be smallest recorded change in \code{dataNorm})
#' \code{thshNumData} Numeric value. Minimum number of non-NAs in window to calculate median, otherwise NA is returned.
#' \code{dataNormFiltMed} Numeric vector. Median-filtered timeseries of \code{dataNorm}. 
#' \code{histDiff} Numeric vector. Initial histogram of differences of \code{dataNormFiltMed}
#' \code{crit} Logical. Criteria for ending iteration over histogram bins
#' \code{locBin} Numeric vector. Bin edges for \code{histDiff}
#' \code{histDiffFinl} Numeric vector. Final histogram of differences of \code{dataNormFiltMed} using bin edges in \code{locBin}
#' \code{posBinMin} Integer. Index of histogram bin within \code{histDiffFinl} with minimum number of counts.
#' \code{posBinMax} Integer. Index of histogram bin within \code{histDiffFinl} with maximum number of counts.
#' \code{posThshBinMin} Integer. Current iteration of minimum threshold index of histogram bin within \code{histDiffFinl} for spike determination
#' \code{posThshBinMax}  Integer. Current iteration of maximum threshold index of histogram bin within \code{histDiffFinl} for spike determination
#' \code{posThshBinMinFinl} Integer. Final minimum threshold index of histogram bin within \code{histDiffFinl} for spike determination
#' \code{posThshBinMinFinl} Integer. Final maximum threshold index of histogram bin within \code{histDiffFinl} for spike determination
#' \code{posSpk} Indices of determined spikes within \code{dataIn}

#' 
#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-14)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Cove Sturtevant (2016-09-21)
#     conform to eddy4R coding style
#     
##############################################################################################

def.dspk.filt.med <- function(
    dataIn, # input data, univariate vector of integers or numerics
    WidtFilt = 9, # filter width
    NumBin = 2, # initial number/step size of histogram bins
    ThshReso = 10 # resolution threshold
  ) {
  
  # Initialize output
  rpt <- base::list(dataIn=dataIn,WidtFilt=WidtFilt,NumBin=NumBin,ThshReso=ThshReso)
  
  #Error catching 1: 
  #fewer non-NAs in dataset than required for 0.5 h out of 24 h of data (approx 2.5%)
  if(base::length(base::which(!base::is.na(rpt$dataIn)))/base::length(rpt$dataIn) < 0.025) {
  ###

    
    
    rpt$numBinFinl <- -1 # Set to error code
    rpt$numSpk <- -1 # Set to error code
    rpt$dataOut <- rpt$dataIn      
    
    
    
  ###
  #mid error catching 1
  #no non-NAs in dataset
  } else {
  ###
    
    

    #normalizing time-series to mean = 0 and sd = 1
      rpt$dataNorm <- (rpt$dataIn - base::mean(rpt$dataIn, na.rm=TRUE)) / stats::sd(rpt$dataIn, na.rm=TRUE)  
    #determine differences
      rpt$dataNormDiff <- base::diff(rpt$dataNorm)
  
    
      
    ###
    #Error catching 2: 
    #(i) singular or constant value, or (ii) no consecutive values.
    #in (i) the first case sd(rpt$dataIn) will return NA, and so rpt$dataNorm will be all NAs
    #in (ii) all differences are NA
    if(base::length(base::which(!base::is.na(rpt$dataNormDiff))) == 0) {
    ###
      
      
      
      rpt$numBinFinl <- -2 # Set to error code
      rpt$numSpk <- -2 # Set to error code
      rpt$dataOut <- rpt$dataIn      
      
      
      
    ###
    #mid intercept 2
    #singular or constant value
    } else {
    ###


        
      #determine measurement resolution (assumed to be smallest recorded change in data series)
        rpt$resoDataNorm <- base::min(base::abs(rpt$dataNormDiff[base::which(rpt$dataNormDiff != 0)]), na.rm=TRUE)
      #     plot(rpt$dataIn[1:100], type="l")
      #     min(diff(rpt$dataIn[1:100]), na.rm=TRUE)
      
      #apply median filter and calculate differences
        #minimum number of non-NAs in window to calculate median, else return NA
          rpt$thshNumData <- base::max(c(5, base::floor(1/2 * rpt$WidtFilt)))
        #calculating median-filtered time-series
          rpt$dataNormFiltMed <- robfilter::med.filter(y=rpt$dataNorm, width=rpt$WidtFilt, minNonNAs=rpt$thshNumData, online=FALSE, extrapolate=FALSE)
      
        #intercept if robfilter returns NA
      
          if(base::length(rpt$dataNormFiltMed) == 1) {
            
            rpt$histDiff <- base::rep(0, base::length(rpt$dataIn))
            
          } else {
          
            #calculating histogram of differences
              rpt$histDiff <- rpt$dataNormFiltMed$y - rpt$dataNormFiltMed$level[,1]
        
          }
        
      
      ###
      #Error catching 3: 
      #measurement changes so slow that time-series and filter value are identical
      #Or: less non-NAs in differences than required for 0.5 h out of 24 h of data (approx 2.5%)      
      if(base::max(rpt$histDiff, na.rm=TRUE) == 0 |
         base::length(base::which(!base::is.na(rpt$histDiff)))/base::length(rpt$dataIn) < 0.025 |
         base::length(stats::na.omit(base::unique(rpt$histDiff)))/base::length(rpt$dataIn) < 0.001
         ) {
      ###
        
        
        
        rpt$numBinFinl <- -3 # Set to error code
        rpt$numSpk <- -3 # Set to error code
        rpt$dataOut <- rpt$dataIn      
    
        
        
      ###
      #mid intercept 3
      #measurement changes fast enough so that time-series and filter value are not identical
      } else {
      ###
        
    
        
          ###
          #start while loop around PDF bins
          rpt$crit <- FALSE
          #rpt$iter <- 1
          rpt$numBinFinl <- rpt$NumBin
          while(rpt$crit == FALSE) {
          ###
            
            
            
            #number of histogram bins
              #rpt$numBinFinl <- rpt$NumBin * rpt$iter + 1
              rpt$locBin <- base::seq(base::min(rpt$histDiff, na.rm=TRUE), base::max(rpt$histDiff, na.rm=TRUE), length.out=rpt$numBinFinl)
            #calculate histogram
              rpt$histDiffFinl <- graphics::hist(rpt$histDiff, breaks = rpt$locBin, plot=FALSE)
            
            #determine bin with most values
              rpt$posBinMax <- base::which.max(rpt$histDiffFinl$counts)
            #data needs to have extrema, otherwise it will not identify the zero crossing
            #hence attaching sin(1:10) to the end
              rpt$posBinMin <- EMD::extrema(y=c(rpt$histDiffFinl$counts, base::sin(1:10)))$minindex[,1]
            #remove sin(1:10) minima from end
              rpt$posBinMin <- rpt$posBinMin[-base::which(rpt$posBinMin > (rpt$numBinFinl - 1))]
          
            #minimum threshold
              rpt$posThshBinMin <- base::which(rpt$histDiffFinl$counts[rpt$posBinMin] == 0 & rpt$posBinMin < rpt$posBinMax)
            #maximum threshold
              rpt$posThshBinMax <- base::which(rpt$histDiffFinl$counts[rpt$posBinMin] == 0 & rpt$posBinMin > rpt$posBinMax)
            
      #      #message to screen
      #        print(paste("PDF with ", rpt$numBinFinl, " bins is completed", sep=""))
          
            #conditional statement for while loop
            #continue iterating if no bins with zero entries are found below AND above distribution maximum, else calculate thresholds
              if(base::length(rpt$posThshBinMin) == 0 | length(rpt$posThshBinMax) == 0) {
                
                #rpt$iter <- rpt$iter + 1
                rpt$numBinFinl <- 2 * rpt$numBinFinl
                
              } else {
          
                #final minimum threshold, i.e. lower break of closest bin with zero observations below distribution maximum
                  rpt$posThshBinMinFinl <- rpt$histDiffFinl$breaks[rpt$posBinMin[rpt$posThshBinMin][base::length(rpt$posThshBinMin)]]
                #final maximum threshold, i.e. upper break of closest bin with zero observations above distribution maximum
                  rpt$posThshBinMaxFinl <- rpt$histDiffFinl$breaks[rpt$posBinMin[rpt$posThshBinMax][1] + 1]
                
                rpt$crit <- TRUE
                
              }
            
            
            
          ###
          }
          #end while loop around PDF bins
          ###
          
          
          
          #determine indices of spikes
          #considers spikes only if difference larger than measurement resolution x rpt$ThshReso
            rpt$posSpk <- base::which(
                              rpt$histDiff < rpt$posThshBinMinFinl & base::abs(rpt$histDiff) > rpt$resoDataNorm * rpt$ThshReso |
                              rpt$histDiff > rpt$posThshBinMaxFinl & base::abs(rpt$histDiff) > rpt$resoDataNorm * rpt$ThshReso
                            )
            rpt$numSpk <- base::length(rpt$posSpk) # Number of spikes found
        
          #remove spikes
            rpt$dataOut <- rpt$dataIn
            if(rpt$numSpk > 0) rpt$dataOut[rpt$posSpk] <- NA

  
      
      ###
      }
      #end intercept 3
      #measurement changes fast enough so that time-series and filter value are not identical
      ###
  
      
      
    ###
    }
    #end intercept 2
    #singular or constant value
    ###

    
    
  ###
  }
  #end intercept 1
  #no non-NAs in dataset
  ###
  
  
  
#  #print message to screen
#    print(paste("De-spiking completed, ", rpt$numSpk, " spikes have been removed", sep=""))

  #return des-spiked time-series and all additional info
    return(rpt)

  }
