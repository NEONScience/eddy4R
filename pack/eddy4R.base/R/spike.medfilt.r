##############################################################################################
#' @title Median filter de-spiking

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-14)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Median filter de-spiking.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################



spike.medfilt <- function(DESP=list(
  #input data, univariate vector of integers or numerics
    dati,
  #filter width
    widt = 9,
  #initial number/step size of histogram bins
    nbin = 2,
  #resolution threshold
    rest = 10
  )) {
  
  # median filter de-spiking  
    # after Brock (1986), Starkenburg et al. (2014);
    # order N = 3 - 4 (i.e., a window of 7 - 9 points);
    # such a filter will be sensitive to spikes of up to 3 - 4 consecutive points (Brock, 1986);
    # this is the maximum number of points typically allowed to be considered spikes (Vickers and Mahrt, 1997; Mauder and Foken, 2011);
      # raw signal is normalized so that its mean is zero and standard deviation is 1; 
      # initial number of bins is 2, and doubles iteratively until minima are found;
      # distribution  of the differences (D1) between the filtered signal and the raw signal is assessed;
      # good data points will be centered within the histogram near zero;
      # spikes will lie farthest from the center, resulting in subpopulations that make the distribution multi-modal;
      # determine the the range of accepted values, DT by searching the histogram for the first minima from the center;
      # points where |D1| > DT are considered spikes, provided the difference is >= 10 x the measurement resolution (smallest detected change);
      # escapes;
        # -1: no non-NAs in dataset
        # -2: singular or constant value
        # -3: measurement changes so slow that time-series and filter value are identical

  
  
  ###
  #start intercept 1: 
  #less non-NAs in dataset than required for 0.5 h out of 24 h of data (approx 2.5%)
  if(length(which(!is.na(DESP$dati)))/length(DESP$dati) < 0.025) {
  ###

    
    
    DESP$nbin_loc <- -1
    DESP$lens <- -1
    DESP$dato <- DESP$dati      
    
    
    
  ###
  #mid intercept 1
  #no non-NAs in dataset
  } else {
  ###
    
    

    #normalizing time-series to mean = 0 and sd = 1
      DESP$datn <- (DESP$dati - mean(DESP$dati, na.rm=TRUE)) / sd(DESP$dati, na.rm=TRUE)  
    #determine differences
      DESP$datn_diff <- diff(DESP$datn)
  
    
      
    ###
    #start intercept 2: 
    #(i) singular or constant value, or (ii) no consecutive values.
    #in (i) the first case sd(DESP$dati) will return NA, and so DESP$datn will be all NAs
    #in (ii) all differences are NA
    if(length(which(!is.na(DESP$datn_diff))) == 0) {
    ###
      
      
      
      DESP$nbin_loc <- -2
      DESP$lens <- -2
      DESP$dato <- DESP$dati      
      
      
      
    ###
    #mid intercept 2
    #singular or constant value
    } else {
    ###


        
      #determine measurement resolution (assumed to be smallest recorded change in data series)
        DESP$datn_reso <- min(abs(DESP$datn_diff[which(DESP$datn_diff != 0)]), na.rm=TRUE)
      #     plot(DESP$dati[1:100], type="l")
      #     min(diff(DESP$dati[1:100]), na.rm=TRUE)
      
      #apply median filter and calculate differences
        #minimum number of non-NAs in window to calculate median, else return NA
          DESP$noNA <- max(c(5, floor(1/2 * DESP$widt)))
        #calculating median-filtered time-series
          DESP$filt <- robfilter::med.filter(y=DESP$datn, width=DESP$widt, minNonNAs=DESP$noNA, online=FALSE, extrapolate=FALSE)
      
        #intercept if robfilter returns NA
      
          if(length(DESP$filt) == 1) {
            
            DESP$diff <- rep(0, length(DESP$dati))
            
          } else {
          
            #calculating histogram of differences
              DESP$diff <- DESP$filt$y - DESP$filt$level[,1]
        
          }
        
      
      ###
      #start intercept 3: 
      #measurement changes so slow that time-series and filter value are identical
      #Or: less non-NAs in differences than required for 0.5 h out of 24 h of data (approx 2.5%)      
      if(max(DESP$diff, na.rm=TRUE) == 0 |
         length(which(!is.na(DESP$diff)))/length(DESP$dati) < 0.025 |
         length(na.omit(unique(DESP$diff)))/length(DESP$dati) < 0.001
         ) {
      ###
        
        
        
        DESP$nbin_loc <- -3
        DESP$lens <- -3
        DESP$dato <- DESP$dati      
    
        
        
      ###
      #mid intercept 3
      #measurement changes fast enough so that time-series and filter value are not identical
      } else {
      ###
        
    
        
          ###
          #start while loop around PDF bins
          DESP$crit <- FALSE
          #DESP$runner <- 1
          DESP$nbin_loc <- DESP$nbin
          while(DESP$crit == FALSE) {
          ###
            
            
            
            #number of histogram bins
              #DESP$nbin_loc <- DESP$nbin * DESP$runner + 1
              DESP$bins <- seq(min(DESP$diff, na.rm=TRUE), max(DESP$diff, na.rm=TRUE), length.out=DESP$nbin_loc)
            #calculate histogram
              DESP$hist <- hist(DESP$diff, breaks = DESP$bins, plot=FALSE)
            
            #determine bin with most values
              DESP$maxi <- which.max(DESP$hist$counts)
            #data needs to have extrema, otherwise it will not identify the zero crossing
            #hence attaching sin(1:10) to the end
              DESP$mini <- EMD::extrema(y=c(DESP$hist$counts, sin(1:10)))$minindex[,1]
            #remove sin(1:10) minima from end
              DESP$mini <- DESP$mini[-which(DESP$mini > (DESP$nbin_loc - 1))]
          
            #minimum threshold
              DESP$trmi <- which(DESP$hist$counts[DESP$mini] == 0 & DESP$mini < DESP$maxi)
            #maximum threshold
              DESP$trma <- which(DESP$hist$counts[DESP$mini] == 0 & DESP$mini > DESP$maxi)
            
      #      #message to screen
      #        print(paste("PDF with ", DESP$nbin_loc, " bins is completed", sep=""))
          
            #conditional statement for while loop
            #continue iterating if no bins with zero entries are found below AND above distribution maximum, else calculate thresholds
              if(length(DESP$trmi) == 0 | length(DESP$trma) == 0) {
                
                #DESP$runner <- DESP$runner + 1
                DESP$nbin_loc <- 2 * DESP$nbin_loc
                
              } else {
          
                #final minimum threshold, i.e. lower break of closest bin with zero observations below distribution maximum
                  DESP$trmi_out <- DESP$hist$breaks[DESP$mini[DESP$trmi][length(DESP$trmi)]]
                #final maximum threshold, i.e. upper break of closest bin with zero observations above distribution maximum
                  DESP$trma_out <- DESP$hist$breaks[DESP$mini[DESP$trma][1] + 1]
                
                DESP$crit <- TRUE
                
              }
            
            
            
          ###
          }
          #end while loop around PDF bins
          ###
          
          
          
          #determine indices of spikes
          #considers spikes only if difference larger than measurement resolution x DESP$rest
            DESP$whrs <- which(
                              DESP$diff < DESP$trmi_out & abs(DESP$diff) > DESP$datn_reso * DESP$rest |
                              DESP$diff > DESP$trma_out & abs(DESP$diff) > DESP$datn_reso * DESP$rest
                            )
            DESP$lens <- length(DESP$whrs)
        
          #remove spikes
            DESP$dato <- DESP$dati
            if(DESP$lens > 0) DESP$dato[DESP$whrs] <- NA

  
      
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
#    print(paste("De-spiking completed, ", DESP$lens, " spikes have been removed", sep=""))

  #return des-spiked time-series and all additional info
    return(DESP)

  }
