#' Define Simple Rotation of Wind Vectors
#' 
#' Perform various types of wind vector rotations incl. single, double
#' 
#' @param data data.frame containing veloXaxs, veloYaxs and veloZaxs \cr
#'             vectors should be defined correspondingly as u/v/w == E/N/U == x/y/z [data.frame]
#' @param MethRot method of rotation to be used, one of: \itemize{
#'                \item "single" - rotate into the mean wind
#'                \item "double" - apply single rotation and additionally rotate to align w and z (minimise w)
#'                \item "none" - perform no rotation
#' }
#' @return Data object with rotated wind vectors names as veloXaxs, veloYaxs and veloZaxs with correction angle attributes
#' 
#' @references 
#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' 
#' @export
#' 

def.rot.2d = function(data, MethRot = c("single","double","none")[2]){

  #Check rotation type
  if(MethRot %in% c("single","double")){
    rotAngXaxs <- atan2(mean(data$veloYaxsErth, na.rm = TRUE),mean(data$veloXaxsErth, na.rm = TRUE))
    
    tmpVeloXaxs <- data$veloXaxsErth*cos(rotAngXaxs) + data$veloYaxsErth*sin(rotAngXaxs)
    tmpVeloYaxs <- -data$veloXaxsErth*sin(rotAngXaxs) + data$veloYaxsErth*cos(rotAngXaxs) 
    tmpVeloZaxs <- data$veloZaxsErth

    if(MethRot == "double"){
      #second rotation
    rotAngYaxs = atan2(mean(tmpVeloZaxs,na.rm = TRUE),mean(tmpVeloXaxs,na.r = TRUE))
    tmpVeloXaxs <- tmpVeloXaxs*cos(rotAngYaxs) + tmpVeloZaxs*sin(rotAngYaxs)
    tmpVeloYaxs <- tmpVeloYaxs
    tmpVeloZaxs <- -tmpVeloXaxs*sin(rotAngYaxs) + tmpVeloZaxs*cos(rotAngYaxs)
      
    }
    
    data$veloXaxsErth <- tmpVeloXaxs
    data$veloYaxsErth <- tmpVeloYaxs
    data$veloZaxsErth <- tmpVeloZaxs
    
    #Add attribute for Xaxs rotation (into mean wind)
    attributes(data)$rotAngXaxs <- rotAngXaxs
    #Add attribute for Yaxs rotation
    attributes(data)$rotAngYaxs <- ifelse(MethRot == "double", rotAngYaxs, NA)
    
  }
  
  if(MethRot == "none"){
    return(data)
  }
  
}