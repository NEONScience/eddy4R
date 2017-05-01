

wrap.neon.dp01.agr.prd <- function(
 inpList,
 
  
  
){

rpt <- list()
tmp <- list()

#Grab sub-indices to calculate shorter timescale
tmp$idx <- def.idx.agr(time = wrk$data$time$UTC, PrdAgr = 60, FreqLoca = 20)

tmp$idxSoni <- def.idx.agr(time = DATA$soniAmrs$time, PrdAgr = 120, FreqLoca = 20)

tmp$idxSoniAmrs <- def.idx.agr(time = DATA$soniAmrs$time, PrdAgr = 60, FreqLoca = 40)

numAgr <- 0

for(idxAgr in 1:length(tmp$idxSoniAmrs)){
  # only the first 10 half-hours for standard testing
  #for(idxAgr in 1:10){
  # idxAgr <- 1
  numAgr <- numAgr + 1
  
  #Create a list identifier for the Aggregation loops
  levlAgr <- paste0("numAgr",ifelse(numAgr < 10, paste0("0",numAgr),numAgr))

for(idxSens in names(inpList$data)){
  
  # assign data
  
  if(idxSens == "soniAmrs") {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idxSoniAmrs$idxBgn[idxAgr]:tmp$idxSoniAmrs$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idxSoniAmrs$idxBgn[idxAgr]:tmp$idxSoniAmrs$idxEnd[idxAgr],]
    
  } else if (idxSens == "soni") {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idxSoni$idxBgn[idxAgr]:tmp$idxSoni$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idxSoni$idxBgn[idxAgr]:tmp$idxSoni$idxEnd[idxAgr],]
    
   
  } else {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idx$idxBgn[idxAgr]:tmp$idx$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idx$idxBgn[idxAgr]:tmp$idx$idxEnd[idxAgr],]
    
  }
  


}