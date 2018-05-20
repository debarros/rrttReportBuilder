# exportUploads_REPORT

exportUploads.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){
    message("Running exportUploads.REPORT")
  }
  
  write.csv(
    x = report$getUploadTab(), 
    file = paste0(report$getDataLocation(),"\\","upload_percentages.csv"), 
    row.names = F)
  
  write.csv(
    x = report$getUploadTotalPoints(), 
    file = paste0(report$getDataLocation(),"\\","upload_totalpoints.csv"), 
    row.names = F)
  
} # /function
