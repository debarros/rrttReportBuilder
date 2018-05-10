# setUploadTab_REPORT

setUploadTab.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){
    message("Running setUploadTab.REPORT")
  }
  
  # Pull the necessary components from the report
  ItemResponses = as.data.frame(report$getResponses())
  
  # Make the upload tab
  UploadTab = data.frame(StudentID = ItemResponses$StudentID)
  UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
  UploadTab$Percentage = round(ItemResponses$score, digits = 2)
  
  # Make the Total Points object
  TotalPoints = UploadTab[,c(1,2)]
  TotalPoints$TotalPoints = ItemResponses$TotalPoints
  
  # Set the new components in the report
  report$setUploadTabQuick(UploadTab)
  report$setUploadTotalPointsQuick(TotalPoints)
  
} # /function
