# setUploadTab_REPORT

setUploadTab.REPORT = function(report) {
  ItemResponses = as.data.frame(report$getResponses())
  UploadTab = data.frame(StudentID = ItemResponses$StudentID)
  UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
  UploadTab$Percentage = round(ItemResponses$score, digits = 2)
  report$setUploadTabQuick(UploadTab)
  
  TotalPoints = UploadTab[,c(1,2)]
  TotalPoints$TotalPoints = ItemResponses$TotalPoints
  report$setUploadTotalPointsQuick(TotalPoints)
}