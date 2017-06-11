# setUploadTab_REPORT

setUploadTab.REPORT = function(report) {
  ItemResponses = as.data.frame(report$getResponses())
  UploadTab = data.frame(StudentID = ItemResponses$StudentID)
  UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
  UploadTab$Percentage = round(ItemResponses$score, digits = 2)
  report$.__enclos_env__$private$UploadTab = UploadTab
  
  TotalPoints = UploadTab[,c(1,2)]
  TotalPoints$TotalPoints = ItemResponses$TotalPoints
  report$.__enclos_env__$private$UploadTotalPoints = TotalPoints
}