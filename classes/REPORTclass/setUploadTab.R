#setUploadTab.R

ItemResponses = self$getResponses()
UploadTab = ItemResponses[,c("StudentID")]
UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
UploadTab$Percentage = ItemResponses$score
private$UploadTab = UploadTab