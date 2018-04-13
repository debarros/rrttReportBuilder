# exportUpdate_REPORT

exportUpdate.REPORT = function(uploadFilePath, report, messageLevel = 0) {
  
  # pull necessary stuff from the report
  AllScores = report$getUploadTab()
  testName = report$getTestName()
  DataLocation = report$getDataLocation()
  scoreupdatefilepath = paste0(DataLocation, "\\", "ScoreUpdates.txt")
  
  # Get the existing scores and add an identifying code
  ExistingScores = read.csv(uploadFilePath, stringsAsFactors = F)
  ExistingScores$code = apply(X = ExistingScores, MARGIN = 1, FUN = paste0, collapse = "-")
  
  # Add an identifying code to AllScores
  AllScores$code = apply(X = AllScores, MARGIN = 1, FUN = paste0, collapse = "-")
  
  # Get the subset of scores that are new or different
  NewScores = AllScores[!(AllScores$code %in% ExistingScores$code),]
  
  if(nrow(NewScores) > 0){ # If there are new scores, write a new score update
    
    # Pad student names to make them the same length
    maxName = max(nchar(NewScores$StudentName)) 
    NewScores$StudentName = stringr::str_pad(
      string = NewScores$StudentName, 
      width = maxName, 
      side = "right")
    
    # Put in the header line for the score update email
    out = paste0("Updated scores for ", testName, ":  \n") 
    
    # for each score append a line with that student's name, id, and score
    for(i in 1:nrow(NewScores)){ 
      out = paste0(out, "\t",NewScores$StudentName[i], "\t", NewScores$StudentID[i], "\t", NewScores$Percentage[i], "  \n")
    } # /for loop
    
    write(out, scoreupdatefilepath)  # write the score update emails to a text file
    
  } else {
    
    # If there are no new scores and the score update file exists, delete it.
    if(file.exists(scoreupdatefilepath)){
      file.remove(scoreupdatefilepath)
    } # /if 
    
  } # /if-else
  
} # /function
