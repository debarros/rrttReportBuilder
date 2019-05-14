# exportUpdate_REPORT

exportUpdate.REPORT = function(uploadFilePath, report, messageLevel = 0) {
  
  if(messageLevel > 0){ message("Running exportUpdate.REPORT") }
  
  # pull necessary stuff from the report
  AllScores = report$getUploadTab()
  testName = report$getTestName()
  DataLocation = report$getDataLocation()
  scoreupdatefilepath = paste0(DataLocation, "\\", "ScoreUpdates.html")
  
  # Get the existing scores and add an identifying code
  ExistingScores = read.csv(uploadFilePath, stringsAsFactors = F)
  ExistingScores$code = apply(X = ExistingScores, MARGIN = 1, FUN = paste0, collapse = "-")
  
  # Add an identifying code to AllScores
  AllScores$code = apply(X = AllScores, MARGIN = 1, FUN = paste0, collapse = "-")
  
  # Get the subset of scores that are new or different
  NewScores = AllScores[!(AllScores$code %in% ExistingScores$code),]
  
  if(nrow(NewScores) > 0){ # If there are new scores, write a new score update
    
    # # Pad student names to make them the same length
    # maxName = max(nchar(NewScores$StudentName)) 
    # NewScores$StudentName = stringr::str_pad(
    #   string = NewScores$StudentName, 
    #   width = maxName, 
    #   side = "right")
    
    # Start the document
    out = "<!DOCTYPE html>\n<html>\n<body>\n"
    
    # Put in the header line for the score update email
    out = paste0(out, "<span>Updated scores for </span>", '<span style="font-weight:bold">', testName,'</span>',  ":  \n") 
    
    # Start the table
    out = paste0(out, '<table style = "border: 1px solid black;">')
    
    # for each score append a row with that student's name, id, and score
    for(i in 1:nrow(NewScores)){ 
      out = paste0(out,"<tr><td>",NewScores$StudentName[i], "</td>")
      out = paste0(out,"<td>", NewScores$StudentID[i], "</td>")
      out = paste0(out,'<td style = "border: 1px solid black; text-align:center;">', NewScores$Percentage[i], "</td></tr> \n")
    } # /for loop
    
    # Close the table and the html document
    out = paste0(out, '</table> \n <br> </body> \n </html>')
    
    write(out, scoreupdatefilepath)  # write the score update emails to a text file
    
  } else {
    
    # If there are no new scores and the score update file exists, delete it.
    if(file.exists(scoreupdatefilepath)){
      file.remove(scoreupdatefilepath)
    } # /if 
    
  } # /if-else
  
} # /function
