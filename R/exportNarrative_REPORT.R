# exportNarrative_REPORT

exportNarrative.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){
    message("Running exportNarrative.REPORT")
  }
  
  # pull the necessary stuff from the report
  DataLocation = report$getDataLocation()
  Narrative = report$getNarrative()
  
  # establish a connection to the narrative file
  fileConn <- file(paste0(DataLocation,"\\narrative.Rmd"))
  
  # write the narrative to the file
  writeLines(
    text = c("---",
             'title: "Report Narrative"', 
             "output: html_document",
             "---",
             "   ",
             Narrative), 
    con = fileConn)
  
  # Close the file connection
  close(fileConn)
  
  knit(paste0(DataLocation,"\\narrative.Rmd"), paste0(DataLocation,"\\narrative.md")) # creates md file
  markdownToHTML(paste0(DataLocation,"\\narrative.Rmd"), paste0(DataLocation,"\\narrative.html")) # creates html file
  file.remove(paste0(DataLocation,"\\narrative.Rmd"))
  file.remove(paste0(DataLocation,"\\narrative.md"))
  
} # /function
