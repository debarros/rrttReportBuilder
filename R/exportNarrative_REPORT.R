# exportNarrative_REPORT

exportNarrative.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){
    message("Running exportNarrative.REPORT")
  }
  
  # pull the necessary stuff from the report
  DataLocation = report$getDataLocation()
  Narrative = report$getNarrative()
  NarrativeStyleSheet = system.file("extdata", "narrative.css", package = "rrttReportBuilder")  # This should probable be a part of the REPORT class
  
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
  
  knitr::knit(paste0(DataLocation,"\\narrative.Rmd"), paste0(DataLocation,"\\narrative.md"), quiet = T) # creates md file
  markdown::markdownToHTML(file = paste0(DataLocation,"\\narrative.Rmd"),                               # creates html file
                           output = paste0(DataLocation,"\\narrative.html"),
                           stylesheet = NarrativeStyleSheet) 
  file.remove(paste0(DataLocation,"\\narrative.Rmd"))
  file.remove(paste0(DataLocation,"\\narrative.md"))
  
} # /function
