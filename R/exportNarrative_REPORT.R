# exportNarrative_REPORT

exportNarrative.REPORT = function(report, messageLevel = 0) {
  
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
  
  # Convert the RMarxkdown file to an html file
  rmarkdown::render(input =         paste0(DataLocation,"\\narrative.Rmd"),
                    output_format = "html_document", 
                    output_file =   "narrative.html", 
                    output_dir =    DataLocation,
                    quiet =         T)
  
} # /function
