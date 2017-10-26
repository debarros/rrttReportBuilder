# exportNarrative_REPORT

exportNarrative.REPORT = function(report) {
  DataLocation = report$getDataLocation()
  Narrative = report$getNarrative()
  
  
  fileConn <- file(paste0(DataLocation,"\\narrative.Rmd"))
  
  writeLines(c("---",
               'title: "Report Narrative"', 
               "output: html_document",
               "---",
               "   ",
               Narrative), 
             fileConn)
  
  close(fileConn)
  
  rmarkdown::render(input = paste0(DataLocation,"\\narrative.Rmd"),
                    output_format = "html_document", 
                    output_file = "narrative.html", 
                    output_dir = DataLocation,
                    quiet = T)
} # /function
