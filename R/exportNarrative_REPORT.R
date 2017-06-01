# exportNarrative_REPORT

exportNarrative.REPORT = function(report) {
      fileConn <- file(paste0(report$.__enclos_env__$private$DataLocation,"\\narrative.Rmd"))
      writeLines(c("---",'title: "Report Narrative"', "output: html_document",
                   "---","   ",report$.__enclos_env__$private$Narrative), 
                 fileConn)
      close(fileConn)
      rmarkdown::render(input = paste0(report$.__enclos_env__$private$DataLocation,"\\narrative.Rmd"),
                        output_format = "html_document", 
                        output_file = "narrative.html", 
                        output_dir = report$.__enclos_env__$private$DataLocation,
                        quiet = T)
}