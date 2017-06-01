# exportUploads_REPORT

exportUploads.REPORT = function(report) {
      write.csv(x = report$.__enclos_env__$private$UploadTab, 
                file = paste0(report$.__enclos_env__$private$DataLocation,"\\","upload.csv"), 
                row.names = F)
}