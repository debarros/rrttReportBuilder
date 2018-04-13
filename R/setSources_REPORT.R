# setSources_REPORT

setSources.REPORT = function(report, messageLevel = 0) {
  
  # pull the necessary stuff from the report
  DataLocation = report$getDataLocation()
  
  # set the relevant members of the report object
  report$setSourceFileNamesQuick(list.files(paste0(DataLocation,"\\exports")))
  report$setSourcesQuick(list.files(paste0(DataLocation,"\\exports"), full.names = T))
  
} # /setSources.REPORT function
