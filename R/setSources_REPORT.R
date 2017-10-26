# setSources_REPORT

setSources.REPORT = function(report) {
  DataLocation = report$getDataLocation()
  if(is.null(DataLocation)){
    return("Need a data location first.")
  } else {
    report$setSourceFileNamesQuick(list.files(paste0(DataLocation,"\\exports")))
    report$setSourcesQuick(list.files(paste0(DataLocation,"\\exports"), full.names = T))
  } # /if-else
} # /setSources.REPORT function
