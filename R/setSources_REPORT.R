# setSources_REPORT

setSources.REPORT = function(report) {
      if(is.null(report$.__enclos_env__$private$DataLocation)){
        return("Need a data location first.")
      } else {
        report$.__enclos_env__$private$Sources = list.files(paste0(report$.__enclos_env__$private$DataLocation,"\\exports"), full.names = T)  
      }
}