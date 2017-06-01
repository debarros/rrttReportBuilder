# setTestName_REPORT

setTestName.REPORT = function(report) {
      if(is.null(report$.__enclos_env__$private$Sources)){
        return("Need sources first.")
      } else {
        report$.__enclos_env__$private$TestName = read.csv(report$.__enclos_env__$private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
      }
}