# setTestName_REPORT

setTestName.REPORT = function(report) {
  if(report$getTMS() == "LinkIt"){
    report$.__enclos_env__$private$TestName = read.csv(report$.__enclos_env__$private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
  } else if(report$getTMS() == "ScantronAS"){
    i = 0
    testname = NA_character_
    while(is.na(testname)){
      i = i+1
      if(i > length(report$.__enclos_env__$private$Sources)){
        stop("There is no data")
      }
      testname = read.csv(report$.__enclos_env__$private$Sources[i], stringsAsFactors = F)[1,3]
    }
    report$.__enclos_env__$private$TestName = testname
  } else {
    stop(paste0("Unknown TMS: ",report$getTMS()))  
  } # /if-else
} # /function