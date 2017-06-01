# setResults_REPORT

setResults.REPORT = function(report) {
      if(is.null(report$.__enclos_env__$private$Sources)){
        return("Need sources first.")
      } else {
        #set up list to hold the response sets for the various sections
        results = vector(mode = "list", length = length(report$.__enclos_env__$private$Sources)) 
        #add names to the list so they can be set later
        names(results) = paste0("a", 1:length(report$.__enclos_env__$private$Sources)) 
        for (i in 1:length(report$.__enclos_env__$private$Sources)){  #for each source/section
          SectionName = read.csv(file = report$.__enclos_env__$private$Sources[i], 
                                 skip = 1, 
                                 header = F, 
                                 nrows = 1, 
                                 stringsAsFactors = F)[1,2] #get the section name
          thisResult = RESULT$new(SectionName)
          thisResult$setItemResponses(report$.__enclos_env__$private$Sources[i], report$.__enclos_env__$private$ItemInfo$ItemName, report$.__enclos_env__$private$ItemInfo$Value)
          results[[i]] = thisResult #put the response info in the list
          names(results)[i] = SectionName #set the element name in the list to be the name of the section
        }
        report$.__enclos_env__$private$Results = results
      }
}