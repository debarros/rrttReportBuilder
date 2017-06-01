# addCorrelations_REPORT

addCorrelations.REPORT = function(report) {
      badmessage = ""
      if(length(report$.__enclos_env__$private$Results) == 0){
        badmessage = paste0(badmessage, "Need Results first.  ")
      }
      if(is.null(report$.__enclos_env__$private$ComparisonLocation)){ 
        badmessage = paste0(badmessage, "Need Comparison Location first.  ")
      }
      if(is.null(report$.__enclos_env__$private$ItemInfo)){ 
        badmessage = paste0(badmessage, "Need Item Info first.  ")
      }
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the DropScores data.frames
        DropScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(report$.__enclos_env__$private$Results)){
          report$.__enclos_env__$private$Results[[i]]$setDropScores(report$.__enclos_env__$private$ItemInfo)
          DropScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getDropScores()
        }
        # make a single data.table with all of the dropscores from all of the sections
        DropScores = data.table::rbindlist(DropScores) 
        # Calculate the correlations between the student scores on the item 
        # and the student total scores after dropping the item
        for(i in 1:nrow(report$.__enclos_env__$private$ItemInfo)){
          thisItem = report$.__enclos_env__$private$ItemInfo$ItemName[i]
          if(sd(report$.__enclos_env__$private$ItemResponseScores[[thisItem]]) * sd(DropScores[[thisItem]]) == 0){
            report$.__enclos_env__$private$ItemInfo$Correlation[i] = 0
          } else {
            report$.__enclos_env__$private$ItemInfo$Correlation[i] = cor(report$.__enclos_env__$private$ItemResponseScores[[thisItem]], DropScores[[thisItem]])  
          }
          
        }
      }
      report$.__enclos_env__$private$Correlations = report$.__enclos_env__$private$ItemInfo$Correlation
      report$.__enclos_env__$private$DropScores = DropScores
}