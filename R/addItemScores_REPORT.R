# addItemScores_REPORT

addItemScores.REPORT = function(report) {
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
        #establish a list that will hold the Item Response Scores data.frames
        ItReScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(report$.__enclos_env__$private$Results)){
          report$.__enclos_env__$private$Results[[i]]$setItemResponseScores(report$.__enclos_env__$private$ItemInfo)
          ItReScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getItemResponseScores()
        }
        #make a single data.table with all of the item response scores from all of the sections
        ItReScores = data.table::rbindlist(ItReScores) 
        #Calculate the average score for each question
        for(i in 1:nrow(report$.__enclos_env__$private$ItemInfo)){
          report$.__enclos_env__$private$ItemInfo$AverageScore[i] = mean(
            ItReScores[[report$.__enclos_env__$private$ItemInfo$ItemName[i]]])/report$.__enclos_env__$private$ItemInfo$Value[i]
        }
        report$.__enclos_env__$private$ItemScores = report$.__enclos_env__$private$ItemInfo$AverageScore
        report$.__enclos_env__$private$ItemResponseScores = ItReScores
      }
}