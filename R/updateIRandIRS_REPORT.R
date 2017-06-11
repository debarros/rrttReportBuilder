

updateIRandIRS.REPORT = function(report){
  
  #establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
  
  # Load the item response scores for each section into the list
  for(i in 1:length(report$.__enclos_env__$private$Results)){
    ItReScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getItemResponseScores()
  }
  
  # Make a single data.table with all of the item response scores from all of the sections
  ItReScores = data.table::rbindlist(ItReScores) 
  
  # Calculate the average score for each question
  for(i in 1:nrow(report$.__enclos_env__$private$ItemInfo)){
    report$.__enclos_env__$private$ItemInfo$AverageScore[i] = mean(
      ItReScores[[report$.__enclos_env__$private$ItemInfo$ItemName[i]]], na.rm = T)/report$.__enclos_env__$private$ItemInfo$Value[i]
  }
  report$.__enclos_env__$private$ItemScores = report$.__enclos_env__$private$ItemInfo$AverageScore
  report$.__enclos_env__$private$ItemResponseScores = ItReScores
  
  
  
  
}