# addItemScores_REPORT

addItemScores.REPORT = function(report) {
  # put badmessage call here
  
  #establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(report$getResults()))
  
  #calculate the item response scores for each section and load them in the list
  for(i in 1:length(report$getResults())){
    report$getResults()[[i]]$setItemResponseScores(report$getItemInfo())
    ItReScores[[i]] = report$getResults()[[i]]$getItemResponseScores()
  }
  
  #make a single data.table with all of the item response scores from all of the sections
  ItReScores = data.table::rbindlist(ItReScores) 
  
  #Calculate the average score for each question
  for(i in 1:nrow(report$getItemInfo())){
    report$.__enclos_env__$private$ItemInfo$AverageScore[i] = mean(
      ItReScores[[report$.__enclos_env__$private$ItemInfo$ItemName[i]]])/report$.__enclos_env__$private$ItemInfo$Value[i]
  }
  report$.__enclos_env__$private$ItemScores = report$.__enclos_env__$private$ItemInfo$AverageScore
  report$.__enclos_env__$private$ItemResponseScores = ItReScores
  
}
