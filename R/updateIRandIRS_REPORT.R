

updateIRandIRS.REPORT = function(report){
  Results = report$getResults()
  ItemInfo = report$getItemInfo()
  #establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(Results))
  
  
  # Load the item response scores for each section into the list
  for(i in 1:length(Results)){
    currentResult = Results[[i]]
    ItReScores[[i]] = currentResult$getItemResponseScores()
  }
  
  # Make a single data.table with all of the item response scores from all of the sections
  ItReScores = data.table::rbindlist(ItReScores) 
  
  # Calculate the average score for each question
  for(i in 1:nrow(ItemInfo)){
    ItemInfo$AverageScore[i] = mean(ItReScores[[ItemInfo$ItemName[i]]], na.rm = T)/ItemInfo$Value[i]
  }
  
  # Store the results
  report$setItemInfoQuick(ItemInfo)
  report$setItemScoresQuick(ItemInfo$AverageScore)
  report$setItemResponseScoresQuick(ItReScores)
  
} # /function
