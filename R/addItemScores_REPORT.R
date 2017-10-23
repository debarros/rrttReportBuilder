# addItemScores_REPORT

addItemScores.REPORT = function(report) {
  # put badmessage call here
  
  # Pull the necessary info
  ItemInfo = report$getItemInfo()
  TMS = report$getTMS()
  
  # Establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(report$getResults()))
  
  # Calculate the item response scores for each section and load them in the list
  for(i in 1:length(report$getResults())){
    currentResult = report$getResults()[[i]]
    currentResult$setItemResponseScores(ItemInfo = report$getItemInfo(), TMS = TMS)
    ItReScores[[i]] = currentResult$getItemResponseScores()
  } # /for each result
  
  # Make a single data.table with all of the item response scores from all of the sections
  ItReScores = data.table::rbindlist(ItReScores) 
  
  # Calculate the average score for each question
  for(i in 1:nrow(ItemInfo)){
    ItemInfo$AverageScore[i] = mean(ItReScores[[ItemInfo$ItemName[i]]])/ItemInfo$Value[i]
  } # /for each item
  
  # Store the results
  report$setItemInfoQuick(ItemInfo)
  report$setItemScoresQuick(ItemInfo$AverageScore)
  report$setItemResponseScoresQuick(ItReScores)
  
} # /addItemScores.REPORT function
