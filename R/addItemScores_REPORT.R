# addItemScores_REPORT

addItemScores.REPORT = function(report, HaltOnMultiResponse, messageLevel = 0) {
  # put badmessage call here
  
  if(messageLevel > 0){message("Running addItemScores.REPORT")}
  
  # Pull the necessary info from the report
  ItemInfo = report$getItemInfo()
  TMS =      report$getTMS()
  results =  report$getResults()
  
  # Establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(results))
  
  # Calculate the item response scores for each section and load them in the list
  for(i in 1:length(results)){
    if(messageLevel > 1){message(paste0("Calculating item response scores for result", i))} 
    currentResult = results[[i]]
    currentResult$setItemResponseScores(ItemInfo = ItemInfo, TMS = TMS, HaltOnMultiResponse = HaltOnMultiResponse, messageLevel = messageLevel - 1)
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
