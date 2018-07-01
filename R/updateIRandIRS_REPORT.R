# updateIRandIRS_REPORT.R

updateIRandIRS.REPORT = function(report, messageLevel = 0){

  if(messageLevel > 0){message("running updateIRandIRS.REPORT")}
    
  # pull the necessary stuff from the report
  Results = report$getResults()
  ItemInfo = report$getItemInfo()
  
  #establish a list that will hold the Item Response Scores data.frames
  ItReScores = vector(mode = "list", length = length(Results))
  
  # Load the item response scores for each section into the list
  if(messageLevel > 1){message("Loading item response scores from each section")}
  for(i in 1:length(Results)){
    if(messageLevel > 2){message(paste0("Loading item response scores from section ", i))}
    currentResult = Results[[i]]
    ItReScores[[i]] = currentResult$getItemResponseScores()
  }
  
  # Make a single data.table with all of the item response scores from all of the sections
  ItReScores = data.table::rbindlist(ItReScores) 
  
  # Calculate the average score for each question
  if(messageLevel > 1){message("Calculating average scores for each item")}
  for(i in 1:nrow(ItemInfo)){
    ItemInfo$AverageScore[i] = mean(ItReScores[[ItemInfo$ItemName[i]]], na.rm = T)/ItemInfo$Value[i]
  }
  
  # Store the results
  report$setItemInfoQuick(ItemInfo)
  report$setItemScoresQuick(ItemInfo$AverageScore)
  report$setItemResponseScoresQuick(ItReScores)
  
} # /function
