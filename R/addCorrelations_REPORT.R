# addCorrelations_REPORT

addCorrelations.REPORT = function(report) {
  Results = report$getResults()
  ItemInfo = report$getItemInfo()
  ItemResponseScores = report$getItemResponseScores()
  
  #establish a list that will hold the DropScores data.frames
  DropScores = vector(mode = "list", length = length(Results))
  
  #calculate the drop scores for each section and load them in the list
  for(i in 1:length(Results)){
    currentResult = Results[[i]]
    currentResult$setDropScores(ItemInfo)
    DropScores[[i]] = currentResult$getDropScores()
  }
  
  # make a single data.table with all of the dropscores from all of the sections
  DropScores = data.table::rbindlist(DropScores) 
  
  # Calculate the correlations between 
  #   - the student scores on the item 
  #   - the student total scores after dropping the item
  
  for(i in 1:nrow(ItemInfo)){
    thisItem = ItemInfo$ItemName[i]
    itemsd = sd(ItemResponseScores[[thisItem]], na.rm = T)
    dropsd = sd(DropScores[[thisItem]], na.rm = T)
    if(is.na(itemsd)){
      ItemInfo$Correlation[i] = 0
    } else if(itemsd * dropsd == 0){
      ItemInfo$Correlation[i] = 0
    } else {
      ItemInfo$Correlation[i] = cor(ItemResponseScores[[thisItem]], DropScores[[thisItem]], use = "complete.obs")  
    }
  }
  
  report$setItemInfoQuick(ItemInfo)                                      #put the ItemInfo back
  report$setCorrelationsQuick(ItemInfo$Correlation)
  report$setDropScoresQuick(DropScores)
} # /function
