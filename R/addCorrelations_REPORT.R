# addCorrelations_REPORT

addCorrelations.REPORT = function(report) {
  
  #establish a list that will hold the DropScores data.frames
  DropScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
  
  #calculate the drop scores for each section and load them in the list
  for(i in 1:length(report$getResults())){
    report$.__enclos_env__$private$Results[[i]]$setDropScores(report$.__enclos_env__$private$ItemInfo)
    DropScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getDropScores()
  }
  
  # make a single data.table with all of the dropscores from all of the sections
  DropScores = data.table::rbindlist(DropScores) 
  
  # Calculate the correlations between 
  #   - the student scores on the item 
  #   - the student total scores after dropping the item
  ItemInfo = report$getItemInfo()
  for(i in 1:nrow(ItemInfo)){
    thisItem = ItemInfo$ItemName[i]
    itemsd = sd(report$.__enclos_env__$private$ItemResponseScores[[thisItem]], na.rm = T)
    dropsd = sd(DropScores[[thisItem]], na.rm = T)
    if(is.na(itemsd)){
      ItemInfo$Correlation[i] = 0
    } else if(itemsd * dropsd == 0){
      ItemInfo$Correlation[i] = 0
    } else {
      ItemInfo$Correlation[i] = cor(report$.__enclos_env__$private$ItemResponseScores[[thisItem]], DropScores[[thisItem]], use = "complete.obs")  
    }
  }
  report$updateItemInfo(ItemInfo) #put the ItemInfo back
  report$.__enclos_env__$private$Correlations = ItemInfo$Correlation
  report$.__enclos_env__$private$DropScores = DropScores
}