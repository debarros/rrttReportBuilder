# setDropScores_RESULT.R

setDropScores.RESULT = function(ItemInfo, result){
  
  # Get item response scores
  ItemResponseScores = result$getItemResponseScores()
  
  #Set up a dataframe to hold the scores of each students with each item dropped
  DropScores = ItemResponseScores
  
  # Calculate the drop scores
  for(j in ItemInfo$ItemName){ # for each item
    DropScores[,j] = as.numeric(DropScores[,j]) # make sure the variable is numeric
    for(i in 1:nrow(DropScores)){ # for each student
      DropScores[i,j] = DropScores$TotalPoints[i] - ItemResponseScores[i,j] # Calculate the drop score
    } # /for each item
  } # /for each student
  
  result$setDSquick(DropScores)
} # /function
