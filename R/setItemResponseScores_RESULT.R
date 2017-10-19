# setItemResponseScores_RESULT.R

setItemResponseScores.RESULT = function(ItemInfo, TMS, result){
  
  # Get the relevant data
  ItemResp = result$getItemResponses()
  
  # Create a data.frame to hold the item scores
  ItemResponseScores = setNames(as.data.frame(
    array(data = NA_integer_, dim = dim(ItemResp))),
    colnames(ItemResp)) 
  ItemResponseScores[,1:6] = ItemResp[,1:6] #pull in the student info from the results data.table
  
  # Calculate scores for each response on each item
  # Note: For gridded response items with tolerance, this section will have to be edited
  for(i in 1:nrow(ItemInfo)){
    if(ItemInfo$Type[i] == "MC"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
    } else if(ItemInfo$Type[i] == "ER"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemResp[,ItemInfo$ItemName[i]]
    } else if(ItemInfo$Type[i] == "WH"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
    } else if(ItemInfo$Type[i] == "FL"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
    } else if(ItemInfo$Type[i] == "FI"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
    }
  } # /for
  
  # If this is a TMS that doesn't include total points in the exports, add it now
  if(TMS %in% c("ScantronAS")){
    ItemResponseScores$TotalPoints = apply(X = ItemResponseScores[,ItemInfo$ItemName], MARGIN = 1, FUN = sum)
    ItemResponseScores$score = ItemResponseScores$TotalPoints/sum(ItemInfo$Value)*100
    ItemResp$TotalPoints = ItemResponseScores$TotalPoints
    ItemResp$score = ItemResponseScores$score
    result$setIRquick(ItemResp)
  }
  
  result$setIRSquick(ItemResponseScores)
} # /function
