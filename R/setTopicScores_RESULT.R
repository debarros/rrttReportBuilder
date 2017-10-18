# setTopicScores_RESULT.R

setTopicScores.RESULT = function(TopicAlignments, ItemInfo, result){
  # Does this section need na.rm?
  
  ItemResponseScores = result$getItemResponseScores()
  ItemResponses = result$getItemResponses()
  
  TopicNames = colnames(TopicAlignments)[-1]
  TopicAlignments$Value = ItemInfo$Value
  TopicScores = ItemResponses[,2:4]
  TopicScores[,TopicNames] = NA_real_
  for(i in TopicNames){
    itemset = as.logical(TopicAlignments[,i])
    totalpoints = sum(TopicAlignments$Value[itemset])
    for(j in 1:nrow(TopicScores)){
      TopicScores[j,i] = sum(t(ItemResponseScores[j,TopicAlignments$ItemName[itemset]]))/totalpoints
    } # /for each student
  } # /for each topic
  
  result$setTSquick(TopicScores)
  result$setTopicSummary(TopicScores)
  
} # /function