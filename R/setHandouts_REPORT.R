# setHandouts_REPORT

setHandouts.REPORT = function(report) {
  
  # pull the needed info from the report
  ItemInfo = report$getItemInfo()
  ItemResponseScores = report$getItemResponseScores()
  ItemResponses = report$getResponses()
  HasTopics = report$checkTopics()
  TopicSummary = report$getTopicSummary()
  TopicScores = report$getTopicScores()
  
  # Set up the Handouts
  Handouts = as.data.frame(ItemResponses, stringsAsFactors = F)[,1:5]
  colnames(ItemResponseScores) = paste0(colnames(ItemResponseScores),"_Score")
  colnames(ItemResponses) = paste0(colnames(ItemResponses),"_Response")
  
  for(i in 7:ncol(ItemResponses)){
    ItemResponseScores[,i] = ItemResponseScores[,i]/ItemInfo$Value[i-6]
    Handouts = cbind.data.frame(Handouts, 
                                ItemResponses[,i, drop = F], 
                                stringsAsFactors = F)
    Handouts = cbind.data.frame(Handouts, 
                                ItemResponseScores[,i, drop = F], 
                                stringsAsFactors = F)
  } # /for
  
  if(HasTopics){
    topicNames = row.names(TopicSummary)
    for(i in 1:length(topicNames)){
      Handouts = cbind.data.frame(Handouts, 
                                  NA, 
                                  stringsAsFactors = F)
      Handouts = cbind.data.frame(Handouts, 
                                  TopicScores[,topicNames[i], drop = F], 
                                  stringsAsFactors = F)
    } # /for each topic
  } # /if HasTopics
  
  report$setHandoutsQuick(Handouts)
  
} # /function
