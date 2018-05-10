# setTopicSummary_REPORT

setTopicSummary.REPORT = function(report, messageLevel = 0) {
  
  #put badmessage call here
  
  if(messageLevel > 0){
    message("Running setTopicSummary.RESULT")
  }
  
  # pull needed stuff from the report
  ItemInfo =        report$getItemInfo()
  TopicAlignments = report$getTopicAlignments()
  Results =         report$getResults()
  HasTopics =       report$checkTopics()
  
  # default the topic summary to null
  TopicSummary = NULL 
  
  if(HasTopics){
    TopicNames = colnames(TopicAlignments)[-1]
    TopicScores = vector(mode = "list", length = length(Results))        # initialize a list to hold the data.frames of topics scores
    sectionNames = c("All Classes", names(Results))
    TopicSummary = magrittr::set_rownames(
      magrittr::set_colnames(
        as.data.frame.matrix(
          matrix(data = NA_real_, 
                 nrow = ncol(TopicAlignments)-1, 
                 ncol = length(sectionNames))),
        sectionNames),
      TopicNames)
    
    for(i in 1:length(Results)){
      currentResult = Results[[i]]
      currentResult$setTopicScores(TopicAlignments, ItemInfo)
      TopicScores[[i]] = currentResult$getTopicScores()
      TopicSummary[,names(Results)[i]] = currentResult$getTopicSummary()
    } # /for
    
    TopicScores = data.table::rbindlist(TopicScores)                     # Combine the topics scores data.frames
    
    for(i in TopicNames){
      itemset = TopicAlignments[,i]
      totalpoints = sum(TopicAlignments$Value[itemset], na.rm = T)
      TopicSummary$`All Classes`[rownames(TopicSummary) == i] = mean(unlist(TopicScores[,i, with = F]), na.rm = T)
    } # /for
  } # /if has topics 
  
  report$setTopicSummaryQuick(TopicSummary)
  
} # /function
