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
    
    TopicNames = colnames(TopicAlignments)[-1]                            # Get the topic names
    
    for(thisTopic in TopicNames){
      if(any(is.na(TopicAlignments[,thisTopic]))){
        stop(paste0("There are missing values in the alignment for the ", # Check for problems with the topic alignments
                    thisTopic, " topic."))
      }
    }

    TopicScores = vector(mode = "list", length = length(Results))         # initialize a list to hold the data.frames of topics scores
    sectionNames = c("All Classes", names(Results))
    TopicSummary = magrittr::set_rownames(
      magrittr::set_colnames(
        as.data.frame.matrix(
          matrix(data = NA_real_, 
                 nrow = ncol(TopicAlignments)-1, 
                 ncol = length(sectionNames))),
        sectionNames),
      TopicNames)
    
    for(thisResult in 1:length(Results)){
      currentResult = Results[[thisResult]]
      currentResult$setTopicScores(TopicAlignments, ItemInfo)
      TopicScores[[thisResult]] = currentResult$getTopicScores()
      TopicSummary[,names(Results)[thisResult]] = currentResult$getTopicSummary()
    } # /for
    
    TopicScores = data.table::rbindlist(TopicScores)                     # Combine the topics scores data.frames
    
    for(thisTopic in TopicNames){
      itemset = TopicAlignments[,thisTopic]
      totalpoints = sum(TopicAlignments$Value[itemset], na.rm = T)
      TopicSummary$`All Classes`[rownames(TopicSummary) == thisTopic] = mean(unlist(TopicScores[,thisTopic, with = F]), na.rm = T)
    } # /for
  } # /if has topics 
  
  report$setTopicSummaryQuick(TopicSummary)
  
} # /function
