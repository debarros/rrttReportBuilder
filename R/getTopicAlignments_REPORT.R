# getTopicAlignments_REPORT

getTopicAlignments.REPORT = function(report) {
  
  HasTopics = report$checkTopics()
  
  if(HasTopics){
    TopicAlignments =  report$.__enclos_env__$private$TopicAlignments
    for(i in 2:ncol(TopicAlignments)){
      TopicAlignments[,i] = as.integer(TopicAlignments[,i])
    } # /for
  } else {
    TopicAlignments = NULL
  } # /if-else
  
  return(TopicAlignments)
  
} # /function
