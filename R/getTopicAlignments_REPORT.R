# getTopicAlignments_REPORT

getTopicAlignments.REPORT = function(report) {
  
  HasTopics = report$checkTopics()
  TopicAlignments =  report$getTopicAlignmentsQuick()
  
  if(HasTopics){
    for(i in 2:ncol(TopicAlignments)){
      TopicAlignments[,i] = as.integer(TopicAlignments[,i])
    } # /for
  } else {
    TopicAlignments = NULL
  } # /if-else
  
  return(TopicAlignments)
  
} # /function
