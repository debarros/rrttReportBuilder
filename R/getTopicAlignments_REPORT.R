# getTopicAlignments_REPORT

getTopicAlignments.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 1){message("running getTopicAlignments.REPORT")}
  
  # pull the needed stuff from the report
  HasTopics =        report$checkTopics()
  TopicAlignments =  report$getTopicAlignmentsQuick()
  
  # Check if there are topics and act accordingly
  if(HasTopics){
    for(i in 2:ncol(TopicAlignments)){
      TopicAlignments[,i] = as.integer(TopicAlignments[,i])
    } # /for
  } else {
    TopicAlignments = NULL
  } # /if-else
  
  return(TopicAlignments)
  
} # /function
