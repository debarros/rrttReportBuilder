# setTopicAlignments_REPORT

setTopicAlignments.REPORT = function(d2, report, messageLevel = 0) {
  
  # Initialize HasTopics and Topic Alignments
  HasTopics = F
  TopicAlignments = NULL
  
  if(ncol(d2) > 5){                           # check to see if there are topics at all
    HasTopics = T
    TopicAlignments = d2[,5:ncol(d2)]         # set up a data.frame to hold topic info
    colnames(TopicAlignments)[1] = "ItemName" # set the name of the first column
    for(i in 2:ncol(TopicAlignments)){        # for each topic, set the alignments
      TopicAlignments[,i] = as.logical(as.numeric(TopicAlignments[,i]))
    } # /for
  } # /if
  
  # Store info
  report$setHasTopicsQuick(HasTopics)
  report$setTopicAlignmentsQuick(TopicAlignments)
  
} # /function
