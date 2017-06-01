# getTopicAlignments_REPORT

getTopicAlignments.REPORT = function(report) {
      if(report$.__enclos_env__$private$HasTopics){
        TopicAlignments =  report$.__enclos_env__$private$TopicAlignments
        for(i in 2:ncol(TopicAlignments)){
          TopicAlignments[,i] = as.integer(TopicAlignments[,i])
        } # /for
        return(TopicAlignments)    
      } else {
        return(NULL)
      } # /if-else
}