# setTopicScores_REPORT

setTopicScores.REPORT = function(report) {
  
  TopicScores = NULL
  
  if(report$.__enclos_env__$private$HasTopics){
    
    #establish a list that will hold the Topic Scores data.frames
    TopicScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
    
    #pull the topic scores for each section and load them in the list
    for(i in 1:length(report$.__enclos_env__$private$Results)){
      TopicScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getTopicScores()
    }
    
    #make a single data.table with all of the item response scores from all of the sections
    TopicScores = data.table::rbindlist(TopicScores) 
    
  } 
  
  report$.__enclos_env__$private$TopicScores = TopicScores
  
} # /function