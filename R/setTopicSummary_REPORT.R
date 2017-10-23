# setTopicSummary_REPORT

setTopicSummary.REPORT = function(report) {
  
  #put badmessage call here
  
  TopicSummary = NULL # default the topic summary to null
  
  if(report$checkTopics()){
    TopicNames = colnames(report$getTopicAlignments())[-1]
    TopicScores = vector(mode = "list", length = length(report$getResults()))
    sectionNames = c("All Classes", names(report$getResults()))
    TopicSummary = magrittr::set_rownames(
      magrittr::set_colnames(
        as.data.frame.matrix(
          matrix(data = NA_real_, 
                 nrow = ncol(report$getTopicAlignments())-1, 
                 ncol = length(sectionNames))),
        sectionNames),
      TopicNames)
    
    for(i in 1:length(report$getResults())){
      currentResult = report$getResults()[[i]]
      currentResult$setTopicScores(report$getTopicAlignments(),report$getItemInfo())
      TopicScores[[i]] = currentResult$getTopicScores()
      TopicSummary[,names(report$getResults())[i]] = currentResult$getTopicSummary()
    } # /for
    
    TopicScores = data.table::rbindlist(TopicScores)
    for(i in TopicNames){
      itemset = report$getTopicAlignments()[,i]
      totalpoints = sum(report$getTopicAlignments()$Value[itemset], na.rm = T)
      TopicSummary$`All Classes`[rownames(TopicSummary) == i] = mean(unlist(TopicScores[,i, with = F]), na.rm = T)
    } # /for
  } # /if has topics 
  
  report$.__enclos_env__$private$TopicSummary = TopicSummary  
  
} # /function
