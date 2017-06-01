# setTopicSummary_REPORT

setTopicSummary.REPORT = function(report) {
      if(report$.__enclos_env__$private$HasTopics){
        #put badmessage call here
        TopicNames = colnames(report$.__enclos_env__$private$TopicAlignments)[-1]
        TopicScores = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
        sectionNames = c("All Classes", names(report$.__enclos_env__$private$Results))
        TopicSummary = magrittr::set_rownames(
          magrittr::set_colnames(
            as.data.frame.matrix(
              matrix(data = NA_real_, 
                     nrow = ncol(report$.__enclos_env__$private$TopicAlignments)-1, 
                     ncol = length(sectionNames))),
            sectionNames),
          TopicNames)
        
        for(i in 1:length(report$.__enclos_env__$private$Results)){
          report$.__enclos_env__$private$Results[[i]]$setTopicScores(report$.__enclos_env__$private$TopicAlignments,report$.__enclos_env__$private$ItemInfo)
          TopicScores[[i]] = report$.__enclos_env__$private$Results[[i]]$getTopicScores()
          TopicSummary[,names(report$.__enclos_env__$private$Results)[i]] = report$.__enclos_env__$private$Results[[i]]$getTopicSummary()
        }
        
        
        TopicScores = data.table::rbindlist(TopicScores)
        for(i in TopicNames){
          itemset = report$.__enclos_env__$private$TopicAlignments[,i]
          totalpoints = sum(report$.__enclos_env__$private$TopicAlignments$Value[itemset])
          TopicSummary$`All Classes`[rownames(TopicSummary) == i] = mean(unlist(TopicScores[,i, with = F]))
        }
        report$.__enclos_env__$private$TopicSummary = TopicSummary  
      } else {
        report$.__enclos_env__$private$TopicSummary = NULL
      }
}