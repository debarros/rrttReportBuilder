# setHandouts_REPORT

setHandouts.REPORT = function(report) {
      ItemInfo = report$.__enclos_env__$private$ItemInfo
      ItemResponseScores = report$.__enclos_env__$private$ItemResponseScores
      ItemResponses = report$getResponses()
      Handouts = as.data.frame(ItemResponses, stringsAsFactors = F)[,1:5]
      colnames(ItemResponseScores) = paste0(colnames(ItemResponseScores),"_Score")
      colnames(ItemResponses) = paste0(colnames(ItemResponses),"_Response")
      
      for(i in 7:ncol(ItemResponses)){
        ItemResponseScores[,i] = ItemResponseScores[,i]/ItemInfo$Value[i-6]
        Handouts = cbind.data.frame(Handouts, 
                                    ItemResponses[,i, drop = F], 
                                    stringsAsFactors = F)
        Handouts = cbind.data.frame(Handouts, 
                                    ItemResponseScores[,i, drop = F], 
                                    stringsAsFactors = F)
      } # /for
      
      if(report$.__enclos_env__$private$HasTopics){
        topicNames = row.names(report$.__enclos_env__$private$TopicSummary)
        for(i in 1:length(topicNames)){
          Handouts = cbind.data.frame(Handouts, 
                                      NA, 
                                      stringsAsFactors = F)
          Handouts = cbind.data.frame(Handouts, 
                                      report$.__enclos_env__$private$TopicScores[,topicNames[i], drop = F], 
                                      stringsAsFactors = F)
        } # /for
      } # /if HasTopics
      report$.__enclos_env__$private$Handouts = Handouts
}