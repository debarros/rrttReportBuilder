# setItemSummary_REPORT

setItemSummary.REPORT = function(report) {
badmessage = ""
      if(is.null(report$.__enclos_env__$private$DataLocation)){ badmessage = paste0(badmessage, "Need Data Location first.  ")}
      if(length(report$.__enclos_env__$private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(report$.__enclos_env__$private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(report$.__enclos_env__$private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(is.null(report$.__enclos_env__$private$ResponseSet)){ badmessage = paste0(badmessage, "Need Response Frequencies first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        
        # set the parameters
        DifficultCutoff = min(
          report$.__enclos_env__$private$DifficultCutoffParams$Lower, 
          max(report$.__enclos_env__$private$DifficultCutoffParams$Upper, 
              quantile(report$.__enclos_env__$private$ItemInfo$AverageScore, report$.__enclos_env__$private$DifficultCutoffParams$Proportion)))
        
        # find the 1-RelatedCutoffProportion quantile of the item correlations
        RelatedCutoff = quantile(report$.__enclos_env__$private$ItemInfo$Correlation, 1 - report$.__enclos_env__$private$RelatedCutoffProportion)   
        DistractorCutoffCount = nrow(report$.__enclos_env__$private$ItemInfo) * report$.__enclos_env__$private$DistractorCutoffProportion
        
        #set up the ItemSummary data.frame
        ItemSummary = data.frame(ItemName = report$.__enclos_env__$private$ItemInfo$ItemName, stringsAsFactors = F) 
        
        # For MC items only, check if at least one wrong answer was selected by 
        # at least report$.__enclos_env__$private$DistractorCutoffProportion percent of students.
        # This should be altered to not be a loop.
        ItemSummary$PowerDistrators = FALSE
        for(i in 1:nrow(ItemSummary)){
          if(report$.__enclos_env__$private$ItemInfo$Type[i] == "MC"){
            wrongSet = grep(pattern = report$.__enclos_env__$private$ItemInfo$Answer[i], x = report$.__enclos_env__$private$ResponseSet, value = T, invert = T) 
            ItemSummary$PowerDistrators[i] = any(report$.__enclos_env__$private$ItemInfo[i,wrongSet] > DistractorCutoffCount)
          }
        }
        
        
        #Correlation < report$.__enclos_env__$private$OverThinkCutoff
        ItemSummary$OverThinking = report$.__enclos_env__$private$ItemInfo$Correlation <  report$.__enclos_env__$private$OverThinkCutoff
        
        # average score < DifficultCutoff
        ItemSummary$Difficult = report$.__enclos_env__$private$ItemInfo$AverageScore < DifficultCutoff
        
        # average score >= report$.__enclos_env__$private$EasyCutoff
        ItemSummary$Easy = report$.__enclos_env__$private$ItemInfo$AverageScore >= report$.__enclos_env__$private$EasyCutoff
        
        #item fits one of the ChaffRules (score < ChaffRules$score and correlation > ChaffRules$correlation)
        #this should be altered to not be a loop
        ItemSummary$WheatFromChaff = 0
        for(i in 1:nrow(report$.__enclos_env__$private$ChaffRules)){
          ItemSummary$WheatFromChaff = ItemSummary$WheatFromChaff + 
            ((report$.__enclos_env__$private$ItemInfo$AverageScore < report$.__enclos_env__$private$ChaffRules$score[i]) & 
               (report$.__enclos_env__$private$ItemInfo$Correlation > report$.__enclos_env__$private$ChaffRules$correlation[i]))
        }
        ItemSummary$WheatFromChaff = as.logical(ItemSummary$WheatFromChaff)
        
        # correlation greater than RelatedCutoff
        ItemSummary$HighlyRelated = report$.__enclos_env__$private$ItemInfo$Correlation > RelatedCutoff
        
        #for MC items only, (score is 0) OR (item is both Difficult and Overthinking)
        ItemSummary$CheckKey = report$.__enclos_env__$private$ItemInfo$Type == "MC" & 
          (report$.__enclos_env__$private$ItemInfo$AverageScore == 0 | (ItemSummary$Difficult & ItemSummary$OverThinking))
        
        report$.__enclos_env__$private$ItemSummary = ItemSummary
      }
}