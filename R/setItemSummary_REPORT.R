# setItemSummary_REPORT
# Note: Throughout, there is code to convert NA to FALSE
# This is because items that have no information should not be included in a category

setItemSummary.REPORT = function(report) {
  
  # put badmessage call here
  
  # Grab the item info
  ItemInfo = report$getItemInfo()
  
  # set the parameters
  DCP = report$.__enclos_env__$private$DifficultCutoffParams
  DifficultCutoff = min(DCP$Lower, max(DCP$Upper, quantile(ItemInfo$AverageScore, DCP$Proportion, na.rm = T), na.rm = T), na.rm = T)
  
  # find the 1-RelatedCutoffProportion quantile of the item correlations
  RelatedCutoff = quantile(ItemInfo$Correlation, 1 - report$.__enclos_env__$private$RelatedCutoffProportion, na.rm = T)   
  DistractorCutoffCount = nrow(ItemInfo) * report$.__enclos_env__$private$DistractorCutoffProportion
  
  #set up the ItemSummary data.frame
  ItemSummary = data.frame(ItemName = ItemInfo$ItemName, stringsAsFactors = F) 
  
  # For MC items only, check if at least one wrong answer was selected by 
  # at least DistractorCutoffProportion percent of students.
  # This should be altered to not be a loop.
  ItemSummary$PowerDistrators = FALSE
  for(i in 1:nrow(ItemSummary)){
    if(ItemInfo$Type[i] == "MC"){
      wrongSet = grep(pattern = ItemInfo$Answer[i], x = report$getResponseSet(), value = T, invert = T) 
      ItemSummary$PowerDistrators[i] = any(ItemInfo[i,wrongSet] > DistractorCutoffCount)
    }
  }
  ItemSummary$PowerDistrators[is.na(ItemSummary$PowerDistrators)] = FALSE
  
  # Overthinking: items where Correlation < OverThinkCutoff
  ItemSummary$OverThinking = ItemInfo$Correlation <  report$.__enclos_env__$private$OverThinkCutoff
  ItemSummary$OverThinking[is.na(ItemSummary$OverThinking)] = FALSE
  # completely dropped items will still show a correlation of 0, but should not be marked as OverThinking
  ItemSummary$OverThinking[is.na(ItemInfo$AverageScore)] = FALSE  
  
  # Difficult: items where average score < DifficultCutoff
  ItemSummary$Difficult = ItemInfo$AverageScore < DifficultCutoff
  ItemSummary$Difficult[is.na(ItemSummary$Difficult)] = FALSE
  
  # Easy: items where average score >= EasyCutoff
  ItemSummary$Easy = ItemInfo$AverageScore >= report$.__enclos_env__$private$EasyCutoff
  ItemSummary$Easy[is.na(ItemSummary$Easy)] = FALSE
  
  # Wheat from chaff: items that fit one of the ChaffRules 
  # Each rule has the form score < ChaffRules$score and correlation > ChaffRules$correlation
  # this should be altered to not be a loop
  ItemSummary$WheatFromChaff = 0
  ChaffRules = report$.__enclos_env__$private$ChaffRules
  for(i in 1:nrow(ChaffRules)){
    add2chaff = (ItemInfo$AverageScore < ChaffRules$score[i]) & (ItemInfo$Correlation > ChaffRules$correlation[i])
    ItemSummary$WheatFromChaff = ItemSummary$WheatFromChaff + add2chaff
  }
  ItemSummary$WheatFromChaff = as.logical(ItemSummary$WheatFromChaff) # only 0's become FALSE
  ItemSummary$WheatFromChaff[is.na(ItemSummary$WheatFromChaff)] = FALSE
  
  # correlation greater than RelatedCutoff
  ItemSummary$HighlyRelated = ItemInfo$Correlation > RelatedCutoff
  ItemSummary$HighlyRelated[is.na(ItemSummary$HighlyRelated)] = FALSE
  
  # Check Key: for MC items only, (score is 0) OR (item is both Difficult and Overthinking)
  ItemSummary$CheckKey = ItemInfo$Type == "MC" & (ItemInfo$AverageScore == 0 | (ItemSummary$Difficult & ItemSummary$OverThinking))
  ItemSummary$CheckKey[is.na(ItemSummary$CheckKey)] = FALSE
  
  report$.__enclos_env__$private$ItemSummary = ItemSummary
} # /function