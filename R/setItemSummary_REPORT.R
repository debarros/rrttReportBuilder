# setItemSummary_REPORT
# Note: Throughout, there is code to convert NA to FALSE
# This is because items that have no information should not be included in a category

setItemSummary.REPORT = function(report, messageLevel = 0) {
  
  # put badmessage call here
  
  if(messageLevel > 0){
    message("Running setItemSummary.RESULT")
  }
  
  # Grab the item info and whatnot from the report
  ItemInfo =                   report$getItemInfo()
  RelatedCutoffProportion =    report$getRelatedCutoffProportion()
  DCP =                        report$getDifficultCutoffParams()
  DistractorCutoffProportion = report$getDistractorCutoffProportion()
  ResponseSet =                report$getResponseSet()
  OverThinkCutoff =            report$getOverThinkCutoff()
  EasyCutoff =                 report$getEasyCutoff()
  ChaffRules =                 report$getChaffRules()
  nStudents =                  report$getSummary()$N
  
  # set the parameters
  DifficultCutoff = min(DCP$Lower, max(DCP$Upper, quantile(ItemInfo$AverageScore, DCP$Proportion, na.rm = T), na.rm = T), na.rm = T)
  
  # find the 1-RelatedCutoffProportion quantile of the item correlations
  RelatedCutoff = quantile(ItemInfo$Correlation, 1 - RelatedCutoffProportion, na.rm = T)   
  
  # Determine how many times a distractor must be selected before it is considered powerful
  DistractorCutoffCount = nStudents * DistractorCutoffProportion
  
  #set up the ItemSummary data.frame
  ItemSummary = data.frame(ItemName = ItemInfo$ItemName, stringsAsFactors = F) 
  
  # For MC items only, check if at least one wrong answer was selected by 
  # at least DistractorCutoffProportion percent of students.
  # This should be altered to not be a loop.
  ItemSummary$PowerDistrators = FALSE
  for(i in 1:nrow(ItemSummary)){                                             # For each item, 
    if(ItemInfo$Type[i] == "MC"){                                            # If the item is MC,
      answerset = ItemInfo$Answer[i]                                         # Get the answer for this item
      answerset = stringr::str_split(string = answerset, pattern = ",")[[1]] # Get all answers for this item (usually just 1)
      wrongSet = ResponseSet                                                 # Initialize the wrongSet as the entire ResponseSet
      for(ans in answerset){                                                 # For each correct answer
        wrongSet = grep(pattern = ans, x = wrongSet, value = T, invert = T)  # Remove matching elements from the wrongSet
      }
      distractorCounts = as.integer(unlist(ItemInfo[i,wrongSet]))            # Count the times each wrong answer occurs
      distractorTF = distractorCounts > DistractorCutoffCount                # Get a vector of whether each response is a powerful distractor
      ItemSummary$PowerDistrators[i] = any(distractorTF, na.rm = T)          # if any are, indicate that this item has one
    }
  }
  ItemSummary$PowerDistrators[is.na(ItemSummary$PowerDistrators)] = FALSE
  
  # Overthinking: items where Correlation < OverThinkCutoff
  ItemSummary$OverThinking = ItemInfo$Correlation <  OverThinkCutoff
  ItemSummary$OverThinking[is.na(ItemSummary$OverThinking)] = FALSE
  # completely dropped items will still show a correlation of 0, but should not be marked as OverThinking
  ItemSummary$OverThinking[is.na(ItemInfo$AverageScore)] = FALSE  
  
  # Difficult: items where average score < DifficultCutoff
  ItemSummary$Difficult = ItemInfo$AverageScore < DifficultCutoff
  ItemSummary$Difficult[is.na(ItemSummary$Difficult)] = FALSE
  
  # Easy: items where average score >= EasyCutoff
  ItemSummary$Easy = ItemInfo$AverageScore >= EasyCutoff
  ItemSummary$Easy[is.na(ItemSummary$Easy)] = FALSE
  
  # Wheat from chaff: items that fit one of the ChaffRules 
  # Each rule has the form score < ChaffRules$score and correlation > ChaffRules$correlation
  # this should be altered to not be a loop
  ItemSummary$WheatFromChaff = 0
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
  
  report$setItemSummaryQuick(ItemSummary)
  
} # /function
