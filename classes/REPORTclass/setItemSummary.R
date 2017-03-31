#setItemSummary.R

badmessage = ""
if(is.null(private$DataLocation)){ badmessage = paste0(badmessage, "Need Data Location first.  ")}
if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
if(is.null(private$ResponseSet)){ badmessage = paste0(badmessage, "Need Response Frequencies first.  ")}
if(nchar(badmessage) > 0){
  return(badmessage)
} else {
  
  # set the parameters
  DifficultCutoff = min(
    private$DifficultCutoffParams$Lower, 
    max(private$DifficultCutoffParams$Upper, 
        quantile(private$ItemInfo$AverageScore, private$DifficultCutoffParams$Proportion)))
  RelatedCutoff = quantile(private$ItemInfo$Correlation, 1 - RelatedCutoffProportion)   # find the 1-RelatedCutoffProportion quantile of the item correlations
  DistractorCutoffCount = nrow(private$ItemInfo) * private$DistractorCutoffProportion
  
  #set up the ItemSummary data.frame
  ItemSummary = data.frame(ItemName = private$ItemInfo$ItemName) 
  
  #for MC items only, if at least one wrong answer was selected by at least private$DistractorCutoffProportion percent of students
  #this should be altered to not be a loop
  ItemSummary$PowerDistrators = FALSE
  for(i in 1:nrow(ItemSummary)){
    if(private$ItemInfo$Type == "MC"){
      wrongSet = grep(pattern = private$ItemInfo$Answer[i], x = private$ResponseSet, value = T, invert = T) 
      ItemSummary$PowerDistrators = any(private$ItemInfo[,wrongSet] > DistractorCutoffCount)
    }
  }
  
  
  #Correlation < private$OverThinkCutoff
  ItemSummary$OverThinking = private$ItemInfo$Correlation <  private$OverThinkCutoff
  
  # average score < DifficultCutoff
  ItemSummary$Difficult = private$ItemInfo$AverageScore < DifficultCutoff
  
  # average score >= private$EasyCutoff
  ItemSummary$Easy = private$ItemInfo$AverageScore >= private$EasyCutoff
  
  #item fits one of the ChaffRules (score < ChaffRules$score and correlation > ChaffRules$correlation)
  #this should be altered to not be a loop
  ItemSummary$WheatFromChaff = 0
  for(i in 1:nrow(ChaffRules)){
    ItemSummary$WheatFromChaff = ItemSummary$WheatFromChaff + (private$ItemInfo$AverageScore < private$ChaffRules$score[i] & private$ItemInfo$Correlation > private$ChaffRules$correllation[i])
  }
  ItemSummary$WheatFromChaff = as.logical(ItemSummary$WheatFromChaff)
  
  # correlation greater than RelatedCutoff
  ItemSummary$HighlyRelated = private$ItemInfo$Correlation > RelatedCutoff
  
  #for MC items only, (score is 0) OR (item is both Difficult and Overthinking)
  ItemSummary$CheckKey = private$ItemInfo$Type == "MC" & (private$ItemInfo$AverageScore == 0 | (ItemSummary$Difficult & ItemSummary$OverThinking))
  
  private$ItemSummary = ItemSummary
}