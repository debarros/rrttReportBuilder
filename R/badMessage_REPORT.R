# badMessage_REPORT.R
# This is called from the badMessage method in the REPORT class, which is called from other methods
#
# The parameters are:
#   report - the entire report object
#   method - the calling method
#
# Any members that are required for a given method are checked before the method is called.
# If the member doesn't exist, a message about that is added to the badmessage character string.
# When badmessage is returned, the method that called the badMessage method should check its length.
# If the length is > 0, the method should print that message and quit.

# Needed improvements:
#   The structure of this function mostly assumes that the methods are called in order.
#   If there are any members that are only required in certain circumstances, 
#   those circumstances should be included in the if condition
# 
#   Need to put getPassingScore and setPassingScore in here somewhere, once those are implemented

badMessage.REPORT = function(method, report){
  badmessage = ""
  if(is.null(report$.__enclos_env__$private$DataLocation)) badmessage = paste0(badmessage, "Need a data location first.  ")
  if(method %in% c("setSources", "getDataLocation")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$Sources)) badmessage = paste0(badmessage, "Need sources first.  ")
  if(method %in% c("setTestName", "getSources"))  return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$TestName)) badmessage = paste0(badmessage, "Need the test name first.  ")
  if(method %in% c("setItemInfo", "getTestName")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$ItemInfo)) badmessage = paste0(badmessage, "Need Item Info first.  ")
  if(method %in% c("setResults", "getItemInfo")) return(badmessage)
  
  if(length(report$.__enclos_env__$private$Results) == 0) badmessage = paste0(badmessage, "Need results first.  ")
  if(method %in% c("setComparisonLocation", "getResults", "getResponses")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$ComparisonLocation)) badmessage = paste0(badmessage, "Need Comparison Location first.  ")
  if(method %in% c("enhanceItemInfo", "getComparisonLocation", "setTopicAlignments")) return(badmessage)
  
  if((is.null(report$HasTopics)) | (report$HasTopics & is.null(report$.__enclos_env__$private$TopicAlignments))) badmessage = paste0(badmessage, "Need Topic Alignments first.  ")
  if(method %in% c("addItemScores", "getTopicAlignments")) return(badmessage) 
  
  if(is.null(report$.__enclos_env__$private$ItemScores)) badmessage = paste0(badmessage, "Need item scores first.  ")
  if(method %in% c("addCorrelations", "getItemScores")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$Correlations)) badmessage = paste0(badmessage, "Need item correlations first.  ")
  if(method %in% c("addResponseFrequencies", "getCorrelations")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$ResponseSet))  badmessage = paste0(badmessage, "Need Response Frequencies first.  ")
  if(method %in% c("setUploadTab", "getResponseSet")) return(badmessage)
    
  if(is.null(report$.__enclos_env__$private$UploadTab)) badmessage = paste0(badmessage, "Need upload tab first.  ")
  if(method %in% c("setSummary","getUploadTab")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$Summary)) badmessage = paste0(badmessage, "Need Summary first.  ")
  if(method %in% c("setTopicSummary", "getSummary")) return(badmessage)

  if(report$HasTopics & is.null(report$.__enclos_env__$private$TopicSummary)) badmessage = paste0(badmessage, "Need Topic Summary first.  ")
  if(method %in% c("setItemSummary", "getTopicSummary")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$ItemSummary)) badmessage = paste0(badmessage, "Need Item Summary first.  ")
  if(method %in% c("setComparison", "setNarrative", "getItemSummary")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$Narrative)) badmessage = paste0(badmessage, "Need Narrative first.  ")
  if(method %in% c("setTopicScores", "getNarrative")) return(badmessage)
  
  if(report$HasTopics & is.null(report$.__enclos_env__$private$TopicScores)) badmessage = paste0(badmessage, "Need Topic Scores first.  ")
  if(method %in% c("setHandouts", "getTopicScores")) return(badmessage)
  
  if(is.null(report$.__enclos_env__$private$Handouts)) badmessage = paste0(badmessage, "Need Handouts first.  ")
  if(method %in% c("exportNarrative", "exportReport", "exportUploads", "getHandouts", "exportUpdate")) return(badmessage)
  
}