#badMessage.R
#method for the REPORT class

  badmessage = ""
  if(is.null(private$DataLocation)){
    badmessage = paste0(badmessage, "Need a data location first.  ")
  }
  if(method %in% c("setSources", "getDataLocation")){
    return(badmessage)
  }
  
  if(is.null(private$Sources)){
    badmessage = paste0(badmessage, "Need sources first.  ")
  }
  if(method %in% c("setTestName", "getSources")){
    return(badmessage)
  }
  
  if(is.null(private$TestName)){
    badmessage = paste0(badmessage, "Need the test name first.  ")
  }
  if(method %in% c("setItemInfo", "getTestName")){
    return(badmessage)
  }
  
  if(is.null(private$ItemInfo)){
    badmessage = paste0(badmessage, "Need Item Info first.  ")
  }
  if(method %in% c("setResults", "getItemInfo")){
    return(badmessage)
  }
  
  if(length(private$Results) == 0){
    badmessage = paste0(badmessage, "Need results first.  ")
  }
  if(method %in% c("setComparisonLocation", "getResults")){
    return(badmessage)
  }
  
  if(is.null(private$ComparisonLocation)){
    badmessage = paste0(badmessage, "Need Comparison Location first.  ")
  }
  if(method %in% c("getComparisonLocation")){
    return(badmessage)
  }
  
  if(is.null(private$TopicAlignments)){
    badmessage = paste0(badmessage, "Need Topic Alignments first.  ")
  }
  if(method %in% c("addItemScores", "getTopicAlignments")){
    return(badmessage)
  }
  
  if(is.null(private$ItemScores)){
    badmessage = paste0(badmessage, "Need item scores first.  ")
  }
  if(method %in% c("addCorrelations", "getItemScores")){
    return(badmessage)
  }
  
  if(is.null(private$Correlations)){
    badmessage = paste0(badmessage, "Need item correlations first.  ")
  }
  if(method %in% c("addResponseFrequencies", "getCorrelations")){
    return(badmessage)
  }
  
  if(is.null(private$ResponseSet)){
    badmessage = paste0(badmessage, "Need Response Frequencies first.  ")
  }
  if(method %in% c("setUploadTab")){
    return(badmessage)
  }
  
  if(is.null(private$UploadTab)){
    badmessage = paste0(badmessage, "Need upload tab first.  ")
  }
  if(method %in% c("getUploadTab")){
    return(badmessage)
  }