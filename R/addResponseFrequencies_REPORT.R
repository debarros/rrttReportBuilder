##Needs to be properly commented ^_^

addResponseFrequencies.REPORT = function(report) {
  badmessage = ""
  if(length(report$.__enclos_env__$private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
  if(is.null(report$.__enclos_env__$private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
  if(is.null(report$.__enclos_env__$private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
  if(nchar(badmessage) > 0){
    return(badmessage)
  } else {
    ItemResponses = report$getResponses()
    
    #Set default values for the number of letter options and number of point options
    topletter = 0
    toppoint = 0
    
    #Find the max number of letter and point options in the test
    if("MC" %in% report$.__enclos_env__$private$ItemInfo$Type){topletter = max(report$.__enclos_env__$private$ItemInfo$options, na.rm = T)}
    if("ER" %in% report$.__enclos_env__$private$ItemInfo$Type){toppoint = max(report$.__enclos_env__$private$ItemInfo$Value[report$.__enclos_env__$private$ItemInfo$Type == "ER"], na.rm = T)} 
    
    # Find the max number of response columns needed and the number 
    # of columns that will represent both numbers and letters
    totalset = max(topletter, toppoint+1, na.rm = T)
    overlapset = min(topletter, toppoint+1, na.rm = T)
    
    #Build the set of response column names
    responseSet = paste0(
      c(LETTERS[1:topletter], rep("", times = totalset - topletter)),
      c(rep("/", times = overlapset), rep("", times = totalset - overlapset)),
      c(as.character(0:toppoint), rep("", times = totalset - (toppoint+1) )))
    
    basecolumn = ncol(report$.__enclos_env__$private$ItemInfo) #How many columns does ItemInfo have already?
    report$.__enclos_env__$private$ItemInfo[,responseSet] = "" #Initialize those columns
    
    #This nested loop should be rewritten using lapply or something
    for(i in 1:nrow(report$.__enclos_env__$private$ItemInfo)){ #for every item
      for(j in 1:report$.__enclos_env__$private$ItemInfo$options[i]){ #for every possible response for that item
        #if it's an ER item, count how many times that point level was awarded
        if(report$.__enclos_env__$private$ItemInfo$Type[i] == "ER"){ 
          report$.__enclos_env__$private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,report$.__enclos_env__$private$ItemInfo$ItemName[i], with = F] == j-1, na.rm = T)
        } else { #if it's an MC item, count how many times that letter was used
          report$.__enclos_env__$private$ItemInfo[i,j+basecolumn] = sum(
            ItemResponses[,report$.__enclos_env__$private$ItemInfo$ItemName[i], with = F] == LETTERS[j], na.rm = T)
        }
      }
    }
    report$.__enclos_env__$private$ResponseSet = responseSet
  }
}