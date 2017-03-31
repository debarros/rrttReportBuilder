#addResponseFrequencies.R

badmessage = ""
if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
if(nchar(badmessage) > 0){
  return(badmessage)
} else {
  ItemResponses = self$getResponses()
  
  #Set default values for the number of letter options and number of point options
  topletter = 0
  toppoint = 0
  
  #Find the max number of letter and point options in the test
  if("MC" %in% private$ItemInfo$Type){topletter = max(private$ItemInfo$options, na.rm = T)}
  if("ER" %in% private$ItemInfo$Type){toppoint = max(private$ItemInfo$Value[private$ItemInfo$Type == "ER"])} 
  
  #Find the max number of response columns needed and the number of columns that will represent both numbers and letters
  totalset = max(topletter, toppoint+1)
  overlapset = min(topletter, toppoint+1)
  
  #Build the set of response column names
  responseSet = paste0(
    c(LETTERS[1:topletter], rep("", times = totalset - topletter)),
    c(rep("/", times = overlapset), rep("", times = totalset - overlapset)),
    c(as.character(0:toppoint), rep("", times = totalset - (toppoint+1) )))
  
  basecolumn = ncol(private$ItemInfo) #How many columns does ItemInfo have already?
  private$ItemInfo[,responseSet] = "" #Initialize those columns
  
  #This nested loop should be rewritten using lapply or something
  for(i in 1:nrow(private$ItemInfo)){ #for every item
    for(j in 1:private$ItemInfo$options[i]){ #for every possible response for that item
      if(private$ItemInfo$Type[i] == "ER"){ #if it's an ER item, count how many times that point level was awarded
        private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,private$ItemInfo$ItemName[i], with = F] == j-1)
      } else { #if it's an MC item, count how many times that letter was used
        private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,private$ItemInfo$ItemName[i], with = F] == LETTERS[j])
      }
    }
  }
  private$ResponseSet = responseSet
}