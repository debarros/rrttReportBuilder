#addItemScores.R
badmessage = ""
if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
if(nchar(badmessage) > 0){
  return(badmessage)
} else {
  #establish a list that will hold the Item Response Scores data.frames
  ItemResponseScores = vector(mode = "list", length = length(private$Results))
  #calculate the item response scores for each section and load them in the list
  for(i in 1:length(private$Results)){
    private$Results[[i]]$setItemResponseScores(private$ItemInfo)
    ItemResponseScores[[i]] = private$Results[[i]]$getItemResponseScores()
  }
  ItemResponseScores = rbindlist(ItemResponseScores) #make a single data.table with all of the item response scores from all of the sections
  #Calculate the average score for each question
  for(i in 1:nrow(private$ItemInfo)){
    private$ItemInfo$AverageScore[i] = mean(ItemResponseScores[[private$ItemInfo$ItemName[i]]])/private$ItemInfo$Value[i]*100
  }
  private$ItemScores = private$ItemInfo$AverageScore
  private$ItemResponseScores = ItemResponseScores
}