#addCorrelations.R

badmessage = ""
if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
if(nchar(badmessage) > 0){
  return(badmessage)
} else {
  #establish a list that will hold the DropScores data.frames
  DropScores = vector(mode = "list", length = length(private$Results))
  #calculate the item response scores for each section and load them in the list
  for(i in 1:length(private$Results)){
    private$Results[[i]]$setDropScores(private$ItemInfo)
    DropScores[[i]] = private$Results[[i]]$getDropScores()
  }
  DropScores = rbindlist(DropScores) #make a single data.table with all of the dropscores from all of the sections
  #Calculate the correlations between the student scores on the item and the student total scores after dropping the item
  for(i in 1:nrow(private$ItemInfo)){
    thisItem = private$ItemInfo$ItemName[i]
    private$ItemInfo$Correlation[i] = cor(private$ItemResponseScores[[thisItem]], DropScores[[thisItem]])
  }
}
private$Correlations = private$ItemInfo$Correlation
private$DropScores = DropScores