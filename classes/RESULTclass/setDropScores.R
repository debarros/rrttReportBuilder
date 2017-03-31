#setDropScores.R

#Set up a dataframe to hold the scores of each students with each item dropped and then calculate those scores
DropScores = private$ItemResponseScores 
for(i in 1:nrow(DropScores)){
  for(j in ItemInfo$ItemName){
    DropScores[i,j] = DropScores$TotalPoints[i] - private$ItemResponseScores[i,j]
  }
}
private$DropScores = DropScores