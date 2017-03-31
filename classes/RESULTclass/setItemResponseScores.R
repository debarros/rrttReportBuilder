#setItemResponseScores.R

#create a data.frame to hold the item scores
ItemResponseScores = setNames(as.data.frame(array(data = NA_integer_, dim = dim(private$ItemResponses))), colnames(private$ItemResponses)) 
ItemResponseScores[,1:5] = private$ItemResponses[,1:5] #pull in the student info from the results data.table
#Calculate scores for each response on each item
for(i in 1:nrow(ItemInfo)){
  if(ItemInfo$Type[i] == "MC"){
    ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(private$ItemResponses[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
  } else {
    ItemResponseScores[,ItemInfo$ItemName[i]] = private$ItemResponses[,ItemInfo$ItemName[i]]
  }
}
private$ItemResponseScores = ItemResponseScores