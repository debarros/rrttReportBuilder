#setItemResponses.R

ItemResponses = read.csv(sourceLocation, skip = 13, header = F, stringsAsFactors = F) #read the item response info
colnames(ItemResponses) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",itemNames) #set the column names 
ItemResponses$score = ItemResponses$TotalPoints/sum(itemValues)*100
private$ItemResponses = ItemResponses