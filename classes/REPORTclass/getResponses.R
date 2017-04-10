#getResponses.R

#getResponses method for REPORT class

#establish a list that will hold the Item Response data.frames
ItemResponses = vector(mode = "list", length = length(private$Results))
#load the item responses for each section in the list
for(i in 1:length(private$Results)){
  ItemResponses[[i]] = private$Results[[i]]$getItemResponses()
}
ItemResponses = rbindlist(ItemResponses) #make a single data.table with all of the item responses from all of the sections
return(ItemResponses)