# getResponses_REPORT

getResponses.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){ message("Running getResponses.REPORT") }
  
  # pull the needed info from the report
  Results = report$getResults()
  
  # establish a list that will hold the Item Response data.frames
  ItemResponses = vector(mode = "list", length = length(Results))
  
  # load the item responses for each section in the list
  for(i in 1:length(Results)){
    currentResult = Results[[i]]
    ItemResponses[[i]] = currentResult$getItemResponses()
  } # /for
  
  # make a data.table with all of the item responses from all of the sections
  ItemResponses = data.table::rbindlist(ItemResponses) 
  
  return(ItemResponses)
  
} # /function
