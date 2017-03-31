#setResults.R

if(is.null(private$Sources)){
  return("Need sources first.")
} else {
  results = vector(mode = "list", length = length(private$Sources)) #set up a list to hold the response sets for the various sections
  names(results) = paste0("a", 1:length(private$Sources)) #add names to the list so they can be set later
  for (i in 1:length(private$Sources)){  #for each source/section
    SectionName = read.csv(private$Sources[i], skip = 1, header = F, nrows = 1, stringsAsFactors = F)[1,2] #get the section name
    thisResult = RESULT$new(SectionName)
    thisResult$setItemResponses(private$Sources[i], private$ItemInfo$ItemName, private$ItemInfo$Value)
    results[[i]] = thisResult #put the response info in the list
    names(results)[i] = SectionName #set the element name in the list to be the name of the section
  }
  private$Results = results
}