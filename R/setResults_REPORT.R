# setResults_REPORT

setResults.REPORT = function(report) {
  
  Sources = report$getSources()
  nsources = length(Sources)
  ItemInfo = report$getItemInfo()
  TMS = report$getTMS()
  
  if(TMS == "ScantronAS"){
    sourcenames = report$.__enclos_env__$private$SourceFileNames
    SectionNames = strsplit(x = sourcenames, split = "_")
    for(i in 1:length(sourcenames)){
      x = SectionNames[[i]]
      SectionNames[[i]] = paste0(x[1:(length(x)-1)], collapse = " ")
    } # /for
    results = vector(mode = "list", length = nsources) # set up list to hold the response sets for the various sections
    hasData = vector(mode = "logical", length = nsources) # set up a logical vector to hold whether or not the result has any data
    names(results) = paste0("a", 1:nsources)  #add names to the list so they can be set later
    for (i in 1:nsources){  #for each source/section
      thisSource = sourcenames[i]
      SectionName = SectionNames[[i]]
      thisResult = RESULT$new(SectionName)
      hasData[i] = thisResult$setItemResponses(sourceLocation = Sources[i], itemNames = ItemInfo$ItemName, itemValues = ItemInfo$Value, TMS = TMS)
      if(hasData[i]){
        results[[i]] = thisResult #put the response info in the list
      }
      names(results)[i] = SectionName #set the element name in the list to be the name of the section
    } # for each source/section
    missingSections = SectionNames[!hasData]
    missingSections = unlist(missingSections)
    
    report$.__enclos_env__$private$MissingSections = missingSections
    report$.__enclos_env__$private$Results = results[hasData]
    
  } else if(TMS == "LinkIt"){
    #set up list to hold the response sets for the various sections
    results = vector(mode = "list", length = nsources) 
    #add names to the list so they can be set later
    names(results) = paste0("a", 1:nsources) 
    for (i in 1:nsources){  #for each source/section
      SectionName = read.csv(file = Sources[i], 
                             skip = 1, header = F, nrows = 1, 
                             stringsAsFactors = F)[1,2] #get the section name
      thisResult = RESULT$new(SectionName)
      thisResult$setItemResponses(Sources[i], ItemInfo$ItemName, ItemInfo$Value, TMS)
      results[[i]] = thisResult #put the response info in the list
      names(results)[i] = SectionName #set the element name in the list to be the name of the section
    }
    report$.__enclos_env__$private$Results = results
    
  } else {
    stop(paste0("Unknown or unsupported TMS: ", TMS))
  } # /if-else
  
} # /setResults.REPORT function