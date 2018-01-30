# setResults_REPORT

setResults.REPORT = function(report) {
  
  # Get the relevant parts of the report object
  Sources = report$getSources()             # file paths to csvs with item response data
  ItemInfo = report$getItemInfo()           # data.frame with info about items
  TMS = report$getTMS()                     # name of the testing management system
  sourcenames = report$getSourceFileNames() # names of the sourse data files (for determining section names)
  
  # Initialize a couple of variables
  nsources = length(Sources)
  missingSections = NULL
  
  if(TMS == "ScantronAS"){
    SectionNames = strsplit(x = sourcenames, split = "_")
    for(i in 1:length(sourcenames)){
      x = SectionNames[[i]]
      SectionNames[[i]] = paste0(x[1:(length(x)-1)], collapse = " ")
    } # /for
    results = vector(mode = "list", length = nsources)    # set up list to hold the response sets for the various sections
    hasData = vector(mode = "logical", length = nsources) # set up a logical vector to hold whether or not the result has any data
    names(results) = paste0("a", 1:nsources)              # add names to the list so they can be set later
    
    for (i in 1:nsources){                                # for each source/section
      thisSource = sourcenames[i]
      SectionName = SectionNames[[i]]
      thisResult = RESULT$new(SectionName)
      sourceLocation = Sources[i]
      itemNames = ItemInfo$ItemName
      itemValues = ItemInfo$Value
      hasData[i] = thisResult$setItemResponses(sourceLocation, itemNames, itemValues, TMS)
      if(hasData[i]){
        results[[i]] = thisResult                         # put the response info in the list
      }
      names(results)[i] = SectionName                     # set the element name in the list to be the name of the section
    } # /for each source/section
    
    missingSections = SectionNames[!hasData]
    missingSections = unlist(missingSections)
    results = results[hasData]
    
    
  } else if(TMS == "LinkIt"){
    results = vector(mode = "list", length = nsources)    # set up list to hold the response sets for the various sections
    names(results) = paste0("a", 1:nsources)              # add names to the list so they can be set later
    
    for (i in 1:nsources){                                # for each source/section
      SectionName = read.csv(file = Sources[i], skip = 1, # get the section name
                             header = F, nrows = 1, 
                             stringsAsFactors = F)
      SectionName = SectionName[1,2]
      thisResult = RESULT$new(SectionName)
      sourceLocation = Sources[i]
      itemNames = ItemInfo$ItemName
      itemValues = ItemInfo$Value
      thisResult$setItemResponses(sourceLocation, itemNames, itemValues, TMS)
      results[[i]] = thisResult                           # put the response info in the list
      names(results)[i] = SectionName                     # set the element name in the list to be the name of the section
    } # /for each source/section

    
  } else if(TMS == "ASAP"){
    SectionNames = substr(sourcenames, 1, nchar(sourcenames) - 4) # get the section names
    results = vector(mode = "list", length = nsources)            # set up list to hold the response sets for the various sections
    names(results) = paste0("a", 1:nsources)                      # add names to the list so they can be set later
    
    for (i in 1:nsources){                 # for each source/section
      thisSource = sourcenames[i]
      SectionName = SectionNames[[i]]
      thisResult = RESULT$new(SectionName)
      sourceLocation = Sources[i]
      itemNames = ItemInfo$ItemName
      itemValues = ItemInfo$Value
      thisResult$setItemResponses(sourceLocation, itemNames, itemValues, TMS)
      results[[i]] = thisResult            # put the response info in the list
      names(results)[i] = SectionName      # set the element name in the list to be the name of the section
    } # /for each source/section
    
    
  } else {
    stop(paste0("Unknown or unsupported TMS: ", TMS))
  } # /if-else
  
  report$setMissingSectionsQuick(missingSections)
  report$setResultsQuick(results)
  
} # /setResults.REPORT function
