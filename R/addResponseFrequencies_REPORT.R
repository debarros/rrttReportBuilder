# addResponseFrequencies_REPORT.R

addResponseFrequencies.REPORT = function(report, messageLevel = 0) {
  # put badmessage call here
  
  if(messageLevel > 0){
    message("Running addResponseFrequencies.REPORT")
  }
  
  # Grab the data that will be needed for this part
  ItemResponses = report$getResponses()
  ItemInfo =      report$getItemInfo()
  TMS =           report$getTMS()
  testName =      report$getTestName()
  
  # For each item, determine the response set
  # This currently assumes that, for multiple choice questions, responses are letters in alphabetical order, starting with A
  ItemResponseOptions = vector(mode = "list", length = nrow(ItemInfo))
  names(ItemResponseOptions) = ItemInfo$ItemName
  for(thisItem in 1:nrow(ItemInfo)){
    if(ItemInfo$Type[thisItem] == "MC"){
      if(TMS == "ASAP"){                                       # This is a terrible piece of code that should be replaced
        currentOptions = as.character(1:ItemInfo$options[thisItem])
      } else {
        currentOptions = LETTERS[1:ItemInfo$options[thisItem]]  
      }
    } else if(ItemInfo$Type[thisItem] == "ER"){
      currentOptions = as.character(0:ItemInfo$Value[thisItem])
      if(TMS == "ASAP"){
        if(grepl("global", testName, ignore.case = T) | grepl("US History", testName, ignore.case = T)){
          currentOptions = as.character(seq(from = 0, to = 5, by = .5))
        }
      }
    } else if(ItemInfo$Type[thisItem] == "WH"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[thisItem], with = F])))
      currentOptions.numeric = suppressWarnings(as.numeric(currentOptions))     # Convert to numeric
      naNumOpts = is.na(currentOptions.numeric)
      currentOptions.numeric[naNumOpts] = currentOptions[naNumOpts]             # Anything not numeric, put it back in
      currentOptions = sort(unique(currentOptions.numeric))                     # Remove duplicates from equivalent answers
    } else if(ItemInfo$Type[thisItem] == "FL"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[thisItem], with = F])))
    } else if(ItemInfo$Type[thisItem] == "FI"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[thisItem], with = F])))
    } # /if-else
    currentOptions = currentOptions[currentOptions != "--"]
    ItemResponseOptions[[thisItem]] = currentOptions
  } # /for
  
  
  # Determine the unique set of item types
  responseSetByType = vector(mode = "list", length = length(unique(ItemInfo$Type)))
  names(responseSetByType) = unique(ItemInfo$Type)
  
  # For each type, determine the possible options
  for(thisRespType in 1:length(responseSetByType)){
    allresponseoptions = unlist(ItemResponseOptions[ItemInfo$Type == names(responseSetByType)[thisRespType]])
    uniqueResponseOptions = unique(allresponseoptions)
    uniqueResponseOptions.numeric = suppressWarnings(as.numeric(uniqueResponseOptions))
    if(any(is.na(uniqueResponseOptions.numeric))){                                       # if any options are not numbers,
      responsesToUse = sort(uniqueResponseOptions)                                       # sort alphabetically
    } else {                                                                             # if all options are numbers,
      responsesToUse = as.character(sort(uniqueResponseOptions.numeric))                 # sort numerically
    }
    responseSetByType[[thisRespType]] = responsesToUse
  } # /for
  
  # Initialize the responseSet vector 
  responseSet = rep("", times = max(lengths(responseSetByType)))
  
  
  for(thisRespType in 1:length(responseSetByType)){                                   # For each element in responseSetByType,
    if(length(responseSetByType[[thisRespType]]) > 0){                                # If there are any responses for that type,
      for(thisRespOption in 1:length(responseSetByType[[thisRespType]])){                          # For each entry in that vector, 
        if(nchar(responseSet[thisRespOption]) > 0){                                     # if there is aleady something there
          responseSet[thisRespOption] = paste0(responseSet[thisRespOption], "/")                     # append a slash
        }
        responseSet[thisRespOption] = paste0(responseSet[thisRespOption], responseSetByType[[thisRespType]][thisRespOption]) # append the response option
      } # /for each response option for this type
    }
  } # /for each type of response
  
  
  # Add frequency columns to ItemInfo and load the response frequencies
  basecolumn = ncol(ItemInfo)                       # How many columns does ItemInfo have already?
  ItemInfo[,responseSet] = ""                       # Initialize columns for response frequencies
  for(thisItem in 1:nrow(ItemInfo)){                       # For each item
    if(ItemInfo$Type[thisItem] %in% c("ER", "MC")){
      curRespSet = ItemResponseOptions[[thisItem]]
    } else {
      curRespSet = responseSetByType[[ItemInfo$Type[thisItem]]]  
    }
    
    for(j in 1:length(curRespSet)){   # For each possible option for that item type category,
      currentResponse = curRespSet[j] # Grab the response
      ItemInfo[thisItem,j+basecolumn] = sum(ItemResponses[,ItemInfo$ItemName[thisItem], with = F] == currentResponse, na.rm = T) # Get the frequency
    }
  }
  
  report$setItemInfoQuick(ItemInfo)                       # store the ItemInfo
  report$setResponseSetQuick(responseSet)                 # store the responseSet
  report$setItemResponseOptionsQuick(ItemResponseOptions) # store the ItemResponseOptions list
  
} # /addResponseFrequencies.REPORT
