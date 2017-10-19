# addResponseFrequencies_REPORT.R

addResponseFrequencies.REPORT = function(report) {
  # put badmessage call here
  
  # Grab the data that will be needed for this part
  ItemResponses = report$getResponses()
  ItemInfo = report$getItemInfo()
  
  # For each item, determine the response set
  ItemResponseOptions = vector(mode = "list", length = nrow(ItemInfo))
  names(ItemResponseOptions) = ItemInfo$ItemName
  for(i in 1:nrow(ItemInfo)){
    if(ItemInfo$Type[i] == "MC"){
      currentOptions = LETTERS[1:ItemInfo$options[i]]
    } else if(ItemInfo$Type[i] == "ER"){
      currentOptions = as.character(0:ItemInfo$Value[i])
    } else if(ItemInfo$Type[i] == "WH"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[i], with = F])))
    } else if(ItemInfo$Type[i] == "FL"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[i], with = F])))
    } else if(ItemInfo$Type[i] == "FI"){
      currentOptions = sort(unlist(unique(ItemResponses[,colnames(ItemResponses) == ItemInfo$ItemName[i], with = F])))
    } # /if-else
    currentOptions = currentOptions[currentOptions != "--"]
    ItemResponseOptions[[i]] = currentOptions
  } # /for
  
  
  # Determine the unique set of item types
  responseSetByType = vector(mode = "list", length = length(unique(ItemInfo$Type)))
  names(responseSetByType) = unique(ItemInfo$Type)
  
  # For each type, determine the possible options
  for(i in 1:length(responseSetByType)){
    allresponseoptions = unlist(ItemResponseOptions[ItemInfo$Type == names(responseSetByType)[i]])
    responseSetByType[[i]] = sort(unique(allresponseoptions))
  } # /for
  
  # Initialize the responseSet vector 
  responseSet = rep("", times = max(lengths(responseSetByType)))
  
   
  for(i in 1:length(responseSetByType)){                                 # For each element in responseSetByType,
    for(j in 1:length(responseSetByType[[i]])){                          # For each entry in that vector, 
      if(nchar(responseSet[j]) > 0){                                     # if there is aleady something there
        responseSet[j] = paste0(responseSet[j], "/")                     # append a slash
      }
      responseSet[j] = paste0(responseSet[j], responseSetByType[[i]][j]) # append the response option
    } # /for each response option for this type
  } # /for each type of response
  
  
  # Add frequency columns to ItemInfo and load the response frequencies
  basecolumn = ncol(ItemInfo)                       # How many columns does ItemInfo have already?
  ItemInfo[,responseSet] = ""                       # Initialize columns for response frequencies
  for(i in 1:nrow(ItemInfo)){                       # For each item
    if(ItemInfo$Type[i] %in% c("ER", "MC")){
      curRespSet = ItemResponseOptions[[i]]
    } else {
      curRespSet = responseSetByType[[ItemInfo$Type[i]]]  
    }
    
    for(j in 1:length(curRespSet)){   # For each possible option for that item type category,
      currentResponse = curRespSet[j] # Grab the response
      ItemInfo[i,j+basecolumn] = sum(ItemResponses[,ItemInfo$ItemName[i], with = F] == currentResponse, na.rm = T) # Get the frequency
    }
  }
  
  report$setItemInfoQuick(ItemInfo)                       # store the ItemInfo
  report$setResponseSetQuick(responseSet)                 # store the responseSet
  report$setItemResponseOptionsQuick(ItemResponseOptions) # store the ItemResponseOptions list
  
} # /addResponseFrequencies.REPORT
