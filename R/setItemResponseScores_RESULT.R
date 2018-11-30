# setItemResponseScores_RESULT.R
           
setItemResponseScores.RESULT = function(ItemInfo, TMS, HaltOnMultiResponse, result, messageLevel = 0){

  if(messageLevel > 0){
    message("Running setItemResponseScores.RESULT")
  }
  
  if(messageLevel > 1){
    message(paste0("str(result) = ", str(result)))
  }
  
  # Get the relevant data from the result
  ItemResp = result$getItemResponses()  # all responses to all items
  sectionName = result$getSectionName()  
  
  # Create a data.frame to hold the item scores
  ItemResponseScores = setNames(as.data.frame(
    array(data = NA_integer_, dim = dim(ItemResp))),
    colnames(ItemResp)) 
  ItemResponseScores[,1:6] = ItemResp[,1:6]          # pull in the student info from the results data.table
  
  # Calculate scores for each response on each item
  # Note: For gridded response items with tolerance, this section will have to be edited
  
  if(messageLevel > 0){
    message("Calculating item scores")
  }
  
  for(i in 1:nrow(ItemInfo)){ # for each item on the assessment
    if(messageLevel > 1){
      message(paste0("Calculating scores for item ", i , " of ", nrow(ItemInfo)))
    }
    if(ItemInfo$Type[i] == "MC"){
      AnswerSet = strsplit(ItemInfo$Answer[i], split = ",")[[1]]
      uniqueResponses = ItemResp[,ItemInfo$ItemName[i]]
      if(HaltOnMultiResponse){
      if(any(grepl(pattern = ",", x = uniqueResponses, fixed = T))){
        stop(paste0("Error!  The data for ", sectionName, " has MC responses that contain commas."))
      }
        }
      
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] %in% AnswerSet)
      
    } else if(ItemInfo$Type[i] == "ER"){
      responses = ItemResp[,ItemInfo$ItemName[i]]
      if(is.character(responses)){
        responses = suppressWarnings(as.numeric(responses))
        # If any values get converted to NA's, stop.
        if(any(is.na(responses))){                          
          stop(paste0("Error!  The data for ", sectionName, " has extended response item scores that are not numbers."))
        }  
      }
      ItemResponseScores[,ItemInfo$ItemName[i]] = responses
      
    } else if(ItemInfo$Type[i] == "WH"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
      
    } else if(ItemInfo$Type[i] == "FL"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
      
    } else if(ItemInfo$Type[i] == "FI"){
      ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
    }
  } # /for
  
  # If this is a TMS that doesn't include total points in the exports, add it now
  if(TMS %in% c("ScantronAS", "ASAP")){
    if(messageLevel > 1){
      message("Adding total points to the ItemResponseScores")
    }
    ItemResponseScores$TotalPoints = apply(X = ItemResponseScores[,ItemInfo$ItemName], MARGIN = 1, FUN = sum, na.rm = T)
    ItemResponseScores$score = ItemResponseScores$TotalPoints/sum(ItemInfo$Value)*100
    ItemResp$TotalPoints = ItemResponseScores$TotalPoints
    ItemResp$score = ItemResponseScores$score
    result$setIRquick(ItemResp)
  } # /if

  if(messageLevel > 0){
    message("Finished setItemResponseScores.RESULT")
  }
    
  result$setIRSquick(ItemResponseScores)
  
} # /function
