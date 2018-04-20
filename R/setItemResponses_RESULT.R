# setItemResponses_RESULT.R

setItemResponses.RESULT = function(sourceLocation, ItemInfo, TMS, result, messageLevel = 0){

  SectionName  = result$getSectionName() # grab the name of the section associated with the current result object
  
  if(messageLevel > 0){message(paste0("setting item responses for ", SectionName))}
  
  itemNames = ItemInfo$ItemName          # grab item names
  itemValues = ItemInfo$Value            # grab item values
  
  ret = TRUE           # initialize the return value
  ItemResponses = NULL # initialize the item responses object
  
  
  if(TMS == "LinkIt"){
    ItemResponses = read.csv(sourceLocation, skip = 13, header = F, stringsAsFactors = F)               # read the item response info
    colnames(ItemResponses) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",itemNames) # set the column names 
    
    # Set the basic score column.  Note that, if there is special scoring, that will be applied later.
    ItemResponses$score = ItemResponses$TotalPoints/sum(itemValues)*100
    
    #put the score column first
    ItemResponses = ItemResponses[,c(which(colnames(ItemResponses)=="score"),which(colnames(ItemResponses)!="score"))] 
    
    
  } else if (TMS == "ScantronAS"){
    ItemResponses = read.csv(sourceLocation, stringsAsFactors = F)                # read the item response info
    if(nrow(ItemResponses) == 1){                                    # if there is not data, return FALSE
      ret = FALSE 
    } else {
      ItemResponses = ItemResponses[-nrow(ItemResponses),1:(ncol(ItemResponses)-3)]
      colnames(ItemResponses) = c("Student", "StudentID", "Test.Name", itemNames)
      
      # Split the full names into first and last names
      commaSpot = regexpr(pattern = ",",text = ItemResponses$Student)
      ItemResponses$LastName = substr(x = ItemResponses$Student, start = 1, stop = commaSpot - 1)
      ItemResponses$FirstName = substr(x = ItemResponses$Student, start = commaSpot + 2, stop = nchar(ItemResponses$Student))
      ItemResponses$TotalPoints = NA_integer_
      ItemResponses$TestDate = NA_character_
      ItemResponses$score = NA_real_
      
      # Reorder the columns
      ItemResponses = ItemResponses[,c("score","StudentID", "LastName", "FirstName", "TestDate","TotalPoints",itemNames)]
      
      # Convert gridded responses to numeric values so equivalent responses are manageable
      for(i in 1:nrow(ItemInfo)){
        itName = ItemInfo$ItemName[i]
        itType = ItemInfo$Type[i]
        itResp = ItemResponses[,itName]
        if(itType %in% c("WH", "FL", "FI")){
          itRespNum = suppressWarnings(as.numeric(itResp))
          if(any(itResp[is.na(itRespNum)] != "--")){
            stop(paste0("Error! ", SectionName, " has gridded responses that are not numbers or '--'."))
          } else {
            itRespNum[is.na(itRespNum)] = itResp[is.na(itRespNum)]
            ItemResponses[,itName] = as.character(itRespNum)
          }
        }
      }
    }
    
  } else if (TMS == "ASAP") {
    ItemResponses = read.csv(sourceLocation, stringsAsFactors = F) # read the item response info
    ItemResponses = ItemResponses[,4:(ncol(ItemResponses))]
    colnames(ItemResponses) = c("Student", "StudentID","Test.Name",itemNames)
    
    # Split the full names into first and last names
    commaSpot = regexpr(pattern = ",",text = ItemResponses$Student)
    ItemResponses$LastName = substr(x = ItemResponses$Student, start = 1, stop = commaSpot - 1)
    ItemResponses$FirstName = substr(x = ItemResponses$Student, start = commaSpot + 2, stop = nchar(ItemResponses$Student))
    ItemResponses$TotalPoints = NA_integer_
    ItemResponses$TestDate = NA_character_
    ItemResponses$score = NA_real_
    
    # Reorder the columns
    ItemResponses = ItemResponses[,c("score","StudentID", "LastName", "FirstName", "TestDate","TotalPoints",itemNames)]
    
    
  } else {
    stop(paste0("Unknown or unsupported TMS: ", TMS))
  } # /if-else based on TMS
  
  result$setIRquick(ItemResponses)  
  return(ret)
  
} # /function
