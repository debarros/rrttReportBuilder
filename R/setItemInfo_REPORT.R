# setItemInfo_REPORT.R

setItemInfo.REPORT = function(report) {
  # put badmessage call here
  
  ItemInfo = openxlsx::read.xlsx(xlsxFile = report$getComparisonLocation(), 
                                 sheet = "Topic Alignment", 
                                 startRow = 2, 
                                 colNames = F)
  ItemInfo = ItemInfo[1:(which(is.na(ItemInfo[,3]))[1] - 1),3:ncol(ItemInfo)] #remove unnecessary columns and rows
  
  # Check for missing info.  If there is any, halt and throw an error.
  errorMessage = character(0)
  if(any(is.na(ItemInfo[ItemInfo[,1] == "Question #:",]))){
    if(sum(is.na(ItemInfo[ItemInfo[,1] == "Question #:",])) == 1){
      errorMessage = c(errorMessage,paste0("Item number ",which(is.na(ItemInfo[ItemInfo[,1] == "Question #:",]))," needs a name."))
    } else {
      erroritems = VectorSentence(which(is.na(ItemInfo[ItemInfo[,1] == "Question #:",])))
      errorMessage = c(errorMessage,paste0("Item numbers ",erroritems," need names."))  
    }
  }
  if(any(is.na(ItemInfo[ItemInfo[,1] == "Value:",]))){
    if(sum(is.na(ItemInfo[ItemInfo[,1] == "Value:",])) == 1){
      errorMessage = c(errorMessage,paste0("Item number ",which(is.na(ItemInfo[ItemInfo[,1] == "Value:",]))," needs a value."))
    } else {
      erroritems = VectorSentence(which(is.na(ItemInfo[ItemInfo[,1] == "Value:",])))
      errorMessage = c(errorMessage,paste0("Item numbers ", erroritems," need values."))  
    }
  }
  if(any(is.na(ItemInfo[ItemInfo[,1] == "Type:",]))){
    if(sum(any(is.na(ItemInfo[ItemInfo[,1] == "Type:",]))) == 1){
      errorMessage = c(errorMessage,paste0("Item number ",which(is.na(ItemInfo[ItemInfo[,1] == "Type:",]))," needs a type."))
    } else {
      erroritems = VectorSentence(which(is.na(ItemInfo[ItemInfo[,1] == "Type:",])))
      errorMessage = c(errorMessage,paste0("Item numbers ", erroritems," need types."))  
    }
  }
  if(length(errorMessage) > 0){ # if there is an error message, halt and report it
    stop(errorMessage)
  }
  
  # Begin to organize the ItemInfo
  ItemInfo = t(ItemInfo)                                   # transpose it
  colnames(ItemInfo) = ItemInfo[1,]                        # use the first row as the column names
  ItemInfo = ItemInfo[-1,]                                 # remove the first row
  row.names(ItemInfo) = NULL                               # remove the row names
  ItemInfo = as.data.frame(ItemInfo, stringsAsFactors = F) # convert it to a data.frame
  ItemInfo$`Answer:` = stringr::str_replace_all(ItemInfo$`Answer:`, fixed(" "), "") # remove whitespace from the answer key
 
  # Topic Alignments
  report$setTopicAlignments(ItemInfo) #set the topic alignments
  ItemInfo = ItemInfo[,!(colnames(ItemInfo) %in% colnames(report$getTopicAlignments()))] # remove the topic alignments from the ItemInfo
 
  # Type, Value, Options, and Answers
  ItemInfo$Type = toupper(substr(x = ItemInfo$`Type:`, 1, 2)) # determine the item type category
  ItemInfo$`Value:` = as.integer(ItemInfo$`Value:`)           # convert the Value column to integer
  ItemInfo$options = ItemInfo$`Value:` + 1                    # default the number of options to what it should be for ER questions
  ItemInfo$`Answer:` = toupper(ItemInfo$`Answer:`)            # convert alpha answers to upper case
  
  badTypes = ItemInfo$Type[!(ItemInfo$Type %in% report$getItemTypeCategories())]
  if(length(badTypes) > 0){
    stop(paste0("The following item types are not acceptable: ", VectorSentence(badTypes),"."))
  }
  
  # set the number of options for MC questions
  isMC = c(ItemInfo$Type == "MC")
  ItemInfo$options[isMC] = as.integer(substr(x = ItemInfo$`Type:`[isMC], start = 3, stop = nchar(ItemInfo$`Type:`[isMC])))
  
  # Make the Answers for ER items be their values
  isER = which(is.na(ItemInfo$Answer))
  ItemInfo$Answer[isER] = ItemInfo$Value[isER]
  
  # Organize the item info to have the correct columns in the correct order with the correct names
  colnames(ItemInfo) = c("Value", "FullType","Tolerance","Answer","ItemName","Type","options")
  ItemInfo$AverageScore = NA_real_
  ItemInfo$Correlation = NA_real_
  ItemInfo = ItemInfo[,c("ItemName","Value", "Answer", "AverageScore","Correlation","Type","options")]
  
  report$setItemInfoQuick(ItemInfo)
  
} # /setItemInfo.REPORT
