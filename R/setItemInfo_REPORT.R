# setItemInfo_REPORT.R

setItemInfo.REPORT = function(report) {
  # put badmessage call here
  
  ItemInfo = openxlsx::read.xlsx(xlsxFile = report$getComparisonLocation(), 
                                 sheet = "Topic Alignment", 
                                 startRow = 2, 
                                 colNames = F)
  ItemInfo = ItemInfo[1:(which(is.na(ItemInfo[,3]))[1] - 1),3:ncol(ItemInfo)] #remove unnecessary columns and rows
  
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
  if(length(errorMessage) > 0){
    stop(errorMessage)
  }
  
  ItemInfo = t(ItemInfo) #transpose it
  colnames(ItemInfo) = ItemInfo[1,] #use the first row as the column names
  ItemInfo = ItemInfo[-1,] #remove the first row
  row.names(ItemInfo) = NULL #remove the row names
  ItemInfo = as.data.frame(ItemInfo, stringsAsFactors = F) #convert it to a data.frame
 
  report$setTopicAlignments(ItemInfo) #set the topic alignments
  ItemInfo = ItemInfo[,!(colnames(ItemInfo) %in% colnames(report$getTopicAlignments()))] #remove the topic alignments from the ItemInfo
 
  ItemInfo$isMC = grepl("mc",ItemInfo$`Type:`, ignore.case = T) #determine which questions are MC
  ItemInfo$`Value:` = as.integer(ItemInfo$`Value:`) #convert the Value column to integer
  ItemInfo$options = ItemInfo$`Value:` + 1 #default the number of options to what it should be for ER questions
  
  #set the number of options for MC questions
  ItemInfo$options[ItemInfo$isMC] = as.integer(substr(ItemInfo$`Type:`[ItemInfo$isMC], 3, nchar(ItemInfo$`Type:`[ItemInfo$isMC])))
  ItemInfo$Type = "ER" #default the question type to ER
  ItemInfo$Type[ItemInfo$isMC] = "MC" #set the question type to MC for MC questions
  

  colnames(ItemInfo) = c("Value", "FullType","Tolerance","Answer","ItemName","isMC","options","Type")
  ItemInfo$AverageScore = NA_real_
  ItemInfo$Correlation = NA_real_
  ItemInfo = ItemInfo[,c("ItemName","Value", "Answer", "AverageScore","Correlation","Type","options")]
  
  # Make the Answers for ER items be their values
  toFix = which(is.na(ItemInfo$Answer))
  ItemInfo$Answer[toFix] = ItemInfo$Value[toFix]
  
  report$.__enclos_env__$private$ItemInfo = ItemInfo
  
} #/setItemInfo.REPORT