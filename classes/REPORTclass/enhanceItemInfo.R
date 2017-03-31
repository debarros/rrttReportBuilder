#enhanceItemInfo.R

badmessage = ""
if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
if(nchar(badmessage) > 0){
  return(badmessage)
} else {
  d2 = read.xlsx(xlsxFile = private$ComparisonLocation, sheet = "Topic Alignment", startRow = 2, colNames = F)
  d2 = d2[1:(which(is.na(d2[,3]))[1] - 1),3:ncol(d2)] #remove unnecessary columns and rows
  d2 = t(d2) #transpose it
  colnames(d2) = d2[1,] #use the first row as the column names
  d2 = d2[-1,] #remove the first row
  row.names(d2) = NULL #remove the row names
  d2 = as.data.frame(d2, stringsAsFactors = F) #convert it to a data.frame
  self$setTopicAlignments(d2) #set the topic alignments
  d2$isMC = grepl("mc",d2$`Type:`, ignore.case = T) #determine which questions are MC
  d2$`Value:` = as.integer(d2$`Value:`) #convert the Value column to integer
  d2$options = d2$`Value:` + 1 #default the number of options to what it should be for ER questions
  d2$options[d2$isMC] = substr(d2$`Type:`[d2$isMC], 3, nchar(d2$`Type:`[d2$isMC])) #set the number of options for MC questions
  d2$type = "ER" #default the question type to ER
  d2$type[d2$isMC] = "MC" #set the question type to MC for MC questions
  private$ItemInfo$Type = d2$type[match(private$ItemInfo$ItemName, d2$`Question #:`)] #set the type 
  private$ItemInfo$options = as.integer(d2$options[match(private$ItemInfo$ItemName, d2$`Question #:`)]) #set the number of options
}