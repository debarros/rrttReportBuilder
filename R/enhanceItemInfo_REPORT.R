# enhanceItemInfo_REPORT.R

enhanceItemInfo.REPORT = function(report, useLocalNames = T, useLocalValues = T) {
  # put badmessage call here
  
  d2 = openxlsx::read.xlsx(xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
                           sheet = "Topic Alignment", 
                           startRow = 2, 
                           colNames = F)
  d2 = d2[1:(which(is.na(d2[,3]))[1] - 1),3:ncol(d2)] #remove unnecessary columns and rows
  d2 = t(d2) #transpose it
  colnames(d2) = d2[1,] #use the first row as the column names
  d2 = d2[-1,] #remove the first row
  row.names(d2) = NULL #remove the row names
  d2 = as.data.frame(d2, stringsAsFactors = F) #convert it to a data.frame
  
  # Should the report use item names from the test set up file?
  if(useLocalNames){ # if so,
    report$.__enclos_env__$private$ItemInfo$ItemName = d2$`Question #:` # pull them in from the setup info
    #do something here to set the column names in the ItemResponse members of the results 
  } else { # if not,
    d2$`Question #:` = report$.__enclos_env__$private$ItemInfo$ItemName # rename the items in the setup info
  } # /if-else
  
  report$setTopicAlignments(d2) #set the topic alignments
  d2$isMC = grepl("mc",d2$`Type:`, ignore.case = T) #determine which questions are MC
  d2$`Value:` = as.integer(d2$`Value:`) #convert the Value column to integer
  d2$options = d2$`Value:` + 1 #default the number of options to what it should be for ER questions
  
  #set the number of options for MC questions
  d2$options[d2$isMC] = substr(d2$`Type:`[d2$isMC], 3, nchar(d2$`Type:`[d2$isMC])) 
  d2$type = "ER" #default the question type to ER
  d2$type[d2$isMC] = "MC" #set the question type to MC for MC questions
  report$.__enclos_env__$private$ItemInfo$Type = d2$type[match(report$.__enclos_env__$private$ItemInfo$ItemName, d2$`Question #:`)] #set the type 
  
  #set the number of options
  report$.__enclos_env__$private$ItemInfo$options = as.integer(d2$options[match(report$.__enclos_env__$private$ItemInfo$ItemName, d2$`Question #:`)]) 
  
  # Should the report use item values from the test set up file?
  if(useLocalValues){
    report$.__enclos_env__$private$ItemInfo$Value = as.integer(d2$`Value:`[match(report$.__enclos_env__$private$ItemInfo$ItemName, d2$`Question #:`)]) 
  }
  
} # /function