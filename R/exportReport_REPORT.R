# exportReport_REPORT

exportReport.REPORT = function(filename, template, report, messageLevel = 0) {
  
  if(messageLevel > 0){
    message("Running exportReport.REPORT")
  }
  
  # put badmessage call here ####

  # pull the needed stuff from the report ####
  DataLocation =    report$getDataLocation()
  UploadFiles =     report$getUpLoadFiles()
  Results =         report$getResults()
  nResults =        length(Results)
  ItemInfo =        report$getItemInfo()
  Summary =         report$getSummary()
  UploadTab =       report$getUploadTab()
  TopicAlignments = report$getTopicAlignments()
  TopicSummary =    report$getTopicSummary()
  HasTopics =       report$checkTopics()
  Narrative =       report$getNarrative()
  Handouts =        report$getHandouts()
  Comparisons =     report$getComparison()
  ResponseSet =     report$getResponseSet()
    
  # check to see if the report has been run before ####
  uploadFilePath = paste0(DataLocation, "\\", UploadFiles[1])
  if(file.exists(uploadFilePath)){
    report$exportUpdate(uploadFilePath, messageLevel = messageLevel - 1)
  }
  
  # Load the template ####
  # This next part creates a slight delay
  
  if(messageLevel > 1){message("Load the template")}
  
  if(is.null(template)){
    wb1 = openxlsx::loadWorkbook(file = system.file("extdata", "template", package = "rrttReportBuilder"), isUnzipped = T)  
  } else {
    wb1 = openxlsx::loadWorkbook(file = template, isUnzipped = F)
  } # /if-else
  
  
  # For each class section, write the student responses to the Responses tab ####
  if(messageLevel > 1){message("Write the Responses tab")}
  for(res in 1:nResults){
    currentResult = Results[res]
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = names(currentResult), 
                        startCol = 1, 
                        startRow = 100*(res-1) + 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = currentResult[[1]]$getItemResponses(), 
                        startCol = 1, 
                        startRow = 100*(res-1) + 2)
  } # /for
  
  
  # For each class section, write the student item response scores to the ItemScores tab ####
  if(messageLevel > 1){message("Write the ItemScores tab")}
  for(i in 1:nResults){
    currentResult = Results[i]
    openxlsx::writeData(wb = wb1, 
                        sheet = "ItemScores", 
                        x = names(currentResult), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "ItemScores", 
                        x = currentResult[[1]]$getItemResponseScores(), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 2)
  } # /for
  
  
  # Write the ItemInfo and Summary tabs ####
  if(messageLevel > 1){message("Write the ItemInfo and Summary tabs")}
  openxlsx::writeData(wb = wb1, 
                      sheet = "ItemInfo", 
                      x = ItemInfo)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = data.frame(Summary), 
                      startRow = 1)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = names(Results), 
                      startRow = 3)
  
  
  # If there are topics, write data to the relevant tabs ####
  if(HasTopics){
    if(messageLevel > 1){message("Write the TopicAlignments and TopicSummary tabs")}
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicAlignments", 
                        x = TopicAlignments, 
                        startRow = 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicSummary", 
                        x = TopicSummary, 
                        startRow = 1, 
                        rowNames = T)
    
    # Write the student topic scores one section at a time
    if(messageLevel > 1){message("Write the TopicScores tab")}
    for(i in 1:nResults){
      currentResult = Results[i]
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = names(currentResult), 
                          startCol = 1, 
                          startRow = 100*(i-1) + 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = currentResult[[1]]$getTopicScores(), 
                          startCol = 1, 
                          startRow = 100*(i-1) + 2)
    } # /for each section
  } # /if there are topics
  
  
  # Write the Item_Summary tab and raw handouts tab ####
  if(messageLevel > 1){message("Write the Item_Summary and Raw Handout Data tabs")}
  ItemSummary = data.frame(Narrative)
  ItemSummary = ItemSummary[4:(nrow(ItemSummary)-2), , drop = F]
  ItemSummary = ItemSummary[!(ItemSummary[,1] %in% c("* Boxplots", "* Topics")), , drop = F]
  openxlsx::writeData(wb = wb1, 
                      sheet = "Item_Summary", 
                      x = ItemSummary, 
                      startRow = 3, 
                      colNames = F)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Raw Handout Data", 
                      x = Handouts, 
                      startRow = 1)
  
  
  # If there are comparisons, write data to the comparison tab ####
  numberOfComparisons = length(Comparisons)
  if(numberOfComparisons > 0){
    if(messageLevel > 1){message("Write the Comparison tab")}
    
    for(i in numberOfComparisons:1){
      colShift = 14*(numberOfComparisons - i)
      currentComparison = Comparisons[[i]]
      CompSummary = currentComparison$getSummary()
      Items = currentComparison$getItemComparisons()
      
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$Total), 
                          startCol = 3 + colShift, startRow = 4)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$sd), 
                          startCol = 3 + colShift, startRow = 5)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$n), 
                          startCol = 3 + colShift, startRow = 6)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Compare test:`, 
                          startCol = 11 + colShift, startRow = 3)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Year:`, 
                          startCol = 11 + colShift, startRow = 4)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Taken by:`, 
                          startCol = 11 + colShift, startRow = 5)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Compari-bility:`, 
                          startCol = 11 + colShift, startRow = 6)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(currentComparison$getGrowth()), 
                          startCol = 3 + colShift, startRow = 8)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test item`, 
                          startCol = 1 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test score`, 
                          startCol = 3 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = currentComparison$getSignificance(), 
                          startCol = 3 + colShift, startRow = 9)
      
      # If there are both topics and comparisons, write topic comparison data to the comparison tab
      if(HasTopics){
        Topics = currentComparison$getTopicComparisons()
        if(!is.null(Topics)){
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Topics$Average.score, 
                              startCol = 10 + colShift, startRow = 14, colNames = F)
        } # /if there are topic comparisons
      } # /if HasTopics
    } # /for each comparion
  } # /if # of comparison > 0
  
  
  # Write the responseSet to the Breakdown tab and do some formatting ####
  if(messageLevel > 1){message("Write the Breakdown tab")}
  openxlsx::writeData(wb = wb1, sheet = "Breakdown", x = t(ResponseSet), 
                      startCol = 7, startRow = 3, colNames = F)
  openxlsx::removeColWidths(wb = wb1, sheet = "Breakdown", cols = 7:47)
  openxlsx::setColWidths(wb = wb1, sheet = "Breakdown", cols = 7:47, widths = "auto")
  
  
  # Output the scores workbook ####
  # This next line creates a long delay
  if(messageLevel > 1){message("Output the report file")}
  openxlsx::saveWorkbook(wb1, paste0(DataLocation, "\\", filename), overwrite = TRUE)
  
  
} # /function
