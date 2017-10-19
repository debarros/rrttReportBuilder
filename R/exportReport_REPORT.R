# exportReport_REPORT

exportReport.REPORT = function(filename, template, report) {
  # put badmessage call here ####
  
  # check to see if the report has been run before ####
  uploadFilePath = paste0(report$getDataLocation(),"\\",report$getUpLoadFiles()[1])
  if(file.exists(uploadFilePath)){
    report$exportUpdate(uploadFilePath)
  }
  
  # Load the template ####
  # This next part creates a slight delay
  if(is.null(template)){
    wb1 = loadWorkbook2(file = system.file("extdata","template", package = "rrttReportBuilder"), isUnzipped = T)  
  } else {
    wb1 = loadWorkbook2(file = template, isUnzipped = F)
  } # /if-else
  
  
  # For each class section, write the student responses to the Responses tab ####
  for(i in 1:length(report$getResults())){
    currentResult = report$getResults()[i]
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = names(currentResult), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = currentResult[[1]]$getItemResponses(), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 2)
  } # /for
  
  # For each class section, write the student item response scores to the ItemScores tab ####
  for(i in 1:length(report$getResults())){
    currentResult = report$getResults()[i]
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
  
  # Write the ItemInfo, Summary, and Upload tabs ####
  openxlsx::writeData(wb = wb1, 
                      sheet = "ItemInfo", 
                      x = report$getItemInfo())
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = data.frame(report$getSummary()), 
                      startRow = 1)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = names(report$getResults()), 
                      startRow = 3)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Upload", 
                      x = report$getUploadTab(), 
                      startRow = 1)
  
  # If there are topics, write data to the relevant tabs ####
  if(report$checkTopics()){
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicAlignments", 
                        x = report$getTopicAlignments(), 
                        startRow = 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicSummary", 
                        x = report$getTopicSummary(), 
                        startRow = 1, 
                        rowNames = T)
    
    # Write the student topic scores one section at a time
    for(i in 1:length(report$getResults())){
      currentResult = report$getResults()[i]
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = names(currentResult), 
                          startCol = 1, 
                          startRow = 100*(i-1) + 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = currentResult[[i]]$getTopicScores(), 
                          startCol = 1, 
                          startRow = 100*(i-1) + 2)
    } # /for each section
  } # /if there are topics
  
  # Write the Item_Summary tab and raw handouts tab ####
  ItemSummary = data.frame(report$getNarrative())
  ItemSummary = ItemSummary[4:(nrow(ItemSummary)-2),,drop=F]
  ItemSummary = ItemSummary[!(ItemSummary[,1] %in% c("* Boxplots", "* Topics")),,drop=F]
  openxlsx::writeData(wb = wb1, 
                      sheet = "Item_Summary", 
                      x = ItemSummary, 
                      startRow = 3, 
                      colNames = F)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Raw Handout Data", 
                      x = report$getHandouts(), 
                      startRow = 1)
  
  
  # If there are comparisons, write data to the comparison tab ####
  numberOfComparisons = length(report$getComparison())
  if(numberOfComparisons > 0){
    
    for(i in numberOfComparisons:1){
      colShift = 14*(numberOfComparisons - i)
      currentComparison = report$getComparison()[[i]]
      CompSummary = currentComparison$getSummary()
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
      openxlsx::writeData(wb = wb1, sheet = "Comparison", 
                          x = as.numeric(currentComparison$getGrowth()), 
                          startCol = 3 + colShift, startRow = 8)
      Items = currentComparison$getItemComparisons()
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test item`, 
                          startCol = 1 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test score`, 
                          startCol = 3 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = currentComparison$getSignificance(), 
                          startCol = 3 + colShift, startRow = 9)
      
      # If there are both topics and comparisons, write topic comparison data to the comparison tab
      if(report$checkTopics()){
        Topics = currentComparison$getTopicComparisons()
        if(!is.null(Topics)){
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Topics$Average.score, 
                              startCol = 10 + colShift, startRow = 14, colNames = F)
        } # /if there are topic comparisons
      } # /if HasTopics
    } # /for each comparion
  } # /if # of comparison > 0
  
  
  # Write the responseSet to the Breakdown tab and do some formatting ####
  openxlsx::writeData(wb = wb1, sheet = "Breakdown", x = t(report$getResponseSet()), 
                      startCol = 7, startRow = 3, colNames = F)
  removeColWidths(wb = wb1, sheet = "Breakdown", cols = 7:47)
  setColWidths(wb = wb1, sheet = "Breakdown", cols = 7:47, widths = "auto")
  
  
  # Output the scores workbook ####
  # This next line creates a long delay
  openxlsx::saveWorkbook(wb1, paste0(report$getDataLocation(),"\\",filename), overwrite = TRUE)
  

} # /function
