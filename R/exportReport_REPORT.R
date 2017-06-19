# exportReport_REPORT

exportReport.REPORT = function(report, filename) {
  # put badmessage call here
  
  # check to see if the report has been run before
  uploadFilePath = paste0(report$.__enclos_env__$private$DataLocation,"\\",report$getUpLoadFiles()[1])
  if(file.exists(uploadFilePath)){
    report$exportUpdate(uploadFilePath)
  }
  
  #This next line creates a slight delay
  wb1 = loadWorkbook2(file = system.file("extdata", 
                                         "template", 
                                         package = "rrttReportBuilder"), 
                      isUnzipped = T)
  
  # For each class section, write the student responses to the Responses tab
  for(i in 1:length(report$.__enclos_env__$private$Results)){
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = names(report$.__enclos_env__$private$Results)[i], 
                        startCol = 1, 
                        startRow = 100*(i-1) + 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "Responses", 
                        x = report$.__enclos_env__$private$Results[[i]]$getItemResponses(), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 2)
  } # /for
  
  # For each class section, write the student item response scores to the ItemScores tab
  for(i in 1:length(report$.__enclos_env__$private$Results)){
    openxlsx::writeData(wb = wb1, 
                        sheet = "ItemScores", 
                        x = names(report$.__enclos_env__$private$Results)[i], 
                        startCol = 1, 
                        startRow = 100*(i-1) + 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "ItemScores", 
                        x = report$.__enclos_env__$private$Results[[i]]$getItemResponseScores(), 
                        startCol = 1, 
                        startRow = 100*(i-1) + 2)
  } # /for
  
  openxlsx::writeData(wb = wb1, 
                      sheet = "ItemInfo", 
                      x = report$.__enclos_env__$private$ItemInfo)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = data.frame(report$.__enclos_env__$private$Summary), 
                      startRow = 1)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Summary", 
                      x = names(report$.__enclos_env__$private$Results), 
                      startRow = 3)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Upload", 
                      x = report$.__enclos_env__$private$UploadTab, 
                      startRow = 1)
  
  # If there are topics, write data to the relevant tabs
  if(report$.__enclos_env__$private$HasTopics){
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicAlignments", 
                        x = report$getTopicAlignments(), 
                        startRow = 1)
    openxlsx::writeData(wb = wb1, 
                        sheet = "TopicSummary", 
                        x = report$.__enclos_env__$private$TopicSummary, 
                        startRow = 1, 
                        rowNames = T)
    
    # Write the student topic scores one section at a time
    for(i in 1:length(report$.__enclos_env__$private$Results)){
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = names(report$.__enclos_env__$private$Results)[i], 
                          startCol = 1, 
                          startRow = 100*(i-1) + 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicScores", 
                          x = report$.__enclos_env__$private$Results[[i]]$getTopicScores(), 
                          startCol = 1, 
                          startRow = 100*(i-1) + 2)
    } # /for
  } # /if
  ItemSummary = data.frame(report$.__enclos_env__$private$Narrative)
  ItemSummary = ItemSummary[4:(nrow(ItemSummary)-2),,drop=F]
  ItemSummary = ItemSummary[!(ItemSummary[,1] %in% c("* Boxplots", "* Topics")),,drop=F]
  openxlsx::writeData(wb = wb1, 
                      sheet = "Item_Summary", 
                      x = ItemSummary, 
                      startRow = 3, 
                      colNames = F)
  openxlsx::writeData(wb = wb1, 
                      sheet = "Raw Handout Data", 
                      x = report$.__enclos_env__$private$Handouts, 
                      startRow = 1)
  
  
  # If there are comparisons, write data to the comparison tab
  numberOfComparisons = length(report$.__enclos_env__$private$Comparison)
  if(numberOfComparisons > 0){
    
    for(i in numberOfComparisons:1){
      colShift = 14*(numberOfComparisons - i)
      CompSummary = report$.__enclos_env__$private$Comparison[[i]]$getSummary()
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
                          x = as.numeric(report$.__enclos_env__$private$Comparison[[i]]$getGrowth()), 
                          startCol = 3 + colShift, startRow = 8)
      Items = report$.__enclos_env__$private$Comparison[[i]]$getItemComparisons()
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test item`, 
                          startCol = 1 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test score`, 
                          startCol = 3 + colShift, startRow = 14, colNames = F)
      openxlsx::writeData(wb = wb1, sheet = "Comparison", x = report$.__enclos_env__$private$Comparison[[i]]$getSignificance(), 
                          startCol = 3 + colShift, startRow = 9)
      
      # If there are both topics and comparisons, write topic comparison data to the comparison tab
      if(report$.__enclos_env__$private$HasTopics){
        Topics = report$.__enclos_env__$private$Comparison[[i]]$getTopicComparisons()
        if(!is.null(Topics)){
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Topics$Average.score, 
                              startCol = 10 + colShift, startRow = 14, colNames = F)
        } # /if there are topic comparisons
      } # /if HasTopics
    } #/for 
  } # /if # of comparison > 0
  
  # This next line creates a long delay
  openxlsx::saveWorkbook(wb1, paste0(report$.__enclos_env__$private$DataLocation,"\\",filename), overwrite = TRUE)
} # /function