# setComparison_REPORT

setComparison.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){message("running setComparison.REPORT")}
  if(messageLevel > 1){message("pulling info from the report")}
  # pull the necessary stuff from the report
  HasTopics =          report$checkTopics()
  ItemScores =         report$getItemScores()
  TopicSummary =       report$getTopicSummary()
  Summary =            report$getSummary()
  ComparisonLocation = report$getComparisonLocation()
  CompCuts =           report$getComparisonCutoffs()
  SigCuts =            report$getSignificanceCutoffs()
  
  if(messageLevel > 1){message("create the comparison header")}
  
  # Create the comparison header
  d2 = openxlsx::read.xlsx(xlsxFile = ComparisonLocation, sheet = "Overall Comparison", startRow = 2, colNames = F)
  CompHeader = d2[1:8,-1]                                  # Get the header, but drop the first column
  row.names(CompHeader) = CompHeader[,1]                   # Use the new first column as the row names
  CompHeader = CompHeader[,2*(1:(ncol(CompHeader)/2))]     # Select only the even numbered columns, since those are the ones with data
  CompHeader = CompHeader[1:nrow(CompHeader),              # Drop columns that have no data at all
                          apply(X = !is.na(CompHeader), 
                                MARGIN = 2, FUN = any), 
                          drop = FALSE]
  
  if(messageLevel > 1){message("if there are any comparisons")}
  
  if(ncol(CompHeader) > 0){                                         # If there are any comparisons
    Comparisons = vector(mode = "list", length = ncol(CompHeader))  # Create a list to hold them
    d3 = openxlsx::read.xlsx(xlsxFile = ComparisonLocation,         # Grab the topic comparison info
                             sheet = "Topic Comparison", 
                             startRow = 4, colNames = T)
    
    if(messageLevel > 2){message("loop through comparisons")}
    
    for(thisComp in 1:ncol(CompHeader)){ # For each comparison,
      if(messageLevel > 3){message(paste0("Comparison ", thisComp))}
      Comparisons[[thisComp]] = COMPARISON$new()                                  # Set up the comparison object
      
      # Set the year (Last year, 2 yrs ago, etc).  Also, make sure it exists.
      thisYear = CompHeader[5,thisComp]
      if(is.na(thisYear)){
        stop(paste0("The year of comparison #",thisComp,
                    " is missing.  Check the test setup file."))
      }
      Comparisons[[thisComp]]$setDescription(
        DescriptionLookup$Description[DescriptionLookup$Year == thisYear])  
      
      Comparisons[[thisComp]]$setSummary(                                         # Set the comparison summary
        CompHeader[,thisComp], row.names(CompHeader)) 
      
      # Item Comparisons
      ItemComparisons = magrittr::set_colnames(                                   # Set up the item comparisons                        
        d2[-c(1:9), c(1,(2*thisComp), (1+2*thisComp))],
        c("This test item", "Prior test item", "Prior test score"))
      
      # Check if entries in Prior test score column can be converted to numeric
      PriorTestScores.original = ItemComparisons$`Prior test score`
      PriorTestScores.numeric = suppressWarnings(
        as.numeric(PriorTestScores.original))
      if(any(!is.na(PriorTestScores.original[is.na(PriorTestScores.numeric)]))){  # If any values get converted to NA's, stop.
        stop(paste0("Error!  Comparison ", thisComp ,
                    " has prior test item scores that are not numbers."))
      }
      
      ItemComparisons$`Prior test score` = PriorTestScores.numeric                # Convert the prior test item scores to numeric
      ItemComparisons$Higher = ItemScores > ItemComparisons[,3] + CompCuts$Item.H # Determine which items are noticeably higher this time
      ItemComparisons$Higher[is.na(ItemComparisons$Higher)] = F                   # If there is no comparison for that item, mark as FALSE
      ItemComparisons$Lower = ItemScores < ItemComparisons[,3] - CompCuts$Item.L  # Determine which items are noticeably lower this time
      ItemComparisons$Lower[is.na(ItemComparisons$Lower)] = F                     # If there is no comparison for that item, mark as FALSE
      Comparisons[[thisComp]]$setItemComparisons(ItemComparisons)                 # Load the item comparisons into the comparison object
      
      
      # Topic Comparisons
      if(HasTopics){                                                                   # If there are topics
        if(messageLevel > 3){
          message(paste0("looking at topics for comparison ", thisComp))
        }
        if(nrow(d3) != 0){                                                             # If there is a topic comparison
          TopComp = d3[,c(1,thisComp+1)]                                               # Get the topic comparison info
          if(nrow(TopComp) != nrow(TopicSummary)){                                     # Check size of table of topic comparisons
            stop(paste0("There's a problem with the number of topic comparison ",
                        "values in comparison ", thisComp))
          }
          TopComp$Higher = TopicSummary$`All Classes` > TopComp[,2] + CompCuts$Topic.H # Identify noticeably higher topics
          TopComp$Lower = TopicSummary$`All Classes` < TopComp[,2] - CompCuts$Topic.L  # Identify noticeably lower topics
          Comparisons[[thisComp]]$setTopicComparisons(TopComp)                         # Load topic comparison into comparison object
        } # /if there is topic comparison data
      } # /if there are topics
      
      
      # Overall Comparison
      if(!is.na(CompHeader[1,thisComp]) & Summary$N > 1){                          # If there's an overall comparison & enough students to make one
        if(messageLevel > 3){
          message(paste0("looking at overall comparison ", thisComp))
        }
        
        if(identical(rownames(CompHeader),CompHeader[,thisComp])){
          stop(paste0("There is an issue with comparison ", thisComp, ".  "))
        }
        
        tTestSummary = t.test2(                                                # Do a t test to see if the mean difference is significant
          m1 = Summary$Average, m2 = as.numeric(CompHeader[1,thisComp]), 
          s1 = Summary$SD,      s2 = as.numeric(CompHeader[2,thisComp]), 
          n1 = Summary$N,       n2 = as.numeric(CompHeader[3,thisComp]),
          messageLevel = messageLevel - 1) # /t.test2
        Comparisons[[thisComp]]$setGrowth(tTestSummary$`Difference of means`)  # Load the growth score into the comparison object
        Comparisons[[thisComp]]$setTtest(tTestSummary$t)                       # Load the t score into the comparison object 
        Comparisons[[thisComp]]$setPvalue(tTestSummary$`p-value`)              # Load the p-value into the comparison object
        
        # Significance
        significance = "not a significant difference, and is probably due to chance."        # Default the significance to the lowest level
        if(tTestSummary$`p-value` < SigCuts$somewhat){                                       # If there was a signficant difference,
          significance = "a somewhat significant difference, and could be due to chance."    # Make a note of it
        }
        if(tTestSummary$`p-value` < SigCuts$very){
          significance = "a very significant difference, and is unlikely to be due to chance."
        }
        if(tTestSummary$`p-value` < SigCuts$extremely){
          significance = "an extremely significant difference, and could not be due to chance."
        }
        Comparisons[[thisComp]]$setSignificance(significance)   # Load the significance description into the comparison object
      } # /if there is an overall comparison
    } # /for each comparison
    
    report$setComparisonQuick(Comparisons) # Load the list of comparisons into the report object
    
    if(messageLevel > 0){message("done with setComparison_REPORT")}
    
  } # /if there are comparisons
  
} # /function
