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
  CompHeader = d2[1:8,-1]
  row.names(CompHeader) = CompHeader[,1]
  CompHeader = CompHeader[,2*(1:(ncol(CompHeader)/2))]
  CompHeader = CompHeader[1:nrow(CompHeader), 
                          apply(X = !is.na(CompHeader), MARGIN = 2, FUN = any), 
                          drop = FALSE]
  
  if(messageLevel > 1){message("if there are any comparisons")}
  
  if(ncol(CompHeader) > 0){                                         # If there are any comparisons
    Comparisons = vector(mode = "list", length = ncol(CompHeader))  # Create a list to hold them
    d3 = openxlsx::read.xlsx(xlsxFile = ComparisonLocation,         # Grab the topic comparison info
                             sheet = "Topic Comparison", 
                             startRow = 4, colNames = T)
    
    if(messageLevel > 2){message("loop through comparisons")}
    for(i in 1:ncol(CompHeader)){ # For each comparison,
      if(messageLevel > 3){message(paste0("Comparison ", i))}
      Comparisons[[i]] = COMPARISON$new()                                         # Set up the comparison object
      Comparisons[[i]]$setDescription(                                            # Last year, 2 yrs ago, etc
        DescriptionLookup$Description[DescriptionLookup$Year == CompHeader[5,i]])  
      Comparisons[[i]]$setSummary(CompHeader[,i], row.names(CompHeader))          # Set the comparison summary
      
      # Item Comparisons
      ItemComparisons = magrittr::set_colnames(                                   # Set up the item comparisons                        
        d2[-c(1:9), c(1,(2*i), (1+2*i))],
        c("This test item", "Prior test item", "Prior test score"))
      
      # Check whether the entries in the Prior test score column can be converted to numeric
      PriorTestScores.original = ItemComparisons$`Prior test score`
      PriorTestScores.numeric = suppressWarnings(as.numeric(PriorTestScores.original))
      if(any(!is.na(PriorTestScores.original[is.na(PriorTestScores.numeric)]))){             # If any values get converted to NA's, stop.
        stop(paste0("Error!  Comparison ", i ," has prior test item scores that are not numbers."))
      }
      
      ItemComparisons$`Prior test score` = PriorTestScores.numeric                # Convert the prior test item scores to numeric
      ItemComparisons$Higher = ItemScores > ItemComparisons[,3] + CompCuts$Item.H # Determine which items are noticeably higher this time
      ItemComparisons$Higher[is.na(ItemComparisons$Higher)] = F                   # If there is no comparison for that item, mark as FALSE
      ItemComparisons$Lower = ItemScores < ItemComparisons[,3] - CompCuts$Item.L  # Determine which items are noticeably lower this time
      ItemComparisons$Lower[is.na(ItemComparisons$Lower)] = F                     # If there is no comparison for that item, mark as FALSE
      Comparisons[[i]]$setItemComparisons(ItemComparisons)                        # Load the item comparisons into the comparison object
      
      
      # Topic Comparisons
      if(HasTopics){                                                                   # If there are topics
        if(messageLevel > 3){message(paste0("looking at topics for comparison ", i))}
        if(nrow(d3) != 0){                                                             # If there is a topic comparison
          TopComp = d3[,c(1,i+1)]                                                      # Get the topic comparison info
          TopComp$Higher = TopicSummary$`All Classes` > TopComp[,2] + CompCuts$Topic.H # Identify noticeably higher topics
          TopComp$Lower = TopicSummary$`All Classes` < TopComp[,2] - CompCuts$Topic.L  # Identify noticeably lower topics
          Comparisons[[i]]$setTopicComparisons(TopComp)                                # Load topic comparison into comparison object
        } # /if there is topic comparison data
      } # /if there are topics
      
      
      # Overall Comparison
      if(!is.na(CompHeader[1,i]) & Summary$N > 1){                                 # If there's an overall comparison & enough students to make one
        if(messageLevel > 3){message(paste0("looking at overall comparison ", i))}
        tTestSummary = t.test2(                                                    # Do a t test to see if the mean difference is significant
          m1 = Summary$Average, m2 = as.numeric(CompHeader[1,i]), 
          s1 = Summary$SD,      s2 = as.numeric(CompHeader[2,i]), 
          n1 = Summary$N,       n2 = as.numeric(CompHeader[3,i]),
          messageLevel = messageLevel - 1) # /t.test2
        Comparisons[[i]]$setGrowth(tTestSummary$`Difference of means`)  # Load the growth score into the comparison object
        Comparisons[[i]]$setTtest(tTestSummary$t)                       # Load the t score into the comparison object 
        Comparisons[[i]]$setPvalue(tTestSummary$`p-value`)              # Load the p-value into the comparison object
        
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
        Comparisons[[i]]$setSignificance(significance)   # Load the significance description into the comparison object
      } # /if there is an overall comparison
    } # /for each comparison
    
    report$setComparisonQuick(Comparisons) # Load the list of comparisons into the report object
    if(messageLevel > 0){message("done with setComparison_REPORT")}
  } # /if there are comparisons
} # /function
