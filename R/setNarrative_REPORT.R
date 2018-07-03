# setNarrative_REPORT

setNarrative.REPORT = function(report, messageLevel = 0) {
  
  if(messageLevel > 0){message("starting setNarrative.REPORT")}
  
  # pull needed info from the report ####
  ItemSummary =     report$getItemSummary()
  MissingSections = report$getMissingSections()
  TestName =        report$getTestName()
  Results =         report$getResults()
  HasTopics =       report$checkTopics()
  allComps =        report$getComparison()
  nResults =        length(Results)
  Summary =         report$getSummary()
  nStudents =       Summary$N
  
  
  # Set up the part of the narrative specifying the number of students and sections ####
  SectAndStuCount = paste0("This report reflects ", nStudents, " student")
  if(nStudents > 1){ SectAndStuCount = paste0(SectAndStuCount, "s") }
  SectAndStuCount = paste0(SectAndStuCount, " in ", nResults, " section")
  if(nResults > 1){ SectAndStuCount = paste0(SectAndStuCount, "s") }
  SectAndStuCount = paste0(SectAndStuCount, ".  ")
  
  
  # Initialize the narrative ####
  if(is.null(MissingSections)){
    narrative = paste0("Here are your scores and analysis for **", TestName, "**.  ", SectAndStuCount)  
  } else {
    narrative = paste0("Here are your initial scores and analysis for **", TestName, "**.  ", SectAndStuCount,
                       "We are still waiting on data for ", VectorSentence(x = MissingSections, hyphenate = 1), ".  ")  
  } # /if-else
  narrative = c(narrative,"", "* The score distribution ")
  
  
  # Check key?  ####
  # If there are check key items, add the line
  if(sum(ItemSummary$CheckKey, na.rm = T) > 0){
    x = "* **Check the answer key for the following question"
    if(sum(ItemSummary$CheckKey, na.rm = T) > 1){
      x = paste0(x,"s")
    } 
    x = paste0(x, ": ", VectorSentence(ItemSummary$ItemName,ItemSummary$CheckKey), "**")
    narrative = c(narrative, x)
  }
  
  
  # Distractors?  ####
  # If there are powerful distractors, add the line
  if(sum(ItemSummary$PowerDistrators, na.rm = T) > 0){
    x = "* The following question"
    if(sum(ItemSummary$PowerDistrators, na.rm = T) > 1){
      x = paste0(x, "s")
    }
    x = paste0(
      x, " had powerful distractors: ", 
      VectorSentence(as.character(ItemSummary$ItemName), ItemSummary$PowerDistrators), 
      ".  Looking at those wrong answers might help you understand where students are making mistakes.")
    narrative = c(narrative, x)
  } # /if there are powerful distractors
  
  
  # Overthinking?  ####
  # If there are overthinking items, add the line
  if(sum(ItemSummary$OverThinking) > 0){
    x = "* The following question"
    if(sum(ItemSummary$OverThinking) > 1){
      x = paste0(
        x, "s were missed just as often by your high scoring students as your low scoring students.  ",
        "This indicates that they are potential overthinking questions: ")
    } else {
      x = paste0(
        x, " was missed just as often by your high scoring students as your low scoring students.  ",
        "This indicates that it is a potential overthinking question: ")
    }
    x = paste0(x, VectorSentence(ItemSummary$ItemName, ItemSummary$OverThinking), ".")
    narrative = c(narrative, x)
  } # /if there are overthinking items
  
  
  # Difficult?  ####
  # If there are difficult items, add the line
  if(sum(ItemSummary$Difficult, na.rm = T) > 0){
    x = "* Your students found the following question"
    if(sum(ItemSummary$Difficult, na.rm = T) > 1){
      x = paste0(x, "s")
    } 
    x = paste0(x, " very difficult: ", VectorSentence(ItemSummary$ItemName, ItemSummary$Difficult), ".")
    narrative = c(narrative, x)
  } # /if there are difficult items
  
  
  # Easy?  ####
  # If there are easy items, add the line
  if(sum(ItemSummary$Easy, na.rm = T) > 0){
    x = "* Your students found the following question"
    if(sum(ItemSummary$Easy, na.rm = T) > 1){
      x = paste0(x, "s")
    } 
    x = paste0(x, " very easy: ", VectorSentence(ItemSummary$ItemName, ItemSummary$Easy), ".")
    narrative = c(narrative, x)
  } # /if there are easy items
  
  
  # Wheat from Chaff?  ####
  # If there are wheat from chaff items, add the line
  if(sum(ItemSummary$WheatFromChaff, na.rm = T) > 0){
    a = "  Those are very difficult, but the best students get them right."
    x = paste0("* ", VectorSentence(ItemSummary$ItemName, ItemSummary$WheatFromChaff))
    if(sum(ItemSummary$WheatFromChaff, na.rm = T) > 1){
      x = paste0(x, " might be wheat from chaff questions.", a)
    } else {
      x = paste0(x, " might be a wheat from chaff question.", a)
    }
    narrative = c(narrative, x)
  } # /if there are wheat from chaff items
  
  
  # Highly related? ####
  # If there are highly related items, add the line
  if(sum(ItemSummary$HighlyRelated, na.rm = T) > 0){
    x = "* The highly related item"
    if(sum(ItemSummary$HighlyRelated, na.rm = T) > 1){
      x = paste0(x, "s were ")
    } else {
      x = paste0(x, " was ")
    }
    x = paste0(
      x, VectorSentence(ItemSummary$ItemName, ItemSummary$HighlyRelated), 
      ".  Those are questions to keep, since they are good indicators of student knowledge.")
    narrative = c(narrative, x)
  } # /if there are highly related items
  
  
  # Boxplots and topics?  ####
  if(nResults > 1){ narrative = c(narrative, "* Boxplots") } # Add line for boxplots, if necessary
  if(HasTopics){ narrative = c(narrative, "* Topics") }      # Add line for topics, if necessary
  
  
  # Comparison? ####
  # Add sections for the comparisons, if they exist
  if(length(allComps) > 0){                                                                           # if there are comparisons
    if(!is.null(MissingSections)){                                                                    # if there are missing sections
      narrative = c(narrative, "* Comparisons will be added when all sections have been scanned. ")   # don't do comparisons
    } else {                                                                                          # if no missing sections, do the comparison
      for(i in length(allComps):1){                                                                   # for each comparison
        
        thisComp = allComps[[i]]                                                                      # get the comparison
        CompNar = character(0)                                                                        # initialize the current comparison narrative
        ItemComparisons = thisComp$getItemComparisons()                                               # get the item comparisons
        if(HasTopics){ TopicComparisons = thisComp$getTopicComparisons() }                            # if there are topics, get the topic comparisons
        desc = thisComp$getDescription()                                                              # get the description
        growth = thisComp$getGrowth()                                                                 # get the growth
        if(!is.null(growth)){                                                                         # if an overall comparison is needed, create it
          if(growth > 0){                                                                             # Set the word descriptor for the growth
            growthDirection = "higher"
          } else {
            growthDirection = "lower"
          }
          growth = abs(round(growth))                                                                 # Round off the growth
          if(growth == 1){                                                                            # If the growth is 1, use no s on points
            growthDirection = paste0(" point ", growthDirection)
          } else {
            growthDirection = paste0(" points ", growthDirection)
          }
          x = paste0(
            "* Compared to ", desc, " your students scored about ", growth, growthDirection, 
            " on average.  This is ", thisComp$getSignificance())
        } else {                                                                                      # if there is no overall comparson, 
          x = paste0("* Comparison to ", desc, ": ")
        }
        CompNar = c(CompNar, x)
        
        if(sum(ItemComparisons$Higher, na.rm = T) > 0){                                               # if there are higher items
          x = paste0("    * Compared to ", desc, ", your students did noticeably better on question")
          if(sum(ItemComparisons$Higher, na.rm = T) > 1){ x = paste0(x, "s") }
          x = paste0(x, " ", VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Higher))
          CompNar = c(CompNar, x)
        }
        
        if(sum(ItemComparisons$Lower) > 0){                                                           # if there are lower items
          x = paste0("    * Compared to ", desc, ", your students did noticeably worse on question")
          if(sum(ItemComparisons$Lower, na.rm = T) > 1){ x = paste0(x, "s") }
          x = paste0(x, " ", VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Lower))
          CompNar = c(CompNar, x)
        }
        
        #If there are topic comparisons, add the lines in
        if(HasTopics){
          if(!is.null(TopicComparisons)){
            if(sum(TopicComparisons$Higher, na.rm = T) > 0){
              x = paste0("    * Compared to ", desc, ", your students did noticeably better on ", 
                         VectorSentence(TopicComparisons$Topic, TopicComparisons$Higher, hyphenate = 0))
              CompNar = c(CompNar, x)
            } # /if there are higher topics
            if(sum(TopicComparisons$Lower, na.rm = T) > 0){
              x = paste0("    * Compared to ", desc, ", your students did noticeably worse on ", 
                         VectorSentence(TopicComparisons$Topic, TopicComparisons$Lower, hyphenate = 0))
              CompNar = c(CompNar, x)
            } # /if there are lower topics
          } # /if TopicComparisons not null 
        } # /if there are Topics
        
        # if there is no overall comparison and no noticeable differences, indicate it
        if(identical(CompNar, paste0("* Comparison to ", desc, ": "))){  
          CompNar = c(CompNar, "    * There were no questions or topics that showed a significant or noticeable change. ")
        }
        
        # Add the current comparison narrative to the overall narrative
        narrative = c(narrative, CompNar)
        
      } # /for each comparison
    } # /if-else missing sections
  } # /if there are comparisons
  
  
  # Closing ####
  narrative = c(narrative,"", "Let me know if you need anything else.")
  
  report$setNarrativeQuick(narrative)
  
  if(messageLevel > 0){message("finishing setNarrative.REPORT")}
  
} # /function
