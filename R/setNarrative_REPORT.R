# setNarrative_REPORT

setNarrative.REPORT = function(report) {
  
  # grab the item summary
  ItemSummary = report$getItemSummary()
  
  #initialize the narrative
  MissingSections = report$getMissingSections()
  if(is.null(MissingSections)){
    narrative = paste0("Here are your scores and analysis for **", report$getTestName(),"**.  ")  
  } else {
    narrative = paste0("Here are your initial scores and analysis for **", report$getTestName(),"**.  ",
                       "We are still waiting on data for ", VectorSentence(x = MissingSections, hyphenate = 1))  
  } # /if-else
  narrative = c(narrative,"", "* The score distribution ")
  
  # If there are check key items, add the line
  if(sum(ItemSummary$CheckKey, na.rm = T) > 0){
    x = "* **Check the answer key for the following question"
    if(sum(ItemSummary$CheckKey, na.rm = T) > 1){
      x = paste0(x,"s")
    } 
    x = paste0(x,": ", VectorSentence(ItemSummary$ItemName,ItemSummary$CheckKey), "**")
    narrative = c(narrative, x)
  }
  
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
  }
  
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
  }
  
  # If there are difficult items, add the line
  if(sum(ItemSummary$Difficult, na.rm = T) > 0){
    x = "* Your students found the following question"
    if(sum(ItemSummary$Difficult, na.rm = T) > 1){
      x = paste0(x, "s")
    } 
    x = paste0(
      x, " very difficult: ",
      VectorSentence(ItemSummary$ItemName, ItemSummary$Difficult), ".")
    narrative = c(narrative, x)
  }
  
  # If there are easy items, add the line
  if(sum(ItemSummary$Easy, na.rm = T) > 0){
    x = "* Your students found the following question"
    if(sum(ItemSummary$Easy, na.rm = T) > 1){
      x = paste0(x, "s")
    } 
    x = paste0(x, " very easy: ",VectorSentence(ItemSummary$ItemName, ItemSummary$Easy), ".")
    narrative = c(narrative, x)
  }
  
  # If there are wheat from chaff items, add the line
  if(sum(ItemSummary$WheatFromChaff, na.rm = T) > 0){
    a = "  Those are very difficult, but the best students get them right."
    x = paste0("* ",VectorSentence(ItemSummary$ItemName, ItemSummary$WheatFromChaff))
    if(sum(ItemSummary$WheatFromChaff, na.rm = T) > 1){
      x = paste0(x, " might be wheat from chaff questions.", a)
    } else {
      x = paste0(x, " might be a wheat from chaff question.", a)
    }
    narrative = c(narrative, x)
  }
  
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
  }
  
  # Add lines for boxplots, if necessary
  if(length(report$getResults()) > 1){
    narrative = c(narrative, "* Boxplots") 
  }
  
  # Add line for topics, if necessary
  if(report$checkTopics()){
    narrative = c(narrative, "* Topics")
  }
  
  # Add sections for the comparisons, if they exist
  allComps = report$getComparison()
  if(length(allComps) > 0){
    for(i in length(allComps):1){
      thisComp = allComps[[i]]
      ItemComparisons = thisComp$getItemComparisons()
      if(report$checkTopics()){
        TopicComparisons = thisComp$getTopicComparisons()  
      }
      desc = thisComp$getDescription()
      growth = thisComp$getGrowth()
      if(!is.null(growth)){ # if an overall comparison is needed, create it
        if(growth > 0){
          growthDirection = "higher"
        } else {
          growthDirection = "lower"
        }
        growth = abs(round(growth))
        if(growth == 1){
          growthDirection = paste0(" point ", growthDirection)
        } else {
          growthDirection = paste0(" points ", growthDirection)
        }
        x = paste0(
          "* Compared to ", desc, " your students scored about ", 
          growth, growthDirection, " on average.  This is ", 
          thisComp$getSignificance())
      } else {
        # if there is no overal comparson, 
        x = paste0("* Comparison to ", desc, ": ")
      }
      narrative = c(narrative, x)
      
      if(sum(ItemComparisons$Higher, na.rm = T) > 0){
        x = paste0(
          "    * Compared to ", desc, 
          ", your students did noticeably better on question")
        if(sum(ItemComparisons$Higher, na.rm = T) > 1){
          x = paste0(x, "s")
        }
        x = paste0(
          x, " ",
          VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Higher))
        narrative = c(narrative, x)
      }
      
      if(sum(ItemComparisons$Lower) > 0){
        x = paste0(
          "    * Compared to ", desc, 
          ", your students did noticeably worse on question")
        if(sum(ItemComparisons$Lower, na.rm = T) > 1){
          x = paste0(x, "s")
        }
        x = paste0(
          x, " ",
          VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Lower))
        narrative = c(narrative, x)
      }
      
      #If there are topic comparisons, add the lines in
      if(report$checkTopics()){
        if(!is.null(TopicComparisons)){
          if(sum(TopicComparisons$Higher, na.rm = T) > 0){
            x = paste0(
              "    * Compared to ", desc, 
              ", your students did noticeably better on ", 
              VectorSentence(TopicComparisons$Topic, TopicComparisons$Higher))
            narrative = c(narrative, x)
          } # /if there are higher topics
          if(sum(TopicComparisons$Lower, na.rm = T) > 0){
            x = paste0(
              "    * Compared to ", desc, 
              ", your students did noticeably worse on ", 
              VectorSentence(x = TopicComparisons$Topic, y = TopicComparisons$Lower))
            narrative = c(narrative, x)
          } # /if there are lower topics
        } # /if TopicComparisons not null 
      } # /if there are Topics
    } # /for each comparison
  } # /if there are comparisons
  
  # Add the closing line
  narrative = c(narrative,"", "Let me know if you need anything else.")
  report$.__enclos_env__$private$Narrative = narrative
}