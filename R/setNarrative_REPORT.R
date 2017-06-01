# setNarrative_REPORT

setNarrative.REPORT = function(report) {
	narrative = paste0("Here are your scores and analysis for **", report$.__enclos_env__$private$TestName,"**.  ")
      narrative = c(narrative,"", "* The score distribution ")
      # If there are check key items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$CheckKey) > 0){
        x = "* **Check the answer key for the following question"
        if(sum(report$.__enclos_env__$private$ItemSummary$CheckKey) > 1){
          x = paste0(x,"s")
        } 
        x = paste0(x,": ", VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName,report$.__enclos_env__$private$ItemSummary$CheckKey), "**")
        narrative = c(narrative, x)
      }
      # If there are powerful distractors, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$PowerDistrators) > 0){
        x = "* The following question"
        if(sum(report$.__enclos_env__$private$ItemSummary$PowerDistrators) > 1){
          x = paste0(x, "s")
        }
        x = paste0(x, 
                   " had powerful distractors: ", 
                   VectorSentence(as.character(report$.__enclos_env__$private$ItemSummary$ItemName), report$.__enclos_env__$private$ItemSummary$PowerDistrators), 
                   ".  Looking at those wrong answers might help you understand where students are making mistakes.")
        narrative = c(narrative, x)
      }
      # If there are overthinking items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$OverThinking) > 0){
        x = "* The following question"
        if(sum(report$.__enclos_env__$private$ItemSummary$OverThinking) > 1){
          x = paste0(x, 
                     "s were missed just as often by your high scoring students as your low scoring students.  ",
                     "This indicates that they are potential overthinking questions: ")
        } else {
          x = paste0(x, 
                     " was missed just as often by your high scoring students as your low scoring students.  ",
                     "This indicates that it is a potential overthinking question: ")
        }
        x = paste0(x, VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName, report$.__enclos_env__$private$ItemSummary$OverThinking), ".")
        narrative = c(narrative, x)
      }
      # If there are difficult items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$Difficult) > 0){
        x = "* Your students found the following question"
        if(sum(report$.__enclos_env__$private$ItemSummary$Difficult) > 1){
          x = paste0(x, "s")
        } 
        x = paste0(x, 
                   " very difficult: ",
                   VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName, report$.__enclos_env__$private$ItemSummary$Difficult), 
                   ".")
        narrative = c(narrative, x)
      }
      # If there are easy items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$Easy) > 0){
        x = "* Your students found the following question"
        if(sum(report$.__enclos_env__$private$ItemSummary$Easy) > 1){
          x = paste0(x, "s")
        } 
        x = paste0(x, " very easy: ",VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName, report$.__enclos_env__$private$ItemSummary$Easy), ".")
        narrative = c(narrative, x)
      }
      # If there are wheat from chaff items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$WheatFromChaff) > 0){
        a = "  Those are very difficult, but the best students get them right."
        x = paste0("* ",VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName, report$.__enclos_env__$private$ItemSummary$WheatFromChaff))
        if(sum(report$.__enclos_env__$private$ItemSummary$WheatFromChaff) > 1){
          x = paste0(x, " might be wheat from chaff questions.", a)
        } else {
          x = paste0(x, " might be a wheat from chaff question.", a)
        }
        narrative = c(narrative, x)
      }
      # If there are highly related items, add the line
      if(sum(report$.__enclos_env__$private$ItemSummary$HighlyRelated) > 0){
        x = "* The highly related item"
        if(sum(report$.__enclos_env__$private$ItemSummary$HighlyRelated) > 1){
          x = paste0(x, "s were ")
        } else {
          x = paste0(x, " was ")
        }
        x = paste0(x, 
                   VectorSentence(report$.__enclos_env__$private$ItemSummary$ItemName, report$.__enclos_env__$private$ItemSummary$HighlyRelated), 
                   ".  Those are questions to keep, since they are good indicators of student knowledge.")
        narrative = c(narrative, x)
      }
      # Add lines for boxplots, if necessary
      if(length(report$.__enclos_env__$private$Results) > 1){
        narrative = c(narrative, "* Boxplots") 
      }
      # Add line for topics, if necessary
      if(report$.__enclos_env__$private$HasTopics){
        narrative = c(narrative, "* Topics")
      }
      # Add sections for the comparisons, if they exist
      if(length(report$.__enclos_env__$private$Comparison) > 0){
        for(i in length(report$.__enclos_env__$private$Comparison):1){
          ItemComparisons = report$.__enclos_env__$private$Comparison[[i]]$getItemComparisons()
          if(report$.__enclos_env__$private$HasTopics){
            TopicComparisons = report$.__enclos_env__$private$Comparison[[i]]$getTopicComparisons()  
          }
          desc = report$.__enclos_env__$private$Comparison[[i]]$getDescription()
          growth = report$.__enclos_env__$private$Comparison[[i]]$getGrowth()
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
          x = paste0("* Compared to ", 
                     desc, 
                     " your students scored about ", 
                     growth, 
                     growthDirection, 
                     " on average.  This is ", 
                     report$.__enclos_env__$private$Comparison[[i]]$getSignificance())
          narrative = c(narrative, x)
          
          if(sum(ItemComparisons$Higher)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably better on question")
            if(sum(ItemComparisons$Higher)>1){
              x = paste0(x, "s")
            }
            x = paste0(x, 
                       " ",
                       VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Higher))
            narrative = c(narrative, x)
          }
          
          if(sum(ItemComparisons$Lower)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably worse on question")
            if(sum(ItemComparisons$Lower)>1){
              x = paste0(x, "s")
            }
            x = paste0(x, 
                       " ",
                       VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Lower))
            narrative = c(narrative, x)
          }
          
          #If there are topic comparisons, add the lines in
          if(report$.__enclos_env__$private$HasTopics){
            if(!is.null(TopicComparisons)){
              if(sum(TopicComparisons$Higher)>0){
                x = paste0("    * Compared to ", 
                           desc, 
                           ", your students did noticeably better on ", 
                           VectorSentence(TopicComparisons$Topic, TopicComparisons$Higher))
                narrative = c(narrative, x)
              } # /if there are higher topics
              if(sum(TopicComparisons$Lower)>0){
                x = paste0("    * Compared to ", 
                           desc, 
                           ", your students did noticeably worse on ", 
                           VectorSentence(TopicComparisons$Topic, TopicComparisons$Lower))
                narrative = c(narrative, x)
              } # /if there are lower topics
            } # /if TopicComparisons not null 
          } # /if HasTopics
        }
      }
      # Add the closing line
      narrative = c(narrative,"", "Let me know if you need anything else.")
      report$.__enclos_env__$private$Narrative = narrative
}