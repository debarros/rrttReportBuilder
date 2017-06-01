# setComparison_REPORT

setComparison.REPORT = function(report) {
      d2 = openxlsx::read.xlsx(xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
                               sheet = "Overall Comparison", 
                               startRow = 2, 
                               colNames = F)
      CompHeader = d2[1:8,-1]
      row.names(CompHeader) = CompHeader[,1]
      CompHeader = CompHeader[,2*(1:(ncol(CompHeader)/2))]
      CompHeader = CompHeader[1:nrow(CompHeader),apply(X = !is.na(CompHeader), MARGIN = 2, FUN = any), drop = FALSE]
      if(ncol(CompHeader)>0){
        Comparisons = vector(mode = "list", length = ncol(CompHeader))
        
        d3 = openxlsx::read.xlsx(xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
                                 sheet = "Topic Comparison", 
                                 startRow = 4, 
                                 colNames = T)
        i = 1
        for(i in 1:ncol(CompHeader)){
          
          Comparisons[[i]] = COMPARISON$new()
          Comparisons[[i]]$setDescription(DescriptionLookup$Description[DescriptionLookup$Year == CompHeader[5,i]])
          Comparisons[[i]]$setSummary(CompHeader[,i], row.names(CompHeader))
          ItemComparisons = magrittr::set_colnames(
            d2[-c(1:9),c(1,(2*i),(1+2*i))],
            c("This test item", "Prior test item","Prior test score"))
          ItemComparisons$`Prior test score` = as.numeric(ItemComparisons$`Prior test score`)
          ItemComparisons$Higher = report$.__enclos_env__$private$ItemScores > ItemComparisons[,3] + 0.1
          #if there is no comparison for that item, mark as FALSE
          ItemComparisons$Higher[is.na(ItemComparisons$Higher)] = F 
          ItemComparisons$Lower =  report$.__enclos_env__$private$ItemScores < ItemComparisons[,3] - 0.1
          #if there is no comparison for that item, mark as FALSE
          ItemComparisons$Lower[is.na(ItemComparisons$Lower)] = F 
          Comparisons[[i]]$setItemComparisons(ItemComparisons)
          
          # If there are topics and there is a topic comparison:
          if(report$.__enclos_env__$private$HasTopics){
            if(nrow(d3) != 0){
              TopicComparisons = d3[,c(1,i+1)]
              TopicComparisons$Higher = report$.__enclos_env__$private$TopicSummary$`All Classes` > TopicComparisons[,2] + 0.1
              TopicComparisons$Lower = report$.__enclos_env__$private$TopicSummary$`All Classes` < TopicComparisons[,2] - 0.1
              Comparisons[[i]]$setTopicComparisons(TopicComparisons)
            } # /if
          } # /if
          
          # If there is an overall comparison:
          if(!is.na(CompHeader[1,i])){ 
            #use t.test2 here
            tTestSummary = t.test2(
              m1 = report$.__enclos_env__$private$Summary$Average, 
              m2 = as.numeric(CompHeader[1,i]), 
              s1 = report$.__enclos_env__$private$Summary$SD, 
              s2 = as.numeric(CompHeader[2,i]), 
              n1 = report$.__enclos_env__$private$Summary$N, 
              n2 = as.numeric(CompHeader[3,i])
            )
            Comparisons[[i]]$setGrowth(tTestSummary$`Difference of means`)
            Comparisons[[i]]$setTtest(tTestSummary$t)
            Comparisons[[i]]$setPvalue(tTestSummary$`p-value`)
            significance = "not a significant difference, and is probably due to chance."
            if(tTestSummary$`p-value`<0.1){
              significance = "a somewhat significant difference, and could be due to chance."
            }
            if(tTestSummary$`p-value`<0.05){
              significance = "a very significant difference, and is unlikely to be due to chance."
            }
            if(tTestSummary$`p-value`<0.01){
              significance = "an extremely significant difference, and could not be due to chance."
            }
            Comparisons[[i]]$setSignificance(significance)
          }
        }
        report$.__enclos_env__$private$Comparison = Comparisons
      }
}