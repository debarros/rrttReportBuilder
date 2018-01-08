# Special Scoring

applySpecialScoring.REPORT = function(report){
  
  # put badmessage call here
  
  # pull necessary info from the report
  HasSpecScor = report$checkSpecScor()
  HasStuScor = report$checkStudScor()
  StuScoRules = report$getSpecialScoringTable()
  SpecialScoring = report$getSpecialScoring()
  ItemInfo = report$getItemInfo()[,c("ItemName","Value")]
  Results = report$getResults()
  nResults = length(Results)
  itemValues = as.data.frame(t(ItemInfo$Value))
  colnames(itemValues) = ItemInfo$ItemName
  
  if(HasSpecScor){          # only proceed if there is special scoring
    updateIRandIRS = FALSE  # Should the ItemResponses and ItemResponseScores be updated?  Default to FALSE.
    
    for(res in 1:nResults){ # for each section
      CurrentResult = Results[[res]]
      ItRespSco = CurrentResult$getItemResponseScores()
      ItResp = CurrentResult$getItemResponses()
      
      for(stu in 1:nrow(ItRespSco)){      # for each student
        curSpecScor = SpecialScoring[[1]] # start with the default special scoring
        
        if(HasStuScor){                   # if there is student specific scoring
          if(ItRespSco$StudentID[stu] %in% StuScoRules$Student.ID){ # if this student is getting a custom special scoring
            curSpecScor = SpecialScoring[[StuScoRules[StuScoRules$Student.ID == ItRespSco$StudentID[stu],2]]]
          } # /if this student has special student scoring
        } # /if HasStudentScoring
        
        if(curSpecScor$checkSubset()){                                # if there are subsets to use
          SubsetAlign = curSpecScor$getSubsetAlign()                  # get the weights of the items
          itemWeights = as.data.frame(t(SubsetAlign$`Subset weight`))
          colnames(itemWeights) = SubsetAlign$Item
          
          SubsetScores = curSpecScor$getSubsetSetup()                 # set up a data.frame to hold the subset scores
          SubsetScores$SubsetScore = NA_real_
          
          # calculate the subset scores
          for(subst in 1:nrow(SubsetScores)){                              # for each subset
            curFunction = SubsetScores[subst,]$`Subset function`           # get the scoring function
            subsetName = SubsetScores$Subset[subst]                        # get the name of the subset
            itemNames = SubsetAlign$Item[SubsetAlign$Subset == subsetName] # get the names of the items to use in this subset
            
            # calculate the current subset score
            SubsetScores$SubsetScore[subst] = curveScore(
              itemScores = ItRespSco[stu,itemNames], itemValues = itemValues[,itemNames],
              itemWeights = itemWeights[,itemNames], specialScoring = SubsetScores[subst,],
              lookup = curSpecScor$getLookups())
            
            if(curFunction %in% c("Drop", "Full credit")){             # If this special scoring is Drop or Full Credit
              updateIRandIRS = TRUE                                    # set the update Item Respsonses flag
              ItRespSco[stu,itemNames] = NA                            # change the item responses and item response scores
              ItResp[stu,itemNames] = NA
            } # /if drop or full credit
            
            if(curFunction == c("Drop by response")){                  # If this special scoring is Drop by response
              updateIRandIRS = TRUE                                    # set the update Item Respsonses flag
              x = SubsetScores[subst,]                                 # get the rule
              p1 = x[1,grep("parameter 1", colnames(x), T, value = T)] # get the value to be dropped
              for(i in itemNames){                                     # fix relevant item responses and item response scores
                if(ItRespSco[stu,i] == p1){
                  ItRespSco[stu,i] = NA
                  ItResp[stu,i] = NA
                } # /if score indicates drop
              } # /for each item
            } # /if drop by reponse
            
          } # /for each subset
          
          # Use the subset score to calculate the test score
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = SubsetScores$SubsetScore,     itemValues = rep.int(1,nrow(SubsetScores)),
            itemWeights = SubsetScores$`Score weight`, specialScoring = curSpecScor$getOverallSetup(),
            lookup = curSpecScor$getLookups())
          
        } else { # If there are no subsets to use, 
          
          #get the total test score from the items
          curFunction = curSpecScor$getOverallSetup()$`Score function`
          itemNames = colnames(itemValues)
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = ItRespSco[stu,itemNames], 
            itemValues = itemValues[,itemNames],
            itemWeights = itemValues[,itemNames],                    # since this is an overall score, items are weighted by their values
            specialScoring = curSpecScor$getOverallSetup(), 
            lookup = curSpecScor$getLookups())
          
          if(curFunction == c("Drop by response")){                  # If this special scoring is Drop by response
            updateIRandIRS = TRUE                                    # set the update Item Respsonses flag
            x = curSpecScor$getOverallSetup()                        # get the rule
            p1 = x[1,grep("parameter 1", colnames(x), T, value = T)] # get the value to be dropped
            for(i in itemNames){                                     # fix relevant item responses and item response scores
              if(ItRespSco[stu,i] == p1){
                ItRespSco[stu,i] = NA
                ItResp[stu,i] = NA
              } # /for
            } # /if
          } # /if drop by reponse
          
        } # / if-else checkSubset
        
        # In case any of the student's item response scores changed (due to dropped or full credit items),
        # Recalculate the total points, and put it the Item Responses and Item Response Scores
        ItRespSco$TotalPoints[stu] = sum(ItRespSco[stu,ItemInfo$ItemName], na.rm = TRUE)
        ItResp$TotalPoints[stu] = ItRespSco$TotalPoints[stu]
        
      } # / for each student
      
      CurrentResult$setIRSquick(ItRespSco) # put the item response scores back in the result
      ItResp$score = ItRespSco$score       # fix the scores in the Item Responses table
      CurrentResult$setIRquick(ItResp)     # put the item responses back in the result
      
    } # / for each section
    
    # check the Update Item Responses flag and update item responses and item response scores if necessary
    if(updateIRandIRS){ report$updateIRandIRS() } 
    
  } # / if HasSpecialScoring
} # / function
