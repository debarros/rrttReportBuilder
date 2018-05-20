# Special Scoring

applySpecialScoring.REPORT = function(report, messageLevel = 0){
  
  # put badmessage call here
  
  if(messageLevel > 0){
    message("Running applySpecialScoring.REPORT")
  }
  
  # pull necessary info from the report
  HasSpecScor =    report$checkSpecScor()
  HasStuScor =     report$checkStudScor()
  StuScoRules =    report$getSpecialScoringTable()
  SpecialScoring = report$getSpecialScoring()
  ItemInfo =       report$getItemInfo()[,c("ItemName","Value")]
  Results =        report$getResults()
  nResults =       length(Results)
  itemValues =     as.data.frame(t(ItemInfo$Value))
  colnames(itemValues) = ItemInfo$ItemName
  
  if(HasSpecScor){          # only proceed if there is special scoring
    if(messageLevel > 1){message("Applying Special Scoring")}
    
    updateIRandIRS = FALSE  # Should the ItemResponses and ItemResponseScores be updated?  Default to FALSE.
    
    for(res in 1:nResults){ # for each section
      if(messageLevel > 1){message(paste0("Applying Special Scoring to result ", res))}
      CurrentResult = Results[[res]]
      ItRespSco = CurrentResult$getItemResponseScores()
      ItResp = CurrentResult$getItemResponses()
      
      for(stu in 1:nrow(ItRespSco)){      # for each student
        if(messageLevel > 2){message(paste0("Applying Special Scoring to student ", stu))}
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
            itemScores = ItRespSco[stu,itemNames]
            itemVals = itemValues[,itemNames]
            itemWts = itemWeights[,itemNames]
            specScor = SubsetScores[subst,]
            lookup = curSpecScor$getLookups()
            SubsetScores$SubsetScore[subst] = curveScore(itemScores, itemVals, itemWts, specScor, lookup, messageLevel = messageLevel - 2)
            
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
          itemScores = SubsetScores$SubsetScore
          itemVals = rep.int(1,nrow(SubsetScores))
          itemWts = SubsetScores$`Score weight`
          specScor = curSpecScor$getOverallSetup()
          lookup = curSpecScor$getLookups()
          subsetnames = SubsetScores$Subset
          ItRespSco$score[stu] = 100 * curveScore(itemScores, itemVals, itemWts, specScor, lookup, subsetnames, messageLevel = messageLevel - 2)
          
        } else { # If there are no subsets to use, 
          
          #get the total test score from the items
          curFunction = curSpecScor$getOverallSetup()$`Score function`
          itemNames = colnames(itemValues)
          itemScores = ItRespSco[stu,itemNames]
          itemVals = itemValues[,itemNames]
          itemWts = itemValues[,itemNames]                    # since this is an overall score, items are weighted by their values
          specScor = curSpecScor$getOverallSetup()
          lookup = curSpecScor$getLookups()
          ItRespSco$score[stu] = 100 * curveScore(itemScores, itemVals, itemWts, specScor, lookup, messageLevel = messageLevel - 2)
          
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
        # recalculate the total points and put it the Item Responses and Item Response Scores
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
