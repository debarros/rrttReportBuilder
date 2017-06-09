
# Special Scoring
applySpecialScoring.REPORT = function(report){
  
  if(report$.__enclos_env__$private$HasSpecialScoring){ # if there is special scoring
    
    updateIRandIRS = FALSE
    
    # Grab the special scoring rules
    SpecialScoring = report$.__enclos_env__$private$SpecialScoring
    
    # Get a data.frame of the item values, one value per column
    ItemInfo = report$getItemInfo()[,c("ItemName","Value")]
    itemValues = as.data.frame(t(ItemInfo$Value))
    colnames(itemValues) = ItemInfo$ItemName
    
    for(res in 1:length(report$.__enclos_env__$private$Results)){ # for each section
      
      CurrentResult = report$.__enclos_env__$private$Results[[res]]
      ItRespSco = CurrentResult$.__enclos_env__$private$ItemResponseScores
      
      for(stu in 1:nrow(ItRespSco)){ # for each student
      
        curSpecScor = SpecialScoring[[1]] # start with the default special scoring
        if(report$.__enclos_env__$private$HasStudentScoring){ # if there is student specific scoring
          # if this student is getting a custom special scoring
          if(ItRespSco$StudentID[stu] %in% report$.__enclos_env__$private$SpecialScoringTable$Student.ID){
            #determine the special scoring to use
            #curSpecScor = match(stuff)
          }
        } # / if HasStudentScoring
      
        if(curSpecScor$checkSubset()){ # if there are subsets to use
      
          #get the weights of the items
          SubsetAlign = curSpecScor$getSubsetAlign()
          itemWeights = as.data.frame(t(SubsetAlign$`Subset weight`))
          colnames(itemWeights) = SubsetAlign$Item
      
          # set up a data.frame to hold the subset scores
          SubsetScores = curSpecScor$getSubsetSetup()
          SubsetScores$SubsetScore = NA_real_
      
          # calculate the subset scores
          for(subst in 1:nrow(SubsetScores)){ # for each subset
      
            #get the name of the subset
            subsetName = SubsetScores$Subset[subst]
      
            # get the names of the items to use in this subset
            itemNames = SubsetAlign$Item[SubsetAlign$Subset == subsetName]
      
            # calculate the subset score
            # note: if there is a lookup table, there needs to be another parameter
            SubsetScores$SubsetScore[subst] = curveScore(
              itemScores = ItRespSco[stu,itemNames],
              itemValues = itemValues[,itemNames],
              itemWeights = itemWeights[,itemNames],
              specialScoring = SubsetScores[subst,])
            
            # If this special scoring is Drop or Full Credit, 
            #     1) change the item responses and item response scores
            #     2) set the updateItemResp flag
          }
      
          # calculate the test score
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = SubsetScores$SubsetScore,
            itemValues = rep.int(1,nrow(SubsetScores)),
            itemWeights = SubsetScores$`Score weight`,
            specialScoring = curSpecScor$getOverallSetup())
      
        } else {
          #get the total test score from the items
          #store the test score
        } # / if-else checkSubset
      } # / for each student
      
      CurrentResult$setIRSquick(ItRespSco) # put the item response scores back in the result
      
      ItResp = CurrentResult$getItemResponses() # grab the item responses
      
      ItResp$score = ItRespSco$score # fix the scores
      
      CurrentResult$setIRquick(ItResp) # put the item responses back in the result
      
    } # / for each section
    # check the updateItemResp flag and update item responses and item response scores if necessary
  } # / if HasSpecialScoring
} # / function
