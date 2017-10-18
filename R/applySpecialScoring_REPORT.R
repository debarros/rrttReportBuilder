# Special Scoring

applySpecialScoring.REPORT = function(report){
  
  # put badmessage call here
  
  if(report$checkSpecScor()){ # only proceed if there is special scoring
    
    StuScoring = report$checkStudScor()
    if(StuScoring){ # if there is student specific scoring
      StuScoRules = report$getSpecialScoringTable() # get the table of special scoring rules by students
    }
    
    # Should the ItemResponses and ItemResponseScores be updated?  Default to FALSE.
    updateIRandIRS = FALSE  
    
    # Grab the special scoring rules
    SpecialScoring = report$getSpecialScoring()
    
    # Get a data.frame of the item values, one value per column
    ItemInfo = report$getItemInfo()[,c("ItemName","Value")]
    itemValues = as.data.frame(t(ItemInfo$Value))
    colnames(itemValues) = ItemInfo$ItemName
    
    for(res in 1:length(report$getResults())){ # for each section
      
      CurrentResult = report$getResults()[[res]]
      ItRespSco = CurrentResult$getItemResponseScores()
      ItResp = CurrentResult$getItemResponses()
      
      for(stu in 1:nrow(ItRespSco)){ # for each student
        
        curSpecScor = SpecialScoring[[1]] # start with the default special scoring
        if(StuScoring){ # if there is student specific scoring
          # if this student is getting a custom special scoring
          if(ItRespSco$StudentID[stu] %in% StuScoRules$Student.ID){
            curSpecScor = SpecialScoring[[StuScoRules[StuScoRules$Student.ID == ItRespSco$StudentID[stu],2]]]
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
            
            # If this special scoring is Drop or Full Credit
            if(SubsetScores[subst,]$`Subset function` %in% c("Drop", "Full credit")){
              # set the update Item Respsonses flag
              updateIRandIRS = TRUE 
              # change the item responses and item response scores
              ItRespSco[stu,itemNames] = NA
              ItResp[stu,itemNames] = NA
            } # /if drop or full credit
            
            # If this special scoring is Drop by response
            if(SubsetScores[subst,]$`Subset function` == c("Drop by response")){
              # set the update Item Respsonses flag
              updateIRandIRS = TRUE 
              # get the rule
              x = SubsetScores[subst,]
              # get the value to be dropped
              p1 = x[1,grep(pattern = "parameter 1", x = colnames(x), ignore.case = T, value = T)]
              # change the appropriate item responses and item response scores
              for(i in itemNames){
                if(ItRespSco[stu,i] == p1){
                  ItRespSco[stu,i] = NA
                  ItResp[stu,i] = NA
                } # /for
              } # /if
            } # /if drop by reponse
            
          } # /for each subset
          
          # calculate the test score
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = SubsetScores$SubsetScore,
            itemValues = rep.int(1,nrow(SubsetScores)),
            itemWeights = SubsetScores$`Score weight`,
            specialScoring = curSpecScor$getOverallSetup())
          
        } else { # if there are no subsets to use
        
          #get the total test score from the items
          itemNames = colnames(itemValues)
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = ItRespSco[stu,itemNames],
            itemValues = itemValues[,itemNames],
            itemWeights = itemValues[,itemNames], # since this is an overall score, the items are weighted by their values
            specialScoring = curSpecScor$getOverallSetup())
          
          # If this special scoring is Drop by response
          if(curSpecScor$getOverallSetup()$`Score function` == c("Drop by response")){
            # set the update Item Respsonses flag
            updateIRandIRS = TRUE 
            # get the rule
            x = curSpecScor$getOverallSetup()
            # get the value to be dropped
            p1 = x[1,grep(pattern = "parameter 1", x = colnames(x), ignore.case = T, value = T)]
            # change the appropriate item responses and item response scores
            for(i in itemNames){
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
      ItResp$score = ItRespSco$score # fix the scores in the Item Responses table
      CurrentResult$setIRquick(ItResp) # put the item responses back in the result
      
    } # / for each section
    
    # check the Update Item Responses flag and update item responses and item response scores if necessary
    if(updateIRandIRS){
      report$updateIRandIRS()
    }
    
  } # / if HasSpecialScoring
} # / function
