
# Special Scoring
applySpecialScoring.REPORT = function(report){
  
  if(report$.__enclos_env__$private$HasSpecialScoring){ # if there is special scoring
    # Grab the special scoring rules
    SpecialScoring = report$.__enclos_env__$private$SpecialScoring
    
    # Get a data.frame of the item values, one value per column
    ItemInfo = report$getItemInfo()[,c("ItemName","Value")]
    itemValues = as.data.frame(t(ItemInfo$Value))
    colnames(itemValues) = ItemInfo$ItemName
    
    for(res in 1:length(report$.__enclos_env__$private$Results)){ # for each section
      print(paste0(res," a"))
      CurrentResult = report$.__enclos_env__$private$Results[[res]]
      print(paste0(res," b"))
      ItRespSco = CurrentResult$.__enclos_env__$private$ItemResponseScores
      print(paste0(res," c"))
      for(stu in 1:nrow(ItRespSco)){ # for each student
        print(paste0(res," ",stu," a"))
        curSpecScor = SpecialScoring[[1]] # start with the default special scoring
        print(paste0(res," ",stu," b"))
        if(report$.__enclos_env__$private$HasStudentScoring){ # if there is student specific scoring
          print(paste0(res," ",stu," c"))
          # if this student is getting a custom special scoring
          if(ItRespSco$StudentID[stu] %in% report$.__enclos_env__$private$SpecialScoringTable$Student.ID){
            print(paste0(res," ",stu," d"))
            #determine the special scoring to use
            #curSpecScor = match(stuff)
          }
        } # / if HasStudentScoring
        print(paste0(res," ",stu," e"))
        if(curSpecScor$checkSubset()){ # if there are subsets to use
          print(paste0(res," ",stu," f"))
          #get the weights of the items
          SubsetAlign = curSpecScor$getSubsetAlign()
          itemWeights = as.data.frame(t(SubsetAlign$`Subset weight`))
          colnames(itemWeights) = SubsetAlign$Item
          print(paste0(res," ",stu," g"))
          # set up a data.frame to hold the subset scores
          SubsetScores = curSpecScor$getSubsetSetup()
          SubsetScores$SubsetScore = NA_real_
          print(paste0(res," ",stu," h"))
          # calculate the subset scores
          for(subst in 1:nrow(SubsetScores)){ # for each subset
            print(paste0(res," ",stu," ",subst," a"))
            #get the name of the subset
            subsetName = SubsetScores$Subset[subst]
            print(paste0(res," ",stu," ",subst," b"))
            # get the names of the items to use in this subset
            itemNames = SubsetAlign$Item[SubsetAlign$Subset == subsetName]
            print(paste0(res," ",stu," ",subst," c"))
            # calculate the subset score
            # note: if there is a lookup table, there needs to be another parameter
            SubsetScores$SubsetScore[subst] = curveScore(
              itemScores = ItRespSco[stu,itemNames],
              itemValues = itemValues[,itemNames],
              itemWeights = itemWeights[,itemNames],
              specialScoring = SubsetScores[subst,])
          }
          print(paste0(res," ",stu," i"))
          # calculate the test score
          ItRespSco$score[stu] = 100 * curveScore(
            itemScores = SubsetScores$SubsetScore,
            itemValues = rep.int(1,nrow(SubsetScores)),
            itemWeights = SubsetScores$`Score weight`,
            specialScoring = curSpecScor$getOverallSetup())
          print(paste0(res," ",stu," j"))
        } else {
          #get the total test score from the items
          #store the test score
        } # / if-else checkSubset
      } # / for each student
      print(paste0(res," d"))
      CurrentResult$setIRSquick(ItRespSco) # put the item response scores back in the result
      print(paste0(res," e"))
      ItResp = CurrentResult$getItemResponses() # grab the item responses
      print(paste0(res," f"))
      ItResp$score = ItRespSco$score # fix the scores
      print(paste0(res," g"))
      CurrentResult$setIRquick(ItResp) # put the item responses back in the result
      print(paste0(res," f"))
    } # / for each section
  } # / if HasSpecialScoring
} # / function
