#' @title Curve a Score
#' @description Apply special scoring rule to a set of item scores to get an overall score
#' @param itemScores a vector or a 1 row data frame containing whole number scores for a set of items
#' @param itemVals a vector or a 1 row data frame containing whole number max scores for a set of items
#' @param itemWts a vector or a 1 row data frame containing whole number weights for a set of items
#' @param specScor a 1 row data frame containing a scoring rule
#' @param lookup a named list of data.frames, each with a score lookup table in it
#' @param subsetnames a vector of names of the subsets in the same order as the item parameters
#' @return numeric of length 1 representing the curved score (generally on a scale of 0 to 1)
#' @examples
#' # Sample Data 1
#' itemScores = c(1,2,1,1,2)
#' itemVals = c(2,2,2,2,3)
#' itemWts = c(2,2,2,2,3)
#' specScor = data.frame(t(c("Extra Credit items", "9",NA,NA,NA,NA)), stringsAsFactors = F)
#' names(specScor) = c("function",paste0("parameter ",1:5))
#' 
#' # Test Run 1
#' curveScore(itemScores, itemVals, itemWts, specScor)
#' 
#' # Sample Data 2
#' specScor2 = data.frame(t(c("Polynomial", ".0005", "2.15", "-2.09", ".92", NA)), stringsAsFactors = F)
#' names(specScor2) = c("function",paste0("parameter ",1:5))
#' 
#' # Test Run 2
#' curveScore(itemScores, itemVals, itemWts, specScor2)
curveScore = function(itemScores, itemVals, itemWts, specScor, lookup = NULL, subsetnames = NULL){
  
  # TYPE is a character of length 1 indicating the function to use on the score
  TYPE  = specScor[1,grep(pattern = "function", x = colnames(specScor), ignore.case = T, value = T)]
  
  # p1 through p5 are the parameters for the function
  p1 = specScor[1,grep(pattern = "parameter 1", x = colnames(specScor), ignore.case = T, value = T)]
  p2 = specScor[1,grep(pattern = "parameter 2", x = colnames(specScor), ignore.case = T, value = T)]
  p3 = specScor[1,grep(pattern = "parameter 3", x = colnames(specScor), ignore.case = T, value = T)]
  p4 = specScor[1,grep(pattern = "parameter 4", x = colnames(specScor), ignore.case = T, value = T)]
  p5 = specScor[1,grep(pattern = "parameter 5", x = colnames(specScor), ignore.case = T, value = T)]
  itemPercents = itemScores / itemVals
  
  
  # Identity
  if(TYPE == "Identity"){
    thisScore = sum(itemPercents * itemWts) / sum(itemWts)
    return(thisScore)
  }
  
  
  # Drop or Full credit
  if(TYPE  %in% c("Drop", "Full credit")){
    thisScore = sum(itemPercents * itemWts) / sum(itemWts)
    return(thisScore)
  }
  
  
  # Extra credit / out of x
  if(TYPE %in% c("Extra Credit items", "Out of x points")){
    thisScore = sum(itemPercents * itemWts) / sum(itemWts)
    thisScore = thisScore * sum(itemVals) / as.numeric(p1)
    return(thisScore)
  }
  
  
  # Lookup score / Regents
  if(TYPE %in% c("Lookup score", "Regents curve")){
    if(is.na(p1)){
      stop(paste0("Lookup score and Regents special scoring require the 1st parameter to be the name of the tab with the table."))
    }
    currentLookup = lookup[[p1]] # Get the lookup table associated with this particular scoring rule
    if(is.null(currentLookup)){
      stop(paste0("Special scoring requires a lookup table on a tab named ", p1, "."))
    }
    thisScore = currentLookup[currentLookup[,1] == sum(itemScores),2]
    if(is.na(thisScore)){
      stop(paste0("The lookup table on the tab named ", p1, " is missing a row for the raw score ", sum(itemScores), "."))
    }
    return(thisScore)
  } # /Lookup Score / Regents
  
  
  # Lookup table (2 dimensional)
  if(TYPE == "Lookup table"){
    if(is.na(p1)){
      stop(paste0("Lookup table special scoring requires the 1st parameter to be the name of the tab with the table."))
    }
    if(any(is.na(c(p2, p3)))){
      stop(paste0("Lookup table special scoring requires the 2nd and 3rd parameters to be the names of subsets."))
    }
    if(!(p2 %in% subsetnames)){
      stop(paste0("Special scoring requires a subset named ", p2, "."))
    }
    if(!(p3 %in% subsetnames)){
      stop(paste0("Special scoring requires a subset named ", p3, "."))
    }
    if(!(p1 %in% names(lookup))){
      stop(paste0("Special scoring requires a lookup table on a tab named ", p1, "."))
    }
    currentLookup = lookup[[p1]] # Get the lookup table associated with this particular scoring rule
    if(is.null(currentLookup)){
      stop(paste0("Special scoring requires a lookup table on a tab named ", p1, "."))
    }
    names(itemScores) = subsetnames
    if(!(as.character(itemScores[p3]) %in% colnames(currentLookup))){
      stop(paste0("The special scoring lookup table on the ", p1, " tab requires the value ", itemScores[p3], 
                  " in the first row.  This represents a possible score for the ", p3, " subset."))
    }
    if(!(itemScores[p2] %in% currentLookup[,1])){
      stop(paste0("The special scoring lookup table on the ", p1, " tab requires the value ", itemScores[p2], 
                  " in the first column.  This represents a possible score for the ", p2, " subset."))
    }
    thisScore = currentLookup[currentLookup[,1] == itemScores[p2],as.character(itemScores[p3])]
    return(thisScore)
  } # /Lookup table
  
  
  # Mutford scoring
  # NOT IMPLEMENTED
  if(TYPE == "Mutford Scoring"){
  }
  
  
  # Give back x%
  if(TYPE == "Give back x%"){
    rawScore = sum(itemPercents * itemWts) / sum(itemWts)
    missed  = 1 - rawScore
    thisScore = rawScore + (as.numeric(p1) * missed)
    return(thisScore)
  }
  
  
  # Add x points
  if(TYPE == "Add x points"){
    thisScore = sum(itemPercents * itemWts) / sum(itemWts)
    thisScore = thisScore + (as.numeric(p1) / sum(itemVals))
    return(thisScore)
  }
  
  
  # Out of x%
  if(TYPE == "Out of x%"){
    thisScore = sum(itemPercents * itemWts) / sum(itemWts)
    thisScore = thisScore / as.numeric(p1)
    return(thisScore)
  }
  
  
  # Drop by response
  if(TYPE == "Drop by response"){
    useItems = itemScores != as.numeric(p1) # determine which items to use
    thisScore = sum(itemPercents[useItems] * itemWts[useItems]) / sum(itemWts[useItems])
    return(thisScore)
  }
  
  
  # x power
  if(TYPE == "X power"){
    rawScore = sum(itemPercents * itemWts) / sum(itemWts)
    thisScore = rawScore ^ as.numeric(p1)
    return(thisScore)
  }
  
  
  # x root 
  if(TYPE == "X root"){
    rawScore = sum(itemPercents * itemWts) / sum(itemWts)
    thisScore = rawScore ^ (1 / as.numeric(p1))
    return(thisScore)
  }
  
  
  # decrement
  if(TYPE == "Decrement"){
    rawScore = sum(itemScores)
    rawLost = sum(itemVals) - rawScore
    pointsLost = as.numeric(p1) * rawLost
    thisScore = (100 - pointsLost) / 100
    return(thisScore)
  }
  
  
  # polynomial
  if(TYPE == "Polynomial"){
    coeffs = as.numeric(c(p2, p3, p4, p5))                        # get the coefficients
    degree = sum(!is.na(coeffs))                                  # determine the highest degree term
    rawScore = sum(itemPercents * itemWts) / sum(itemWts) # get the raw score as a percentage
    thisScore = as.numeric(p1)                                    # initialize the score as the constant coefficient
    for(i in 1:degree){                                           # add the other terms
      thisScore = thisScore + (rawScore^i)*coeffs[i]
    }
    return(thisScore)                      # return the score
  } # /polynomial

  
  # Sum and Round
  if(TYPE == "Sum and Round"){
    rawScore = sum(itemScores * itemWts)
    p1 = tolower(p1)
    p2 = tolower(p2)
    if(p1 == "up"){
      thisScore = ceiling(rawScore)  
    } else if(p1 == "down"){
      thisScore = floor(rawScore)
    } else if(p1 == "farmer"){
      thisScore = round(rawScore)
      if(thisScore + 0.5 == rawScore){
        thisScore = thisScore + 1
      }
    } else if(p1 == "scientist"){
      thisScore = round(rawScore)
    } else {
      stop(paste0("In Sum and Round special scoring, ", p1, " is not an implemented type of rounding.",  
                  "The options are up, down, farmer, and scientist."))
    }
    if(p2 == "percent"){
      thisScore = thisScore / (sum(itemVals * itemWts))
    }
    return(thisScore)
  } # /Sum and Round
  
  
    
} # /function
