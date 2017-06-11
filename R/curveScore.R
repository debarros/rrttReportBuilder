#' @title Curve a Score
#' @description Apply special scoring rule to a set of item scores to get an overall score
#' @param itemScores a vector or a 1 row data frame containing whole number scores for a set of items
#' @param itemValues a vector or a 1 row data frame containing whole number max scores for a set of items
#' @param itemWeights a vector or a 1 row data frame containing whole number weights for a set of items
#' @param specialScoring a 1 row data frame containing a scoring rule
#' @return numeric of length 1 representing the curved score (generally on a scale of 0 to 1)
#' @examples
#' # Sample Data 1
#' itemScores = c(1,2,1,1,2)
#' itemValues = c(2,2,2,2,3)
#' itemWeights = c(2,2,2,2,3)
#' specialScoring = data.frame(t(c("Extra Credit items", "9",NA,NA,NA,NA)), stringsAsFactors = F)
#' names(specialScoring) = c("function",paste0("parameter ",1:5))
#' 
#' # Test Run 1
#' curveScore(itemScores, itemValues, itemWeights, specialScoring)
curveScore = function(itemScores, itemValues, itemWeights, specialScoring, lookup = NULL){
  
  # TYPE is a character of length 1 indicating the function to use on the score
  TYPE  = specialScoring[1,grep(pattern = "function", x = colnames(specialScoring), ignore.case = T, value = T)]
  
  # p1 through p5 are the parameters for the function
  p1 = specialScoring[1,grep(pattern = "parameter 1", x = colnames(specialScoring), ignore.case = T, value = T)]
  p2 = specialScoring[1,grep(pattern = "parameter 2", x = colnames(specialScoring), ignore.case = T, value = T)]
  p3 = specialScoring[1,grep(pattern = "parameter 3", x = colnames(specialScoring), ignore.case = T, value = T)]
  p4 = specialScoring[1,grep(pattern = "parameter 4", x = colnames(specialScoring), ignore.case = T, value = T)]
  p5 = specialScoring[1,grep(pattern = "parameter 5", x = colnames(specialScoring), ignore.case = T, value = T)]
  itemPercents = itemScores / itemValues
  
  # Identity
  if(TYPE == "Identity"){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    return(thisScore)
  }
  
  # Drop or Full credit
  if(TYPE  %in% c("Drop", "Full credit")){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    return(thisScore)
  }
  
  
  # Extra credit / out of x
  if(TYPE %in% c("Extra Credit items", "Out of x points")){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    thisScore = thisScore * sum(itemValues) / p1
    return(thisScore)
  }
  
  # Lookup score / Regents
  if(TYPE %in% c("Lookup score", "Regents curve")){
    # error if no lookup provided
    
    thisScore = lookup[lookup[,1] == sum(itsemScores),2]
    return(thisScore)
  }
  
  # Lookup table (2 dimensional)
  if(TYPE == "Lookup table"){
    # error if no lookup provided
  }
  
  # Mutford scoring
  # NOT IMPLEMENTED
  if(TYPE == "Mutford Scoring"){
    
  }
  
  # Give back x%
  if(TYPE == "Give back x%"){
    rawScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    missed  = 1 - rawScore
    thisScore = rawScore + (p1 * missed)
    return(thisScore)
  }
  
  # Add x points
  if(TYPE == "Add x points"){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    thisScore = thisScore + (p1 / sum(itemWeights))
    return(thisScore)
  }
  
  # Drop by response
  # NOT IMPLEMENTED
  if(TYPE == "Drop by response"){
    
  }
  
  # x power
  if(TYPE == "X power"){
    rawScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    thisScore = rawScore ^ p1
    return(thisScore)
  }
 
  # x root 
  if(TYPE == "X root"){
    rawScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    thisScore = rawScore ^ (1 / p1)
    return(thisScore)
  }
} # /function
