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
  
  if(TYPE == "Identity"){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    return(thisScore)
  }
  
  if(TYPE %in% c("Extra Credit items", "Out of x points")){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    thisScore = thisScore * sum(itemValues) / p1
    return(thisScore)
  }
  
  if(TYPE == "Lookup score"){
    # error if no lookup provided
  }
  
  if(TYPE == "Lookup table"){
    # error if no lookup provided
  }
  
  if(TYPE == "Mutford Scoring"){
    
  }
  
  if(TYPE == "Regents curve"){
    
  }
  
  if(TYPE == "Give back x%"){
    # check this. Alan wrote it...
    # is p1 where the "x%" would be stored? As a fraction or whole number?
    thisScore = (sum(itemPercents * itemWeights) / sum(itemWeights)) + p1
    return(thisScore)
  }
  
  if(TYPE == "Add x points"){
    # check this. Alan wrote it...
    # is p1 where the "x points" would be stored?
    thisScore = (sum(itemPercents * itemWeights) + p1) / sum(itemWeights)
    return(thisScore)
  }
  
  if(TYPE == "Drop by response"){
    
  }
  
  if(TYPE == "X power"){
    
  }
  
  if(TYPE == "X root"){
    
  }
} # /function
