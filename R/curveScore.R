#' @title Curve a Score
#' @description Apply special scoring rule to a set of item scores to get an overall score
#' @param itemScores a data frame with 1 row containing scores for a set of items
#' @param itemValues a data frame with 1 row containing max scores for a set of items
#' @param itemWeights a data frame with 1 row containing weights for a set of items
#' @param specialScoring a data frame with 1 row containing a scoring rule
#' @return numeric of length 1 representing the curved score
curveScore = function(itemScores, itemValues, itemWeights, specialScoring, lookup = NULL){
  TYPE  = specialScoring[,grep(pattern = "function", x = colnames(specialScoring), ignore.case = T, value = T)]
  p1 = specialScoring[,grep(pattern = "parameter 1", x = colnames(specialScoring), ignore.case = T, value = T)]
  p2 = specialScoring[,grep(pattern = "parameter 2", x = colnames(specialScoring), ignore.case = T, value = T)]
  p3 = specialScoring[,grep(pattern = "parameter 3", x = colnames(specialScoring), ignore.case = T, value = T)]
  p4 = specialScoring[,grep(pattern = "parameter 4", x = colnames(specialScoring), ignore.case = T, value = T)]
  p5 = specialScoring[,grep(pattern = "parameter 5", x = colnames(specialScoring), ignore.case = T, value = T)]
  itemPercents = itemScores / itemValues
  
  if(TYPE == "Identity"){
    thisScore = sum(itemPercents * itemWeights) / sum(itemWeights)
    return(thisScore)
  }
  
  if(TYPE %in% c("Extra Credit items", "Out of x points")){
    
  }
  
  if(TYPE == "Lookup score"){
    
  }
  
  if(TYPE == "Lookup table"){
    
  }
  
  if(TYPE == "Mutford Scoring"){
    
  }
  
  if(TYPE == "Regents curve"){
    
  }
  
  if(TYPE == "Give back x%"){
    
  }
  
  if(TYPE == "Add x points"){
    
  }
  
  if(TYPE == "Drop by response"){
    
  }
  
  if(TYPE == "X power"){
    
  }
  
  if(TYPE == "X root"){
    
  }
} # /function
