#setTestName.R

if(is.null(private$Sources)){
  return("Need sources first.")
} else {
  private$TestName = read.csv(private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
}