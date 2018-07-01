#' @title t test 2
#' @description Perform a t test using summary stats rather than source data
#' @param m1 mean of sample 1
#' @param m2 mean of sample 2
#' @param s1 the sample standard deviation of sample 1
#' @param s2 the sample standard deviation of sample 2
#' @param n1 the size of sample 1
#' @param n2 the size of sample 2
#' @param m0 the null value for the difference in means to be tested for.
#'   Default is 0.
#' @param equal.variance whether or not to assume equal variance. Default is
#'   FALSE.
#' @param messageLevel integer of length 1 indicating the level of diagnostic
#'   messages to generate
#' @return list of length 4 with elements "Difference of means", "Std Error",
#'   "t", and "p-value"
#' @note This is based on code written by CrossValidated user Macro.  The
#'   original can be found at https://stats.stackexchange.com/a/30450
#' @examples
#' x1 = rnorm(100)
#' x2 = rnorm(200)
#' # you'll find this output agrees with that of t.test when you input x1,x2
#' t.test2( mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)
#' t.test(x1, x2)
t.test2 <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE, messageLevel = 0){
  
  if(messageLevel > 0){message("running t.test2")}
  
  if(equal.variance == FALSE){
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  } # /if-else
  
  t <- (m1 - m2 - m0)/se 
  dat <- list(m1 - m2, se, t, 2*pt(-abs(t), df))
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  
  return(dat) 
  
} # /function
