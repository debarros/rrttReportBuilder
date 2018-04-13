#t.test2.R
# This is based on code written by CrossValidated user Macro
# The original can be found at https://stats.stackexchange.com/a/30450
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE, messageLevel = 0){
  
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

# Sample:
# x1 = rnorm(100)
# x2 = rnorm(200) 
# you'll find this output agrees with that of t.test when you input x1,x2
# t.test2( mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)
# Difference of means       Std Error               t         p-value 
# -0.05692268      0.12192273     -0.46687500      0.64113442 