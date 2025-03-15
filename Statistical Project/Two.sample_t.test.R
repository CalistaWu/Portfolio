
#Independent Random Samples
#Example 10.5

online <- c(32, 37, 35, 28, 41, 44, 35, 31, 34)
classroom <- c(35, 31, 29, 25, 34, 40, 27, 32, 31)


#t.test( online, classroom, alternative = "greater" , var.equal = FALSE )
t.test( online, classroom, alternative = "greater" , var.equal = TRUE )
t.test( online, classroom, alternative = "two.sided" , var.equal = TRUE )

two.sample_t.test <- function(x, y, alternative = c("less", "greater", "two.sided"), alpha = 0.05){
  # 檢查 alternative 的輸入值是否有效
  alternative <- match.arg(alternative)
  
  if(alternative == "less"){
    H_a <- paste("True difference in means is", alternative, "than",0)
  }else if(alternative == "greater"){
    H_a <- paste("True difference in means is", alternative, "than",0)
  }else{
    H_a <-paste("True difference in means is not equal to",0)
  }
  
  x_bar1 <- mean(x)
  x_bar2 <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  s1 <- sd(x)
  s2 <- sd(y)
  s_square <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
  t <- (x_bar1 - x_bar2) / sqrt(s_square * (1/n1 + 1/n2))
  
  
  p_value <- switch(alternative,
                    less = pt(tn1+n2-2),
                    greater = 1 - pt(t,n1+n2-2),
                    two.sided = 2 * (1 - pt(abs(t),n1+n2-2)))
  
  UL <- (x_bar1 - x_bar2) + ( qt(1-alpha/2,n1+n2-2)*sqrt(s_square * (1/n1 + 1/n2)))
  LL <- (x_bar1 - x_bar2) - ( qt(1-alpha/2,n1+n2-2)*sqrt(s_square * (1/n1 + 1/n2)))

  
  CI <- switch(alternative,
               less = c(-Inf, (x_bar1 - x_bar2) +  qt(1-alpha,n1+n2-2)*sqrt(s_square * (1/n1 + 1/n2))),
               greater = c((x_bar1 - x_bar2) - ( qt(1-alpha,n1+n2-2)*sqrt(s_square * (1/n1 + 1/n2))), Inf),
               two.sided = c(LL, UL)
               
  )
  
  
  if(p_value >= alpha){
    return(list(alternative_hypothesis = H_a, t = t, df = n1+n2-2, p_value = p_value, Confidence_Interval = CI,
                conclusion = "Do not reject null hypothesis"))
  }else{
    return(list(alternative_hypothesis = H_a, t = t, df = n1+n2-2, p_value = p_value, Confidence_Interval = CI,
                conclusion = "Reject null hypothesis"))
  }
}
two.sample_t.test(online, classroom, alternative = "greater")
two.sample_t.test(online, classroom, alternative = "two.sided")
