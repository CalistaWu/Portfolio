library(ggplot2)


plot_hypothesis_test <- function(mu0, mu_a, sigma, n, alpha = 0.05) {
  
  z_alpha_half <- qnorm(1 - alpha / 2)
  crit_val_left <- mu0 - z_alpha_half * sigma / sqrt(n)
  crit_val_right <- mu0 + z_alpha_half * sigma / sqrt(n)
  
  x_H0 <- seq(mu0 - 5 * sigma/ sqrt(n), mu0 + 5 * sigma / sqrt(n), length.out = 1000)
  x_Ha <- seq(mu_a - 5 * sigma/ sqrt(n), mu_a + 5 * sigma / sqrt(n), length.out = 1000)
  
  y_H0 <- dnorm(x_H0, mean = mu0, sd = sigma / sqrt(n))
  y_Ha <- dnorm(x_Ha, mean = mu_a, sd = sigma / sqrt(n))
  
  data <- data.frame(
    x = c(x_H0, x_Ha),
    y = c(y_H0, y_Ha),
    Hypothesis = factor(rep(c(paste("H0: μ =", mu0), 
                              paste("Ha: μ =", mu_a)), each = length(x_H0)))
  )
  
  p <- ggplot(data, aes(x = x, y = y, color = Hypothesis)) +
    geom_line(size = 1) +
    geom_vline(xintercept = c(crit_val_left, crit_val_right), 
               linetype = "dashed", color = "black") +
#Power
    geom_ribbon(data = subset(data, x <= crit_val_left & Hypothesis == paste("Ha: μ =", mu_a)), 
                aes(ymax = y), ymin = 0, fill = "purple", alpha = 0.2) +
    geom_ribbon(data = subset(data, x >= crit_val_right & Hypothesis == paste("Ha: μ =", mu_a)), 
                aes(ymax = y), ymin = 0, fill = "purple", alpha = 0.2) +
    annotate("text", x = crit_val_left - 3, y = max(data$y) * 0.4, 
             label = "Power", size = 3) +
#alpha
    geom_ribbon(data = subset(data, x <= crit_val_left & Hypothesis == paste("H0: μ =", mu0)), 
                aes(ymax = y), ymin = 0, fill = "blue", alpha = 0.2) +
    geom_ribbon(data = subset(data, x >= crit_val_right & Hypothesis == paste("H0: μ =", mu0)), 
                aes(ymax = y), ymin = 0, fill = "blue", alpha = 0.2) +
#Beta
   
    geom_ribbon(data = subset(data, x >= crit_val_left & x <= crit_val_right & Hypothesis == paste("Ha: μ =", mu_a)), 
                aes(ymax = y), ymin = 0, fill = "green", alpha = 0.2) +
    
 
    labs(title = "Power Analysis with Hypothesis Testing",
         x = expression(bar(x)),
         y = expression(f(bar(x)))) +
    theme_minimal()
  
  print(p)
}
 

plot_hypothesis_test(mu0 = 880, mu_a = 870, sigma = 10, n = 5, alpha = 0.05)


