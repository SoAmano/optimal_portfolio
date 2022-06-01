draw_markowitz_theo <- function(data){
  
  means <- seq(0, 0.005, 0.00001)
  omega <- var(data)
  variance <- c()
    
  for (i in means){
    weights <- get_weights_markowitz(data, i)
    variance <- c(variance, t(weights) %*% omega %*% weights)
  }
  
  df <- data.frame(
    mean = means,
    variance = variance
  )
  
  ggplot(df, aes(variance, mean)) +
    geom_line() +
    theme_few()
}
