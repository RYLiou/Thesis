
# distribution generate data
dist_generate_data <- function(N, dim, r){
  
  set.seed(2020)
  
  X = matrix(rnorm(N*dim), ncol=dim)
  w = rep(1, dim)
  t = rnorm(1) #first threshold
  beta = rnorm(r-2)
  
  threshold = cumsum(c(t, exp(beta)))
  y = X %*% w
  
  cdf = t(1 / (1 + exp(t(matrix(rep(y,r-1), ncol=(r-1))) - threshold)))
  cdf = cbind(0,cdf,1)
  
  pdf = t(apply(cdf, MARGIN = 1, diff))
  rating = c()
  for(i in 1:nrow(pdf)){
    rating = c(rating, sample(1:ncol(pdf), 1, prob = pdf[i,]))
  }
  
  print(table(rating))
  
  data <- data.frame(rating, X)
  return(data)
}

## generate data
data1 <- dist_generate_data(50000, 1, 5)
data2 <- dist_generate_data(50000, 2, 5)
data3 <- dist_generate_data(50000, 3, 5)
data4 <- dist_generate_data(50000, 4, 5)
data5 <- dist_generate_data(50000, 5, 5)

data6 <- dist_generate_data(50000, 6, 5)
data7 <- dist_generate_data(50000, 7, 5)
data8 <- dist_generate_data(50000, 8, 5)
data9 <- dist_generate_data(50000, 9, 5)
data10 <- dist_generate_data(50000, 10, 5)
