## use kmeans algortim to predict the class of the iris plant by using 4 features.

library(rmr2)

kmeans.mr = function(P, num.clusters, num.iter) {
  ## if k clusters, C is k by 2 matrix, calculated the distance between point and k centers of clusters, 
  ## we get a 1 by k distances. there are 100 points, we get 100 by k matrix.
  dist.fun = function(C, P) {
    apply(C, 1, function(x) colSums((t(P) - x)^2))
  }
  
  kmeans.map = function(., P) {
    if(is.null(C)) {
      nearest = sample(1:num.clusters, nrow(P), replace = TRUE)
    }
    else {
      D = dist.fun(C, P)
      nearest = max.col(-D)
    }
    keyval(nearest, P) 
  }
  
  kmeans.reduce = function(., P) {
    t(as.matrix(apply(P, 2, mean)))
  }
  
  C = NULL
  for(i in 1:num.iter ) {
    C = values(
      from.dfs(
        mapreduce(P, map = kmeans.map, reduce = kmeans.reduce)))
  } 
  ## keep the number of centers the desired one (when centers are nearest to no points, they are lost). 
  ## we generate a point and rbind with the previous C to give desired k by 2 matrix
  if(nrow(C) < num.clusters) {
    C = 
      rbind(
        C,
        matrix(
          rnorm(
            (num.clusters - nrow(C)) * nrow(C)), 
          ncol = nrow(C)) %*% C) 
  }
  C
}

out = kmeans.mr(to.dfs(P), num.clusters = 3, num.iter = 5)

D <- dist.fun(out, P)
nearest <- max.col(-D)
table(data[[5]], nearest)

par(mfrow = c(1, 2))
plot(P[,3:4], col = nearest, main = “plot by k-means”)
plot(P[,3:4], col = data[[5]], main = "plot of original classes")

plot(P[,1:2], col = nearest, main = “plot by k-means”)
plot(P[,1:2], col = data[[5]], main = "plot of original classes")
