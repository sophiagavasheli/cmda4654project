
# knn outlier implementation, adapted from the handbook for anomaly detection


# calculate outlier scores with knn, default k = 5
knn_scores <- function(X, k = 5) {
  
  X <- as.matrix(X)
  
  # calculate pairwise Euclidean distances
  distance_matrix <- as.matrix(dist(X))
  
  # exclude each observation's distance to itself
  diag(distance_matrix) <- Inf
  
  # find the distance to the kth nearest neighbor
  scores <- apply(distance_matrix, 1, function(d) sort(d)[k])
  
  return(scores)
}


contamination <- 0.05

scores <- knn_scores("data")

threshold <- quantile(scores, probs = 1 - contamination)

# predict anomalies
pred <- ifelse(scores > threshold, 1, 0)


cat("KNN threshold:", threshold, "\n")
cat("Number of normal observations:", sum(pred == 0), "\n")
cat("Number of outliers:", sum(pred == 1), "\n")
