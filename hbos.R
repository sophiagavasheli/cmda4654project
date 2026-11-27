
# HBOS implementation with iris data, adapted from the handbook for anomaly detection

library(dplyr)
library(ggplot2)
library(patchwork)

# analyze only one species since each species has its own distribution of flower measurements
petal = iris %>% 
  filter(Species == "setosa") %>% 
  select(-Species)

X_scaled <- as.data.frame(scale(petal))

## HBOS implementation
hbos_fit <- function(X, n_bins = 10) {
  
  X <- as.data.frame(X)
  
  models <- list()
  
  for (j in seq_along(X)) {
    
    x <- X[[j]]
    
    # create equally spaced histogram bins
    breaks <- seq(min(x), max(x), length.out = n_bins + 1)
    
    # count observations in each bin
    counts <- hist(x, breaks = breaks, plot = FALSE, include.lowest = TRUE)$counts
    
    # estimate density for each bin
    widths <- diff(breaks)
    
    densities <- counts / (length(x) * widths)
    
    densities[densities == 0] <- 1e-10 # ensure everything is nonzero
    
    models[[j]] <- list(breaks = breaks, densities = densities)
  }
  
  return(models)
}


hbos_predict <- function(model, X) {
  
  X <- as.data.frame(X)
  
  scores <- matrix(0, nrow = nrow(X), ncol = ncol(X))
  
  for (j in seq_along(X)) {
    
    x <- X[[j]]
    
    breaks <- model[[j]]$breaks
    densities <- model[[j]]$densities
    
    # identify the histogram bin for each observation
    bin <- findInterval(x, breaks)
    
    # keep bin indices within valid range
    bin <- pmax(1, pmin(bin, length(densities)))
    
    # calculate negative log density
    scores[, j] <- -log(densities[bin])
  }
  
  return(rowSums(scores))
}

hbos_model <- hbos_fit(X_scaled, n_bins = 10)
iris_scores <- hbos_predict(hbos_model, X_scaled)

contamination <- 0.05 # assume 5% of data is an anomaly

# determine threshold for what's considered an anomaly
threshold <- quantile(iris_scores, probs = 1 - contamination)

# flag observations above the threshold
iris_pred <- ifelse(iris_scores > threshold, 1, 0)

cat("HBOS threshold:", threshold, "\n")
cat("Number of flagged observations:", sum(iris_pred), "\n")

results <- data.frame(
  Observation = 1:nrow(petal),
  Sepal.Length = petal$Sepal.Length,
  Sepal.Width = petal$Sepal.Width,
  Petal.Length = petal$Petal.Length,
  Petal.Width = petal$Petal.Width,
  Anomaly.Score = iris_scores,
  Anomaly = ifelse(iris_pred == 1, "Outlier", "Normal")
)

results <- results[order(-results$Anomaly.Score), ]

head(results, 15)

# histogram of anomaly scores
ggplot(as.data.frame(iris_scores), aes(x = iris_scores)) +
  geom_histogram(bins = 15, fill = "steelblue") +
  geom_vline(aes(xintercept = threshold, color = "Anomaly Threshold")) +
  labs(
    title = "HBOS Anomaly Scores",
    x = "Anomaly Score",
    y = "Count",
    color = ""
  ) +
  theme(legend.position = "bottom")

# visualizing flagged observations
petal$iris_pred <- as.factor(iris_pred)

p1 = ggplot(petal, aes(x = Sepal.Length, y = Sepal.Width, colour = iris_pred)) +
  geom_point() +
  scale_color_manual(
    name = "",
    values = c("0" = "steelblue", "1" = "red3"),
    labels = c("0" = "Non-outlier", "1" = "Outlier")
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Visualizing Flagged Observations",
    x = "Sepal Length",
    y = "Sepal Width"
  )

p2 = ggplot(petal, aes(x = Petal.Length, y = Petal.Width, colour = iris_pred)) +
  geom_point() +
    scale_color_manual(
    name = "",
    values = c("0" = "steelblue", "1" = "red3"),
    labels = c("0" = "Non-outlier", "1" = "Outlier")
  ) +
  theme(legend.position = "none") +
  labs(
    x = "Petal Length",
    y = "Petal Width"
  )

p1 + p2

