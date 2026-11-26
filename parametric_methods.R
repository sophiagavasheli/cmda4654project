
# this isn't gonna get used, these tests are a lot easier than i thought so we'll teach the more complicated methods

library(ggplot2)
library(tidyr)
library(dplyr)
library(outliers)
library(EnvStats)


iris_long <- iris %>%
  pivot_longer(
    cols = -Species,
    names_to = "Feature",
    values_to = "Value"
  )

ggplot(iris_long, aes(x = Value, fill = Species, color = Species)) +
  geom_density() +
  facet_wrap(~Feature, scales = "free_y") +
  labs(title = "Iris Measurements by Species")

# since the species have different distributions, it's better to look for anomalies within one species

# analyzing anomalies in petal width

petal = iris %>% 
  filter(Species == "setosa") %>% 
  select(-Species)

x = petal$Petal.Length

# check normality
hist(x, probability = TRUE)
lines(density(x), col = "blue")

## z score method

z_scores <- (x - mean(x)) / sd(x)

# threshold = 2
z_outliers <- which(abs(z_scores) > 2)

data.frame(
  Observation = z_outliers,
  Petal.Length = x[z_outliers],
  Z_Score = z_scores[z_outliers]
)

z_df <- data.frame(
  Observation = 1:length(x),
  Z_Score = z_scores
)

ggplot(z_df, aes(x = Observation, y = Z_Score)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2),
             color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Z-Scores for Petal Width",
    x = "Observation",
    y = "Z-score"
  )


## grubb's test
grubbs.test(x)


## dixon's test
set.seed(67)

# dixon's test is for small samples
petal_sample = sample(x, 25)

dixon.test(petal_sample)


## rosners test

# based on z scoring we don't expect many outliers so we set k to a small val
ros = rosnerTest(x, k = 5, alpha = 0.05)
ros$all.stats



