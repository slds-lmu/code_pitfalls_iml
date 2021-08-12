library("randomForest")
library("patchwork")
library("iml")
library("data.table")
source("code/theme.R")
set.seed(9)


# =============================================================================
# Generate data
# =============================================================================
n = 100
p = 10
ylimits = c(-1, 1)

draw_x = function(n) data.frame(matrix(runif(p * n), ncol = p))
draw_y = function(X) rowSums(X[,2:10]) + rnorm(nrow(X), sd = 0.9)

X = draw_x(n)
y = draw_y(X)
X$y = y

# =============================================================================
# Train and interpret model repeatedly
# =============================================================================
rf = randomForest(y ~ ., data = X)

pred = Predictor$new(rf, y = "y")
res1 = FeatureEffect$new(pred, feature = "X1", method = "pdp")$results

# Repeat Monte Carlo sampling
repeated = lapply(1:10, function(i){
  X2 = draw_x(n)
  pred = Predictor$new(rf, data = X2)
  res = FeatureEffect$new(pred, feature = "X1", method = "pdp")$results
  res$repetition = i
  res
})
res2 = rbindlist(repeated)

# Repeat model training
repeated = lapply(1:10, function(i){
  X2 = draw_x(n)
  y2 = draw_y(X2)
  rf2 = randomForest(y2 ~ ., data = X2)
  pred = Predictor$new(rf2, data = X2)
  res = FeatureEffect$new(pred, feature = "X1", method = "pdp")$results
  res$repetition = i
  res
})

# =============================================================================
# Combine results
# =============================================================================
res3 = rbindlist(repeated)

types = c("PDP estimate",
          "PDP estimation variance",
          "Model estimation variance")


res1$type = types[1]
res2$type = types[2]
res3$type = types[3]

results= rbindlist(list(res1, res2, res3), fill = TRUE)
results$type = factor(results$type, levels = types)

# =============================================================================
# Plot
# =============================================================================
p = ggplot(results) +
  geom_line(aes(x = X1, y = .value, group = repetition)) +
  facet_grid(. ~ type) +
  scale_y_continuous("Partial Dependence") +
  scale_x_continuous("Feature"~X[1])

pdf(file = "figures/variance.pdf", height = 3, width = 9)
print(p)
dev.off()
