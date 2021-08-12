library("ranger")
library("iml")
library("knockoff")
library("xtable")
library("patchwork")

set.seed(1)

## Load bike data
bike = read.csv("../../data/bike-sharing-daily.csv")
bike$temp = bike$temp * (39 - (-8)) + (-8)
# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
bike$atemp = bike$atemp * (50 - (16)) + (16)


features = c("temp", "atemp", "hum")
target = "cnt"
bike = bike[c(target, features)]
plot(bike$temp, bike$atemp)


rf = ranger(cnt ~ ., data = bike)

predf = function(model, newdata){
  predict(model, newdata)$predictions
}

# Compute PFI
pred = Predictor$new(model = rf,  predict.fun = predf, data = bike, y = target)

pfi = FeatureImp$new(pred, loss = "mae", compare = "difference")$results
pfi = pfi[c("feature", "importance")]


# TODO Compute cPFI using model-x knockoffs
gaussian_ko = create.second_order(as.matrix(bike[features]))

cpfis = lapply(features, function(fname){
  dat2 = bike
  dat2[fname] = gaussian_ko[,fname]
  e_orig = mean(abs(bike[[target]] - pred$predict(bike)[[1]]))
  e_perm = mean(abs(bike[[target]] - pred$predict(dat2)[[1]]))
  data.frame(feature = fname, importance = e_perm - e_orig)
})

cpfi = rbindlist(cpfis)
importances = cbind(pfi, cpfi$importance)
colnames(importances) = c("Feature", "PFI", "cPFI")
fmap = c("temp" = "temperature", "atemp" = "apparent temperature", "hum" = "humidity")
importances$Feature = fmap[importances$Feature]
cap = "Comparison of unconditional and conditional permutation feature importance (PFI and cPFI) for the bike rental dataset and features temperature , apparent temperature and humidity. Temperature and apparent temperature are highly correlated, and therefore PFI and cPFI differ strongly."
xtab = xtable(importances, caption = cap, label = "tab:cpfi")
print(xtab, file = "cpfi-table.tex", include.rownames = FALSE)


p1 = ggplot(bike) +
  geom_point(aes(x = temp, y = atemp)) +
  scale_x_continuous("Temperature") +
  scale_y_continuous("Apparent temperature")

bike2 = bike
bike2$atemp = sample(bike2$atemp)

p2 = ggplot(bike2) +
  geom_point(aes(x = temp, y = atemp)) +
  scale_x_continuous("Temperature") +
  scale_y_continuous("Apparent temperature (permuted)")



pdf(file = "temperature.pdf", height = 4)
p1 + p2
dev.off()



