# Simulation study interactions
library(iml)
library(ggpubr)
library(gbm)
library(mlr)
library(plotly)
library(randomForest)
library(sfsmisc)
library(egg)
source("code/theme.R")
source("code/figure7_interactions/dice.R")


####################################################################################################
# Figure 7: Goldstein adjusted Example

# simulate data
set.seed(1234)
n = 1000
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
eps = rnorm(n, sd = 0.3)

y = 3*x1 - 6*x2 + ifelse(x3 > 0, 12*x2, 0) + eps

data = data.frame(cbind(x1,x2,x3,y))

# fit a random forest model and create a predictor object
rf <- randomForest(y ~., data = data, ntree = 500)
mod <- Predictor$new(rf, data = data[sample(1:n, 200),])

# calculate effects (pdp and ice curves) for x1 and x2
effect_ice_x1 <- FeatureEffect$new(mod, feature = "x1", method = "pdp+ice")
effect_ice_x2 <- FeatureEffect$new(mod, feature = "x2", method = "pdp+ice")

# effect plots
p1 = effect_ice_x1$plot(rug=FALSE) + scale_x_continuous("Feature"~X[1])
p2 = effect_ice_x2$plot(rug=FALSE) + scale_x_continuous("Feature"~X[2]) + scale_y_continuous("")


# create dice (derivative ice) plot for x2
dice_curves = dice(effect_ice_x2, smooth = TRUE)

# dice curves
p_dice = ggplot(dice_curves$dice_curves, aes(x = x2, y = .value, group = .id)) + 
  geom_line(alpha = 0.2) +
  geom_line(data = dice_curves$dpdp, size = 1.5, color = "gold") + 
  xlab("Feature"~X[2]) + 
  ylab("deriv of predicted y")

# standard deviation of dice curves
p_sd = ggplot(dice_curves$sd_deriv, aes(x = gridpts, y = .sd)) + 
  geom_line(size = 1.5) + 
  xlab("Feature"~X[2]) + 
  ylab("sd(deriv)")

# arrange p_dice and p_sd in one plot
p3 = egg::ggarrange(p_dice, p_sd, heights = c(0.75, 0.25))
grid.arrange(
  grobs = gl,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 2, NA),
                        c(3, 3, 4))
)


# create 2D PDP between x2 and x3
effect_2dpdp = FeatureEffect$new(mod, method = "pdp", feature = c("x2", "x3") )
p4 = effect_2dpdp$plot(rug=FALSE) + xlab("Feature"~X[2]) + ylab("Feature"~X[3])


# arrange all plots in one figure
figure <- ggpubr::ggarrange(p1, p2, p3, p4,
  labels = c("A", "B", "C", "D"),
  ncol = 4)


# save figure
pdf(file = "./figures/interaction.pdf", height = 3.5, width = 15)
figure
dev.off()


