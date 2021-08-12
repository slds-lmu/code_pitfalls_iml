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

# dice function
dice = function(ice_obj, smooth = TRUE){
  
  effect_table = ice_obj$results
  
  if(length(unique(effect_table$.type)) ==1){
    ice = effect_table
  }
  else{
    pdp = effect_table[effect_table$.type == "pdp",]
    ice = effect_table[effect_table$.type == "ice",]
  }
  
  gridpts = sort(unique(ice[,1]))
  
  # dice curves are approximated by calculating the the average slope between neighbouring grid points. 
  # either by ice curves or (by default) first smoothing ice curves. Alternatively: D1ss approximates ice curves by splines,
  # then adds an offset to each grid point as predicts at these points again using the splines to calculate the derivative
  EstimatorWrapper = function(y){
    if(smooth) y = supsmu(x=gridpts,y=y)$y #numerical derivative of supersmooth.alternatively with offset: D1ss(x = gridpts, y = y)#
    D1tr( x = gridpts, y = y)  
  }
  
  #compute derivatives
  dice_obj = list()
  dice_obj$ice_curves = tidyr::pivot_wider(ice, names_from = .id, values_from = .value)
  dice_obj$dice_curves = apply(dice_obj$ice_curves[,-(1:3)], 2, FUN = EstimatorWrapper)
  
  #compute the sd of the derivatives at each gridpt.
  dice_obj$sd_deriv = data.frame("gridpts" = gridpts, ".sd" = apply(dice_obj$dice_curves, 1, sd))
  
  # restructure curves for plotting
  dice_obj$dice_curves = cbind(dice_obj$ice_curves[,1:3], dice_obj$dice_curves)
  dice_obj$dice_curves = tidyr::pivot_longer(dice_obj$dice_curves, cols = 4:ncol(dice_obj$dice_curves), names_to = ".id", values_to = ".value")
  
  # remove ice curves
  dice_obj$ice_curves = NULL
  
  # add derivative for pdp
  dice_obj$dpdp = cbind(pdp[,-which(colnames(pdp)==".value")], ".value" = EstimatorWrapper(pdp$.value))
  
  return(dice_obj)
}


####################################################################################################
# Goldstein adjusted Example
set.seed(1234)
n = 1000
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
#x4 = runif(n, -1, 1)
#x5 = runif(n, -1, 1)
eps = rnorm(n, sd = 0.3)

y = 3*x1 - 6*x2 + ifelse(x3 > 0, 12*x2, 0) + eps

data = data.frame(cbind(x1,x2,x3,y))

# model
rf <- randomForest(y ~., data = data, ntree = 500)
mod <- Predictor$new(rf, data = data[sample(1:n, 200),])

# calculate effects for x1 and x2
effect_ice_x1 <- FeatureEffect$new(mod, feature = "x1", method = "pdp+ice")
effect_ice_x2 <- FeatureEffect$new(mod, feature = "x2", method = "pdp+ice")

# effect plots
p1 = effect_ice_x1$plot(rug=FALSE) + scale_x_continuous("Feature"~X[1])
p2 = effect_ice_x2$plot(rug=FALSE) + scale_x_continuous("Feature"~X[2]) +
  scale_y_continuous("")


# create dice plot for x2
dice_curves = dice(effect_ice_x2, smooth = TRUE)

p_dice = ggplot(dice_curves$dice_curves, aes(x = x2, y = .value, group = .id)) + geom_line(alpha = 0.2) +
  geom_line(data = dice_curves$dpdp, size = 1.5, color = "gold") + theme_bw() + 
  xlab("Feature"~X[2]) + ylab("deriv of predicted y")

p_sd = ggplot(dice_curves$sd_deriv, aes(x = gridpts, y = .sd)) + geom_line(size = 1.5) + theme_bw() + 
  xlab("Feature"~X[2]) + ylab("sd(deriv)")

# arrange plots
p3 = egg::ggarrange(p_dice, p_sd, heights = c(0.75, 0.25))


# create 2D PDP between x2 and x3
effect_2dpdp = FeatureEffect$new(mod, method = "pdp", feature = c("x2", "x3") )
p4 = effect_2dpdp$plot(rug=FALSE) + xlab("Feature"~X[2]) + ylab("Feature"~X[3])


# arrange in one figure
figure <- ggarrange(p1, p2, p3, p4,
  labels = c("A", "B", "C", "D"),
  ncol = 4, nrow = 1)

# save figure
pdf(file = "./figures/interaction.pdf", height = 3.5, width = 15)
figure
dev.off()



# Feature Importance and H Statistics
#FeatureImp$new(mod, "mse")
Interaction$new(mod, feature = "x1")
Interaction$new(mod, feature = "x2")
# Interaction$new(mod, feature = "x3")
# Interaction$new(mod, feature = "x2")
Interaction$new(mod)


# 
# # simulation setting
# n = 500
# set.seed(123)
# x1 = round(runif(n, -1, 1), 3)
# x2 = round(runif(n, -1, 1), 3)
# eps = rnorm(n, 0, 1)
# 
# y = x1 - x2 + 5*x2*x1 + eps
# dat = data.frame(x1, x2, y)
# X = dat[, setdiff(colnames(dat), "y")]
# 
# 
# #---------------------------------------------------------------------------------------------------
# # Fit gbm with interaction depth = 1 and create PDP plots
# task = makeRegrTask(data = dat, target = "y")
# learner = makeLearner("regr.gbm", par.vals = list(interaction.depth = 1))
# set.seed(123)
# mod = mlr::train(learner, task)
# model = Predictor$new(mod, data = dat)
# 
# # create PDP+ICE plots for x1 and x2
# effect1 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x1")
# p1 = effect1$plot() + xlab(expression(X[1])) + ylab(expression(hat(y))) + ylim(-2.5, 3.5) 
#   
# effect2 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x2")
# p2 = effect2$plot()+ xlab(expression(X[2])) + ylab(expression(hat(y))) + ylim(-2.5, 3.5) 
# 
# # create 2D PDP between x1 and x2
# effect3 = FeatureEffect$new(model, method = "pdp", feature = c("x1", "x2") )
# p3 = effect3$plot() + xlab(expression(X[1])) + ylab(expression(X[2]))
# 
# # create centered ICE plots for x1 and x2 (todo: use dICE plots)
# effect4 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x1", center.at = round(min(dat$x1),0))
# p4 = effect4$plot() + xlab(expression(X[1])) #+ ylab("centered")
# 
# effect5 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x2", center.at = round(min(dat$x2),0))
# p5 = effect5$plot()+ xlab(expression(X[2]))# + ylab(expression(hat(y)))
# 
# 
# # arrange in one figure
# figure <- ggarrange(grid.arrange(p1,p2), p3, grid.arrange(p4,p5),
#                     labels = c("A", "B", "C"),
#                     ncol = 3, nrow = 1)
# 
# # save figure
# pdf(file = "paper_lnai/figures/interaction_additive.pdf", height = 6, width = 18)
# figure
# dev.off()
# 
# 
# # # 3D surface plot for ID = 1
# # newx <- effect1$results$x1
# # newy <- effect2$results$x2
# # newxy <- expand.grid(x1 = newx, x2 = newy)
# # z <- matrix(predict(mod, newdata = newxy)$data$response, 20, 20, byrow = TRUE)
# # p3d = plot_ly(x = newx, y = newy, z = z) %>% add_surface() 
# # p3d %>% layout(scene = list(xaxis = list(title = "x1"), yaxis = list(title = "x2"), zaxis = list(title = "y")))
# # # (save png from plotly)
# # 
# # 
# 
# 
# #---------------------------------------------------------------------------------------------------
# # Fit gbm with interaction depth = 2 and create PDP plots
# learner = makeLearner("regr.gbm", par.vals = list(interaction.depth = 2))
# set.seed(123)
# mod = mlr::train(learner, task)
# model = Predictor$new(mod, data = dat)
# 
# # create PDP+ICE plots for x1 and x2
# effect1 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x1")
# p1 = effect1$plot() + xlab(expression(X[1])) + ylab(expression(hat(y))) + ylim(-4.5, 4.5) 
# 
# # create 2D PDP
# effect2 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x2")
# p2 = effect2$plot()+ xlab(expression(X[2])) + ylab(expression(hat(y))) + ylim(-4.5, 4.5) 
# 
# effect3 = FeatureEffect$new(model, method = "pdp", feature = c("x1", "x2") )
# p3 = effect3$plot() + xlab(expression(X[1])) + ylab(expression(X[2]))
# 
# # create centered ICE plots for x1 and x2 (todo: update to dICE)
# effect4 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x1", center.at = round(min(dat$x1),0))
# p4 = effect4$plot() + xlab(expression(X[1])) #+ ylab("centered")
# 
# effect5 = FeatureEffect$new(model, method = "pdp+ice", grid.size = 20, feature = "x2", center.at = round(min(dat$x2),0))
# p5 = effect5$plot()+ xlab(expression(X[2]))# + ylab(expression(hat(y)))
# 
# 
# 
# # arrange in one figure
# figure <- ggarrange(grid.arrange(p1,p2), p3, grid.arrange(p4,p5),
#                     labels = c("D", "E", "F"),
#                     ncol = 3, nrow = 1)
# 
# # save figure
# pdf(file = "paper_lnai/figures/interaction_multiplicative.pdf", height = 6, width = 18)
# figure
# dev.off()
# 
# 
