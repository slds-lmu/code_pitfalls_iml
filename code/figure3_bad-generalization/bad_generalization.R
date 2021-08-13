library("mlr3")
library("iml")
library("mlr3learners")
library("dplyr")
library("mlr3tuning")
library("kableExtra")
library("ggplot2")
library("patchwork")

set.seed(1)
# Simulate mid-complex model, two features, plus one irrelevant feature


# =============================================================================
# Define Theme
# =============================================================================
th = theme_bw() + theme(text = element_text(size = 18))

# =============================================================================
# Define data-generating process
# =============================================================================
f = function(X)  X$X1^2 + X$X2 - 5 * X$X1 * X$X2

gen_dat = function(n, p = 3){
  X = data.frame(matrix(runif(n * p, -3, 3), ncol = p))
  X$y = f(X) + rnorm(n, sd = 5)
  X
}

dat = gen_dat(500)

task = TaskRegr$new(backend = dat, target = "y", id = "why-does-mlr3-need-ids?")

# =============================================================================
# Define models
# =============================================================================
# Train 3 models
lrn_underfitting = lrn("regr.lm")
lrn_overfitting = lrn("regr.ranger", min.node.size = 1)
#lrn_overfitting = lrn("regr.kknn", k = 2)
lrn_justright= lrn("regr.svm", kernel = "radial", type = "eps-regression")

search_space = ps(
  epsilon = p_dbl(lower = 0.01, upper = 1, logscale = TRUE),
  gamma = p_dbl(lower = 0.01, upper = 5, logscale = TRUE),
  cost = p_dbl(lower = 0.01, upper = 5, logscale = TRUE)
)
terminator = trm("evals", n_evals = 300)
tuner = mlr3tuning::tnr("random_search")

at = AutoTuner$new(
  learner = lrn_justright,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)
lrn_justright = at




# =============================================================================
# Train Models
# =============================================================================
lrn_underfitting$train(task)
lrn_overfitting$train(task)
lrn_justright$train(task)

# =============================================================================
# Measure Performance (MSE)
# =============================================================================
newdat = gen_dat(1000)
test_task = TaskRegr$new(backend = newdat, target = "y", id = "why-does-mlr3-need-ids?")
measure = msr("regr.mse")

prediction_underfitting1 = lrn_underfitting$predict(task)
prediction_underfitting2 = lrn_underfitting$predict(test_task)
pu1 = prediction_underfitting1$score(measure)
pu2 = prediction_underfitting2$score(measure)

prediction_overfitting1 = lrn_overfitting$predict(task)
prediction_overfitting2 = lrn_overfitting$predict(test_task)
po1 = prediction_overfitting1$score(measure)
po2 = prediction_overfitting2$score(measure)

prediction_justright1 = lrn_justright$predict(task)
prediction_justright2 = lrn_justright$predict(test_task)
pj1 = prediction_justright1$score(measure)
pj2 = prediction_justright2$score(measure)

tab = data.frame(model = c(rep("Random Forest", 2),
                           rep("SVM", 2),
                           rep("Linear Model", 2)),
                 data = rep(c("Training", "Test"), times = 3),
                 mse = c(po1, po2, pj1, pj2, pu1, pu2))
p_mse = ggplot(tab) + 
  geom_point(aes(x = mse, y = model, shape= data)) +
  scale_x_continuous("Means Squared Error") +
  scale_y_discrete("") +
  th

#lab = "bad-generalization"
#cap = "Mean squared error for different models in simulated data."

#kbl(tab, booktabs = TRUE, format = "latex", digits = 2, caption = cap,
#    label = lab, escape = FALSE, linesep = "") %>% 
#  kable_classic() %>%
#  write("../paper_lnai/figures/bad-generalization-mses.tex")




# =============================================================================
# Compute PDPs
# =============================================================================
pred1 = Predictor$new(lrn_underfitting, data = newdat, y = "y")
pred2 = Predictor$new(lrn_overfitting, data = newdat, y = "y")
pred3 = Predictor$new(lrn_justright, data = newdat, y = "y")



eff1 = data.table::rbindlist(FeatureEffects$new(pred1, method = "pdp")$results)
eff1$model = "Linear Regression"
eff2 = rbindlist(FeatureEffects$new(pred2, method = "pdp")$results)
eff2$model = "Random Forest"
eff3 = rbindlist(FeatureEffects$new(pred3, method = "pdp")$results)
eff3$model = "SVM"
ff = function(model, newdata) {f(newdata)}
pred4 = Predictor$new(data = newdat, y = "y", predict.fun = ff)
eff4 = rbindlist(FeatureEffects$new(pred4, method = "pdp")$results)
eff4$model = "True DGP"


effs = rbindlist(list(eff1, eff2, eff3))
effs = filter(effs, .feature %in% c("X1", "X2", "X3"))

p_pdp = ggplot(effs, aes(x = .borders, y = .value, group = model, color = model)) + 
  geom_line(aes(x = .borders, y = .value, group = model, color = model), data = eff4, lty = 2, size = 1.5, alpha = 0.8) +
  geom_line(aes(x = .borders, y = .value, group = model, color = model)) +
  scale_x_continuous("Feature value") +
  scale_y_continuous("Average prediction (PDP)") +
  facet_grid(. ~ .feature) +
  th


# =============================================================================
# Combine All Into One Plot
# =============================================================================
p = p_mse / p_pdp  + plot_layout(heights = c(1.5, 4))

ggsave(p, file = "bad-generalization.pdf", width = 10, height = 6)
