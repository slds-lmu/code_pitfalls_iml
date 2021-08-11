library("iml")
library("mlr3")
library("mlr3verse")
library("ggplot2")
library("iml")
library("dplyr")

set.seed(123)

nvars = 20
nobs = 10**4
ntrain = 50
target = 'X21'

noise = data.frame(replicate(nvars + 1, runif(nobs, -10, 10)))
task = TaskRegr$new(id = 'noise', backend = noise, target = target)
task
learner = lrn("regr.xgboost")

train_set = sample(task$nrow, ntrain)
test_set = setdiff(seq_len(task$nrow), train_set)

learner$train(task, row_ids = train_set)
prediction = learner$predict(task, row_ids = train_set)
prediction$score()
prediction = learner$predict(task, row_ids = test_set)
prediction$score()

predictor_train = Predictor$new(learner, noise[train_set,], y=target)
predictor_test = Predictor$new(learner, noise[test_set,], y=target)
                          
imp_test <- FeatureImp$new(predictor_test,loss = "mae", n.repetitions = 10, compare='difference')
library("ggplot2")
plot(imp_test)
mean(imp_test$results$importance)

imp_train <- FeatureImp$new(predictor_train,loss = "mae", n.repetitions = 10,  compare='difference')
library("ggplot2")
plot(imp_train)
hist(imp_train$results$importance)

shap <- Shapley$new(predictor_train, sample.size=10)

compute_shap <- function(row) {
  df = data.frame(as.list(row))
  shap$explain(x.interest=df)
  return(abs(shap$results$phi))
}

shap_matrix = apply(noise[test_set, 1:20], 1, compute_shap)
shap_result = as.data.frame(t(shap_matrix))
colnames(shap_result) <- colnames(noise[,1:20])
global_shap = data.frame(as.list(apply(shap_result, 2, mean)))

for (ii in 2:10) {
  print("Iteration ", ii)
  shap_matrix = apply(noise[test_set, 1:20], 1, compute_shap)
  shap_result = as.data.frame(t(shap_matrix))
  colnames(shap_result) <- colnames(noise[,1:20])
  tmp = data.frame(as.list(apply(shap_result, 2, mean)))
  global_shap = bind_rows(global_shap, tmp)
  write.csv(global_shap, "global_shap.csv", row.names = FALSE)
}

global_shap = read.csv("global_shap.csv")

global_shap = global_shap[, order(colnames(global_shap))]

global_shap_mean = apply(global_shap, 2, mean)
global_shap_q05 = apply(global_shap, 2, quantile, probs=0.05)
global_shap_q95 = apply(global_shap, 2, quantile, probs=0.95)

imp_train_res = imp_train$results[order(imp_train$results$feature), ]
imp_test_res = imp_test$results[order(imp_test$results$feature), ]

# TODO get importance and the quantiles

# importance = c(imp_test_res$importance, imp_train_res$importance, global_shap_mean)
# q95 = c(imp_test_res$importance.95, imp_train_res$importance.95, global_shap_q95)
# q05 = c(imp_test_res$importance.05, imp_train_res$importance.05, global_shap_q05)
# type = c(rep('PFI on test data', nvars), rep('PFI on train data', nvars), rep('mean |SHAP|', nvars))
# colname = rep(names(global_shap), 3)

importance = c(imp_test_res$importance, global_shap_mean)
q95 = c(imp_test_res$importance.95, global_shap_q95)
q05 = c(imp_test_res$importance.05, global_shap_q05)
type = c(rep('PFI on test data', nvars), rep('mean |SHAP|', nvars))
colname = rep(names(global_shap), 2)

results = data.frame(importance=importance, type=type, q95=q95, q05=q05, colname=colname)
write.csv(results, 'results.csv')
results = read.csv('results.csv')


# TODO add error bars http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
p = ggplot(data=results, aes(x=reorder(colname, -importance), y=importance, fill=reorder(type, -importance))) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin=q05, ymax=q95), width=.2, position=position_dodge(.9))
p = p + labs(x='Feature', fill='IML method', y='Score')
p = p + theme_bw()
p = p + theme(axis.text.x = element_text(angle=90))

setwd("~/university/phd/2021/research/paper_2021_pitfalls_lnai/code")
#ggplot(results, aes(x=type, y=importance)) + geom_boxplot()
ggsave('pfi_test_vs_train_vs_global_shap.pdf', width=7, height=2)

