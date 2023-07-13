library(mlbench)
n = 1000

friedman = mlbench.friedman1(n, sd = 0)
df = as.data.frame(friedman$x)
df$y = friedman$y
friedman = df
rm(df)
names(friedman) = c(paste("x", seq(1:10), sep = ""), "y")
str(friedman)

# devtools::install_github("holgstr/fme")
library(fmeffects)
devtools::load_all()

library(mlr3verse)
friedmantask = as_task_regr(x = friedman, id = "friedman", target = "y")

load("tuning_data/friedman_tuning_svm.Rdata")
svm_tuned = lrn("regr.svm")
svm_tuned$param_set$values = instance$result_learner_param_vals
svm_tuned$train(friedmantask)

effects1 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = c("x1"),
               step.size = c(0.1),
               compute.nlm = TRUE,
               ep.method = "envelope")
p1 = plot(effects1, with.nlm = TRUE)
p1


effects2 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = c("x2"),
               step.size = c(0.1),
               compute.nlm = TRUE,
               ep.method = "envelope")
p2 = plot(effects2, with.nlm = TRUE)
p2

effects3 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = c("x1", "x2"),
               step.size = c(0.1, 0.1),
               compute.nlm = TRUE,
               ep.method = "envelope")
p3 = plot(effects3, with.nlm = TRUE) +
  scale_fill_gradientn(
    colors = c("#56B4E9", "white", "#E69F00"),
    breaks = c(3, 0, -3),
  )
p3


effects4 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = "x3",
               step.size = 0.1,
               compute.nlm = TRUE,
               ep.method = "envelope")
p4 = plot(effects4, with.nlm = TRUE)
p4

effects5 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = "x4",
               step.size = 0.1,
               compute.nlm = TRUE,
               ep.method = "envelope")
p5 = plot(effects5, with.nlm = TRUE)
p5

effects6 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = "x5",
               step.size = 0.1,
               compute.nlm = TRUE,
               ep.method = "envelope")
p6 = plot(effects6, with.nlm = TRUE)
p6

effects7 = fme(model = svm_tuned,
               data = friedman,
               target = "y",
               feature = "x6",
               step.size = 0.1,
               compute.nlm = TRUE,
               ep.method = "envelope")
p7 = plot(effects7, with.nlm = TRUE)
p7
