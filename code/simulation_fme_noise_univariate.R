source("paper/code/me_nlm_implementation.R")
source("paper/code/helperfct_treesplits.R")
library(partykit)
library(parttree)
library(ggplot2)
library(kernlab)
library(mlr3)
library(mlr3learners)

set.seed(123)
n = 1500
x = runif(n, -5, 5)
linear_dep = function(x) x 
x_lower = x[x < 0]
y_linear = linear_dep(x_lower) + rnorm(length(x_lower), 0, 1)
df_linear = data.frame('x' = x_lower, 'y' = y_linear)

x_middle = x[x >= 0]
quadr_dep = function(x) 5*sin(2*x)
y_quadr = quadr_dep(x_middle) + rnorm(length(x_middle), 0, 1)
df_quadr = data.frame('x' = x_middle, 'y' = y_quadr)

df_total = rbind(df_linear, df_quadr)
colnames(df_total) = c('x', 'y')

tsk = TaskRegr$new(id = "sim.task", backend = df_total, target = 'y')
learner = mlr_learners$get("regr.ksvm")

load("paper/code/simulation_ksvm_tuning.rdata")
learner$param_set$values = list(C = instance$result$C,
                                sigma = instance$result$sigma,
                                type = 'eps-svr', kernel = 'rbfdot')
set.seed(123)
learner$train(tsk)
df_total$predictions = predict(learner, newdata = df_total)

ggplot(df_total) +
  geom_point(aes(x, y), pch = 21, color = 'steelblue') +
  geom_line(aes(x, predictions)) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-8, 27, by = 2)) +
  theme_bw() +
  theme(aspect.ratio = 1,
  text = element_text(size = 15))

step.size = 2
df = df_total[df_total$x < 3, ]
me.result = marginalEffect("x", step.size, df, model = learner)
me.result = as.numeric(me.result)
df$me = me.result
nlm = nonLinearityMeasureDataFrame(learner, df, feature = 'x', step_size = step.size)
df$nlm = nlm

t = ctree(me ~ x, data = df, control = ctree_control(
  minbucket = 30,
  testtype = "Teststatistic",
  mincriterion = 0.99))
df = getSplits(t, df)

ggplot(df) +
  geom_point(aes(x, me), color = 'steelblue', pch = 21) +
  ylab('fME with step size = 2') +
  geom_line(aes(x, cAME)) +
  geom_hline(yintercept = mean(df$me), linetype = 'dashed') +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_segment(aes(x = (0.5 * min(x) + 0.5 * max(x) - 0.5 * abs(step.size)), xend = (0.5 * min(x) + 0.5 * max(x) + 0.5 * abs(step.size)), y = min(me), yend = min(me)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  geom_label(x = 0.95 * max(df$x), y = mean(df$me), label = 'AME', fill = 'white') +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

ggplot(df) +
  geom_point(aes(x, nlm), color = 'steelblue', pch = 21) +
  ylab('NLM with step size = 2') +
  geom_line(aes(x, cANLM)) +
  geom_hline(yintercept = mean(df$nlm), linetype = 'dashed') +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_segment(aes(x = (0.5 * min(x) + 0.5 * max(x) - 0.5 * abs(step.size)), xend = (0.5 * min(x) + 0.5 * max(x) + 0.5 * abs(step.size)), y = min(nlm), yend = min(nlm)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  geom_label(x = 0.95 * max(df$x), y = mean(df$nlm), label = 'ANLM', fill = 'white') +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

