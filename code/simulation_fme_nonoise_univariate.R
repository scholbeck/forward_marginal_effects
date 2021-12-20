source("code/me_nlm_implementation_datagenprocess.R")
source("code/me_nlm_implementation.R")
source("code/helperfct_treesplits.R")
library(ggplot2)
library(parttree)
library(partykit)

set.seed(123)
n = 1500
x = runif(n, -5, 5)
df = data.frame(x, 'y' = NA)

dataGeneratingProcessSingle = function(data) {
  x = data$x
  if (x < 0) {
    y =  x
  } else if (x >= 0) {
    y = 5*sin(2*x)
  }
  return(y)
}

dataGeneratingProcessMultiple = function(data) {
  y = vector(length = nrow(data))
  for (i in 1:nrow(data)) {
    y[i] = dataGeneratingProcessSingle(data[i, ])
  }
  return(unlist(y))
}


df$y = dataGeneratingProcessMultiple(df)
step.size = 2
df$me = marginalEffectDataGenProcess(dataGeneratingProcessMultiple, "x", step.size, df)


library(ggplot2)
ggplot(df) +
  geom_point(aes(x, y), pch = 21, color = 'steelblue') +
  geom_line(aes(x, y)) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-8, 27, by = 2)) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))


step.size = 2
nlm = nonLinearityMeasureDataFrameDataGenProcess(dataGeneratingProcessMultiple, data = df, feature = 'x', step_size = step.size)
df$nlm = nlm

t = ctree(me ~ x, data = df, control = ctree_control(
  minbucket = 30,
  testtype = "Teststatistic",
  mincriterion = 0.99))

df = getSplits(t, df)

ggplot(df) +
  geom_point(aes(x, me), color = 'steelblue', pch = 21) +
  geom_line(aes(x, cAME)) +
  ylab('fME with step size = 2') +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_hline(yintercept = mean(df$me), linetype = 'dashed') +
  geom_segment(aes(x = (0.5 * min(x) + 0.5 * max(x) - 0.5 * abs(step.size)), xend = (0.5 * min(x) + 0.5 * max(x) + 0.5 * abs(step.size)), y = min(me), yend = min(me)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_parttree(data = t, alpha = 0, lwd = 1, show.legend = FALSE) +
  geom_label(x = 0.95 * max(x), y = mean(df$me), label = 'AME', fill = 'white') +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))


ggplot(df) +
  geom_point(aes(x, nlm), color = 'steelblue', pch = 21) +
  geom_line(aes(x, cANLM)) +
  ylab('NLM with step size = 2') +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_hline(yintercept = mean(df$nlm), linetype = 'dashed') +
  geom_segment(aes(x = (0.5 * min(x) + 0.5 * max(x) - 0.5 * abs(step.size)), xend = (0.5 * min(x) + 0.5 * max(x) + 0.5 * abs(step.size)), y = min(nlm), yend = min(nlm)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_parttree(data = t, alpha = 0, lwd = 1, show.legend = FALSE) +
  geom_label(x = 0.95 * max(x), y = mean(df$nlm), label = 'ANLM', fill = 'white') +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))
