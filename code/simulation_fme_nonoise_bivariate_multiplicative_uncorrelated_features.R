source("paper/code/me_nlm_implementation.R")
source("paper/code/me_nlm_implementation_datagenprocess.R")
library(partykit)
library(parttree)
library(ggplot2)

set.seed(123)
n = 1500
x1 = runif(n, -5, 5)
x2 = runif(n, -5, 5)
cor(x1, x2)
df = data.frame(x1, x2, 'y' = NA)

dataGeneratingProcessSingle = function(data) {
  x1 = data$x1
  x2 = data$x2
  if (x1 < 0) {
    y =  x1 * x2
  } else if (x1 >= 0) {
    y = 5*sin(2*x1) * x2
  }
  return(y)
}


df$y = dataGeneratingProcessMultiple(df)
step.size = c(2)
df$me = marginalEffectDataGenProcess(dataGeneratingProcessMultiple, "x1", step.size, df)
df$predictions = dataGeneratingProcessMultiple(df)

nlm = nonLinearityMeasureDataFrameDataGenProcess(dataGeneratingProcessMultiple, df, feature = 'x1', step_size = 2)
df$nlm = nlm

t = ctree(me ~ x1 + x2, data = df, control = ctree_control(
  minbucket = 30,
  testtype = "Teststatistic",
  mincriterion = 0.99))


ggplot(df, aes(x1, x2, fill = me)) +
  geom_point(size = 4, stroke = 1, pch = 21, alpha = 0.8) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size)), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size)), y = min(x2), yend = min(x2)), colour = 'grey', alpha = 0.2, size = 8) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size)), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size)), y = min(x2), yend = min(x2)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  scale_fill_gradient('fME', low = "steelblue", high = "gold") +
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

ggplot(subset(df, nlm > 0)) +
  geom_point(aes(x1, x2, fill = nlm), size = 4, stroke = 1, pch = 21, alpha = 0.8) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size)), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size)), y = min(x2), yend = min(x2)), colour = 'grey', alpha = 0.2, size = 8) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size)), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size)), y = min(x2), yend = min(x2)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  scale_fill_gradient('NLM', low = "steelblue", high = "gold") +
  geom_point(data = subset(df, nlm < 0), aes(x1, x2), size = 4, pch = 21, fill = '#F8766D', alpha = 0.8) + 
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15)) 

