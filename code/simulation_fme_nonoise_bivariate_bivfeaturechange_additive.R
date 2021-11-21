source("paper/code/me_nlm_implementation.R")
source("paper/code/me_nlm_implementation_datagenprocess.R")
library(partykit)
library(parttree)
library(ggplot2)

dataGeneratingProcessSingle = function(data) {
  
  x1 = data$x1
  x2 = data$x2
  if (x1 < 0) {
    y =  x1 + x2
  } else if (x1 >= 0) {
    y = 5*sin(2*x1) + x2
  }
  return(y)
}

set.seed(123)
n = 1500
x1 = runif(n, -5, 5)
x2 = runif(n, -5, 5)
cor(x1, x2)
df = data.frame(x1, x2, 'y' = NA)

step.size = c(2, 3)
df$me = marginalEffectDataGenProcess(dataGeneratingProcessMultiple, c("x1", "x2"), step.size, df)
tree = rpart(df$me ~ df$x1 + df$x2, data = df)
me_pred = predict(tree, df)
nlm = nonLinearityMeasureDataFrameDataGenProcess(dataGeneratingProcessMultiple, df, feature = c('x1', 'x2'), step_size = step.size)
df$nlm = nlm

t = ctree(me ~ x1 + x2, data = df, control = ctree_control(
  minbucket = 30,
  testtype = "Teststatistic",
  mincriterion = 0.99))

ggplot(df) +
  geom_point(aes(x1, x2, fill = me), size = 4, stroke = 1, pch = 21, alpha = 0.6) +
  scale_fill_gradient('fME', low = "steelblue", high = "gold") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size[1])), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size[1])), y = min(x2), yend = min(x2)), colour = 'grey', alpha = 0.2, size = 8) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size[1])), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size[1])), y = min(x2), yend = min(x2)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * abs(step.size[2])), yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * abs(step.size[2])), x = min(x1), xend = min(x1)), colour = 'grey', alpha = 0.2, size = 8) +
  geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * abs(step.size[2])), yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * abs(step.size[2])), x = min(x1), xend = min(x1)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

ggplot(subset(df, nlm > 0)) +
  geom_point(aes(x1, x2, fill = nlm), size = 4, stroke = 1, pch = 21, alpha = 0.6) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) + 
  geom_point(data = subset(df, nlm < 0), aes(x1, x2), size = 4, pch = 21, fill = '#F8766D', alpha = 0.6) +
  geom_parttree(data = t, alpha = 0, lwd = 1) +
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size[1])), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size[1])), y = min(x2), yend = min(x2)), colour = 'grey', alpha = 0.2, size = 8) + 
  geom_segment(aes(x = (0.5 * min(x1) + 0.5 * max(x1) - 0.5 * abs(step.size[1])), xend = (0.5 * min(x1) + 0.5 * max(x1) + 0.5 * abs(step.size[1])), y = min(x2), yend = min(x2)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * abs(step.size[2])), yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * abs(step.size[2])), x = min(x1), xend = min(x1)), colour = 'grey', alpha = 0.2, size = 8) +
  geom_segment(aes(y = (0.5 * min(x2) + 0.5 * max(x2) - 0.5 * abs(step.size[2])), yend = (0.5 * min(x2) + 0.5 * max(x2) + 0.5 * abs(step.size[2])), x = min(x1), xend = min(x1)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  scale_fill_gradient('NLM', low = "steelblue", high = "gold") +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15)) 

