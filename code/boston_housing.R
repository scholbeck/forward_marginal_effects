library(partykit)
library(parttree)
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(ggparty)
library(ggplot2)
library(latex2exp)
source("paper/code/me_nlm_implementation.R")

bh.task = tsk("boston_housing")
bh.data = bh.task$data()
bh.data = as.data.frame(bh.data)
bh.data$town = NULL
bh.data$cmedv = NULL
bh.data$lat = NULL
bh.data$lon = NULL
bh.data$tract = NULL
bh.data$b = NULL

coefficientOfVariation = function(data) {
  cov = sd(data) / (abs(mean(data)))
  return(cov)
}

###############################

bh.task = mlr3::TaskRegr$new(id = "bh.task", backend = bh.data, target = "medv")
library(mlr3)
library(mlr3learners)
load('data/bh_tuning.rdata')

instance$result
learner = mlr_learners$get("regr.ksvm")
learner$param_set$values = list(C = instance$result$C, sigma = instance$result$sigma, type = 'eps-svr', kernel = 'rbfdot')

set.seed(123)
learner$train(bh.task)

################################

set.seed(123)
step.size = -5
data.crim = testEnvelopeNum(bh.data, bh.data, 'crim', step.size)

fme.crim = marginalEffect(c("crim"), step.size = step.size, data = data.crim, model = learner)
nlm.crim = nonLinearityMeasureDataFrame(model = learner, data = data.crim, feature = 'crim', step_size = step.size)
df.crim = data.frame('crim' = data.crim$crim, 'me' = fme.crim, 'nlm' = nlm.crim)

coefficientOfVariation(fme.crim)

library(ggplot2)
ggplot(df.crim, aes(crim, me)) +
  geom_point(color = 'steelblue', size = 3, stroke = 1, pch = 21) +
  ylab('fME with step size = -5') +
  geom_hline(yintercept = mean(df.crim$me), linetype = 'dashed') +
  geom_label(x = 0.95*max(df.crim$crim), y = mean(df.crim$me), label = 'AME', fill = 'white') +
  geom_segment(aes(x = (0.5 * min(crim) + 0.5 * max(crim) + 0.5 * abs(step.size)), xend = (0.5 * min(crim) + 0.5 * max(crim) - 0.5 * abs(step.size)), y = min(me), yend = min(me)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))


ggplot(df.crim) +
  geom_point(aes(crim, nlm), color = 'steelblue', size = 3, stroke = 1, pch = 21) +
  ylab('NLM with step size = -5') +
  geom_hline(yintercept = mean(df.crim$nlm), linetype = 'dashed') +
  geom_label(x = 0.95*max(df.crim$crim), y = mean(df.crim$nlm), label = 'NLM', fill = 'white') +
  geom_segment(aes(x = (0.5 * min(crim) + 0.5 * max(crim) + 0.5 * abs(step.size)), xend = (0.5 * min(crim) + 0.5 * max(crim) - 0.5 * abs(step.size)), y = min(nlm), yend = min(nlm)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))


###############################


data.crim$me = as.numeric(fme.crim)
data.crim$nlm = nlm.crim

t = ctree(me ~ . -medv, data = data.crim, control = ctree_control(
  minbucket = 30,
  testtype = "Teststatistic",
  mincriterion = 0.99))

t

p = ggparty(t,
            terminal_space = 0.75,
            add_vars = list(cAME =
                              function(data, node) {
                                list(round(mean(node$data$me), 2))
                              },
                            cAME_cov =
                              function(data, node) {
                                list(round(coefficientOfVariation(node$data$me), 2))
                              },
                            cANLM =
                              function(data, node) {
                                list(round(mean(node$data$nlm), 2))
                              },
                            cANLM_cov =
                              function(data, node) {
                                list(round(coefficientOfVariation(node$data$nlm), 2))
                              }
            )) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_label(aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nCoV(fME) = ", cAME_cov, "\ncANLM = ", cANLM, "\nCoV(NLM) = ", cANLM_cov)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.05) +
  geom_node_plot(gglist = list(geom_boxplot(mapping = aes(x = me)),
                               xlab("Marginal Effects"),
                               theme_bw(),
                               coord_flip(),
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank())),
                 height = 0.3,
                 nudge_y = -0.12) +
  geom_node_plot(gglist = list(geom_boxplot(mapping = aes(x = nlm)),
                               xlab("Non-Linearity Measure"),
                               theme_bw(),
                               coord_flip(),
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank())),
                 height = 0.3,
                 nudge_y = -0.35)

p = p + 
  ggtitle("cAME and cANLM for step size 'crim - 5'") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(0, 0, -30, 0)),
    plot.margin=unit(c(0.25, -2.5, -5.5, -2.5), "cm"))
p


cAME = 0.86
n_subspace = 30 
cov_fme = 0.65
cANLM = 0.99
cov_nlm = 0.03

#CI cAME
cAME - qt(0.975, df = n_subspace-1) * (cAME * cov_fme) / sqrt(n_subspace)
cAME + qt(0.975, df = n_subspace-1) * (cAME * cov_fme) / sqrt(n_subspace)
#CI cANLM
cANLM - qt(0.975, df = n_subspace-1) * (cANLM * cov_nlm) / sqrt(n_subspace)
cANLM + qt(0.975, df = n_subspace-1) * (cANLM * cov_nlm) / sqrt(n_subspace)

####

set.seed(123)
step.size = c(-5, -0.1)
data.crim.nox = testEnvelopeNum(bh.data, bh.data, c('crim', 'nox'), step.size)
fme.crim.nox = marginalEffect(c("crim", "nox"), step.size = step.size, data = data.crim.nox, model = learner)
nlm.crim.nox = nonLinearityMeasureDataFrame(model = learner, data = data.crim.nox, feature = c('crim', 'nox'), step_size = step.size)
df.crim.nox = data.frame('crim' = data.crim.nox$crim, 'nox' = data.crim.nox$nox, 'me' = fme.crim.nox, 'nlm' = nlm.crim.nox)

ggplot(df.crim.nox) +
  geom_point(aes(crim, nox, fill = me), size = 4, pch = 21) +
  scale_fill_gradient('fME', low = "steelblue", high = "gold") +
  geom_segment(aes(x = (0.5 * min(crim) + 0.5 * max(crim) + 0.5 * abs(step.size[1])), xend = (0.5 * min(crim) + 0.5 * max(crim) - 0.5 * abs(step.size[1])), y = min(nox), yend = min(nox)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_segment(aes(y = (0.5 * min(nox) + 0.5 * max(nox) + 0.5 * abs(step.size[2])), yend = (0.5 * min(nox) + 0.5 * max(nox) - 0.5 * abs(step.size[2])), x = median(nox), xend = median(nox)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15))


ggplot(subset(df.crim.nox, nlm > 0)) +
  geom_point(aes(crim, nox, fill = nlm), size = 4, pch = 21) +
  scale_fill_gradient('NLM', low = "steelblue", high = "gold") +
  geom_point(data = subset(df.crim.nox, nlm < 0), aes(crim, nox), size = 4, pch = 21, fill = '#F8766D') + 
  geom_segment(aes(x = (0.5 * min(crim) + 0.5 * max(crim) + 0.5 * abs(step.size[1])), xend = (0.5 * min(crim) + 0.5 * max(crim) - 0.5 * abs(step.size[1])), y = min(nox), yend = min(nox)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  geom_segment(aes(y = (0.5 * min(nox) + 0.5 * max(nox) + 0.5 * abs(step.size[2])), yend = (0.5 * min(nox) + 0.5 * max(nox) - 0.5 * abs(step.size[2])), x = median(nox), xend = median(nox)), colour = 'black', size = 1, arrow = arrow(length = unit(0.5, "cm"))) +  theme_bw() +
  theme(
    aspect.ratio = 1,
    text = element_text(size = 15))


################################

set.seed(123)
step.size = c(-5, -0.1, -1)
data.crim.nox.ptratio = testEnvelopeNum(bh.data, bh.data, c('crim', 'nox', 'ptratio'), step.size)
fme.crim.nox.ptratio = marginalEffect(c("crim", "nox", "ptratio"), step.size = step.size, data = data.crim.nox.ptratio, model = learner)
nlm.crim.nox.ptratio = nonLinearityMeasureDataFrame(model = learner, data = data.crim.nox.ptratio, feature = c('crim', 'nox', 'ptratio'), step_size = step.size)
df.crim.nox.ptratio = data.frame('crim' = data.crim.nox.ptratio$crim, 'nox' = data.crim.nox.ptratio$nox, 'ptratio' = data.crim.nox.ptratio$ptratio, 'me' = fme.crim.nox.ptratio, 'nlm' = nlm.crim.nox.ptratio)

ggplot(df.crim.nox.ptratio, aes(me)) +
  geom_density(fill = "steelblue", alpha = 0.3) +
  geom_vline(xintercept = mean(df.crim.nox.ptratio$me)) +
  geom_label(x = mean(df.crim.nox.ptratio$me), y = 0, label = 'AME', fill = 'white') +
  xlab(TeX("fME with step size $h_{crim} = -5$, $h_{nox} = -0.1$, $h_{ptratio} = -1$")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

ggplot(subset(df.crim.nox.ptratio, nlm > 0), aes(nlm)) +
  geom_density(fill = "steelblue", alpha = 0.3) +
  geom_vline(xintercept = mean(df.crim.nox.ptratio$nlm)) +
  geom_label(x = mean(df.crim.nox.ptratio$nlm), y = 0, label = 'ANLM', fill = 'white') +
  xlab(TeX("NLM with step size $h_{crim} = -5$, $h_{nox} = -0.1$, $h_{ptratio} = -1$")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))


############################################
# text example

set.seed(123)
step.size = 1
data.cAME = testEnvelopeNum(bh.data, bh.data, 'rm', step.size)
fme.cAME = marginalEffect(c("rm"), step.size = step.size, data = data.cAME, model = learner)
coefficientOfVariation(fme.cAME)
mean(fme.cAME)

nlm.cAME = nonLinearityMeasureDataFrame(model = learner, data = data.cAME, feature = 'rm', step_size = step.size)

df.cAME = data.cAME
df.cAME$me = as.numeric(fme.cAME)
df.cAME$nlm = as.numeric(nlm.cAME)

t = ctree(me ~ . -medv, data = df.cAME, control = ctree_control(
  minbucket = 100,
  testtype = "Teststatistic",
  mincriterion = 0.99))
t

p = ggparty(t,
            terminal_space = 0.75,
            add_vars = list(cAME =
                                function(data, node) {
                                  list(round(mean(node$data$me), 2))
                                },
                            cAME_cov =
                              function(data, node) {
                                list(round(coefficientOfVariation(node$data$me), 2))
                              },
                            cANLM =
                              function(data, node) {
                                list(round(mean(node$data$nlm), 2))
                              },
                            cANLM_cov =
                              function(data, node) {
                                list(round(coefficientOfVariation(node$data$nlm), 2))
                              }
            )) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_label(aes(label = paste0("n = ", nodesize, "\ncAME = ", cAME, "\nCoV(fME) = ", cAME_cov, "\ncANLM = ", cANLM, "\nCoV(NLM) = ", cANLM_cov)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  nudge_y = -0.05) +
  geom_node_plot(gglist = list(geom_boxplot(mapping = aes(x = me)),
                               xlab("Marginal Effects"),
                               theme_bw(),
                               coord_flip(),
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank())),
                 height = 0.3,
                 nudge_y = -0.12) +
  geom_node_plot(gglist = list(geom_boxplot(mapping = aes(x = nlm)),
                               xlab("Non-Linearity Measure"),
                               theme_bw(),
                               coord_flip(),
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank())),
                 height = 0.3,
                 nudge_y = -0.35)

p = p + 
  ggtitle("cAME and cANLM for step size 'rm + 1'") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(0, 0, -30, 0)),
    plot.margin=unit(c(0.25, -2.5, -5.5, -2.5), "cm"))

p

