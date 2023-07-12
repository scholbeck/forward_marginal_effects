library(fme)
library(mlr3verse)
wine = read.table("data/winequality-white.csv", sep = ";", header = TRUE)
winetask = as_task_regr(x = wine, id = "wine", target = "quality")
library(mlr3verse)

load("tuning_data/wine_tuning.Rdata")

svm_tuned = lrn("regr.svm")
svm_tuned$param_set$values = instance$result_learner_param_vals
svm_tuned$train(winetask)

effects_ph = fme(model = svm_tuned,
              data = wine,
              target = "quality",
              feature = "pH",
              step.size = 0.3,
              compute.nlm = TRUE,
              ep.method = "envelope")
cametree = came(effects_ph, number.partitions = 2)
plot(cametree)

cametree = ctree(fme ~ . -quality,
                 ctreedf,
                 control = ctree_control(minbucket = 2000))

cametree
plot(cametree)
stablecame = stablelearner::stabletree(cametree, B = 500)
summary(stablecame)

#########

wine_lowalc = wine
wine_lowalc = wine_lowalc[wine_lowalc$alcohol <= 10, ]


plot(effects, jitter = c(0, 0))
wine_highalc = wine
wine_highalc = wine_highalc[wine_highalc$alcohol > 10, ]


ph_results = effects_ph$results
ph_results$alcohol = wine[rownames(wine) %in% effects_ph$results$obs.id, "alcohol"]
ph_results$pH = wine[rownames(wine) %in% effects_ph$results$obs.id, "pH"]
ph_results$alc_ind = as.factor(ifelse(ph_results$alcohol <= 11.4, 0, 1))


mean.lowalc = mean(ph_results[ph_results$alc_ind == 0]$fme)
mean.highalc = mean(ph_results[ph_results$alc_ind == 1]$fme)
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add

min.x1 = min(ph_results$pH)
max.x1 = max(ph_results$pH)
range.fme = diff(range(ph_results$fme))
step.size = 0.3


pfme = ggplot(ph_results) +
  geom_point(aes(x = pH, y = fme, fill = alc_ind),
              colour = "black",
              size = 2.8,
              shape = 21,
              alpha = 0.5) +
  geom_hline(lwd = 1.2, yintercept = mean.lowalc, color = cbbPalette[3]) +
  geom_hline(lwd = 1.2, yintercept = mean.highalc, color = cbbPalette[7]) +
  scale_fill_manual("Alcohol", labels = c("low", "high"), values = cbbPalette[c(3, 7)]) +
  geom_segment(aes(x = (0.5 * min.x1 + 0.5 * max.x1 - 0.5 * step.size),
      xend = (0.5 * min.x1 + 0.5 * max.x1 + 0.5 * step.size),
      y = min(fme) -0.03 * range.fme,
      yend = min(fme) -0.03 * range.fme),
  colour = 'black', size = 1,
  arrow = arrow(length = unit(0.5, "cm")),
  lineend = "round", linejoin = "mitre") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)) +
  ylab("FME")
pfme


mean.lowalc_reduced = mean(newdf_reduced[newdf_reduced$alc_ind == 0]$fme)
mean.highalc_reduced = mean(newdf_reduced[newdf_reduced$alc_ind == 1]$fme)
range.fme_reduced = diff(range(newdf_reduced$fme))
min.x1_reduced = min(newdf_reduced$pH)
max.x1_reduced = max(newdf_reduced$pH)

pfme2 = ggplot(newdf_reduced) +
  geom_point(aes(x = pH, y = fme, fill = alc_ind),
             colour = "black",
             size = 2.8,
             shape = 21,
             alpha = 0.5) +
  geom_hline(lwd = 1.2, yintercept = mean.lowalc_reduced, color = cbbPalette[3]) +
  geom_hline(lwd = 1.2, yintercept = mean.highalc_reduced, color = cbbPalette[7]) +
  scale_fill_manual("Alcohol", labels = c("low", "high"), values = cbbPalette[c(3, 7)]) +
  geom_segment(aes(x = (0.5 * min.x1_reduced + 0.5 * max.x1_reduced - 0.5 * step.size),
                   xend = (0.5 * min.x1_reduced + 0.5 * max.x1_reduced + 0.5 * step.size),
                   y = min(fme) -0.03 * range.fme,
                   yend = min(fme) -0.03 * range.fme),
               colour = 'black', size = 1,
               arrow = arrow(length = unit(0.5, "cm")),
               lineend = "round", linejoin = "mitre") +
  theme_bw() +
  ylab("FME")
pfme2
ggsave(pfme2, file = "wine_pH_FME_alcindicator_reduced.png")

treepart = came(effects_ph, number.partitions = 2)
singlesplit = plot(treepart)



model = svm_tuned
feature = "alcohol"
ep.method = "envelope"
step.size = 1
compute.nlm = TRUE
nlm.intervals = 1
target = "quality"

alcohol_effects = fme(model = svm_tuned,
    data = wine,
    target = "quality",
    feature = "alcohol",
    step.size = 1,
    compute.nlm = TRUE,
    ep.method = "envelope")

plot(alcohol_effects, with.nlm = TRUE)
sd(alcohol_effects$results$fme)

#######

selectedobs = as.data.table(wine[3068, ])
selectedobs_effect = fme(model = svm_tuned,
    data = selectedobs,
    target = "quality",
    feature = "alcohol",
    step.size = 1,
    compute.nlm = TRUE,
    ep.method = "none")

steps.alcohol = seq(0.1, 2.5, 0.1)

steps.alcohol = c(-steps.alcohol, steps.alcohol)

alceffects_selecteddf = data.frame()


for (step in steps.alcohol) {
  selectedobs_effect = fme(
    model = svm_tuned,
    data = selectedobs,
    target = "quality",
    feature = "alcohol",
    step.size = step,
    compute.nlm = TRUE,
    ep.method = "none")

  alceffects_selecteddf = rbind(
    alceffects_selecteddf,
    data.frame("step" = step,
               "fme" = selectedobs_effect$results$fme,
               "nlm" = selectedobs_effect$results$nlm))
}

alceffects_selecteddf
library(tidyverse)
selectedobsplotalc = ggplot() +
  geom_col(data = pivot_longer(alceffects_selecteddf[alceffects_selecteddf$nlm >= 0.9, ], c("fme", "nlm")),
           aes(x = step, fill = name, y = value), position = "dodge", color = "black", width = 0.1) +
  geom_col(data = pivot_longer(alceffects_selecteddf[alceffects_selecteddf$nlm < 0.9, ], c("fme", "nlm")),
           aes(x = step, fill = name, y = value), alpha = 0.4, position = "dodge", width = 0.1) +
  theme_bw() +
  geom_hline(yintercept = 0.9) +
  xlab("Step sizes for alcohol content") +
  ylab("FME | NLM") +
  scale_fill_manual("", labels = c("FME","NLM"), values = c("#E69F00", "#56B4E9")) +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(-2.5, 2.5, 0.1)) +
  scale_y_continuous(breaks = seq(-3, 3, 0.25))

selectedobsplotalc
ggsave(selectedobsplotalc, file = "wine_stepplot_alc_singleobs.png", width = 8, height = 5)

fmegrid = FMEGrid$new(makePredictor(model, wine, target),
                      feature = feature,
                      step.size = step.size,
                      ep.method = ep.method,
                      compute.nlm = compute.nlm,
                      nlm.intervals = nlm.intervals)
fmegrid$createGrid()
fmegrid$grid
fmegrid$computeFMEs()

gigadata = data.table("feature" = c(), "step" = c(), "FME" = c(), "NLM" = c(), "FME.obj" = c())

for (i in 1:nrow(fmegrid$grid)) {
  # print(fmegrid$FME.list[[i]]$results$fme)
  feature = paste(fmegrid$FME.list[[i]]$feature, collapse = " | ")
  n.FMEs = length(fmegrid$FME.list[[i]]$results$fme)
  step = paste(round(fmegrid$FME.list[[i]]$step.size, 3), collapse = " | ")
  d = data.table(feature = rep(feature, n.FMEs), step = step, FME = fmegrid$FME.list[[i]]$results$fme, NLM = fmegrid$FME.list[[i]]$results$nlm)
  gigadata = rbindlist(list(gigadata, d))
}
# }
gigadata
wine_alc_stepplot_fme = ggplot(na.omit(gigadata), aes(x = step, y = FME)) +
  # geom_violin(width = 4, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(outlier.size = 0.5, notch = TRUE) +
  # geom_point(size = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "#E69F00") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)) +
  xlab("Step size for alcohol content")
wine_alc_stepplot_fme
ggsave(wine_alc_stepplot_fme, file = "wine_alcohol_stepplot_fme.png", width = 6, height = 5)



wine_alc_stepplot_nlm = ggplot(na.omit(gigadata), aes(x = step, y = NLM)) +
  # geom_boxplot(width = 4, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(outlier.size = 0.5, notch = TRUE) +
  # geom_point(size = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "#E69F00") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)) +
  xlab("Step size for alcohol content")
wine_alc_stepplot_nlm
ggsave(wine_alc_stepplot_nlm, file = "wine_alcohol_stepplot_nlm.png", width = 6, height = 5)




#####
model = svm_tuned
target = "count"
feature = c("pH", "alcohol")
ep.method = "envelope"
step.size = c(0.3, 1)
compute.nlm = TRUE
nlm.intervals = 1

fmegrid_biv = FMEGrid$new(makePredictor(model, wine, target),
                      feature = feature,
                      step.size = step.size,
                      ep.method = ep.method,
                      compute.nlm = compute.nlm,
                      nlm.intervals = nlm.intervals)

fmegrid_biv$createGrid()
fmegrid_biv$grid
fmegrid_biv$computeFMEs()

fmegrid$FME.list
gigadata = data.table("feature" = c(), "step" = c(), "FME" = c(), "NLM" = c(), "FME.obj" = c())

for (i in 1:nrow(fmegrid_biv$grid)) {
  # print(fmegrid$FME.list[[i]]$results$fme)
  feature = paste(fmegrid_biv$FME.list[[i]]$feature, collapse = " | ")
  n.FMEs = length(fmegrid_biv$FME.list[[i]]$results$fme)
  step = paste(round(fmegrid_biv$FME.list[[i]]$step.size, 3), collapse = " | ")
  d = data.table(feature = rep(feature, n.FMEs), step = step, FME = fmegrid_biv$FME.list[[i]]$results$fme, NLM = fmegrid_biv$FME.list[[i]]$results$nlm)
  gigadata = rbindlist(list(gigadata, d))
}
# }
gigadata
gigadata[which.max(gigadata$FME), ]

selectedfmeobj = fmegrid_biv$FME.list[[36]]
selectedobs = selectedfmeobj$results[which.max(selectedfmeobj$results$fme), ]
selectedobs$pH = wine[selectedobs$obs.id, "pH"]
selectedobs$alcohol = wine[selectedobs$obs.id, "alcohol"]

selectedobs_aggregate = data.frame()
for (i in 1:36) {
  # print(fmegrid_biv$FME.list[[i]]$results[fmegrid_biv$FME.list[[i]]$results$obs.id == selectedobs$obs.id, ])
  fmeres = fmegrid_biv$FME.list[[i]]$results[fmegrid_biv$FME.list[[i]]$results$obs.id == selectedobs$obs.id, ]
  fmeres$step = paste(round(fmegrid_biv$FME.list[[i]]$step.size, 3), collapse = " | ")
  selectedobs_aggregate = rbind(selectedobs_aggregate, fmeres)


}
library(tidyverse)

selectedobs_aggregate
selectedobsplot = ggplot() +
  geom_col(data = pivot_longer(selectedobs_aggregate[selectedobs_aggregate$nlm >= 0.9], c("fme", "nlm")),
           aes(x = step, fill = name, y = value), position = "dodge", color = "black", width = 0.8) +
  geom_col(data = pivot_longer(selectedobs_aggregate[selectedobs_aggregate$nlm < 0.9], c("fme", "nlm")),
           aes(x = step, fill = name, y = value), alpha = 0.4, position = "dodge", width = 0.8) +
  theme_bw() +
  geom_hline(yintercept = 0.9) +
  xlab("Bivariate step sizes for (pH value | alcohol content)") +
  ylab("FME | NLM") +
  scale_fill_manual("", labels = c("FME","NLM"), values = c("#E69F00", "#56B4E9")) +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 8), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = c(0, 0.9, 1, 2, 3, 4))
selectedobsplot

wine_ph_alc_stepplot_fme = ggplot(na.omit(gigadata), aes(x = step, y = FME)) +
  # geom_violin(width = 4, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(outlier.size = 0.5, notch = TRUE) +
  # geom_point(size = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "#E69F00") +
  theme_bw() +
  xlab("Bivariate step sizes for (pH value | alcohol content)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
wine_ph_alc_stepplot_fme




wine_ph_alc_stepplot_nlm = ggplot(na.omit(gigadata), aes(x = step, y = NLM)) +
  # geom_violin(width = 4, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(outlier.size = 0.5, notch = TRUE) +
  # geom_point(size = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 2, color = "#E69F00") +
  theme_bw() +
  xlab("Bivariate step sizes for (pH value | alcohol content)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
wine_ph_alc_stepplot_nlm



biveffects = fme(svm_tuned, wine, "quality", feature = c("pH", "alcohol"), step.size = c(0.19, 1.9), compute.nlm = TRUE)
biveffects
bivplot = plot(biveffects, with.nlm = FALSE)
bivplot = bivplot +
  scale_fill_gradientn(
    "FME",
    colors = c("#0072B2", "white", "#E69F00"),
    breaks = c(2, 0, -2),
  )
bivplot


fme(model = svm_tuned,
    data = wine,
    target = "quality",
    feature = "alcohol",
    step.size = 1,
    compute.nlm = TRUE,
    ep.method = "envelope")

plot(alcohol_effects, with.nlm = TRUE)
sd(alcohol_effects$results$fme)

potassiumeffects = fme(model = svm_tuned,
                         data = wine,
                         target = "quality",
                         feature = "sulphates",
                         step.size = 0.14,
                         compute.nlm = TRUE,
                         ep.method = "envelope")

steps.alcohol = seq(0.1, 2.5, 0.1)
