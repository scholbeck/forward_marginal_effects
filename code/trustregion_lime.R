library(mlbench)
library(caret)
library(lime)
# Simulate data
# set.seed(101)  # for reproducibility
# trn <- as.data.frame(mlbench.friedman1(n = 500, sd = 1))
#
# model_caret <- train(y  ~ ., data = trn,
#                      method = "ranger")
#
# # lime explanation
# limemodel = lime(trn, model_caret)
# explain(trn[50, ], limemodel, n_features = 15)
#
# library(devtools)
# load_all()
#
# effects = fme(model = model_caret,
#               data = trn[49:51, ],
#               target = "y",
#               feature = "x.1",
#               step.size = 0.1,
#               ep.method = "none",
#               compute.nlm = TRUE)
#
#
# effects$results[2, ]
#
# trn[50, ]
#
# 10*sin(pi*(trn[50, 1] + 0.1)*trn[50, 2]) - 10*sin(pi*trn[50, 1]*trn[50, 2])
#

#####


kernel = function(d, kernel_width){
  sqrt(exp(-(d^2) / kernel_width^2))
}

get_distances = function(point_explain, points_sample){
  # euclidean distance
  apply(points_sample, 1, function(x){
    sum((point_explain - x)^2)
  })
}

get_y = function(x1, x2, noise_prob = 0){
  y = sign(sign(x2-1+abs(x1*2))/3 - sign(x2-.5+abs(x1*3))/3) + 1
  y = y * (1 - rbinom(length(x1), 1, prob = noise_prob))
  # flip classes
  y = 1 - y
  y
}
# ## Creating dataset ###########################################################
# library("dplyr")
# library("ggplot2")
# # Define range of set
# lower_x1 = -2
# upper_x1 = 2
# lower_x2 = -2
# upper_x2 = 1
# # Size of the training set for the black box classifier
# n_training  = 20000
# # Size for the grid to plot the decision boundaries
# n_grid = 100
# # Number of samples for LIME explanations
# n_sample = 500
# # Simulate y ~ x1 + x2
# set.seed(1)
# x1 = runif(n_training, min = lower_x1, max = upper_x1)
# x2 = runif(n_training, min = lower_x2, max = upper_x2)
# y = get_y(x1, x2)
# # Add noise
# y_noisy = get_y(x1, x2, noise_prob = 0.01)
# lime_training_df = data.frame(x1=x1, x2=x2, y=as.factor(y), y_noisy=as.factor(y_noisy))
# # For scaling later on
# x_means = c(mean(x1), mean(x2))
# x_sd = c(sd(x1), sd(x2))
# # Learn model
# rf = randomForest::randomForest(y_noisy ~ x1 + x2, data = lime_training_df, ntree=100)
# lime_training_df$predicted = predict(rf, lime_training_df)
# # The decision boundaries
# grid_x1 = seq(from=lower_x1, to=upper_x1, length.out=n_grid)
# grid_x2 = seq(from=lower_x2, to=upper_x2, length.out=n_grid)
# grid_df = expand.grid(x1 = grid_x1, x2 = grid_x2)
# grid_df$predicted = as.numeric(as.character(predict(rf, newdata = grid_df)))
# # The observation to be explained
# explain_x1 = 1
# explain_x2 = -0.5
# explain_y_model = predict(rf, newdata = data.frame(x1=explain_x1, x2=explain_x2))
# df_explain = data.frame(x1=explain_x1, x2=explain_x2, y_predicted=explain_y_model)
# point_explain = c(explain_x1, explain_x2)
# point_explain_scaled = (point_explain - x_means) / x_sd
# # Drawing the samples for the LIME explanations
# x1_sample = rnorm(n_sample, x_means[1], x_sd[1])
# x2_sample = rnorm(n_sample, x_means[2], x_sd[2])
# df_sample = data.frame(x1 = x1_sample, x2 = x2_sample)
# # Scale the samples
# points_sample = apply(df_sample, 1, function(x){
#   (x - x_means) / x_sd
# }) %>% t
# # Add weights to the samples
# kernel_width = sqrt(dim(df_sample)[2]) * 0.15
# distances = get_distances(point_explain_scaled,
#                           points_sample = points_sample)
# df_sample$weights = kernel(distances, kernel_width=kernel_width)
# df_sample$predicted = predict(rf, newdata = df_sample)
# # Trees
# # mod = rpart(predicted ~ x1 + x2, data = df_sample,  weights = df_sample$weights)
# # grid_df$explained = predict(mod, newdata = grid_df, type='prob')[,2]
# # Logistic regression model
# mod = glm(predicted ~ x1 + x2, data = df_sample,  weights = df_sample$weights, family='binomial')
# grid_df$explained = predict(mod, newdata = grid_df, type='response')
# # logistic decision boundary
# coefs = coefficients(mod)
# logistic_boundary_x1 = grid_x1
# logistic_boundary_x2 = -  (1/coefs['x2']) * (coefs['(Intercept)'] + coefs['x1'] * grid_x1)
# logistic_boundary_df = data.frame(x1 = logistic_boundary_x1, x2 = logistic_boundary_x2)
# logistic_boundary_df = filter(logistic_boundary_df, x2 <= upper_x2, x2 >= lower_x2)
# # Create a smaller grid for visualization of local model boundaries
# x1_steps = unique(grid_df$x1)[seq(from=1, to=n_grid, length.out = 20)]
# x2_steps = unique(grid_df$x2)[seq(from=1, to=n_grid, length.out = 20)]
# grid_df_small = grid_df[grid_df$x1 %in% x1_steps & grid_df$x2 %in% x2_steps,]
# grid_df_small$explained_class = round(grid_df_small$explained)
# colors = c('#132B43', '#56B1F7')
# # Data with some noise
# p_data = ggplot(lime_training_df) +
#   geom_point(aes(x=x1,y=x2,fill=y_noisy, color=y_noisy), alpha =0.3, shape=21) +
#   scale_fill_manual(values = colors) +
#   scale_color_manual(values = colors) +
#   my_theme(legend.position = 'none')
# # The decision boundaries of the learned black box classifier
# p_boundaries = ggplot(grid_df) +
#   geom_raster(aes(x=x1,y=x2,fill=predicted), alpha = 0.3, interpolate=TRUE) +
#   my_theme(legend.position='none') +
#   ggtitle('A')
# # Drawing some samples
# p_samples = p_boundaries +
#   geom_point(data = df_sample, aes(x=x1, y=x2)) +
#   scale_x_continuous(limits = c(-2, 2)) +
#   scale_y_continuous(limits = c(-2, 1))
# # The point to be explained
# p_explain = p_samples +
#   geom_point(data = df_explain, aes(x=x1,y=x2), fill = 'yellow', shape = 21, size=4) +
#   ggtitle('B')
# p_weighted = p_boundaries +
#   geom_point(data = df_sample, aes(x=x1, y=x2, size=weights)) +
#   scale_x_continuous(limits = c(-2, 2)) +
#   scale_y_continuous(limits = c(-2, 1)) +
#   geom_point(data = df_explain, aes(x=x1,y=x2), fill = 'yellow', shape = 21, size=4) +
#   ggtitle('C')
# p_boundaries_lime = ggplot(grid_df)  +
#   geom_raster(aes(x=x1,y=x2,fill=predicted), alpha = 0.3, interpolate=TRUE) +
#   geom_point(aes(x=x1, y=x2, color=explained), size = 2, data = grid_df_small[grid_df_small$explained_class==1,], shape=3) +
#   geom_point(aes(x=x1, y=x2, color=explained), size = 2, data = grid_df_small[grid_df_small$explained_class==0,], shape=95) +
#   geom_point(data = df_explain, aes(x=x1,y=x2), fill = 'yellow', shape = 21, size=4) +
#   geom_line(aes(x=x1, y=x2), data =logistic_boundary_df, color = 'white')


####


dataGeneratingProcessMultiple = function(data) {
  y = vector(length = nrow(data))
  for (i in 1:nrow(data)) {
    y[i] = dataGeneratingProcessSingle(data[i, ])
  }
  return(unlist(y))
}

marginalEffectDataGenProcess = function(f, feature, step.size, data) {
  y.before = f(data)
  data.intervened = as.data.frame(data)
  data.intervened[, feature] = data.intervened[, feature] + step.size
  y.after = f(data.intervened)
  marginal.effects = y.after - y.before
  return(marginal.effects)
}

parametrizedFeatures = function(observation, feature, step_size, t) {
  observation_new = observation
  observation_new[, feature] = observation_new[, feature] + t * step_size
  return(observation_new)
}

dataGenProcessMinusSecantSquared = function(f, observation, feature, step_size, t) {

  modified_obs = parametrizedFeatures(observation, feature, step_size, t)  # x(t)
  y = f(modified_obs) # f(x(t))

  secant_value = secantDataGenProcess(observation, step_size, feature, f, t) # g(x(t))
  return_value = (y - secant_value)^2 # f(x(t) - g(x(t)))^2

  return(return_value)
}

dataGenProcessMinusMeanSquared = function(f, observation, feature, step_size, t) {

  modified_obs = parametrizedFeatures(observation, feature, step_size, t) # x(t)
  y = f(modified_obs) # f(x(t))

  mean_value = yMean(f, observation, feature, step_size) # mean_f
  return_value = (y - mean_value)^2 # (f(x(t)) - mean_f)^2

  return(return_value)
}

yMean = function(f, observation, feature, step_size) {
  fn = function(t) {
    modified_obs = parametrizedFeatures(observation, feature, step_size, t) # x(t)
    y = f(modified_obs) # f(x(t))
    return (y)
  }
  integral = simpsonRule(fn)
  return(integral)
}

secantDataGenProcess = function(observation, step_size, feature, f, m) {
  target_start = f(observation)
  secant_value = as.vector(target_start) + (m * as.vector(marginalEffectDataGenProcess(f, feature, step_size, observation)))
  return(secant_value)
}

nonLinearityMeasureDataGenProcess = function(f, observation, feature, step_size) {
  fn_1 = function(t) {
    dataGenProcessMinusSecantSquared(f, observation, feature, step_size, t)
  }
  fn_2 = function(t) {
    dataGenProcessMinusMeanSquared(f, observation, feature, step_size, t)
  }
  pred_minus_secant_squared = simpsonRule(fn_1)
  pred_minus_mean_squared = simpsonRule(fn_2)
  nlm = 1 - (pred_minus_secant_squared / pred_minus_mean_squared)
  nlm = as.numeric(nlm)
  return(nlm)
}

nonLinearityMeasureDataFrameDataGenProcess = function(f, data, feature, step_size) {

  nlm = list(length = nrow(data))
  for (i in 1:nrow(data)) {
    cat(".")
    nlm[i] = nonLinearityMeasureDataGenProcess(f, observation = data[i, ], feature = feature, step_size = step_size)
  }
  nlm = unlist(nlm)
  nlm = unname(nlm)
  return(nlm)
}


simpsonRule = function(fn) {
  (1 / 8) * (fn(0) + 3 * fn(1/3) + 3 * fn(2/3) + fn(1))
}


####
set.seed(42)
n = 200
x = x = rnorm(200, mean = 0, sd = 3)
df = data.frame(x, 'y' = NA)
df$x[df$x < -5] = -5
dataGeneratingProcessSingle = function(data) {
  x = data$x
  if (x <= 1) {
    y = (x + 2)^2
  } else if (x > 1) {
    y = -x + 10 - 0.05 * x^2
    # df$y[df$x > 1] = -df$x[df$x > 1] + 10 + - 0.05 * df$x[df$x > 1]^2
  }
  return(y)
}

df$y = dataGeneratingProcessMultiple(df)


#df$y = df$y + rnorm(nrow(df), sd = 0.05)
explain.p = data.frame(x = 1.6, y = 8.5)
w1 = kernel(get_distances(data.frame(x = explain.p$x), df), 0.1)
w2 = kernel(get_distances(data.frame(x = explain.p$x), df), 0.75)
w3 = kernel(get_distances(data.frame(x = explain.p$x), df), 2)
lm.1 = lm(y ~ x, data = df, weights = w1)
lm.2 = lm(y ~ x, data = df, weights = w2)
lm.3 = lm(y ~ x, data = df, weights = w3)
df.all = rbind(df, df, df)
df.all$lime = c(predict(lm.1), predict(lm.2), predict(lm.3))
df.all$width = factor(c(rep(c(0.1, 0.75, 2), each = nrow(df))))

step.vector = seq(diff(sort(x))[2], IQR(x), (IQR(x) - min(diff(sort(x)))) / 5)
step.vector = c(-step.vector, step.vector)

me_nlm_df = data.frame("step" = NA, "step_location_x" = NA, "step_location_y" = NA, "me" = NA, "nlm" = NA)
for (step in step.vector) {
  me = marginalEffectDataGenProcess(dataGeneratingProcessMultiple, "x", step, explain.p)
  nlm = nonLinearityMeasureDataFrameDataGenProcess(dataGeneratingProcessMultiple, data = explain.p, "x", step)
  # df = rbind(me_nlm_df, c(me, nlm))
  me_nlm_df = rbind(me_nlm_df, c(step, explain.p$x + step, explain.p$y + me, me, nlm))
}
me_nlm_df = na.omit(me_nlm_df)

me = marginalEffectDataGenProcess(dataGeneratingProcessMultiple, "x", -1, explain.p)
nlm = nonLinearityMeasureDataFrameDataGenProcess(dataGeneratingProcessMultiple, data = explain.p, "x", 3)


#



# -(1.6 + 0.1) + 10 + - 0.05 * (1.6 + 0.1)^2 - (-1.6 + 10 - 0.05 * 1.6^2)
#
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(viridis)
limeplot = ggplot(df.all, aes(x = x, y = y)) +
  geom_line(size = 2.5) +
  geom_line(size = 2, color = "gold") +
  geom_rug(sides = "b") +
  geom_line(aes(x = x, y = lime, color = width), size = 2) +
  geom_point(data = explain.p, aes(x = x, y = y), size = 5) +
  geom_segment(x = explain.p$x, xend = explain.p$x, y = -Inf, yend = explain.p$y, linetype = "dashed") +
  geom_segment(x = -Inf, xend = explain.p$x, y = explain.p$y, yend = explain.p$y, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-5, 8, 1)) +
  scale_y_continuous(breaks = seq(0, 15, 2), "Predicted target") +
  scale_color_manual("LIME kernel width", values = c("#CC79A7", "#0072B2", "#D55E00")) +
  theme_bw() +
  geom_segment(data = me_nlm_df[me_nlm_df$nlm > 0.9, ], aes(x = explain.p$x, xend = step_location_x, y = explain.p$y, yend = step_location_y), arrow = arrow(length = unit(0.4, "cm")), linewidth = 1) +
  theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 12))
limeplot
ggsave(limeplot, file = "fme_trust_region_lime.png", width = 5, height = 5)

library(tidyverse)

selectedobsplot = ggplot() +
  geom_col(data = pivot_longer(me_nlm_df[me_nlm_df$nlm >= 0.9, ], c("me", "nlm")),
           aes(x = step, fill = name, y = value), position = "dodge", color = "black", width = 0.5) +
  geom_col(data = pivot_longer(me_nlm_df[me_nlm_df$nlm < 0.9, ], c("me", "nlm")),
           aes(x = step, fill = name, y = value), alpha = 0.4, position = "dodge", width = 0.5) +
  theme_bw() +
  geom_hline(yintercept = 0.9) +
  xlab("Step size for x") +
  ylab("FME | NLM") +
  scale_fill_manual("", labels = c("FME","NLM"), values = c("#E69F00", "#56B4E9")) +
  theme(legend.position = "bottom", axis.title = element_text(size = 12), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = c(0.9, 0, -2, -4, -6, -8)) +
  scale_x_continuous(breaks = round(me_nlm_df$step, 3))
selectedobsplot

ggsave(selectedobsplot, file = "fme_trust_region_lime_barplot.png", width = 5, height = 5)



