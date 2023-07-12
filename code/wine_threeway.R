
simpsonRule = function(fn) {
  (1 / 8) * (fn(0) + 3 * fn(1/3) + 3 * fn(2/3) + fn(1))
}

testEnvelopeNum = function(train, test, features, step.size) {
  test_intervened = test
  for (j in 1:length(features)) {
    feature = features[j]
    test_intervened[ , feature] = test[ , feature] + step.size[[j]]
  }
  is_outside_train_env = rep(FALSE, nrow(test))
  num.features = colnames(test)[unlist(lapply(test, is.numeric))]
  for (i in 1:nrow(test)) {
    for (feature in num.features) {
      feature.range = range(train[, feature])
      if (test[i, feature] < feature.range[1] || test[i, feature] > feature.range[2]) {
        is_outside_train_env[i] = TRUE
      }
      if (test_intervened[i, feature] < feature.range[1] | test_intervened[i, feature] > feature.range[2]) {
        is_outside_train_env[i] = TRUE
      }
    }
  }

  return.df = test[!is_outside_train_env, !colnames(test) %in% 'id']
  return(return.df)
}

testEnvelopeCateg = function(train, test, level) {

  unif.df = train
  data.num = unlist(lapply(train, is.numeric))
  data.num = train[data.num]
  data.categ = unlist(lapply(train, is.factor))
  data.categ = train[data.categ]

  for (col in colnames(data.num)) {
    col.data = data.num[, col]
    col.range = range(col.data)
    col.min = col.range[1]
    col.max = col.range[2]
    unif.sample = runif(nrow(train), col.min, col.max)
    unif.df[, col] = unif.sample
  }

  for (col in colnames(data.categ)) {
    col.data = data.categ[, col]
    col.levels = levels(col.data)
    p = 1 / length(col.levels)
    unif.sample = sample(x = col.levels,
                         size = nrow(bh.data),
                         replace = TRUE,
                         prob = rep(p, length(col.levels)))
    unif.df[, col] = as.factor(unif.sample)
  }

  unif.df$class = 'Extrapolation'
  train$class = 'Non-Extrapolation'
  train$class = as.factor(train$class)
  combined.df = as.data.frame(rbind(train, unif.df))
  library(randomForest)
  extrap_risk_mod = randomForest(class ~ ., data = combined.df)
  extrap_preds_test = predict(extrap_risk_mod, newdata = combined.df)
  extrap_preds_test = (extrap_preds_test == 'Non-Extrapolation' )

  return(test[extrap_preds_test, ])
}

marginalEffect = function(feature, step.size, data, model) {
  prediction.before = predict(model, newdata = data)
  data.intervened = as.data.frame(data)
  data.intervened[ , feature] = data.intervened[ , feature] + as.numeric(step.size)
  prediction.after = predict(model, newdata = data.intervened)
  marginal.effects = prediction.after - prediction.before
  return(marginal.effects)
}

marginalEffectCateg = function(feature, level, data, model) {
  prediction.before = predict(model, newdata = data[data[, feature] != level, ])
  data.intervened = data[data[, feature] != level, ]
  if (nrow(data.intervened) == 0) {
    return(NULL)
  }

  data.intervened[ , feature] = factor(level, levels = levels(data[ , feature]))
  prediction.after = predict(model, newdata = data.intervened)
  marginal.effects = prediction.after - prediction.before
  return(marginal.effects)
}

parametrizedFeatures = function(observation, feature, step_size, t) {
  observation_new = observation
  observation_new[, feature] = observation_new[, feature] + t * step_size
  return(observation_new)
}

predFunctionMinusSecantSquared = function(model, observation, feature, step_size, t) {

  modified_obs = parametrizedFeatures(observation, feature, step_size, t)  # x(t)
  model_prediction = predict(model, newdata = modified_obs) # f(x(t))

  secant_value = secant(observation, step_size, feature, model, t) # g(x(t))
  return_value = (model_prediction - secant_value)^2 # f(x(t) - g(x(t)))^2

  return(return_value)
}

predFunctionMinusMeanSquared = function(model, observation, feature, step_size, t) {

  modified_obs = parametrizedFeatures(observation, feature, step_size, t) # x(t)
  model_prediction = predict(model, newdata = modified_obs) # f(x(t))

  mean_value = predMean(model, observation, feature, step_size) # mean_f
  return_value = (model_prediction - mean_value)^2 # (f(x(t)) - mean_f)^2

  return(return_value)
}

predMean = function(model, observation, feature, step_size) {
  fn = function(t) {
    modified_obs = parametrizedFeatures(observation, feature, step_size, t) # x(t)
    model_predictions = predict(model, newdata = modified_obs) # f(x(t))
    return (model_predictions)
  }
  integral = simpsonRule(fn)
  return(integral)
}

secant = function(observation, step_size, feature, model, m) {
  target_start = predict(model, newdata = observation)
  secant_value = as.vector(target_start) + (m * as.vector(marginalEffect(feature, step_size, observation, model)))
  return(secant_value)
}

nonLinearityMeasure = function(model, observation, feature, step_size) {
  fn_1 = function(t) {
    predFunctionMinusSecantSquared(model, observation, feature, step_size, t)
  }
  fn_2 = function(t) {
    predFunctionMinusMeanSquared(model, observation, feature, step_size, t)
  }
  pred_minus_secant_squared = simpsonRule(fn_1)
  pred_minus_mean_squared = simpsonRule(fn_2)
  nlm = 1 - (pred_minus_secant_squared / pred_minus_mean_squared)
  nlm = as.numeric(nlm)
  return(nlm)
}

nonLinearityMeasureDataFrame = function(model, data, feature, step_size) {

  nlm = list(length = nrow(data))
  for (i in 1:nrow(data)) {
    cat(".")
    nlm[i] = nonLinearityMeasure(model = model, observation = data[i, ], feature = feature, step_size = step_size)
  }
  nlm = unlist(nlm)
  nlm = unname(nlm)
  return(nlm)
}
###

library(fmeffects)
wine = read.table("data/winequality-white.csv", sep = ";", header = TRUE)
winetask = as_task_regr(x = wine, id = "wine", target = "quality")

load("tuning_data/wine_tuning.Rdata")

svm_tuned = lrn("regr.svm")
svm_tuned$param_set$values = instance$result_learner_param_vals
svm_tuned$train(winetask)


feature = c("pH", "alcohol", "sulphates")
set.seed(123)
step.size = c(0.3, 1, 0.4)
data.threeway = testEnvelopeNum(wine, wine, feature, step.size)
fme.threeway = marginalEffect(feature = feature, step.size = step.size, data = data.threeway, model = svm_tuned)
nlm.threeway = nonLinearityMeasureDataFrame(model = svm_tuned, data = data.threeway, feature = feature, step_size = step.size)
df.threeway = data.frame('pH' = data.threeway$pH, 'alcohol' = data.threeway$alcohol, 'sulphates' = data.threeway$sulphates, 'me' = fme.threeway, 'nlm' = nlm.threeway)

library(latex2exp)
plot_threeway_fme = ggplot(df.threeway, aes(me)) +
  geom_density(fill = "#56B4E9", alpha = 0.3) +
  geom_vline(xintercept = mean(df.threeway$me)) +
  geom_label(x = mean(df.threeway$me), y = 0, label = 'AME', fill = 'white') +
  xlab(TeX("FME with step size $h_{pH} = 0.3$, $h_{alcohol} = 1$, $h_{sulphates} = 0.5$")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))
plot_threeway_fme

plot_threeway_nlm = ggplot(df.threeway, aes(nlm)) +
  geom_density(fill = "#56B4E9", alpha = 0.3) +
  geom_vline(xintercept = mean(df.threeway$nlm)) +
  geom_label(x = mean(df.threeway$nlm), y = 0, label = 'ANLM', fill = 'white') +
  xlab(TeX("NLM with step size $h_{pH} = 0.3$, $h_{alcohol} = 1$, $h_{sulphates} = 0.5$")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        text = element_text(size = 15))

