
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