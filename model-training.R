




trainData$rush_hour = as.factor(trainData$rush_hour)
#preparedData$rush_hour = as.factor(preparedData$is_weekend)

task = makeClassifTask(
  id = 'predict if a trip was done at a rush hour',
  #data = select(trainData,rush_hour,trip_seconds,average_speed_miles_hour,pickup_community_area_factor,dropoff_community_area_factor,is_weekend,fare),
  #data = select(trainData,rush_hour,average_speed_miles_hour),
  #data = select(trainData,rush_hour,pickup_community_area_factor),
  data = select(trainData,rush_hour,trip_seconds,average_speed_miles_hour,pickup_community_area_factor,dropoff_community_area_factor,fare),
  target = "rush_hour",
  weights = NULL,
  blocking = NULL,
  coordinates = NULL,
  positive = 1,
  fixup.data = "warn",
  check.data = TRUE
)

learner = makeLearner(
  cl = 'classif.h2o.randomForest',
  id = 'Naive Bayes Learner',
  predict.type = 'response',
  predict.threshold = NULL,
  fix.factors.prediction = FALSE,
  par.vals = list()
)

resampling_strategy = makeResampleDesc("CV", iters = 5)

resample_result = mlr::resample(
  learner = learner,
  task = task,
  resampling = resampling_strategy,
  measures = list(bac,tnr,tpr),
  keep.pred = TRUE
)

theData = select(trainData,rush_hour,pickup_community_area_factor)