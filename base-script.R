###################################################################################
#Load libraries
###################################################################################
library(tidyverse)
library(lubridate)
library(summarytools)
library(ggmap)
library(mlr)
# library(e1071)
# library(mmpf)
# library(party)
# library(h2o)
# library(xgboost)
# library(FSelectorRcpp)
library(glmnet)
library(evtree)
library(SwarmSVM)
library(LiblineaR)
library(RWeka)
library(geosphere)

###################################################################################
#Set script options
###################################################################################
options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting
set.seed(2020) # set seed so results are reproducible

###################################################################################
#Functions
###################################################################################
holyDaysChicago2018 =c(dmy("01/01/2018"),dmy("15/01/2018"),dmy("12/02/2018"),dmy("19/02/2018"),dmy("05/03/2018"),
                       dmy("28/05/2018"),dmy("04/07/2018"),dmy("03/09/2018"),dmy("06/11/2018"),dmy("12/11/2018"),
                       dmy("22/11/2018"),dmy("05/12/2018"),dmy("25/12/2018"))

holyDaysChicago2019 =c(dmy("01/01/2019"),dmy("21/01/2019"),dmy("12/02/2019"),dmy("18/02/2019"),dmy("04/03/2019"),
                       dmy("27/05/2019"),dmy("04/07/2019"),dmy("02/09/2019"),dmy("14/10/2019"),dmy("11/11/2019"),
                       dmy("28/11/2019"),dmy("25/12/2019"))

calculateDistance = function(lon1,lat1,lon2,lat2) {
  return(distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine))
}

isOnWeekend = function(date){
  
  if(wday(date) == 1 | wday(date) == 7){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

isAHolyDay = function(date){
  
  if(is.element(date(date),holyDaysChicago2018) | is.element(date(date),holyDaysChicago2019)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

isAWorkingDay = function(date){
  if(!isOnWeekend(date) && !isAHolyDay(date)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

isAUsuallyCongestedHour = function(date){
  
  if(between(hour(date),7,8) | between(hour(date),16,17) | (hour(date) == 9 & minute(date) == 0 & second(date) == 0) | (hour(date) == 18 & minute(date) == 0 & second(date) == 0)){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}

isRushHour = function(trip_start_timestamp) {
  
  sapply(trip_start_timestamp, function(trip_start_timestamp){
    
    if(isAWorkingDay(trip_start_timestamp) && isAUsuallyCongestedHour(trip_start_timestamp)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
}

prepareForMlr = function(df){
  df_prepared = df %>% 
    #select(-id) %>% 
    mutate_if(is.Date, decimal_date) %>% 
    mlr::impute(
      classes = list(
        # for each data type, specify a "standard" imputation method to apply
        ## As an example, we'll set NAs in character columns to 'unknown' and numeric columns to their mean
        character = imputeConstant('none'),
        integer = imputeMean(),
        numeric = imputeMean()
      ),
      cols = list(
        # for columns that should NOT use the standard method based on its type, you can overwrite it
        # example: let's impute missing `frequency` values with 0 instead of the mean:
        #cap_share_of_country_gen_by_fuel = imputeConstant(0),
        #cap_share_of_country_gen_total = imputeConstant(0)
      )
    ) %>% .$data %>% as_tibble() %>% #return a data frame instead of mlr's imputation object
    mutate_if(is.character, as_factor)
}

###################################################################################
#So it begins
###################################################################################

#Uncomment this!
#setwd("C:/Users/prest/OneDrive/Escritorio/Analytics Cup/analytics-cup")

#write_csv(trainData, getwd(), na = "NA", append = FALSE, col_names = !append,
#          quote_escape = "double")
#trainData = read_csv('trainWithRush.csv')

trainData = read_csv('train.csv')

#Add rush hour column
trainData = 
  trainData %>%
  mutate(rush_hour = case_when(
    isRushHour(trip_start_timestamp) ~ 1,
    TRUE ~ 0
  ))

############################################################################
#Data cleaning and preparation
############################################################################

#Add column isWeekend
trainData = 
  trainData %>%
  mutate(is_weekend = case_when(
    wday(trip_start_timestamp) == 1 | wday(trip_start_timestamp) == 7 ~ 1,
    TRUE ~ 0
  ))

#Add column isholyday
trainData = 
  trainData %>%
  mutate(is_holyday = case_when(
    is.element(date(trip_start_timestamp),holyDaysChicago2018) | is.element(date(trip_start_timestamp),holyDaysChicago2019) ~ 1,
    TRUE ~ 0
  ))

#Add column is_weekend_or_Holyday
trainData = 
  trainData %>%
  mutate(is_weekend_or_holyday = case_when(
    is_holyday == 1 | is_weekend == 1 ~ 1,
    TRUE ~ 0
  ))

#Add columns as factors
trainData = mutate(trainData,is_weekend_or_holyday_factor = as.factor(trainData$is_weekend_or_holyday))
trainData = mutate(trainData,pickup_community_area_factor = as.factor(trainData$pickup_community_area))
trainData = mutate(trainData,dropoff_community_area_factor = as.factor(trainData$dropoff_community_area))
trainData = mutate(trainData,payment_type_factor = as.factor(trainData$payment_type))

#clean trip_seconds
trainData = filter(trainData,trip_seconds != 0,trip_seconds < 5000)

#clean trip_miles
trainData = trainData %>% rowwise() %>% mutate(calculated_distance = calculateDistance(pickup_centroid_longitude,pickup_centroid_latitude,dropoff_centroid_longitude,dropoff_centroid_latitude)/1609)

trainData = 
  trainData %>%
  mutate(trip_miles = case_when(
    is.na(trip_miles) | trip_miles == 0 ~ calculated_distance,
    TRUE ~ trip_miles
  ))

trainData = filter(trainData,trip_miles != 0,trip_miles < 40, !is.na(trip_miles))

#Add column average_speed_miles_second
trainData = trainData %>%
  mutate(average_speed_miles_second = trip_miles/trip_seconds
  )

#Add column average_speed_miles_hour
trainData = trainData %>%
  mutate(average_speed_miles_hour = trip_miles/(trip_seconds/3600)
  )

#clean column average_speed_miles_hour
trainData = filter(trainData,average_speed_miles_hour < 70)

#Add columns as factors for tests
#trainData = mutate(trainData,is_weekend_or_holyday_factor = as.factor(trainData$is_weekend_or_holyday))

#trainData = prepareForMlr(trainData)

###################################################################################
#Model Training and evaluation
###################################################################################

trainData$rush_hour = as.factor(trainData$rush_hour)
#preparedData$rush_hour = as.factor(preparedData$is_weekend)

task = makeClassifTask(
  id = 'predict if a trip was done at a rush hour',
  #data = select(trainData,rush_hour,trip_seconds,average_speed_miles_hour,pickup_community_area_factor,dropoff_community_area_factor,is_weekend,fare),
  #data = select(trainData,rush_hour,average_speed_miles_hour),
  #data = select(trainData,rush_hour,pickup_community_area_factor),
  #data = select(trainData,rush_hour,trip_seconds,average_speed_miles_hour,pickup_community_area_factor,dropoff_community_area_factor,fare),
  #data = select(trainData,rush_hour,average_speed_miles_hour),
  #data = select(trainData,rush_hour,fare,trip_seconds,trip_miles,pickup_community_area_factor,dropoff_community_area_factor,is_weekend_or_holyday),
  data = select(trainData,rush_hour,trip_miles,trip_seconds,is_weekend_or_holyday,pickup_community_area_factor,dropoff_community_area_factor), 
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
  id = 'Random Forest',
  predict.type = 'response',
  predict.threshold = NULL,
  fix.factors.prediction = FALSE,
  par.vals = list()
)

#Uncomment this!
# resampling_strategy = makeResampleDesc("CV", iters = 5)
# 
# resample_result = mlr::resample(
#   learner = learner,
#   task = task,
#   resampling = resampling_strategy,
#   measures = list(bac,tnr,tpr),
#   keep.pred = TRUE
# )

model = mlr::train(learner, task)

###################################################################################
#Predictions
###################################################################################

#pd = generatePartialDependenceData(model, task)
#plot(model$learner.model, compress = TRUE)
#text(model$learner.model, use.n = TRUE)
#predict(model, task)$data %>% ggplot(aes(x=truth, y=response)) + geom_point() +stat_function(fun=identity)

testData = read_csv('test.csv')

#Data preparation
#Add column isWeekend
testData = 
  testData %>%
  mutate(is_weekend = case_when(
    wday(trip_start_timestamp) == 1 | wday(trip_start_timestamp) == 7 ~ 1,
    TRUE ~ 0
  ))

#Add column isholyday
testData = 
  testData %>%
  mutate(is_holyday = case_when(
    is.element(date(trip_start_timestamp),holyDaysChicago2018) | is.element(date(trip_start_timestamp),holyDaysChicago2019) ~ 1,
    TRUE ~ 0
  ))

#Add column is_weekend_or_Holyday
testData = 
  testData %>%
  mutate(is_weekend_or_holyday = case_when(
    is_holyday == 1 | is_weekend == 1 ~ 1,
    TRUE ~ 0
  ))

#Add columns as factors
testData = mutate(testData,is_weekend_or_holyday_factor = as.factor(testData$is_weekend_or_holyday))
testData = mutate(testData,pickup_community_area_factor = as.factor(testData$pickup_community_area))
testData = mutate(testData,dropoff_community_area_factor = as.factor(testData$dropoff_community_area))
testData = mutate(testData,payment_type_factor = as.factor(testData$payment_type))

#Calculate speed when missing
testData = testData %>% rowwise() %>% mutate(calculated_distance = calculateDistance(pickup_centroid_longitude,pickup_centroid_latitude,dropoff_centroid_longitude,dropoff_centroid_latitude)/1609)

testData = 
  testData %>%
  mutate(trip_miles = case_when(
    is.na(trip_miles) | trip_miles == 0 ~ calculated_distance,
    TRUE ~ trip_miles
  ))

#Add column average_speed_miles_second
testData = testData %>%
  mutate(average_speed_miles_second = trip_miles/trip_seconds
  )

#Add column average_speed_miles_hour
testData = testData %>%
  mutate(average_speed_miles_hour = trip_miles/(trip_seconds/3600)
  )

predictions = predict(model, newdata=testData)$data

submission = rename(bind_cols(select(testData,id), predictions),prediction=response)

write_csv(submission, 'predictions_TheRishman_SubmissionNumber.csv')