######################################################################
#Data cleaning
######################################################################

#Data cleaning
#clean trip_seconds
trainData = filter(trainData,trip_seconds != 0,trip_seconds < 5000)
#clean trip_miles
trainData = filter(trainData,trip_miles != 0,trip_miles < 40)

#Add column miles_per_hour
trainData = trainData %>%
  mutate(average_speed_miles_hour = trip_miles/(trip_seconds/3600)
)

filter(trainData,average_speed_miles_hour = 0)

#Data cleaning
#clean miles per hour
trainData = filter(trainData,average_speed_miles_hour < 70)

#Add columns as factors for tests
trainData = mutate(trainData,is_weekend_factor = as.factor(trainData$is_weekend))

#Data cleaning 
#clean pickup_community_area
trainData = filter(trainData,!is.na(pickup_community_area))

#clean dropoff_community_area
trainData = filter(trainData,!is.na(dropoff_community_area))

#Add pickup_community_area and dropoff_community_area as factors
trainData = mutate(trainData,pickup_community_area_factor = as.factor(trainData$pickup_community_area))
trainData = mutate(trainData,dropoff_community_area_factor = as.factor(trainData$dropoff_community_area))

#clean fare
trainData = filter(trainData,fare < 80)

#clean tips
trainData = filter(trainData,tips < 20)

#clean tolls
trainData = filter(trainData,!is.na(tolls))

#clean extras
#Nothing to do









