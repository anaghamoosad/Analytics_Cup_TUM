trainData2 = trainData
trainData3 = read_csv('train.csv')

#trainData = trainDataBackup

trainData = trainDataBackup
#trainDataBackup = trainData

filter(trainDataBackup,trip_seconds == 0)
filter(trainDataBackup,trip_seconds > 5000)
filter(trainDataBackup,trip_miles == 0)
filter(trainDataBackup,trip_miles > 40)
filter(trainDataBackup,is.na(pickup_community_area))
filter(trainDataBackup,is.na(dropoff_community_area))
filter(trainDataBackup,fare > 80)
filter(trainDataBackup,tips > 20)
theFilter = filter(trainDataBackup,tolls > 4)
filter(trainDataBackup,is.na(tolls))
summarise(filter(trainData,!is.na(tolls),rush_hour == 1),tolls_rush_hour = mean(tolls))
summarise(filter(trainData,!is.na(tolls),rush_hour == 0),tolls_not_rush_hour = mean(tolls))
filter(trainData,is.na(extras))
filter(trainData,extras > 6)
theFilter = filter(trainData,is.na(trip_total))
theFilter = filter(trainData,t)

ggplot(trainData2, aes(trip_total, fill = rush_hour)) + geom_density(alpha = 0.2)

#hist(trainData$average_speed_miles_hour,breaks = 100,col="darkmagenta")

trainData %>% dfSummary %>% view()
testData %>% dfSummary %>% view()

unique(trainData$dropoff_community_area)

# theFilter = filter(testData,is.na(pickup_community_area))
# theFilter = filter(testData,is.na(dropoff_community_area))
# 
# trainData = filter(testData,!is.na(pickup_community_area))
# trainData = filter(testData,!is.na(dropoff_community_area))

filter(trainData,is.na(pickup_community_area))
filter(trainData,is.na(dropoff_community_area))

hist(filter(trainData,fare < 80)$fare)
hist(filter(trainData,tips < 10)$tips, breaks = 1000)
hist(trainData$tolls)
hist(trainData$extras)
hist(trainData$trip_total)

summary(trainData$tips)

df %>% dfSummary %>% view()