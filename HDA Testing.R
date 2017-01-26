crime <- read.csv(file = "./data/ma-crime.csv",na="0")
crime[is.na(crime)] <- 0

crime_long <- gather(crime, "identification", "count", -1)
crime_long$count = as.integer(crime_long$count)

crime_long <- group_by(crime_long, identification)
crime_ids <- summarize(crime_long, total_ids = sum(count))

crime_long <- group_by(crime_long, City, identification)
crime_ids <- summarize(crime_long, total_ids = sum(count))

crime_wide <- spread(crime_long, identification, count)
View(census_wide)
