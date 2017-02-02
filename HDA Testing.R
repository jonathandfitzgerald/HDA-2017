# Read in crime data
crime <- read.csv(file = "./data/ma-crime.csv",na="0")

# Convert NAs to 0s
crime[is.na(crime)] <- 0

# Gather
crime_long <- gather(crime, "identification", "count", -1)
crime_long$count = as.integer(crime_long$count)

# Total crimes
crime_long <- group_by(crime_long, identification)
crime_ids <- summarize(crime_long, total_ids = sum(count))

# Total crimes by city
crime_long <- group_by(crime_long, City, identification)
crime_ids <- summarize(crime_long, total_ids = sum(count))

# Spread
crime_wide <- spread(crime_long, identification, count)
View(census_wide)


#BOOK TITLES
books <- read_tsv(file = "./data/booktitles-sub.tsv",col_names = TRUE)

books <- books %>% group_by(author) %>% mutate(count=n()) %>% arrange(-count)

books <- books %>% filter(lc1 == "PZ") %>% arrange(-year)

books <- books %>% filter(lc1 != "NULL") %>% group_by(lc1) %>% arrange(-year)

L = c(2:6)
test = census_pub[all(census_pub[2:6]) != "0",]

?subset

test = subset(test, census_pub[2:6] == "0", select = 1:6)


set_of_interest <- c(2:6)
test <- census_pub[any(census_pub[2:6])==0,]


test <- sapply(census_pub[2:6], mean)


row_sub = apply(census_pub[2:6], 1, function(row) any(row !=0 ))
test = census_pub[row_sub,]

class(row_sub)
?logical
