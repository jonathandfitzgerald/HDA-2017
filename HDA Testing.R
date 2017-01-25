census = X1840_census_data

gathertest = gather(census, location, value, TotalPopulation)

spreadtest = gathertest %>% group_by(title)

spreadtest = spread(spreadtest, title, value, fill = NA, drop = TRUE )

rm(gathertest)
