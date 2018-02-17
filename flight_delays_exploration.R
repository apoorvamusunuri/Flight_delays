library(data.table)
library(dplyr)

airlines <- read.csv("airlines.csv", stringsAsFactors = F)
airports <- read.csv("airports.csv", stringsAsFactors = F)
#flights <- read.csv("flights.csv", )
flights <- fread("flights.csv", stringsAsFactors = F, colClasses = c("AIR_SYSTEM_DELAY" = "character", "SECURITY_DELAY" = "character", "AIRLINE_DELAY" = "character", "LATE_AIRCRAFT_DELAY" = "character", "WEATHER_DELAY" = "character"))
#names(flights)
flights <- left_join(flights, airlines, by = c("AIRLINE" = "IATA_CODE"))
flights <- left_join(flights, airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE", "DESTINATION_AIRPORT" = "IATA_CODE"))

flights$delay <- ifelse(flights$CANCELLED ==1,6000,flights$DEPARTURE_DELAY+flights$ARRIVAL_DELAY)

delayed <- subset(flights,delay < 500)
flights$ind <- ifelse(flights$delay < 0, -1, ifelse(flights$delay == 0, 0, ifelse(flights$delay == 6000, 5,1)))

hist(delayed$delay)
hist(flights$ind)

sub <- sample_n(flights,200)

plot(sub$STATE,sub$delay)
plot(sub$MONTH, sub$ind)

library(ggplot2)
month <- ggplot(sub, aes(ind)) + geom_bar(position = "dodge") +facet_grid(month~.)
month

sub$AIRPORT <- factor(sub$AIRPORT)

pairs(~MONTH+DAY_OF_WEEK+LATITUDE+LONGITUDE+AIRPORT,data=sub, main = "Scatterplot")