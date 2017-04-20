library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(leaflet)
library(shiny)
library(googleVis)
library(data.table)
library(scales)
library(plotly)


rawdf = fread(input = './DataScience/NYC_DSA/project1_citibike/www/201605-citibike-tripdata.csv')

head(rawdf)

##STEP 1- HYPOTHESIS GENERATION
## PROBLEM: balance the 
## Hypotheses:


##STEP 2- DATA EXPLORATION 

# 1- Identify variables' type
str(rawdf)

# 2- Find missing values in dataset
# NA values:
summary(rawdf)
# only the column birth.year and gender (expressed as 0) have missing values. These variables are relevant for the first part of our analysis but irrelevant to the second part. 
# We'll remove the observations with missing values for the first part of our analysis.
# We'll keep all the observations (and ignore the missing values) in the second part of our analysis.
# the starttime and stoptime columns are treated as characters so we can't detect any potential missing values. We'll convert them.

# 3- Convert & combine/split values
# a- split date and time in the starttime and endtime columns
df = separate(rawdf, starttime, into=c("startday","starttime"), sep=" ")
df = separate(df, stoptime, into=c("stopday","stoptime"), sep=" ")

# b- convert dates to proper format
df$startday = as.Date(df$startday, format = '%m/%d/%Y')
df$stopday = as.Date(df$stopday, format = '%m/%d/%Y')

# c- check for missing values in the time and date columns
summary(df)
# there is no unusual dates.

# e- extract month and day from dates (month and day are similar for start and stop date so only grabbing the values from startday)
df$month = as.numeric(format(df$startday, format = "%m"))
df$day = as.numeric(format(df$startday, format = "%d"))
df$monthname = month.abb[df$month]
df$dayofweek = weekdays(as.Date(df$startday))

df = as.data.frame(df)
names(df)[names(df)=="start station id"] = "start.station.id"
names(df)[names(df)=="start station name"] = "start.station.name"
names(df)[names(df)=="start station latitude"] = "start.station.latitude"
names(df)[names(df)=="start station longitude"] = "start.station.longitude"
names(df)[names(df)=="end station id"] = "end.station.id"
names(df)[names(df)=="end station name"] = "end.station.name"
names(df)[names(df)=="end station latitude"] = "end.station.latitude"
names(df)[names(df)=="end station longitude"] = "end.station.longitude"
names(df)[names(df)=="birth year"] = "birth.year"


# f- convert start and stop times to proper format and group by intervals of 15 mins 
df$starttime = as.POSIXlt(round(as.double(strptime(df$starttime, '%H:%M:%S'))/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')))
df$starttime = format(df$starttime,"%H:%M")

df$stoptime = as.POSIXlt(round(as.double(strptime(df$stoptime, '%H:%M:%S'))/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')))
df$stoptime = format(df$stoptime,"%H:%M")

# add a column where we group the start and stop time by hour
tStart = strptime(paste("2001-01-01", df$starttime), format="%Y-%m-%d %H:%M")
df$starthour = format(round(tStart, units="hours"), format="%H:%M")

tStop = strptime(paste("2001-01-01", df$stoptime), format="%Y-%m-%d %H:%M")
df$stophour = format(round(tStop, units="hours"), format="%H:%M")


# g- convert the time duration to minutes (instead of seconds) to make it more user friendly. 
df$tripduration.min = round(as.numeric(df$tripduration)/60)

# h- add a column with time duration grouped by chunks of 10 min.
df$tripduration.mingroup = 10*floor((df$tripduration.min + 9)/10)

# i- convert the gender values to gender names (i.e. 1 = male, 2 = female)
df$gender = ifelse(df$gender %in% '0', NA, ifelse(df$gender %in% '1', 'male','female'))

# j- convert the birth.year to age
df$age = as.integer(format(Sys.Date(), "%Y")) - as.integer(df$birth.year)

# k- create age groups
df$agegroup = cut(df$age, 
                  breaks = c(17, 25, 35, 45, 55, 65, 75, Inf), 
                  labels = c("17-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 or older"), 
                  right = FALSE)

# l- create group for start and stop hour
df$starthour = strtrim(df$starthour, 2)
df$starthour = as.numeric(df$starthour)
df$startrange = cut(df$starthour, 
                       breaks = c(00, 04, 08, 12, 16, 20, 24), 
                       labels = c("before 4am", "4am-8am", "8am-12pm", "12pm-4pm", "4pm-8pm", "after 8pm"), 
                       right = FALSE)

df$stophour = strtrim(df$stophour, 2)
df$stophour = as.numeric(df$stophour)
df$stoprange = cut(df$stophour, 
                       breaks = c(00, 04, 08, 12, 16, 20, 24), 
                       labels = c("before 4am", "4am-8am", "8am-12pm", "12pm-4pm", "4pm-8pm", "after 8pm"), 
                       right = FALSE)

# m - convert latitude and longitude to numeric
df$start.station.longitude = as.numeric(df$start.station.longitude)
df$end.station.longitude = as.numeric(df$end.station.longitude)
df$start.station.latitude = as.numeric(df$start.station.latitude)
df$end.station.latitude = as.numeric(df$end.station.latitude)

## 4- Understand distribution of numerical values and detect outliers
# outliers in trip duration
ggplot(data = df, aes(x=dayofweek, y= tripduration.min)) + 
  geom_boxplot(outlier.colour = "red", alpha = 0.1) + 
  labs(x = "day of the week", y = "trip duration (min)", title = 'trip duration: identifying outliers') +
  coord_flip() + 
  theme_pander()

ggplot(data = df, aes(x=dayofweek, y= tripduration.min)) +
  geom_boxplot(outlier.colour = "red", alpha = 0.1) +
  labs(x = "day of the week", y = "trip duration (min)", title = 'trip duration: identifying outliers') +
  coord_flip(ylim=c(0,60)) + 
  theme_pander()

ggplot(data = df, aes(x=tripduration.min)) + geom_bar() + coord_cartesian(xlim=c(0,120))
# there are a lot of outliers. We'll keep only the data for trips of 120 min or under.

# outlier in bikeid. We're checking if bikeid is occuring too many times in the table which could signal potential anomaly
table(df$bikeid) # no anomaly

# outliers in age distribution
ggplot(data = df, aes(x=age)) + 
  geom_histogram() + 
  theme_pander()

# there are a few outliers (over 100 years old), which we will remove

## STEP 3- DATA CLEANING
# remove outliers & keep only the relevant variables. The table will be filtered a bit more in part 1 and part 2.
df = filter(df, age <= 85 & tripduration.min <= 120)

df = df %>% select(gender, age, agegroup, monthname, dayofweek,
                   tripduration, tripduration.min, tripduration.mingroup,
                   month, day, monthname, dayofweek,
                   startday, starttime, starthour, startrange, start.station.id, 
                   start.station.name, start.station.latitude, start.station.longitude,
                   stopday, stoptime, stophour, end.station.id,
                   end.station.name, end.station.latitude, end.station.longitude)

saveRDS(df,file="201605-citibike-tripdata_clean.rda")


# STEP 4- HYPOTHESIS TESTING
# filter df with relevant variables (e.g. age, gender, day of week, trip start and stop information)
df = df %>% select(tripduration, tripduration.min, tripduration.mingroup,
                    gender, age, agegroup, monthname, dayofweek,
                    startday, starttime, starthour, startrange, start.station.id, 
                    start.station.name, start.station.latitude, start.station.longitude,
                    stopday, stoptime, stophour, end.station.id,
                    end.station.name, end.station.latitude, end.station.longitude)

# remove observations with missing values (gender and yearbirth)
df = df %>% filter(!is.na(df$gender) & (!is.na(df$age)))

saveRDS(df,file="201605-citibike-tripdata_df.rda")

# create another 'long" df'
melt_df = df %>% select(starthour,stophour,
                        start.station.name, end.station.name,
                        start.station.latitude, end.station.latitude, 
                        start.station.longitude, end.station.longitude, 
                        gender, age, dayofweek) %>% 
                        reshape(direction='long', 
                                varying=list(c('starthour', 'stophour'),
                                            c('start.station.name', 'end.station.name'),
                                            c('start.station.latitude', 'end.station.latitude'),
                                            c('start.station.longitude', 'end.station.longitude')),
                                times=c('start', 'stop'),
                                v.names=c('hour', 'name', 'latitude', 'longitude'))
saveRDS(melt_df,file="201605-citibike-tripdata_melt.rda")

# 1- WHEN

tile_df = df %>% select(gender, agegroup, dayofweek, startrange) %>% # filter by user input (agegroup, gender)
    group_by(gender, agegroup, startrange, dayofweek) %>% 
    summarise(count = n())

tile_df$dayofweek = factor(tile_df$dayofweek, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
tile_df = tile_df[order(tile_df$dayofweek), ]


#let filter by agegroup and gender
tile_df %>% paste("Time range:", startrange , sep = "<br/>", "Day of week: ", dayofweek, sep = "<br/>", "Count: ", count)

g = ggplot(tile_df, aes(x=startrange, y=dayofweek, fill=count)) +
    geom_tile(color="white", size=0.1) +
    scale_x_discrete(expand = c(0, 0)) +
    labs(x=NULL, y=NULL, title="Citi Bike users per day and hour") +
    scale_fill_gradient(low = "#deebf7", high = "#3182bd", name = "# Riders") +
    theme_tufte(base_family="Helvetica") +
    theme(plot.title=element_text(hjust=0)) +
    theme(axis.ticks=element_blank()) +
    theme(axis.text=element_text(size=7)) +
    theme(legend.title=element_text(size=8)) +
    theme(legend.text=element_text(size=6)) +
    theme(legend.key.size=unit(0.2, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    facet_wrap(gender~agegroup)

suppressWarnings(ggplotly(g))

# 2- WHERE

map_df = df %>% select(age, dayofweek, gender, startrange, 
                        start.station.longitude, start.station.latitude, start.stationname) %>%
    filter(gender == 'female' & age > 17 & age < 25 & startrange == '4am-8am' & dayofweek == 'Friday') %>%
    group_by(gender, agegroup, startrange, dayofweek, start.station.longitude, start.station.latitude) %>% 
    summarise(count = n()) # filter by user input (agegroup, gender) radio button for arriving or leaving

#let filter by gender, age, hour, dayofweek, arriving or departing
map_df = melt_df %>% filter(gender == 'female', age > 17, age < 25, hour > 1, hour <5, dayofweek == 'Friday', time == 'start')

# code without bike icons
# leaflet(data = map_df) %>% 
#     addTiles() %>% 
#     addMarkers(~longitude, ~latitude, popup = ~name, label = ~name) %>% addProviderTiles('Esri.NatGeoWorldMap')

#code with bike icons:
bike_icon = makeIcon('www/bikeiconbw.png', iconWidth = 25, iconHeight = 38)
leaflet(data = map_df) %>% 
    addTiles() %>% 
    addMarkers(~longitude, ~latitude, popup = ~name, label = ~name, icon = bike_icon) %>% addProviderTiles('Esri.NatGeoWorldMap')

# would have liked to add the estimated count and color icons accordingly

# 3- HOW
# estimated time by citi bike


subset_df2 = df2[1:5, c('start.station.name','end.station.name', 
                        'starthour', 'tripduration.min', 'dayofweek', 
                        'start.station.latitude', 'start.station.longitude',
                        'end.station.latitude', 'end.station.longitude')]
subset_df2$start.lat_long <- apply( subset_df2[ , c('start.station.latitude', 'start.station.longitude') ] , 1 , paste , collapse = ", " )
subset_df2$end.lat_long <- apply( subset_df2[ , c('end.station.latitude', 'end.station.longitude') ] , 1 , paste , collapse = ", " )
from <- subset_df2$start.lat_long
to <- subset_df2$end.lat_long
mapdist(from, to, mode='bicycling')


direction_df = df %>% select(start.station.name, end.station.name, 
                        starthour, tripduration.min, dayofweek, 
                        start.station.latitude, start.station.longitude,
                        end.station.latitude, end.station.longitude)

direction_df$start.lat_long = apply( direction_df[ , c('start.station.latitude', 'start.station.longitude') ] , 1 , paste , collapse = ", " )
direction_df$end.lat_long = apply( direction_df[ , c('end.station.latitude', 'end.station.longitude') ] , 1 , paste , collapse = ", " )

saveRDS(direction_df,file="201605-citibike-tripdata_direction.rda")

#look at unique combination of lat & long (>100 000 rows)
unique_lat_long = unique(direction_df[,c('start.lat_long','end.lat_long')])

from = unique_lat_long$start.lat_long
to = unique_lat_long$end.lat_long
google_df = mapdist(from, to, mode='bicycling')

# dir_df = merge(direction_df, google_df, by = c('start.lat_long', 'end.lat_long'))

## calculate the average duration according to citi bike

user_selection = direction_df %>% 
                  filter (start.station.name == '1 Ave & E 30 St', 
                          end.station.name == 'E 17 St & Broadway', 
                          dayofweek == 'Sunday', 
                          starthour == '0') %>%
                            group_by(start.lat_long, end.lat_long) %>%
                              summarise(avg_duration = mean(tripduration.min))


# calculate the average duration according to google




## using a valid Google Maps API key


## Using the first and last coordinates as the origin/destination
origin = user_selection$start.lat_long  # grab direction_df$start.lat_long (from filtered list above)
destination = user_selection$end.lat_long # grab direction_df$stop.lat_long (from filtered list above)

google_time = select(mapdist(origin, destination, mode='bicycling'), minutes)

## get the directions from Google Maps API
library(googleway)
key = "AIzaSyB1Oq-rYunwKh-I0Cgx0z2nCy0m8T5FukA"
res = google_directions(origin = origin,
                        destination = destination,
                        key = key)

df_polyline <- decode_pl(res$routes$overview_polyline$points)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)


  # leaflet() %>%
  #   addTiles() %>%
  #   addPolylines(data = df_polyline, lat = ~lat, lng = ~lon) %>% 
  #   addMarkers(~lat, ~lon, popup = c(origin, destination), label = c(origin, destination))


## Could have, should have, would have
# 1- Missing data
# missing the data with # of docks available per station. Unfortunately, data can't be scraped on citibike website.

# 2- Limited scope
# only took the data for May 2017 and we expect the citibike riders to behave differently depedning on the season, temperature, etc. 

# 3- sub-efficient code
# did not use data.table format

# 4- aesthetics
# would have liked to add the estimated count and color icons accordingly




