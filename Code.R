---
title: "Optimizing City-Bike Inspection Rounds: Geospatial Data Analysis and Visualization"
author: "Haider Zulfiqar"
date: "June 21, 2023"
output:
  html_document:
    df_print: paged
  pdf_document: default
  word_document: default
---

Objectives:

1.  Analyze open data on city-bike trips to identify peak usage times for each station.
2.  Develop a geospatial data visualization that clearly displays the usage patterns on a map.
3.  Create a visually compelling one-slider presentation with the map visualization for the client meeting.
4.  Ensure the map is easy to interpret and can be printed out for workers conducting inspections.
5.  Write high-quality, well-documented code/scripts that can be used and modified by the client in the future.

Methodology:

1.  Data Processing and Analysis\
    â¢ Obtain the open data on city-bike trips in Helsinki, including trip timestamps, durations and station locations. Clean and preprocess the data to ensure accuracy and consistency.\
    â¢ Perform exploratory data analysis to identify usage patterns, focusing on peak usage times for each station.\
    â¢ Calculate key metrics and statistics to quantify the intensity of usage at different times of the day.\
    â¢ Ensure robustness in the analysis, investigate any potential seasonality or bimodality trends in peak usage. By incorporating these trends into the visualization, we can create a more comprehensive and accurate representation of the data.

2.  Geospatial Data Visualization\
    â¢ Utilize geospatial visualization libraries in R to create an interactive map showcasing the peak usage times for each city-bike station.\
    â¢ Map the usage intensity using intuitive color gradients to highlight busy periods.\
    â¢ Ensure the map visualization is aesthetically pleasing and easy to interpret, with clear labeling and intuitive design.\
    â¢ Develop a visually appealing one-slider that includes the map visualization of peak usage times for each city-bike station. â¢ Highlight the key insights obtained from the analysis and emphasize the importance of avoiding heavily used times for inspections.\
    â¢ Provide clear explanations and context for the map visualization to ensure easy comprehension by the client and inspection workers.\
    â¢ Generate a printable version of the map visualization suitable for workers conducting inspections.

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

# 1. Libraries

```{r}
library(data.table)
library(dplyr)
library(dlookr)
library(htmltools)
library(stringr)

library(leaflet)
library(sp)
library(sf)

library(plotly)
library(ggplot2)
library(scales)
library(leafpop)
```

# 2. Loading the data

The dataset contains more than 10 million trips made by citizens of Helsinki between 2016-2020. Since 2016, over 10 million rides have been taken, covering a total distance of 25.3 million kilometers. Dataset can be found here: <https://www.kaggle.com/datasets/geometrein/helsinki-city-bikes>

```{r}
#Filepath
file_path <- "E:/Study Material/Tampere - Grad/Assessements/Spatial/database.csv"

# Read the CSV file using fread
bike_data <- fread(file_path)
str(bike_data)
head(bike_data)
```

# 3. Preprocessing

Such huge datasets always have some anomalies and it is usually a good practice to cater those ambiguities before embarking on modeling and shaping analysis.

```{r}
# Data Preparation
bike_data <- rename(bike_data, distance = 'distance (m)',
             duration = 'duration (sec.)',
             speed = 'avg_speed (km/h)',
             temperature = 'Air temperature (degC)')

```

```{r}
summary(bike_data)
```

In the given data, there appear to be anomalies such as negative values and unusually large and small distances. Regarding NAs, they are mostly occurring in the Air Temperature field which isn't really significant to shaping our analysis, so we can ignore it, and we can cater for the one NA appearing in return latitude and return longitude field.

## 3.1. Error Handling

To ensure data quality and remove anomalies, the dataset can be filtered based on the following criteria:

> Distance Constraint: Remove trips with a distance less than 50 meters, as stations are always positioned more than 50 meters apart.

> Duration Constraint: Set an upper limit of 5 hours (18,000 seconds) for trip duration, as this is the maximum rental time for a bike. The lower limit can be determined based on the distance limit of 50 meters and an average speed of 25 km/h.

By applying these filters, we can ensure that the dataset contains meaningful and contextually appropriate data for further analysis.

```{r}
# Remove rows with missing values
bike_data <- na.omit(bike_data, cols=c("return_latitude", "return_longitude"))

filtered_data <- bike_data[
  50 < distance & distance < 10000 &
    120 < duration & duration < 18000]

# Obtain summary statistics for the filtered columns
summary(filtered_data)
```

# 4. Some EDA

It is always a good practice to perform some basic EDA before proceeding with specific analysis and visualizations.

## 4.1. Total Number of Departure Trips for Each Station

```{r}
trips_per_station <- filtered_data %>%
  group_by(departure_name) %>%
  summarize(total_trips = n()) %>%
  arrange(desc(total_trips))  # Arrange the dataframe in descending order

# Top 10 stations
top_10_stations <- head(trips_per_station, 10)

# Create the ggplot
ggplot(top_10_stations, aes(x = reorder(departure_name, -total_trips), y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Station", y = "Total Trips") +
  ggtitle("Top 10 Deaparture Stations by Total Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = comma_format()) 
```

With over 0.3 million trips, ItÃ¤merentori is the departure station with the most number of trips followed by TÃ¶Ã¶lÃ¶nlahdenkatu station and Kamppi (M) station.

## 4.2. Average Number of Departure Trips for Each Hour for all Departure Stations

```{r}
filtered_data$hour_departure <- hour(filtered_data$departure)
filtered_data$hour_return <- hour(filtered_data$return)

average_trips_per_hour <- filtered_data %>%
  group_by(hour_departure, as.Date(departure), departure_name) %>%
  summarize(avg_trips = n(), .groups = 'drop') %>%
  group_by(departure_name, hour_departure) %>%
  summarize(avg_trips = mean(avg_trips), .groups = 'drop')

# Create ggplot
ggplot(as.data.table(average_trips_per_hour)[1:24*5], aes(x = hour_departure, y = avg_trips, color = departure_name)) +
  geom_line() +
  labs(x = "Hour of Departure", y = "Average Trips", color = "Departure Name") +
  ggtitle("Average Trips per Hour by Departure Name") +
  theme_minimal()
```

## 4.3. Total Number of Departure Trips for Each Hour Across all Stations Combined

```{r}
total_trips_each_hour <- filtered_data %>%
  group_by(hour_departure) %>%
  summarize(total_trips = n() )

# Create the ggplot
ggplot(total_trips_each_hour, aes(x = hour_departure, y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Hour", y = "Total Trips") +
  ggtitle("Total Departure Trips Hourwise") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma_format())  # Format y-axis labels as thousands
```

From here, we can get a rough estimate that afternoon-evening hours generally have the highest number of departure station trips. Further, it does give us an indication that there might be a surge of multiple peaks in the day which we will explore later on.

## 4.4. Peak Usage Time for Each Departure Station

```{r}
peak_usage_time <- filtered_data %>%
  group_by(departure_name, hour_departure) %>%
  summarize(trips = n()) %>%
  group_by(departure_name) %>%
  filter(trips == max(trips)) %>%
  select(departure_name, peak_time = hour_departure)

head(peak_usage_time)
```

# 5. Geospatial Data Analysis and Visualization

## 5.1. Subsetting data for the peak usage time station wise

```{r}
# Combine departure and return data into a single dataframe
combined_data <- filtered_data %>% 
  select(departure_name, hour_departure) %>%
  rename(station= departure_name, hour= hour_departure)

combined_data <- combined_data %>% rbind(
  filtered_data %>% 
    select(return_name, hour_return) %>%
    rename(station= return_name, hour= hour_return)
  )

# Peak data based on the highest number of trips per station and hour
peak_data <- combined_data %>%
  group_by(station, hour) %>%
  summarize(trips = n(), .groups = 'drop') %>%
  group_by(station) %>%
  filter(trips == max(trips))

head(peak_data)
```

## 5.2. Data Transformations

```{r}
# Getting the latitude and longitude coordinates for each station
station_coordinates <- filtered_data %>%
  select(station = departure_name, latitude = departure_latitude, longitude = departure_longitude) %>%
  distinct() 

station_coordinates <- station_coordinates %>% rbind(
  filtered_data %>% 
    select(station = return_name, latitude = return_latitude, longitude = return_longitude) %>%
    distinct()) %>%
  distinct()

# Merging the peak data with the station coordinates
peak_sf <- merge(station_coordinates, as.data.table(peak_data), by = "station")

# Convert the data frame to an sf object
peak_sf <- st_as_sf(peak_sf, coords = c("longitude", "latitude"), crs = 4326)
head(peak_sf)
```

# 6. Initial leaflet map

```{r}
pal <- colorNumeric(palette = "Set3", domain = peak_sf$hour)

map_l <- leaflet() %>%
  addTiles() %>%
  setView(lng = 24.9458, lat = 60.1921, zoom = 12)   # Set the initial map view to Helsinki

# Add markers to the map for each station with peak usage time
map_l <- map_l %>%
  addCircleMarkers(
    data = peak_sf,
    color = ~pal(hour),
    radius = 8,
    # clusterOptions = markerClusterOptions(),
    fillColor = ~pal(hour),
    fillOpacity = 1,
    popup = ~paste(
      "<b>Station:</b>", peak_sf$station,
      "<br><b>Peak Hour:</b>", hour,
      "<br><b>Trips:</b>", trips
    ),
    label = ~as.character(hour),
    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                offset = c(0, 0)),
    ) %>% addLegend(pal = pal, values = peak_sf$hour, opacity = 1)

# Display the map
map_l 
```

Here the map reveals a clear pattern where the majority of stations experience peak hours either in the late afternoon to early evening hours, typically between 4 PM and 6 PM, or in the morning hours for very few, particularly 8 AM. This pattern emerges consistently, indicating a predictable and concentrated surge in bike utilization during these time frames.

# 7. Different Nuanced Variations

## 7.1. Plotly map - Same above plot in a different aesthetic way

```{r}
plot_ly() %>%
  add_trace(
    data = peak_sf,
    type = "scattermapbox",
    lat = ~st_coordinates(geometry)[, "Y"],
    lon = ~st_coordinates(geometry)[, "X"],
    colors = c("pink", "purple", "blue", "green","yellow"),  # Specify the colors here
    color = ~hour,
    size = ~sqrt(trips),
    text = ~paste("Station:", station, "<br>Peak Hour:", hour, "<br>Trips:", trips),
    mode = "markers",
    marker = list(
      # size = 8,
      # color = peak_sf$hour,
      opacity = 1.5,
      textfont = list(color = "white")
    )
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 10.5,
      center = list(lon = 24.9458, lat = 60.1921)
    ),
    title = "Peak Bike Usage",
    hovermode = "closest"
  ) 
```

The map pattern is ofcourse consistent with what we observed in the map above using leaflet package.

## 7.2. Plotly map - Same above plot with Hourly Frame

```{r}
plot_ly() %>%
  add_trace(
    data = peak_sf,
    type = "scattermapbox",
    lat = ~st_coordinates(geometry)[, "Y"],
    lon = ~st_coordinates(geometry)[, "X"],
    colors = c("pink", "purple", "blue", "green","yellow"),  # Specify the colors here
    color = ~hour,
    frame = ~hour,
    size = ~sqrt(trips),
    text = ~paste("Station:", station, "<br>Peak Hour:", hour, "<br>Trips:", trips),
    mode = "markers",
    marker = list(
      # size = 8,
      # color = peak_sf$hour,
      opacity = 1.5,
      textfont = list(color = "white")
    )
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 10,
      center = list(lon = 24.9458, lat = 60.1921)
    ),
    title = "Peak Bike Usage",
    hovermode = "closest"
  ) 
```

By adding an hourly frame to the visualization, we visually capture peak bike usage in concentrated time zones, revealing patterns of heightened activity and significant surges. This enhances the presentation and offers a comprehensive understanding of peak bike usage trends.

To explore the influence of seasonality on peak hour bike usage, we will analyze the data monthly and yearly. This analysis aims to identify variations or patterns across seasons, providing insights into how peak hour bike utilization fluctuates throughout the year. By examining these temporal dimensions, we can identify months or seasons with significant deviations from the overall trend, gaining a deeper understanding of seasonal dynamics and their impact on peak bike usage patterns.

```{r}
# Extract month and year from departure date
filtered_data$departure_month <- month(filtered_data$departure)
filtered_data$departure_year <- year(filtered_data$departure)

# Extract month and year from return date
filtered_data$return_month <- month(filtered_data$return)
filtered_data$return_year <- year(filtered_data$return)

# Grouping the filtered data by departure information
grouped_data_departure <- filtered_data %>%
  group_by(departure_name, hour_departure, departure_month, departure_year) %>%
  summarize(total_trips = n())

head(grouped_data_departure)

# Grouping the filtered data by return information
grouped_data_return <- filtered_data %>%
  group_by(return_name, hour_return, return_month, return_year) %>%
  summarize(total_trips = n())

head(grouped_data_return)
```

```{r}
# Creating a new dataset 'usage_month_year_data' by combining grouped departure and return data
usage_month_year_data <- grouped_data_departure %>% 
  select(departure_name, hour_departure, departure_month, departure_year, total_trips) %>%
  rename(station="departure_name", hour= hour_departure, month= departure_month, year= departure_year) %>%
  rbind(grouped_data_return %>%
          select(return_name, hour_return, return_month, return_year, total_trips) %>%
          rename(station="return_name", hour="hour_return", month= return_month, year= return_year)
          )

head(usage_month_year_data)

# Finding the peak month and year for each station based on the total trips
peak_month_year_data <- usage_month_year_data %>%
  group_by(station, hour, month, year) %>%
  summarize(trips = sum(total_trips)) %>%
  group_by(station, month, year) %>%
  filter(trips == max(trips))

head(peak_month_year_data)
```

```{r}
# Merging the peak data with the station coordinates
peak_data_ext <- peak_data
peak_data_ext$month = "Overall"
peak_data_ext$year = "Overall"

peak_month_year_data$month <- as.character(peak_month_year_data$month)
peak_month_year_data$year <- as.character(peak_month_year_data$year)

# Filtering and summarizing peak month data for August
peak_aug_data <- peak_month_year_data %>%
  filter(month == "8") %>%
  group_by(station, hour) %>%
  summarize(month = "August", year = "Overall", trips = sum(trips)) %>%
  group_by(station) %>%
  filter(trips == max(trips))

# Merging station coordinates with peak month and year data
peak_month_year_sf <- merge(station_coordinates, rbind(data.table(peak_month_year_data), data.table(peak_aug_data), data.table(peak_data_ext)), by = "station")

# Convert the data frame to an sf object
peak_month_year_sf <- st_as_sf(peak_month_year_sf, coords = c("longitude", "latitude"), crs = 4326)
head(peak_month_year_sf)
```

## 7.3. Plotly map - With Monthly and Yearly Frame and August Specific

```{r}
map_year_month<- plot_ly() %>%
  add_trace(
    data = peak_month_year_sf,
    type = "scattermapbox",
    lat = ~st_coordinates(geometry)[, "Y"],
    lon = ~st_coordinates(geometry)[, "X"],
    colors = c("pink", "purple", "blue", "green","yellow"),  # Specify the colors here
    color = ~hour,
    frame = ~paste(year, stringr::str_pad(month, 2, side='left', pad='0'), sep = "-"),
    text = ~paste("Station:", station, "<br>Peak Hour:", hour, "<br>Trips:", trips),
    mode = "markers",
    marker = list(
      # size = 8,
      # color = peak_sf$hour,
      opacity = 1.5,
      textfont = list(color = "white")
    )
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 11,
      center = list(lon = 24.9458, lat = 60.1921)
    ),
    title = "Peak Bike Usage",
    hovermode = "closest"
  ) 

# Display the map
map_year_month

```

Upon closer examination, an intriguing insight emerges when considering the seasonal patterns alongside the overall trend. Notably, during the later months of the year, particularly September and October, there is a noteworthy increase in the proportion of morning peak bike usage compared to the broader trend. This observation suggests a seasonal influence on the peak hour dynamics, wherein the morning hours experience heightened activity during these specific months. While the peak usage pattern observed in the month of August closely follows the overall trend we identified earlier.

Furthermore, a cursory exploration of a subset of stations reveals the possibility of bimodality in the peak usage pattern as indicated in one of those such cases in the graph below. This indicates the existence of two distinct peaks in bike usage, potentially occurring at different times within the day. While this finding was observed in only a few stations, it hints at the presence of diverse usage patterns among different locations. Such insights contribute to our understanding of the nuanced variations in peak bike usage and underscore the need for further investigation and analysis to uncover underlying factors driving these patterns. This observation aligns with the earlier exploration during EDA, where we examined total departure trips hourwise for initial insights.

```{r}
# Filtering the data for August departures from 'TÃ¶Ã¶lÃ¶nlahdenkatu' and selecting station and time columns
station_filter <- filtered_data %>%
  filter(month(departure) == 8 & departure_name == 'TÃ¶Ã¶lÃ¶nlahdenkatu') %>%
  select(station = departure_name, time = departure) %>%

# Combining with the filtered data for August returns to 'TÃ¶Ã¶lÃ¶nlahdenkatu' and selecting station and time columns
  bind_rows(filtered_data %>%
              filter(month(return) == 8 & return_name == 'TÃ¶Ã¶lÃ¶nlahdenkatu') %>%
              select(station = return_name, time = return))

# Extracting the hour component from the time column and storing it in the hour column
station_filter <- station_filter %>%
  mutate(hour = as.integer(format(as.POSIXct(time), format = "%H%M")))

# Creating a density plot using ggplot with hour on the x-axis
ggplot(station_filter, aes(x = hour)) +
  geom_density(adjust = 2) +
  labs(
    title = "Density Plot of Departure and Return Hours in August from 'TÃ¶Ã¶lÃ¶nlahdenkatu'",
    x = "Hour",
    y = "Density"
  ) +
  theme_minimal()
```

While the majority of stations exhibited just a single peak in bike usage. As an example, let's consider Station 'YmpyrÃ¤taloi'. Our analysis of Station 'YmpyrÃ¤taloi' indicates a distinct pattern of a single peak in bike usage, aligning with the overall trend observed in the initial analysis. However, it is crucial to acknowledge that variations in usage patterns can exist among different stations.

```{r}
# Filtering the data for August departures from 'Unioninkatu' and selecting station and time columns
filter_station.1 <- filtered_data %>%
  filter(month(departure) == 8 & departure_name == 'Unioninkatu') %>%
  select(station = departure_name, time = departure) %>%

# Combining with the filtered data for August returns to 'Unioninkatu' and selecting station and time columns
  bind_rows(filtered_data %>%
              filter(month(return) == 8 & return_name == 'Unioninkatu') %>%
              select(station = return_name, time = return))

# Extracting the hour component from the time column and storing it in the hour column
filter_station.1 <- filter_station.1 %>%
  mutate(hour = as.integer(format(as.POSIXct(time), format = "%H%M")))

# Creating a density plot using ggplot with hour on the x-axis
ggplot(filter_station.1, aes(x = hour)) +
  geom_density(adjust = 2) +
  labs(
    title = "Density Plot of Departure and Return Hours in August from 'Unioninkatu'",
    x = "Hour",
    y = "Density"
  ) +
  theme_minimal()
```

This highlights the importance of conducting detailed analyses at the individual station level to capture any unique characteristics or deviations from the broader trend. By considering specific examples like Station XYZ, we can gain a more comprehensive understanding of the complexities involved in bike usage patterns and tailor our insights accordingly.

This comprehensive analysis allows us to shape our overall understanding of the bike usage patterns. While our initial analysis provided a generalized overview, it is crucial to address potential ambiguities and nuances by employing a more dynamic approach. Therefore, we have adopted a month-wise analysis, categorizing hours into different combinations of peak periods. This approach effectively resolves various minor issues and provides a more detailed perspective.

Considering the specific requirement for analyzing August 2023, we focus on that particular month for all years moving forward. However, the code is designed to be flexible, enabling us to obtain the desired information by simply modifying the month name. This dynamic nature of the analysis ensures adaptability and allows us to explore other time periods as needed.

```{r}
month_number <- 8   #Exploring August
filtered_data <- filtered_data[(month(departure) == month_number & month(return) == month_number)]

# Create a new data frame combining departure and return data
bikes.data_long <- bind_rows(
  filtered_data %>% select(departure, departure_longitude, departure_latitude, departure_name) %>% rename(time = departure, longitude = departure_longitude, latitude = departure_latitude, station = departure_name),
  filtered_data %>% select(return, return_longitude, return_latitude, return_name) %>% rename(time = return, longitude = return_longitude, latitude = return_latitude, station = return_name)
)

# Add an 'hour' column to the data frame
bikes.data_long <- bikes.data_long %>% mutate(hour = hour(time))

# Display the first few rows of the resulting data frame
head(bikes.data_long)

```

## 7.4. Peak Usage Time for Each Departure Station

```{r}
# Calculate the count of trips by station, hour, longitude, and latitude
bikes.data.peak <- bikes.data_long %>%
  group_by(station, hour, longitude, latitude) %>%
  summarise(N = n()) 

max_scale <- function(x) {x/max(x)}

# Filter the data based on the threshold condition and order it by hour
bikes.data.peak <- bikes.data.peak %>%
  arrange(hour) %>%
  group_by(station) %>%
  mutate(thresh = max_scale(N)) %>%
  filter(thresh > 0.8) %>%
  arrange(N, hour) %>%
  group_by(station, latitude, longitude) %>%
  slice_head(n=3) %>%
  arrange(hour) %>%
  summarise(trips = sum(N),
            hour = paste(str_pad(unique(hour), width = 2, side = 'left', pad = '0'), collapse = '-'),
            .groups = 'drop')

head(bikes.data.peak)

# Convert the data frame to an sf object
bikes_data.sf <- st_as_sf(as.data.table(bikes.data.peak), coords = c("longitude", "latitude"), crs = 4326)
head(bikes_data.sf)
```

Here, we create buckets of peak hours for bike usage by considering a threshold of 80% (chosen arbitrarily) of the total trips. The process involves identifying the peak hour with the highest number of trips and then including two additional peak hours (if applicable) that individually account for at least 80% of the total trips compared to the primary peak hour within the same time window. This methodology aims to address issues such as bimodality, which were discussed in the initial analysis.

To clarify the concept of "Top 3," it refers to selecting the peak hours based on their proportion of trips relative to the highest peak hour. For example, if the first peak hour has 100 trips and the second peak hour has 80 trips, the second peak hour will be included because its trips account for 80/100 = 0.8, meeting the 80% threshold. Conversely, if the second peak hour has 70 trips, it would not be included because its proportion, 70/100 = 0.7, falls below the threshold.

## 7.5. Plotly map - Based on Month and Hourly Buckets

```{r}
plot_ly() %>%
  add_trace(
    data = bikes_data.sf,
    type = "scattermapbox",
    lat = ~st_coordinates(geometry)[, "Y"],
    lon = ~st_coordinates(geometry)[, "X"],
    colors = c("pink", "purple", "blue", "green","yellow", "orange", "red"),  # Specify the colors here
    color = ~hour,
    size = ~trips,
    text = ~paste("Station:", station, "<br>Peak Hour:", hour, "<br>Trips:", trips),
    mode = "markers",
    marker = list(opacity = 1.5)
  ) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      zoom = 10.5,
      center = list(lon = 24.9458, lat = 60.1921)
    ),
    title = "Peak Bike Usage",
    hovermode = "closest"
  )
```

The above visualization provides a good overall understanding of the general pattern, and we can obtain interactive station-wise insights by hovering over the hourly filters. However, for onsite workers conducting inspections, the visual representation can be challenging to interpret. To address this issue, we enhance the visualization by breaking down the hourly buckets, creating a more user-friendly and easily interpretable map.

## 7.6. Bucketing Hours

```{r}
bucket_hour <- function(hours) {
  df.h <- as.data.table(hours) %>%
    group_by(hours) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    mutate(thresh = max_scale(N))
  
  # simple peak hour
  final_b <- df.h %>%
    arrange(desc(N)) %>%
    slice_head(n = 1) %>%
    pull(hours)
  final_b_trips <- df.h %>%
    arrange(desc(N)) %>%
    slice_head(n = 1) %>%
    pull(N)
  
  # bucketed peak if possible  
  buckets <- list(c(16, 17, 18), c(8, 16, 17), c(17, 18, 19))
  for (b in buckets) {
    # only consider bucket if all bucket hours have more trips than threshold of max
    if (min(df.h$thresh[df.h$hours %in% b]) < 0.8)
      next
    
    # only consider bucket hours if they have more avg_trips than any other individual hour outside of bucketed hours
    b_trips <- df.h %>%
      filter(hours %in% b) %>%
      summarise(sum(N)) %>%
      pull()
    if (b_trips > final_b_trips & all(df.h$N[!(df.h$hours %in% b)] > b_trips / length(b)) == 0) {
      final_b <- b
      final_b_trips <- b_trips
    }
  }
  
  return(paste(final_b, collapse = '-'))
}

bikes.data_peak2 <- bikes.data_long %>%
  group_by(station, latitude, longitude) %>%
  summarise(hour = bucket_hour(hour), trips = n()) %>%
  ungroup()

bikes.data_peak2 <- bikes.data_peak2 %>%
  mutate(hour_f = LETTERS[as.numeric(factor(hour))],
         hour_l = paste(hour_f, ":", hour))

bikes.sf2 <- st_as_sf(as.data.table(bikes.data_peak2), coords = c("longitude", "latitude"), crs = 4326)
```

# 8. One Slider Map - Final Presentation

```{r}
scol <-  c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#d385ea', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#aaffc3', '#808000', '#ffd8b1', '#ffffff')

pal <- colorFactor(palette = scol, domain = bikes.sf2$hour_l)

map_final <- leaflet() %>%
  addTiles() %>%
  setView(lng = 24.9458, lat = 60.1921, zoom = 12)   # Set the initial map view to Helsinki

# Add markers to the map for each station with peak usage time
map_final <- map_final %>%
  addCircleMarkers(
    data = bikes.sf2,
    color = ~pal(hour_l),
    radius = 8,
    # clusterOptions = markerClusterOptions(),
    fillColor = ~pal(hour_l),
    fillOpacity = 1,
    popup = ~paste(
      "<b>Station:</b>", station,
      "<br><b>Peak Hour:</b>", hour,
      "<br><b>Trips:</b>", trips
    ),
    label = ~as.character(hour_f),
    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center", 
                                offset = c(0, 0)),
    ) %>% addLegend(pal = pal, values = bikes.sf2$hour_l, opacity = 1)

# Display the map
map_final 
```

We propose the aforementioned map as our final visualization for the onsite inspections and client meetings. This carefully crafted map employs a range of color gradients to signify peak bucket hours, thereby augmenting comprehensibility and simplifying the identification of periods of high usage.

The visualization effectively communicates the peak hour distribution and monthly analysis of city-bike station usage, providing a clear and intuitive representation with attention to granular details like seasonality and bimodality as described in the thought process earlier. By incorporating geospatial visualization aspects, the map ensures a comprehensive understanding of the data. Its visually captivating and informative design enables the client to make informed decisions and optimize inspection rounds efficiently.

# 9. Future Improvements

For future improvements, there are a few more areas we could have addressed but were constrained by time limitations:

1.  Comparative analysis: Comparing the usage patterns of different months or years could provide valuable insights into deeper seasonality trends and long-term changes. This would help the client identify trends and plan inspections accordingly.
2.  Spatial clustering: Applying spatial clustering algorithms could help identify spatially cohesive groups of stations with similar usage patterns, enabling targeted interventions and resource allocation.
3.  Fine-grained analysis: Due to time constraints, we were unable to perform a more detailed analysis at a station-level or explore specific factors influencing peak bike usage, such as weather conditions. As indicated in the start, we dropped the temperature field from our analysis. This additional analysis could provide deeper insights into usage patterns and help identify specific drivers of peak hour trends.

These improvements would further enhance the depth and flexibility of the analysis, providing more comprehensive insights into peak bike usage and empowering the client with additional tools for decision-making and optimization.
