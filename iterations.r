library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 

#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)

### 4a: time-function

to_iso8601(as_datetime("2016-09-01 10:11:12"),0)

to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)

### 4b: GQL for volumes

# Everything looks nice running the code

GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)



### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1)  %$%
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x = from, y = volume)) + 
  geom_line() + 
  theme_classic() +
  labs(x = 'name', y = "Volume of the traffic station")

### 6 making the plot prettier

# Sample a row from stations_metadata_df first, 
# so that it is possible to extract the name

sampled_row <- stations_metadata_df %>%
  filter(latestData > Sys.Date() - days(7)) %>%
  sample_n(1)

# Extract the 'name' value from the sampled row
x_label <- sampled_row$name

# Use the sampled row and 'name' value in the plot
sampled_row %$%
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x = from, y = volume)) + 
  geom_line() + 
  theme_classic() +
  labs(x = x_label, y = "Volume of the traffic station")
