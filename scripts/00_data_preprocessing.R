### Preamble ###
# Purpose: Clean the Daily Shelter occupancy data downloaded from Toronto Open Data
# Author: Yitian Li
# Date: February 5 2021
# Contact: yitian.li@mail.utoronto.ca
# Pre-req: None

### Workspace Set-Up ###
# install.packages("opendatatoronto")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(opendatatoronto)
library(dplyr)

### Import the dataset from Toronto Open Data ###
# get package
package <- show_package("8a6eceb2-821b-4961-a29d-758f3087732d")
package
 
# get all resources for this package
resources <- list_package_resources("8a6eceb2-821b-4961-a29d-758f3087732d")
 
# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
 
# load the first datastore resource as a sample
shelter <- filter(datastore_resources, row_number()==1) %>% get_resource()

### Clean data ###
# convert dates to months, calculate occupancy rates, select cetain columns
shelter_clean <-
  shelter |>
  filter(CAPACITY != 0 & OCCUPANCY != 0 & SHELTER_CITY == "Toronto") |>
  mutate(DATE = mdy(OCCUPANCY_DATE), MONTH = month(DATE), OCCUPANCY_MONTH = 
           case_when(
             MONTH == 1 ~ "Jan",
             MONTH == 2 ~ "Feb",
             MONTH == 3 ~ "Mar",
             MONTH == 4 ~ "Apr",
             MONTH == 5 ~ "May",
             MONTH == 6 ~ "June",
             MONTH == 7 ~ "July",
             MONTH == 8 ~ "Aug",
             MONTH == 9 ~ "Sept",
             MONTH == 10 ~ "Oct",
             MONTH == 11 ~ "Nov",
             MONTH == 12 ~ "Dec",
           ), OCCUPANCY_RATE = round(OCCUPANCY / CAPACITY, 2)) |>
  select(DATE, OCCUPANCY_MONTH, SECTOR, OCCUPANCY, CAPACITY, OCCUPANCY_RATE)

# keep dates and combine data with the same dates
shelter_date <-
  shelter_clean |>
  select(DATE, OCCUPANCY, CAPACITY) |>
  group_by(DATE) |>
  summarise(
    occupancy = sum(OCCUPANCY),
    capacity = sum(CAPACITY)
  ) |>
  mutate(occupancy_rate=round(occupancy/capacity, 3))

# combine data with the same cities
shelter_city <-
  shelter |>
  filter(CAPACITY != 0 & OCCUPANCY != 0) |>
  group_by(SHELTER_CITY) |>
  summarise(occupancy = sum(OCCUPANCY)) |>
  select(SHELTER_CITY,occupancy) 
