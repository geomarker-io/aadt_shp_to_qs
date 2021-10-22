library(tidyverse)
library(sf)
library(stringr)
library(qs)

states <- tigris::states() %>%
  st_drop_geometry() %>%
  select(state = NAME) %>%
  filter(!state %in% c('United States Virgin Islands',
                       'Commonwealth of the Northern Mariana Islands',
                       'Guam', 'American Samoa', 'Puerto Rico')) %>%
  mutate(state = str_to_lower(state),
         state = str_remove_all(state, " "))

states$state[states$state == 'districtofcolumbia'] <- 'district'

download_clean_aadt <- function(state) {
  fl.zip <- glue::glue('{state}2017.zip')
  download.file(glue::glue('https://www.fhwa.dot.gov/policyinformation/hpms/shapefiles/{fl.zip}'),
                destfile=fl.zip)
  unzip(fl.zip, exdir = glue::glue('{state}'))
  unlink(fl.zip)
  fl.shp <- list.files(pattern = "shp", path = glue::glue('{state}'))

  d.tmp <- rgdal::readOGR(glue::glue('{state}/{fl.shp}'))
  d.tmp <- st_as_sf(d.tmp)

  d.tmp <- st_zm(d.tmp)

  d.tmp <- select(d.tmp,
                  state_fips = State_Code,
                  route_id = Route_ID,
                  road_type = F_System,
                  county_fips = County_Cod,
                  aadt = AADT,
                  aadt_single_unit = AADT_Singl,
                  aadt_combination = AADT_Combi)

  d.tmp <- d.tmp %>%
    mutate(state_fips = str_pad(state_fips, width = 2, side = "left", pad = "0"),
           county_fips = str_pad(county_fips, width = 2, side = "left", pad = "0"),
           road_type = factor(road_type, levels = c(1, 2, 3, 4, 5, 6, 7),
                              labels = c("Interstate",
                                         "Principal Arterial - Other Freeways and Expressways",
                                         "Principal Arterial - Other",
                                         "Minor Arterial",
                                         "Major Collector",
                                         "Minor Collector",
                                         "Local"),
                              ordered = TRUE))

  d.tmp <- st_transform(d.tmp, 5072)

  qs::qsave(x = d.tmp,
            file = glue::glue('aadt_by_state/{state}2017.qs'))

  fs::dir_delete(glue::glue('{state}'))
}

purrr::walk(states$state[1:13], download_clean_aadt)

purrr::walk(states$state[15:nrow(states)], download_clean_aadt)

### for some reason these states didn't work... try again
# state <- 'california'
# state <- 'arkansas'
# state <- 'arizona'
# state <- 'iowa'
# state <- 'virginia'
