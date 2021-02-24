library(tidyverse)
library(tidycensus)

counties = get_acs(
  geography = 'county',
  variables = 'B01003_001',
  year = 2019
)

counties

counties %>% 
  select(fips = GEOID, county = NAME, population = estimate) %>% 
  write_csv('population-by-county.csv', na = '')

