library(tidyverse)
library(tidycensus)

# census_api_key(<<KEY_GOES_HERE>>, install = TRUE)
# Sys.getenv("CENSUS_API_KEY")

occ.table = get_acs(
  geography = 'zcta',
  variables = c(
    # https://www.socialexplorer.com/data/ACS2018_5yr/metadata/?ds=ACS18_5yr&table=B25014
    "total" = 'B25014_001',
    "owner_occ" = 'B25014_002',
    "owner_occ_0.5_or_less_per_room" = 'B25014_003',
    "owner_occ_0.51_to_1.0_per_room" = 'B25014_004',
    "owner_occ_1.01_to_1.5_per_room" = 'B25014_005',
    "owner_occ_1.51_to_2.0_per_room" = 'B25014_006',
    "owner_occ_2.01_or_more_per_room" = 'B25014_007',
    "renter_occ" = 'B25014_008',
    "renter_occ_0.5_or_less_per_room" = 'B25014_009',
    "renter_occ_0.51_to_1.0_per_room" = 'B25014_010',
    "renter_occ_1.01_to_1.5_per_room" = 'B25014_011',
    "renter_occ_1.51_to_2.0_per_room" = 'B25014_012',
    "renter_occ_2.01_or_more_per_room" = 'B25014_013'
  ),
  year = 2018,
  cache_table = TRUE
)

occ.table

# totals

total = occ.table %>% 
  filter(variable == 'total') %>% 
  select(zcta = NAME, total = estimate)

total

# crowded

crowded = occ.table %>% 
  filter(str_detect(variable, '1.01|1.51|2.01')) %>% 
  group_by(zcta = NAME) %>% 
  summarise(crowded = sum(estimate))

crowded

# not crowded

not.crowded = occ.table %>% 
  filter(str_detect(variable, '0.5_or_less|0.51')) %>% 
  group_by(zcta = NAME) %>% 
  summarise(not.crowded = sum(estimate))

not.crowded

# join them 

calcs = total %>% 
  left_join(crowded) %>% 
  left_join(not.crowded)

calcs

# make sure the sums line up

calcs %>% 
  mutate(mismatch = !((crowded + not.crowded) == total)) %>% 
  summarise(mismatches = sum(mismatch))

# z-value calculation

overall.crowding = sum(calcs$crowded) / sum(calcs$total)

calcs = calcs %>% 
  mutate(
    pct.crowded = crowded / total,
    zval = (pct.crowded - overall.crowding) / sqrt(overall.crowding * (1 - overall.crowding) / total)
  )

calcs %>% arrange(-pct.crowded)

calcs %>% arrange(-zval)

calcs %>% arrange(-zval) %>% write_csv('crowding-zcta.csv')

occ.table %>% write_csv('occupancy-data-zctas.csv')
