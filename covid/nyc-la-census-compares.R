library(tidycensus)
library(tidyverse)

# Race --------------------------------------------------------------------
# Split into five-category definition

raceraw = get_acs(
  geography = 'zcta',
  variables = c(
    total = 'B03002_001',
    white = 'B03002_003',
    black = 'B03002_004',
    other = 'B03002_005',
    asian = 'B03002_006',
    other = 'B03002_007',
    other = 'B03002_008',
    other = 'B03002_009',
    latino = 'B03002_012'
  ),
  year = 2018
)

raceraw

race = raceraw %>% 
  group_by(zcta = GEOID, variable) %>% 
  summarise(total = sum(estimate)) %>% 
  spread(variable, total) %>% 
  mutate(pct_asian = asian / total,
         pct_black = black / total,
         pct_latino = latino / total,
         pct_other = other / total,
         pct_white = white / total) %>% 
  select(zcta, white, asian:other, total, pct_white, pct_asian:pct_other)

head(race)

race %>% write_csv('census/zcta-race.csv')

# Median household income -------------------------------------------------

hhincomeraw = get_acs(
  geography = 'zcta',
  variables = c(
    hhincome = 'B19013_001'
  ),
  year = 2018
)

hhincome = hhincomeraw %>% 
  select(zcta = GEOID, median_hh_income = estimate)

hhincome

hhincome %>% write_csv('census/zcta-median-hh-income.csv')


# Public transit use ------------------------------------------------------


transitraw = get_acs(
  geography = 'zcta',
  variables = c(
    total = 'B08101_001',
    car.alone = 'B08101_009',
    car.carpool = 'B08101_017',
    public.transit = 'B08101_025',
    walked = 'B08101_033',
    other.commute = 'B08101_041',
    worked.at.home = 'B08101_049'
  ),
  year = 2018
)

transitraw

# transitraw %>% 
#   filter(variable == 'total') %>% 
#   select(GEOID, total = estimate) %>% 
#   left_join(
#     transitraw %>% 
#       filter(variable != 'total') %>% 
#       group_by(GEOID) %>% 
#       summarise(total.sum = sum(estimate))
#   ) %>% 
#   filter(total != total.sum)

transit = transitraw %>% 
  group_by(zcta = GEOID, variable) %>% 
  summarise(total = sum(estimate)) %>% 
  spread(variable, total) %>% 
  mutate(pct.car.alone = car.alone / total,
         pct.car.carpool = car.carpool / total,
         pct.public.transit = public.transit / total,
         pct.walked = walked / total,
         pct.other.commute = other.commute / total,
         pct.worked.at.home = worked.at.home / total) %>% 
  select(zcta, total,
         car.alone, car.carpool, public.transit, walked, other.commute, worked.at.home,
         pct.car.alone, pct.car.carpool, pct.public.transit, pct.walked, pct.other.commute, pct.worked.at.home)

transit

transit %>% write_csv('census/zcta-transit.csv')

transit %>% arrange(-pct.worked.at.home)
