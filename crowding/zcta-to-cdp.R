library(tidycensus)
library(sf)
library(tidyverse)

places = get_acs(
  geography = 'place',
  state = 'CA',
  variables = 'B01001_001',
  geometry = TRUE
)

places

zctas = get_acs(
  geography = 'zcta',
  variables = 'B01001_001',
  geometry = TRUE
)

zctas

places

intersection = st_intersection(
  places %>% transmute(city = NAME),
  zctas %>% transmute(zcta = str_replace(NAME, 'ZCTA5 ', ''))
)

intersection

intersection %>% ggplot() + geom_sf()

intersection %>% 
  st_set_geometry(NULL) %>% 
  filter(str_detect(city, ' CDP \\([\\w\\s]+ County\\), California'))


word(intersection$city, -2) %>% table()


zcta.city.xwalk = intersection %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  mutate(
    city = str_replace(city, ' CDP, California', ''),
    city = str_replace(city, ' city, California', ''),
    city = str_replace(city, ' town, California', ''),
    city = str_replace(city, ' CDP \\([\\w\\s]+ County\\), California', ''),
  ) %>% 
  group_by(zcta) %>% 
  summarise(
    cities = str_c(sort(unique(city)), collapse = ', '),
    .groups = 'drop'
  )

crowd.zcta = read_csv('crowding-zcta.csv')

crowd.zcta

crowd.zcta %>% 
  mutate(zcta = str_replace(zcta, 'ZCTA5 ', '')) %>% 
  left_join(zcta.city.xwalk)
