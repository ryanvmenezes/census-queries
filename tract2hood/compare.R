library(tidyverse)

blockhood = read_csv('mapla_LA_blk_community_alloc_xwalk.csv')
blockhood

blockhood %>% count(community_parent, community_name) %>% filter(community_parent != community_name) %>% View()

blockhood %>% mutate(bchar = nchar(census_2010_geoid)) %>% count(bchar)

blocktracthood = blockhood %>% 
  mutate(tractid = str_sub(census_2010_geoid, end = 11)) %>% 
  select(blockid = census_2010_geoid, tractid, population = census_2010_block_population, hood = community_parent)
blocktracthood

tractpop = blocktracthood %>% 
  group_by(tractid) %>% 
  summarise(tractpop = sum(population))

proportion = blocktracthood %>% 
  group_by(tractid, hood) %>% 
  summarise(tracthoodpop = sum(population, na.rm = TRUE)) %>% 
  left_join(
    blocktracthood %>% 
      group_by(tractid) %>% 
      summarise(tractpop = sum(population, na.rm = TRUE))
  ) %>% 
  arrange(desc(tractid)) %>% 
  mutate(tracthoodpct = tracthoodpop / tractpop)
proportion


tract2hoodgeo = read_csv('tract-neighborhood-allocation.csv')

comparison = tract2hoodgeo %>% 
  select(tractid = tract, hood = name, pct.geo = tract_percent) %>% 
  full_join(
    proportion %>%
      select(tractid, hood, pct.block = tracthoodpct)
  ) %>% 
  arrange(tractid)

comparison %>% write_csv('comparison.csv', na = '')
