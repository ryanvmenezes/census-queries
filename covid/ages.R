library(tidyverse)
library(tidycensus)


state.age = get_acs(
  geography = 'state',
  state = 'CA',
  variables = c(
    "B01001_001",
    "B01001_002",
    "B01001_003",
    "B01001_004",
    "B01001_005",
    "B01001_006",
    "B01001_007",
    "B01001_008",
    "B01001_009",
    "B01001_010",
    "B01001_011",
    "B01001_012",
    "B01001_013",
    "B01001_014",
    "B01001_015",
    "B01001_016",
    "B01001_017",
    "B01001_018",
    "B01001_019",
    "B01001_020",
    "B01001_021",
    "B01001_022",
    "B01001_023",
    "B01001_024",
    "B01001_025",
    "B01001_026",
    "B01001_027",
    "B01001_028",
    "B01001_029",
    "B01001_030",
    "B01001_031",
    "B01001_032",
    "B01001_033",
    "B01001_034",
    "B01001_035",
    "B01001_036",
    "B01001_037",
    "B01001_038",
    "B01001_039",
    "B01001_040",
    "B01001_041",
    "B01001_042",
    "B01001_043",
    "B01001_044",
    "B01001_045",
    "B01001_046",
    "B01001_047",
    "B01001_048",
    "B01001_049"
  ),
  output = 'wide',
  year = 2018
)

state.total = state.age %>% 
  pull(B01001_001E)

age.0.17 = state.age %>%
  select(-ends_with('M')) %>% 
  select(B01001_003E:B01001_006E, B01001_027E:B01001_030E) %>% 
  rowSums()

age.65.99 = state.age %>%
  select(-ends_with('M')) %>% 
  select(B01001_020E:B01001_025E, B01001_044E:B01001_049E) %>% 
  rowSums()

age.18.64 = state.age %>%
  select(-ends_with('M')) %>% 
  select(-B01001_001E, -B01001_002E, -B01001_026E, -B01001_003E:-B01001_006E, -B01001_027E:-B01001_030E, -B01001_020E:-B01001_025E, -B01001_044E:-B01001_049E) %>% 
  select(-GEOID, -NAME) %>% 
  rowSums()

state.total - age.0.17 - age.65.99 == age.18.64

comparison = tibble(
  bracket = c('age.0.17','age.18.64','age.65.99'),
  pop = c(age.0.17, age.18.64, age.65.99)
) %>% 
  mutate(pop.pct = pop / sum(pop)) %>% 
  left_join(
    tibble(
      bracket = c('age.0.17','age.18.64','age.65.99'),
      covid.cases = c(6, 210, 116)
    ) %>% 
      mutate(covid.cases.pct = covid.cases / sum(covid.cases))
  )

comparison

comparison %>% 
  select(bracket, ends_with('pct')) %>% 
  pivot_longer(-bracket, names_to = 'group', values_to = 'pct') %>% 
  ggplot(aes(x = bracket, y = pct, fill = group)) +
  geom_bar(stat = 'identity', position = 'dodge')
