library(glue)
library(tidyverse)
library(tidycensus)

geog = 'place'

state.age = get_acs(
  geography = geog,
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

state.age

state.total = state.age %>% 
  select(NAME, total = B01001_001E)

state.total

age.0.17 = state.age %>%
  select(-ends_with('M')) %>% 
  select(NAME, B01001_003E:B01001_006E, B01001_027E:B01001_030E) %>% 
  transmute(NAME, age.0.17 = rowSums(select_if(., is.numeric)))

age.0.17

age.65.99 = state.age %>%
  select(-ends_with('M')) %>% 
  select(NAME, B01001_020E:B01001_025E, B01001_044E:B01001_049E) %>% 
  transmute(NAME, age.65.99 = rowSums(select_if(., is.numeric)))

age.65.99

age.18.64 = state.age %>%
  select(-ends_with('M')) %>% 
  select(-B01001_001E, -B01001_002E, -B01001_026E, -B01001_003E:-B01001_006E, -B01001_027E:-B01001_030E, -B01001_020E:-B01001_025E, -B01001_044E:-B01001_049E) %>% 
  select(-GEOID) %>% 
  transmute(NAME, age.18.64 = rowSums(select_if(., is.numeric)))

age.18.64

age.groups = age.0.17 %>% left_join(age.18.64) %>% left_join(age.65.99)

age.groups.long = age.groups %>% 
  pivot_longer(-NAME, names_to = 'age.group', values_to = 'pop') %>% 
  group_by(NAME) %>% 
  mutate(pop.pct = pop / sum(pop)) %>% 
  ungroup()

age.groups.long 

age.groups.long %>% 
  write_csv(glue('ca-{geog}-age-groups.csv'))

age.groups %>% 
  mutate(
    total = (age.0.17 + age.18.64 + age.65.99),
    pct.65 = age.65.99 / total
  ) %>% 
  filter(!str_detect(NAME, 'CDP')) %>%
  arrange(-pct.65)

statewide.65.plus = 0.1357759
  
age.groups.long %>% mutate(
    age.group = fct_relevel(age.group, c('age.65.99','age.18.64','age.0.17')),
    NAME = fct_relevel(
      NAME,
      age.groups.long %>% 
        filter(age.group == 'age.65.99') %>%
        mutate(
          zval = (pop.pct - statewide.65.plus) / sqrt(statewide.65.plus * (1 - statewide.65.plus) / pop),
          total = pop / pop.pct
        ) %>% 
        arrange(zval) %>%
        view() %>% 
        pull(NAME)
    )
  ) %>% 
  ggplot(aes(NAME, pop.pct, fill = age.group)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1 - 0.1357759) +
  coord_flip() +
  theme_minimal()
