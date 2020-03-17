library(ipumsr)
library(tidyverse)

ddi = read_ipums_ddi('data/usa_00009.xml')
ddi

data = read_ipums_micro(ddi)
data

str(data)

data %>% distinct(LANGUAGE)
data %>% distinct(LANGUAGED)
data %>% distinct(YEAR)
data %>% distinct(COUNTYFIP)

la.lang.by.year = data %>% 
  filter(COUNTYFIP == 37) %>% 
  group_by(YEAR, LANGUAGED) %>% 
  summarise(total = sum(PERWT)) %>% 
  group_by(YEAR) %>% 
  mutate(percent = total / sum(total)) %>% 
  ungroup(YEAR) %>% 
  transmute(
    year = YEAR,
    langcode = zap_labels(LANGUAGED),
    language = as_factor(LANGUAGED),
    total, percent
  )

la.lang.by.year  

