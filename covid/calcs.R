library(tidyverse)

nyc.cases = read_csv(
  'https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests-by-zcta.csv',
  col_types = cols(
    MODZCTA = col_character(),
    Positive = col_double(),
    Total = col_double(),
    zcta_cum.perc_pos = col_double()
  )
)

nyc.cases


race = read_csv('census/zcta-race.csv')

race.avgs = race %>%
  select(-starts_with('pct'), -zcta, -total) %>%
  summarise_all(sum) %>%
  mutate_all(~. / sum(race$total))

race.avgs

race.avgs$white

race.scaled = race %>% 
  mutate(
    pct_nonwhite = 1 - pct_white,
    z_nonwhite = (pct_nonwhite - (1-race.avgs$white)) / sqrt(race.avgs$white * (1-race.avgs$white) / total)
  ) %>% 
  arrange(-z_nonwhite) %>% 
  select(zcta, total, pct_white, pct_nonwhite, z_nonwhite)

race.scaled

median.hh.income = read_csv('census/zcta-median-hh-income.csv')

median.hh.income

transit = read_csv('census/zcta-transit.csv')

transit.avgs = transit %>%
  select(-starts_with('pct'), -zcta, -total) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate_all(~. / sum(transit$total, na.rm = TRUE))

transit.avgs

transit.scaled = transit %>% 
  select(zcta, total, ends_with('public.transit'), ends_with('at.home')) %>% 
  mutate(
    z_transit = (pct.public.transit - transit.avgs$public.transit) / sqrt(transit.avgs$public.transit * (1-transit.avgs$public.transit) / total),
    z_wfh = (pct.worked.at.home - transit.avgs$worked.at.home) / sqrt(transit.avgs$worked.at.home * (1-transit.avgs$worked.at.home) / total)
  )


case.calcs = nyc.cases %>% 
  drop_na(MODZCTA) %>% 
  select(zcta = MODZCTA, covid = Positive) %>% 
  right_join(race.scaled) %>% 
  right_join(transit.scaled, by = 'zcta', suffix = c('.allpop', '.workforce')) %>% 
  right_join(median.hh.income) %>% 
  mutate(infection.per.cap = covid / total.allpop)

case.calcs

model = glm(infection.per.cap ~ z_nonwhite + z_transit + z_wfh + median_hh_income, data = case.calcs, family = 'binomial')

model

33120 - 32943

summary(model)

predictions = case.calcs %>% 
  mutate(model.pred = predict(model, newdata = .))
