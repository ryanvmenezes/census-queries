library(rvest)
library(tidyverse)

lang.raw = read_csv('languages-la-county.csv')
lang.raw

lang.by.year = lang.raw %>% 
  pivot_longer(`1980`:`2018`, names_to = 'year', values_to = 'popcount') %>% 
  mutate(
    year = parse_integer(year),
    popcount = str_split(popcount, '\n'),
    popcount = map_chr(popcount, `[[`, 3),
    popcount = parse_number(popcount)
  ) %>% 
  group_by(year) %>% 
  mutate(pop.pct = popcount / sum(popcount)) %>% 
  ungroup() %>% 
  arrange(-popcount)

lang.by.year

lang.xwalk = lang.by.year %>% 
  group_by(language) %>%
  summarise(n = sum(popcount)) %>%
  arrange(-n) %>% 
  mutate(
    n = case_when(
      str_starts(language, '0: N/A') ~ 0,
      TRUE ~ n
    )
  ) %>% 
  mutate(
    languagef = str_replace(language, '^\\d+:\\s', ''),
    languagef = word(languagef),
    languagef = str_replace_all(languagef, ',', ''),
    languagef = fct_lump(languagef, n = 10, w = n),
    languagef = fct_reorder(languagef, n)
  )

lang.xwalk %>% head(15)

lang.xwalk$languagef

lang.by.year %>% 
  left_join(lang.xwalk %>% select(-n)) %>% 
  filter(languagef != 'Other') %>% 
  ggplot(aes(languagef, pop.pct * 100, fill = languagef)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(year ~ ., nrow = 1) +
  scale_fill_brewer(palette = "Paired", name = 'Language') +
  ylab('Percent of L.A. County population') +
  ggtitle('Most-spoken languages in L.A. County') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

lang.by.year %>% 
  left_join(lang.xwalk %>% select(-n)) %>% 
  filter(languagef != 'Other') %>% 
  group_by(year) %>%
  mutate(rankinyear = rank(-pop.pct)) %>%
  filter(!(languagef %in% c('English','Spanish'))) %>%
  ggplot(aes(languagef, pop.pct * 100, fill = languagef)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(year ~ ., nrow = 1) +
  # geom_text(aes(languagef, pop.pct * 100 + 0.05, label = rankinyear), size = 3) +
  geom_text(aes(languagef, pop.pct * 100 + 0.18, label = formatC(popcount, format="d", big.mark=",")), size = 3, angle = 90) +
  scale_fill_brewer(palette = "Paired", name = 'Language') +
  ylab('Percent of L.A. County population') +
  ggtitle('Most-spoken languages in L.A. County, excluding English and Spanish') +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())


lang.by.year %>% 
  left_join(lang.xwalk %>% select(-n)) %>% 
  group_by(year, languagef) %>% 
  summarise(pop.pct = sum(pop.pct)) %>% 
  ggplot(aes(year, pop.pct * 100)) +
  geom_line() +
  facet_wrap(languagef ~ ., scales = 'free') +
  theme_minimal()

top10.by.year = lang.by.year %>% 
  filter(!str_detect(language, 'N/A')) %>% 
  group_by(year) %>% 
  mutate(rankinyear = rank(desc(popcount))) %>% 
  filter(rankinyear <= 10) %>% 
  ungroup()

top10.by.year %>% 
  select(year, rankinyear, language) %>% 
  pivot_wider(names_from = year, values_from = language)

top10.by.year %>% 
  select(year, pop.pct, rankinyear) %>% 
  pivot_wider(names_from = year, values_from = pop.pct)


top10.by.year %>% distinct(language)

# 
# 
# lang.by.year %>%
#   left_join(lang.xwalk %>% select(-n)) %>%
#   group_by(year, language) %>%
#   summarise(pop.pct = sum(pop.pct)) %>%
#   ggplot(aes(year, pop.pct * 100)) +
#   geom_line() +
#   facet_wrap(language ~ ., scales = 'free') +
#   ylab('Percent of L.A. County population') +
#   theme_minimal()
