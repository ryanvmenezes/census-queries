library(tidyverse)

lang.by.year = read_csv('language-data.csv')

# lang.by.year %>% 
#   
#   group_by(year) %>% 
#   mutate(pop.pct = pop.count / sum(pop.count))

lang.xwalk = lang.by.year %>% 
  mutate(
    lang = language %>% 
      str_replace('^\\d+:\\s', '') %>% 
      word() %>%
      str_replace_all(',', '')
  ) %>%
  group_by(language, lang) %>%
  summarise(n = sum(pop.count)) %>%
  arrange(-n) %>% 
  mutate(
    n = case_when(
      str_starts(lang, 'N/A') ~ 0,
      TRUE ~ n
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    langf = fct_lump(lang, n = 12, w = n),
    langf = fct_reorder(langf, n)
  )

lang.xwalk %>% head(15)

