library(rvest)
library(tidyverse)

web.query = read_html('sda-queries/query-lacounty-all-languages.html')

column.names = web.query %>% 
  html_nodes('tr[align="center"]') %>% 
  `[`(2) %>% 
  html_nodes('td') %>% 
  html_text() %>% 
  str_sub(end = 4)

column.names

data = web.query %>% 
  html_nodes('table') %>% 
  `[[`(3) %>% 
  html_nodes('tr[align="right"]') %>% 
  html_text() %>% 
  tibble(htmltext = .) %>% 
  mutate(htmltext = str_replace(htmltext, 'language\n', '')) %>% 
  separate(htmltext, sep = '\n', into = c('language', column.names, 'total', 'blank')) %>% 
  select(-blank, -total) %>% 
  mutate_at(vars(-language), ~str_replace_all(., '.*\\)', '')) %>% 
  mutate_at(vars(-language), ~str_replace_all(., '.*---', '')) %>% 
  mutate_at(vars(-language), ~str_replace_all(., ',', '')) %>% 
  mutate_at(vars(-language), ~parse_double(.)) %>% 
  filter(language != 'COL TOTAL') %>% 
  pivot_longer(-language, names_to = 'year', values_to = 'pop.count')

data

data %>% write_csv('language-data.csv')
