citizens = get_acs(
  geography = "congressional district",
  variables = c(
    totalpop = 'B05001_001',
    noncitizen = 'B05001_006'
  ),
  output = 'wide',
)

citizens %<>% 
  # summarise(sum(noncitizenE, na.rm = TRUE) / sum(totalpopE, na.rm = TRUE))
  mutate(pctnoncit = noncitizenE / totalpopE) %>% 
  arrange(-pctnoncit) %>% 
  mutate(cit = totalpopE - noncitizenE)

summary(citizens$totalpopE)

summary(citizens$pctnoncit)

citizens

citizens %>% arrange(cit)
