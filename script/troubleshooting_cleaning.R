#finding the missing private costs of rotateq and other

adj_all_prices %>%
  filter(cost_type == "private") %>%
  filter(grepl("rota", brandname_tradename, ignore.case = TRUE)) %>%
  filter(is.na(ave_annual_cost_per_dose))

#missing in raw data, so go back to web scraping function. 
#raw data has two columns private_sector_cost_doses and private_sector_cost_dose

peds_results %>%
  filter(grepl("rota", brandname_tradename, ignore.case = TRUE)) %>%
  select(date, cdc_cost_dose, private_sector_cost_dose, private_sector_cost_doses ) %>%
  filter(!is.na(private_sector_cost_doses))
 
# all data is in private_sector_cost_dose [SINGULAR] so where is the plural column coming from? 
#What about for other vaccines?

adult_results %>%
  select(date, brandname_tradename, cdc_cost_dose, private_sector_cost_dose, private_sector_cost_doses ) %>%
  filter(!is.na(private_sector_cost_doses)) %>%
  summarise(max(date))


peds_results %>%
  mutate(private_sector_cost_dose = ifelse(is.na(private_sector_cost_dose), private_sector_cost_doses, private_sector_cost_dose)) %>%
select(date, brandname_tradename, cdc_cost_dose, private_sector_cost_dose, private_sector_cost_doses ) %>%
  filter(is.na(private_sector_cost_dose)) 
