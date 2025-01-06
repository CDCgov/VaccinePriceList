# Table 1 Price at intro, now, and % change for all vaccines ----
# Script produces data_out/Tbl1_PCE_2023.xlsx which has multiple sheets with summary statistics  
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(xlsx)
library(readxl)

#define index to use for analysis: PCE or CPI. Run with both indices for 05_supplement.R. File names change with index_use. 
index_use <- "PCE"
vacc_price <-  read.csv("data_out/adjusted_annual_prices.csv") %>%
  select(-X) %>%
  filter(index == index_use)


unadjust_price <- 
  vacc_price %>%
  group_by(name.age, cost_type) %>%
  summarise(intro = min(year), recent = max(year)) %>%
  pivot_longer(!c("name.age", "cost_type"), names_to = "market", values_to = "year") %>%
  
  inner_join( vacc_price, by= c("name.age","year", "cost_type")) %>%
  select(name.age,brandname_tradename, market, cost_type, ave_annual_cost_per_dose) %>%
  pivot_wider(names_from = market, values_from = c(ave_annual_cost_per_dose)) %>%
  rename(price_intro = intro,
         price_recent = recent ) #%>%
 #supplementary analysis to see if gardasil is driving intro year pattern
 # filter(brandname_tradename != 'gardasil9')

#manual grouping of vaccines 
vacc_group <-  read.csv("data/vacc_groups.csv")

# Build Table 1 ----
#pull out max, min of years. Twinrix has different min years for private/cdc. 
Tbl1 <- 
  bind_rows(
    #min/max for private values
    vacc_price %>%
      filter(cost_type == "private") %>%  
      group_by(name.age) %>%
      summarise(intro = min(year), recent = max(year)) %>%
      pivot_longer(!"name.age", names_to = "market", values_to = "year") %>%
      mutate(cost_type = 'private'),
    #min/max for cdc values
    vacc_price %>%
      filter(cost_type == "cdc") %>%  
      group_by(name.age) %>%
      summarise(intro = min(year), recent = max(year)) %>%
      pivot_longer(!"name.age", names_to = "market", values_to = "year") %>%
      mutate(cost_type = 'cdc') ) %>%
  
  #merge with price list so each vaccine has 4 lines one for market entrance and then most recent, and private/cdc price
  inner_join(vacc_price) %>%
  #keep col of interest
  
  select(c(brandname_tradename ,name.age, market, cost_type, year, price.2023  )) %>%
  pivot_wider(names_from = market, values_from = c(price.2023, year)) %>%
  #calculate % change
  mutate( market_years = year_recent - year_intro,
          price_change = price.2023_recent - price.2023_intro) %>%
  mutate( average_annual_change = price_change/market_years,
          annual_pct_change = exp(log(price.2023_recent/price.2023_intro)/market_years)-1) %>%
  select(brandname_tradename, name.age, cost_type, year_intro, price.2023_intro, year_recent, price.2023_recent, price_change, market_years, average_annual_change, annual_pct_change) %>%
  # add non-adjusted price back in for easy comparison 
  inner_join(unadjust_price) %>%
  #add manufacturer
  left_join(vacc_price %>% 
              select(name.age,vaccine, manufacturer) %>% 
              group_by(name.age) %>%
              slice_head()) %>%
  #add vaccine group
  left_join(vacc_group %>%
              select(vaccine_group, brandname_tradename)) %>%
  #clean it up
  arrange(year_intro) %>%
  #Cleaning manufacturer  
  #remove special characters
  mutate(manufacturer= str_replace_all(manufacturer,"\n", ""))%>%
  mutate(manufacturer= str_to_lower(manufacturer)) %>%
  # collapsing Aventis Pasteur, Sanofi Pasteur, and Sanofi into sanofi
  mutate(manufacturer= ifelse(grepl("aventis", manufacturer), "sanofi", manufacturer)) %>%
  mutate(manufacturer= ifelse(grepl("sanofi", manufacturer), "sanofi", manufacturer)) %>%  
  separate_wider_delim(name.age, names=c("name","age"), delim=".", too_few = "align_start", cols_remove = FALSE) %>%
  select(manufacturer, vaccine_group, vaccine, name.age, brandname_tradename, age, cost_type,
         year_intro, price_intro, price.2023_intro,
         year_recent, price_recent, price.2023_recent,
         price_change, market_years, average_annual_change, annual_pct_change) %>%
  distinct() %>%
  filter(year_intro < 2022)

# Summary statistics on table 1 ----
#number of digits to include in copy/paste table ready columns
rd <- 2
byMan <- Tbl1 %>%
  group_by(cost_type, manufacturer) %>%
  summarise(n= length(annual_pct_change),
            mean_annuall_pct_change = mean(annual_pct_change), 
            min_annuall_pct_change = fivenum(annual_pct_change)[1],
            lower_annuall_pct_change = fivenum(annual_pct_change)[2],
            median_annuall_pct_change = fivenum(annual_pct_change)[3],
            upper_annuall_pct_change = fivenum(annual_pct_change)[4],
            max_annuall_pct_change = fivenum(annual_pct_change)[5]) %>%
  mutate(Tbl3 = paste0(round(mean_annuall_pct_change*100,digits=rd), " (",round(lower_annuall_pct_change*100,digits=rd), ";", round(upper_annuall_pct_change*100,digits=rd), ")"), .before= mean_annuall_pct_change)


byVacc <- Tbl1 %>%
  group_by(cost_type, vaccine_group) %>%
  summarise(n= length(annual_pct_change),
            mean_annuall_pct_change = mean(annual_pct_change), 
            min_annuall_pct_change = fivenum(annual_pct_change)[1],
            lower_annuall_pct_change = fivenum(annual_pct_change)[2],
            median_annuall_pct_change = fivenum(annual_pct_change)[3],
            upper_annuall_pct_change = fivenum(annual_pct_change)[4],
            max_annuall_pct_change = fivenum(annual_pct_change)[5])%>%
  mutate(Tbl3 = paste0(round(mean_annuall_pct_change*100,digits=rd), " (",round(lower_annuall_pct_change*100,digits=rd), ";", round(upper_annuall_pct_change*100,digits=rd), ")"), .before= mean_annuall_pct_change)

byAge <- Tbl1 %>%
  group_by(cost_type, age) %>%
  summarise(n= length(annual_pct_change),
            mean_annuall_pct_change = mean(annual_pct_change), 
            min_annuall_pct_change = fivenum(annual_pct_change)[1],
            lower_annuall_pct_change = fivenum(annual_pct_change)[2],
            median_annuall_pct_change = fivenum(annual_pct_change)[3],
            upper_annuall_pct_change = fivenum(annual_pct_change)[4],
            max_annuall_pct_change = fivenum(annual_pct_change)[5]) %>%
  mutate(Tbl3 = paste0(round(mean_annuall_pct_change*100,digits=rd), " (",round(lower_annuall_pct_change*100,digits=rd), ";", round(upper_annuall_pct_change*100,digits=rd), ")"), .before= mean_annuall_pct_change)

byIntroYear <- Tbl1 %>%
  mutate(introduction_year = ifelse(year_intro > 2009, "2010 and later", "2009 and earlier")) %>%
  group_by(cost_type, introduction_year) %>%
  summarise(n= length(annual_pct_change),
            mean_annuall_pct_change = mean(annual_pct_change), 
            min_annuall_pct_change = fivenum(annual_pct_change)[1],
            lower_annuall_pct_change = fivenum(annual_pct_change)[2],
            median_annuall_pct_change = fivenum(annual_pct_change)[3],
            upper_annuall_pct_change = fivenum(annual_pct_change)[4],
            max_annuall_pct_change = fivenum(annual_pct_change)[5]) %>%
  mutate(Tbl3 = paste0(round(mean_annuall_pct_change*100,digits=rd), " (",round(lower_annuall_pct_change*100,digits=rd), ";", round(upper_annuall_pct_change*100,digits=rd), ")"), .before= mean_annuall_pct_change)

byIntroYearb <- Tbl1 %>%
  mutate(introduction_year = ifelse(year_intro > 2008, "2009 and later", "2008 and earlier")) %>%
  group_by(cost_type, introduction_year) %>%
  summarise(n= length(annual_pct_change),
            mean_annuall_pct_change = mean(annual_pct_change), 
            min_annuall_pct_change = fivenum(annual_pct_change)[1],
            lower_annuall_pct_change = fivenum(annual_pct_change)[2],
            median_annuall_pct_change = fivenum(annual_pct_change)[3],
            upper_annuall_pct_change = fivenum(annual_pct_change)[4],
            max_annuall_pct_change = fivenum(annual_pct_change)[5]) %>%
  mutate(Tbl3 = paste0(round(mean_annuall_pct_change*100,digits=rd), " (",round(lower_annuall_pct_change*100,digits=rd), ";", round(upper_annuall_pct_change*100,digits=rd), ")"), .before= mean_annuall_pct_change)

# Save Analysis ----


filename <- paste0(index_use,"_Tbl1_LastIntro2021.xlsx")
write.csv(Tbl1, paste0("data_out/",paste0(index_use, "_Tbl1_LastIntro2021.csv")))
#Tbl1_PCE_2023.xlsx already exists with sheet that has lookup table to go from vaccine -> vaccine group (Label_Vaccine)
xlsx::write.xlsx(Tbl1, paste0("data_out/",filename), sheetName = "Price", append=TRUE)
xlsx::write.xlsx(as.data.frame(byMan), paste0("data_out/",filename), sheetName = "Summary_byManufacturer", append=TRUE)
xlsx::write.xlsx(as.data.frame(byVacc), paste0("data_out/",filename), sheetName = "Summary_byVaccGroup", append=TRUE)
xlsx::write.xlsx(as.data.frame(byAge),paste0("data_out/",filename), sheetName = "Summary_byAge", append=TRUE)
xlsx::write.xlsx(as.data.frame(byIntroYear), paste0("data_out/",filename), sheetName = "Summary_byIntro", append=TRUE)
xlsx::write.xlsx(as.data.frame(byIntroYearb), paste0("data_out/",filename), sheetName = "Summary_byIntrob", append=TRUE)

#create one last sheet for easy renaming for plot labels; manually added nice name in excell
#vector of names in table
# namelabels <- 
#   Tbl1 %>% 
#   group_by(vaccine_group,name.age, age) %>%
#   summarise()
# 
# xlsx::write.xlsx(as.data.frame(namelabels), "data_out/Tbl1_PCE_2023.xlsx", sheetName = "Labels_Vaccine", append=TRUE)




