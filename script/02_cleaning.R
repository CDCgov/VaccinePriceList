# Data cleaning with manual boost; produces data_out/adjusted_annual_prices.csv 
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)
library(xlsx)

#current file can have multiple lines for packaging, and costs per date/brandname_tradename.  
#The html table setup changed on Dec 22, 2005 prior dates only have a single table with adult and peds mixed into the peds file. For dates after 2005, the adult and peds prices are truly separate. The adult file is a bit more straight forward, so I will do those first. 

adult_raw <- read.csv( "data/adult_raw.csv")  
ped_raw <- read.csv("data/peds_raw.csv")

# create new rows when there is a line break ----
# NA warning expected
d_single <- 
  adult_raw %>% 
  #select col of interest
  select(vaccine,manufacturer, brandname_tradename, packaging, cdc_cost_dose, private_sector_cost_dose, date, year) %>%
  #expand col with multiple entries
  separate(private_sector_cost_dose, into=paste0("I",1:6), sep="\n") %>%
  separate(cdc_cost_dose, into=paste0("C",1:6), sep="\n") %>%
  separate(packaging, into=paste0("P",1:6), sep="\n") %>%
  #put it all back together
  unite("CP1",I1, C1, P1, sep ="\n") %>%
  unite("CP2",I2, C2, P2, sep ="\n") %>%
  unite("CP3",I3, C3, P3, sep ="\n") %>%
  #convert to long
  pivot_longer(cols=c(CP1,CP2,CP3),values_to=c("cost_dose_packaging")) %>%
  #separate costs and packaging
  separate_wider_delim(cost_dose_packaging, names=c("private_cost_dose","cdc_cost_dose", "packaging"), delim="\n", too_few = "align_start") %>%  
  #remove dollar sign/convert to numeric
  mutate(private_cost_dose = str_replace(private_cost_dose, "\\$", "") %>% as.numeric(),
         cdc_cost_dose = str_replace(cdc_cost_dose, "\\$", "") %>% as.numeric()) 

# NA warning expected
ped_d_single <- 
  ped_raw %>% 
  #select col of interest
  select(vaccine,manufacturer, brandname_tradename, packaging, cdc_cost_dose, private_sector_cost_dose, date, year) %>%
  #remove flu
  filter(!str_detect(vaccine, "flu")) %>%
  #expand col with multiple entries
  separate(brandname_tradename, into=paste0("V",1:6), sep="\n") %>% #this splits Tripedia and Daptacel, but also cuts hb off of recombivax hb
  separate(private_sector_cost_dose, into=paste0("I",1:6), sep="\n") %>%
  separate(cdc_cost_dose, into=paste0("C",1:6), sep="\n") %>%
  separate(packaging, into=paste0("P",1:6), sep="\n") %>%
  #put it all back together
  unite("CP1",V1,I1, C1, P1, sep ="\n") %>%
  unite("CP2",V2,I2, C2, P2, sep ="\n") %>%
  unite("CP3",V3,I3, C3, P3, sep ="\n") %>%
  unite("CP4",V4,I4, C4, P4, sep ="\n") %>%
  unite("CP5",V5,I5, C5, P5, sep ="\n") %>%
  unite("CP6",V6,I6, C6, P6, sep ="\n") %>%
  #convert to long
  pivot_longer(cols=c(CP1,CP2,CP3,CP4,CP5,CP6),values_to=c("cost_dose_packaging")) %>%
  #separate costs and packaging
  separate_wider_delim(cost_dose_packaging, names=c("brandname_tradename","private_cost_dose","cdc_cost_dose", "packaging"), delim="\n", too_few = "align_start") %>%  
  #remove dollar sign/convert to numeric
  mutate(private_cost_dose = str_replace(private_cost_dose, "\\$", "") %>% as.numeric(),
         cdc_cost_dose = str_replace(cdc_cost_dose, "\\$", "") %>% as.numeric()) %>%
  #full in brandname for times that there are multiple prices due to packaging
  mutate(brandname_tradename = ifelse(brandname_tradename == "NA", NA, brandname_tradename)) %>%
  fill(brandname_tradename, .direction = "down") 

#need to calculate price per year for private and cdc separately since there are entries that have multiple values for cdc price but not private. ----
adult_prices <- 
  bind_rows(d_single %>%
              #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(private_cost_dose)) %>%
              rename(cost = private_cost_dose) %>%
              mutate(cost_type = "private") %>%
              select(-cdc_cost_dose),
            d_single %>%
            #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(cdc_cost_dose)) %>%
              rename(cost = cdc_cost_dose) %>%
              mutate(cost_type = "cdc")%>%
              select(-private_cost_dose)
      ) %>%
  #Cleaning brandname_tradename  
    #remove special characters
    mutate(brandname_tradename= str_replace_all(brandname_tradename,"\\u00AE|\\u00a9|\\u2122|\n", "")) %>%
    # remove punctuation
    mutate(brandname_tradename= str_replace_all(brandname_tradename, "[[:punct:]]", "")) %>%
    #remove spaces
    mutate(brandname_tradename= str_replace_all(brandname_tradename," ", "")) %>%
    #make all lowercase
    mutate(brandname_tradename= str_to_lower(brandname_tradename)) %>%
  #Correct vaccine names  
    #MMR
    mutate(brandname_tradename= ifelse(brandname_tradename == "mmr11", "mmrii", brandname_tradename))  %>%
    # Td
    mutate(brandname_tradename= ifelse(grepl("Mass", manufacturer) & grepl("tet", brandname_tradename), "td.rename", brandname_tradename))  %>%
  mutate(brandname_tradename= ifelse(grepl("Akorn", manufacturer) & grepl("Tet", vaccine), "td.rename", brandname_tradename))  %>%
    mutate(brandname_tradename= ifelse(grepl("Merck", manufacturer) & grepl("Tet", vaccine), "td.merck", brandname_tradename))  %>%
    #drop unneed columns
    select(vaccine, manufacturer, brandname_tradename, date, year, cost, packaging, cost_type) %>%
  #Convert data types
    mutate(cost = as.numeric(cost),
           year = as.numeric(year),
           date = as.Date(date, "%Y-%m-%d"), 
           age = "adult") 

#Pre 2005 ----
# assign age group manually pre- Dec 2005
pre2005_intermediate <- 
  bind_rows(ped_d_single %>%
              #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(private_cost_dose)) %>%
              rename(cost = private_cost_dose) %>%
              mutate(cost_type = "private") %>%
              select(-cdc_cost_dose),
            ped_d_single %>%
              #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(cdc_cost_dose)) %>%
              rename(cost = cdc_cost_dose) %>%
              mutate(cost_type = "cdc")%>%
              select(-private_cost_dose)
  ) %>%
  mutate(cost = as.numeric(cost),
         year = as.numeric(year),
         date = as.Date(date, "%Y-%m-%d")) %>%
  filter(date <= '2005-1-12') %>%
  arrange(desc(date)) %>%
  
  #Cleaning brandname_tradename  
  #remove special characters
  mutate(brandname_tradename= str_replace_all(brandname_tradename,"\\u00AE|\\u00a9|\\u2122|\\u0023|\n", "")) %>%
  # remove punctuation
  mutate(brandname_tradename= str_replace_all(brandname_tradename, "[[:punct:]]", "")) %>%
  #remove spaces
  mutate(brandname_tradename= str_replace_all(brandname_tradename," ", "")) %>%
  #make all lowercase
  mutate(brandname_tradename= str_to_lower(brandname_tradename)) %>%
  #Correct vaccine names  
  #change twinris in 2001 to twinrix
  mutate(brandname_tradename= ifelse(grepl("twinris", brandname_tradename), "twinrix", brandname_tradename)) %>%
  #MMR
  mutate(brandname_tradename= ifelse(brandname_tradename == "mmr11", "mmrii", brandname_tradename))  %>%
  # Td
  mutate(brandname_tradename= ifelse(grepl("Mass", manufacturer) & grepl("tet", brandname_tradename), "td.rename", brandname_tradename))  %>%
  mutate(brandname_tradename= ifelse(grepl("Akorn", manufacturer) & grepl("Tet", vaccine), "td.rename", brandname_tradename))  %>%
  mutate(brandname_tradename= ifelse(grepl("Merck", manufacturer) & grepl("Tet", vaccine), "td.merck", brandname_tradename))  %>%
  
  #Assign age based on vaccine and/or brandname_tradename
  mutate(age = NA) %>%
  #use grepl for fuzzy matching
  mutate(age = case_when(
    grepl("Ped", vaccine, ignore.case = TRUE) ~ "ped", #names with pediatric in it
    grepl("Ado", vaccine, ignore.case = TRUE) ~ "ped", #names with adolescent in it
    #Pathogen specific names
    grepl("DTaP", vaccine, ignore.case = TRUE) ~ "ped", #DTap
    grepl("Meas", vaccine, ignore.case = TRUE) ~ "ped", #measles
    grepl("HiB", vaccine, ignore.case = TRUE) ~ "ped", #HiB
    grepl("Varicella", vaccine, ignore.case = TRUE) ~ "ped", #varicella
    grepl("rubella", vaccine, ignore.case = TRUE) ~ "ped", 
    grepl("mump", vaccine, ignore.case = TRUE) ~ "ped", 
    grepl("mmr", vaccine, ignore.case = TRUE) ~ "ped", 
    grepl("IPV", vaccine, ignore.case = TRUE) ~ "ped", #polio 
    
    grepl("Adu", vaccine, ignore.case = TRUE) ~ "adult",
    grepl("18", vaccine, ignore.case = TRUE) ~ "adult",
    grepl("tetanus", vaccine, ignore.case = TRUE) ~ "adult")) %>%
  #this leaves PPSV23 which is recommended in both ages
  #filter(is.na(age))
  #drop extra columns
  select(vaccine, manufacturer, brandname_tradename, date, year, cost, packaging, cost_type, age)

#duplicate PPSV for both age groups
pre2005_prices <- 
  pre2005_intermediate %>%
  mutate( age = replace_na(age, "adult")) %>%
  bind_rows(pre2005_intermediate %>%
              filter(brandname_tradename == "pneumovax") %>%
              mutate( age = replace_na(age, "ped"))) 

# Peds post 2005 ----
post2005_ped_prices <- 
  bind_rows(ped_d_single %>%
              #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(private_cost_dose)) %>%
              rename(cost = private_cost_dose) %>%
              mutate(cost_type = "private") %>%
              select(-cdc_cost_dose),
            ped_d_single %>%
              #remove NAs introduced by separate and pivots, filter on cdc rather than private if 
              filter(!is.na(cdc_cost_dose)) %>%
              rename(cost = cdc_cost_dose) %>%
              mutate(cost_type = "cdc")%>%
              select(-private_cost_dose)
  ) %>%
  
  mutate(cost = as.numeric(cost),
         year = as.numeric(year),
         date = as.Date(date, "%Y-%m-%d")) %>%
  filter(date > '2005-1-12') %>%
  arrange(desc(date)) %>%
  #Cleaning brandname_tradename  
  #remove special characters
  mutate(brandname_tradename= str_replace_all(brandname_tradename,"\\u00AE|\\u00a9|\\u2122|\\u0023|\\u2122|\n", "")) %>%
  # remove punctuation
  mutate(brandname_tradename= str_replace_all(brandname_tradename, "[[:punct:]]", "")) %>%
  #remove spaces
  mutate(brandname_tradename= str_replace_all(brandname_tradename," ", "")) %>%
  #make all lowercase
  mutate(brandname_tradename= str_to_lower(brandname_tradename)) %>%
  #remove tm
  mutate(brandname_tradename= str_replace_all(brandname_tradename, "tm$", "")) %>%
  #fix recombivax/recombivax HB inconsistency 
  mutate(brandname_tradename= ifelse(brandname_tradename == "recombivax", "recombivaxhb", brandname_tradename)) %>%
  #fix tenivac
  mutate(brandname_tradename= ifelse(grepl("tenivac", brandname_tradename), "tenivac", brandname_tradename)) %>%
  #Td
  mutate(brandname_tradename= ifelse(grepl("mass", brandname_tradename) & grepl("tet", vaccine, ignore.case = TRUE), "td.rename", brandname_tradename))  %>%
  mutate(brandname_tradename= ifelse(grepl("Merck", manufacturer) & grepl("Tet", vaccine, ignore.case = TRUE), "td.merck", brandname_tradename))  %>%
  mutate(age = "ped") %>%  
  #drop extra columns
  select(vaccine, manufacturer, brandname_tradename, date, year, cost, packaging, cost_type, age) %>%
  #Convert data types
  mutate(cost = as.numeric(cost),
         year = as.numeric(year),
         date = as.Date(date, "%Y-%m-%d")) 

#Put all the prices together ----
#Same columns
str(adult_prices)
str(post2005_ped_prices)
str(pre2005_prices)
all_prices <- adult_prices %>%
  bind_rows(post2005_ped_prices) %>%
  bind_rows(pre2005_prices) %>%
  arrange(date) %>%
  unite("name.age",brandname_tradename, age, sep =".", remove = FALSE)
str(all_prices)

write.csv(all_prices, "data_out/all_prices.csv")

#Calculating price per year weighted by days at price (for vaccines currently on the market) ----

## vaccines currently on the market (2023)
m23 <- 
  all_prices %>%
  group_by(brandname_tradename, age) %>%
  summarise(maxy= max(year)) %>%
  filter(maxy == 2023) %>%
  select(brandname_tradename, age) %>%
  ungroup() %>%
  unite("name.age",brandname_tradename, age, sep =".", remove = FALSE) %>%
  arrange(age)

write.xlsx(m23, file= "data_out/vaccine_details.xlsx", sheetName = "OnMarket23", append=TRUE)

##when was vaccine first added
year_added <- 
  all_prices %>%
  inner_join(m23) %>%
  group_by(name.age, cost_type) %>%
  filter(date == min(date)) %>%
  #filter(date != "2023-01-01") %>%
  mutate(year_start = year,
         date_start = date) %>%
  select(brandname_tradename,age, year_start) %>%
  as_tibble() %>%
  group_by(year_start) %>%
  arrange(age) %>%
  ungroup() %>%
  distinct()

write.xlsx(year_added, file= "data_out/vaccine_details.xlsx", sheetName = "YearIntroduced", append = TRUE)

#adding in first and last day of each year for weighted average
#1. date shell
date_shell <-
  data.frame(
    date = as.Date(c(
      paste(c(2001:2023), "01-01", sep = "-"),
      paste(c(2001:2023), "12-31", sep = "-")), "%Y-%m-%d"),
    year = c(2001:2023),
    brandname_tradename = rep(m23$brandname_tradename, each = 46),
    age= "ped"
  ) 
  
#2. duplicate for age
age_shell <- 
  date_shell %>%
    bind_rows(date_shell %>%
                mutate(age = "adult")) %>%
  as_tibble() %>%
    arrange(date) %>%
    #adjust year range of date.cap to year range vaccine was offered
    left_join(year_added, by=c("brandname_tradename", "age")) %>%
    filter(!is.na(year_start)) %>%
    distinct() %>%
    filter(year >= year_start) %>%
    select(!c(year_start)) 


# Add cost type col
date_range <- 
  bind_rows(
    age_shell %>% 
      mutate(cost_type = "private"),
    age_shell %>% 
      mutate(cost_type = "cdc")
  ) %>%
  unite("name.age",brandname_tradename, age, sep =".", remove = FALSE)

ave_annual_prices <- 
  all_prices %>%  
    #keep those currently on market
    filter(name.age %in% m23$name.age) %>%
    #filter(brandname_tradename == "havrix" & packaging == "10 pack – 1 dose vials") %>%
    #add in beginning and end of year
    bind_rows(date_range) %>%
    arrange(date) %>%
    group_by(name.age, cost_type) %>%
    fill(cost, .direction = "down") %>%
    fill(manufacturer, .direction = "down") %>%
    fill(vaccine, .direction = "down") %>%
    fill(packaging, .direction = "down") %>%
    #remove dates prior to vaccine introduction
    filter(!is.na(vaccine)) %>%
    ungroup() %>%
    
    # Packaging does not seem to effect price, so if multiple packaging is reported per date, I will only use the first entry; originally tried to just use the first packaging type introduced but packing changes too much to get a longer time series
    group_by(name.age, date, cost_type) %>%
    slice_head() %>%
    ungroup() %>%
  
      # Calculate the weighted average cost for each year
      #add julian date to calculate days at price
      mutate(j.date = yday(date)) %>%
      group_by(name.age,cost_type, year) %>%
      mutate(lead.date = lead(j.date),
             lead.date = ifelse(is.na(lead.date),365, lead.date),
             #price.date = (j.date -lag.date)/365) %>%
             days_at_price = lead.date - j.date) %>%
      mutate(ave_annual_cost_per_dose = weighted.mean(cost, days_at_price/365)) %>%
      select(vaccine,manufacturer,brandname_tradename, cost_type,ave_annual_cost_per_dose) %>%
      slice(year,1)  

write.csv(ave_annual_prices, "data_out/ave_annual_prices.csv")

#Convert to 2023 USD ----

#PCI index
inflation <- read.csv("data/price_index.csv") %>%
   select(-cpi.2022.factor) #%>%
  # #replacing NA from 2023 as 1 since we are so early in the year. 
  # mutate(pci.2023.factor =ifelse(is.na(pci.2022.factor), 1, pci.2022.factor))

#Add inflation CPI index
adj_all_prices <- 
  ave_annual_prices  %>%
  left_join(inflation) %>%
    mutate(price.2023 = ave_annual_cost_per_dose * cpi.2023.factor) %>%
    ungroup()   
    
  
#write.csv(adj_all_prices, "data_out/CPIadjusted_annual_prices.csv")

#Add inflation PCE index
adjPCE_all_prices <- 
  ave_annual_prices  %>%
  left_join(inflation) %>%
  mutate(price.2023 = ave_annual_cost_per_dose * pce.2023.factor) %>%
  ungroup()   


#write.csv(adjPCE_all_prices, "data_out/PCEadjusted_annual_prices.csv")

adjusted_prices <- 
    bind_rows(
    adj_all_prices %>%
      mutate(index = "CPI"), 
    adjPCE_all_prices %>%
      mutate(index = "PCE")
    ) %>%
    select(-c(PCE, CPI))

write.csv(adjusted_prices, "data_out/adjusted_annual_prices.csv")

