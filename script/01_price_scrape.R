#Scrape CDC archived vaccine prices for prediatric and adult vaccines

#library(xml2)
library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(fedmatch)
library(lubridate)

#Data Prep ----
## pull links of archived price lists from main page ----
url <- "https://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/archive.html"
links <-
  #retrive all links on page
  read_html(url) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  # filter out just price-list links
  str_subset("price-list/2") %>%
  as_tibble()  %>%
  #add metadata
  mutate(date = as.Date(str_extract(value, "[:digit:][:digit:][:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]"), "%Y-%m-%d"),
         year = as.numeric(str_extract(value, "[:digit:][:digit:][:digit:][:digit:]"))
  ) %>%
  #create full link for web scraping
  mutate(full.url = url_absolute(value, "https://www.cdc.gov")) %>%
  select(year, date, full.url)

#remove broken links on site; would be better to add tryCatch but this is faster for now.
links <- links %>%
  filter(date != "2022-10-1")

peds_results <- NULL
adult_results <- NULL

for (i  in 1:dim(links)[1]) {
  #add random pause to avoid CAPTCHA
  Sys.sleep(runif(1, 1, 2))
  print(i)


  tmp <-
    read_html(links$full.url[i])  %>%
    html_nodes("table") %>%
    html_table()

  #pull peds table
  peds_results <-
    tmp %>%
    `[[`(1) %>%
    clean_names() %>%
    mutate(date = links$date[i],
           year = links$year[i]) %>%
    bind_rows(peds_results)

  #pull adult table
  if (length(tmp) > 1) {
    adult_results <-
      tmp %>%
      `[[`(2) %>%
      clean_names() %>%
      mutate(date = links$date[i],
             year = links$year[i]) %>%
      bind_rows(adult_results)
  }
}


## fix col names ----
# with the switch from one to two tables in 2005 private_sector_cost_doses [PLURAL] changed to singular, so need to collapse them into a single col
peds_results <- 
  peds_results %>%
    mutate(private_sector_cost_dose = ifelse(is.na(private_sector_cost_dose), private_sector_cost_doses, private_sector_cost_dose)) %>%
    select(-private_sector_cost_doses  )


## create table for vaccine names ----
peds.names <- cbind(type="child", name=unique(peds_results$brandname_tradename))
adult.names <- cbind(type="adult", name=unique(adult_results$brandname_tradename))
vacc.names <- rbind(peds.names, adult.names)

#save raw data ----
write.csv(peds_results, "data/peds_raw.csv")
write.csv(adult_results, "data/adult_raw.csv")
write.csv(vacc.names, "data/vacc_brandnames.csv")
