pacman::p_load(tidyr, tidyverse, readxl, ggplot2,patchwork, xlsx)
pacman::p_load(tidyr, tidyverse, readxl, ggplot2,patchwork, xlsx, )


  
  
  
  # Figure 2. Time trends of vaccine prices (US$2023) from the CDC vaccine price list  ----

  #create list of vaccines to plot based on year added/on the market
  
  vacc_price <-  read.csv("data_out/adjusted_annual_prices.csv") %>%
    select(-X)
  
  year_added <- read_excel("data/vacc_summary.xlsx", sheet = "YearIntroduced")  %>%
    filter(year_start < 2022) 
  
  
  #m23 <- read_excel("data/vacc_summary.xlsx", sheet = "OnMarket23")
  year_added <- read_excel("data/vacc_summary.xlsx", sheet = "YearIntroduced")  %>%
    filter(year_start < 2022) 

    #price <- 
 F2potential <- 
  vacc_price %>%
    filter(cost_type == "private") %>%
    #pull out vacc that are currently on the market and were introduced prior to 2022 
   inner_join(year_added) %>%
   group_by(name.age) %>%
   count() %>%
   separate_wider_delim(name.age, names=c("name","age"), delim=".", too_few = "align_start", cols_remove = FALSE) 
   
    
 write.csv(F2potential, "data_out/Fig2potential.csv")
 
 
 group_by(name.age) %>%
    mutate(min_year = min(year)) %>%
    filter(min_year < 2022) 
    
  filter(name.age %in% c("prevnar13.ped", "shingrix.adult")) %>%
    #Cleaning manufacturer  
    #remove special characters
    mutate(manufacturer= str_replace_all(manufacturer,"\n", ""))%>%
    mutate(manufacturer= str_to_lower(manufacturer)) %>%
    # collapsing Aventis Pasteur, Sanofi Pasteur, and Sanofi into sanofi
    mutate(manufacturer= ifelse(grepl("aventis", manufacturer), "sanofi", manufacturer)) %>%
    mutate(manufacturer= ifelse(grepl("sanofi", manufacturer), "sanofi", manufacturer)) %>%  
    filter(cost_type == "private") %>%
    separate_wider_delim(name.age, names=c("name","age"), delim=".", too_few = "align_start", cols_remove = FALSE) %>%
  

    #just keep the good stuff
    select(-c(X, ave_annual_cost_per_dose,pci.2023.factor)) %>%
    mutate(year = as.numeric(year)) %>%
    rename(value = price.2023) %>%
    as_tibble()
