pacman::p_load(tidyr, tidyverse, readxl, plotly)

# read in data

#data used in model
model <- 
  read_excel('data/ACIP_request.xlsx', sheet = "model.input" ) %>%
  # pivot_longer(c(matches("input"))) %>%
   separate_wider_delim(currency.year, "$", names = c("A", "year")) %>%
  separate_wider_delim(short.name, ".", names = c("short.name", "B"), too_few = "align_start", too_many = "drop") %>%  
    rename(age =  child.adult, funder = public.private.price) %>%
    rename(value = input.base.2022) %>%
   mutate(year = as.numeric(year)) %>%
  select(!c(A,B)) %>%
  as_tibble()
model
colnames(model)

#market price data
price <- read.csv("data_out/vacc_price.csv") %>%
    # harmonize col names and formats with model data set
    separate_wider_delim(short.name, ".", names = c("short.name", "B"), too_few = "align_start", too_many = "drop") %>%
   pivot_longer(c(cdc.price.2022, private.price.2022)) %>%
  separate_wider_delim(name, ".", names = c("funder", "C"), too_few = "align_start", too_many = "drop") %>%
  mutate(funder = ifelse(funder == "cdc", "public", funder)) %>%
  select(-c(X,B,C, private.mean.price, cdc.mean.price, pci.2022.factor)) %>%
  #only keep vaccines that we have model data for
  filter(short.name  %in% unique(model$short.name)) %>%
  filter(funder == "private") %>%
  mutate(year = as.numeric(year)) %>%
  as_tibble()
price
colnames(price)

#make plot of data used in model and market price
p <- ggplot(data = model, aes(x= year, y = value, color = short.name, pch = funder)) +
  geom_point(size =2) +
  geom_errorbar(aes(ymin=input.low.2022, ymax=input.high.2022)) +
  geom_point(data= price, aes(y = value, x= year, color = short.name, pch = funder))+
  geom_line(data= price, aes(y = value, x= year, color = short.name))+
  facet_wrap(~age) 

ggplotly(p)

#

ggplot(data = model, aes(x= year, y = value, color = short.name)) +
  geom_point( aes(size =4, shape = sponsor.model)) +
  geom_errorbar(aes(ymin=input.low.2022, ymax=input.high.2022)) +
  geom_point(data= price, aes(y = value, x= year, color = short.name))+
  geom_line(data= price, aes(y = value, x= year, color = short.name))+
  facet_wrap(~age) 
