library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(fedmatch)
library(lubridate)
library(RColorBrewer)

#Load data

vacc_price <- read.csv("data_out/vacc_price.csv") %>% 
  select(-X) %>%
  mutate(age = factor(age, levels= c("child", "adult")))



#facet wrap by age of all vaccines
p <- 
  vacc_price %>%
  filter(short.name != c("HPV.2v.adult", "HPV.9v.adult")) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  filter(year < 2023) %>%
  ggplot(aes(y=private.price.2022, x=year, color=vaccine)) +
  geom_line() +
  geom_point(size =4) +
  facet_wrap(~age)  +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  theme(axis.title = element_text(face="bold",  size=20),
        axis.text  = element_text(face="bold",  size=16, color = "black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size = 14, face="bold"), 
        panel.spacing.x = unit(2, "lines")) +
  ylab("Private Sector Cost per Dose \n(in US$2022 based on CPI medical)") +
  labs(color="Vaccine",
       shape="Age group") +
  scale_x_continuous(breaks=c(2006,2010, 2015,2020,2022))


ggsave('Figures/highres_price_evolution.tiff', p, device = "tiff", dpi = 200)


## Deprecated Plots ----
# #all in one
# vacc_price %>%
#   ggplot(aes(y=price.2022, x=year, color=short.name, shape=age)) +
#   geom_line() +
#   geom_point(size =4) +
#   theme(axis.title = element_text(face="bold",  size=20),
#         axis.text  = element_text(face="bold",  size=16, color = "black"),
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=16)) +
#   ylab("Price adjusted by 2023 PCI") +
#   labs(color="Vaccine",
#        shape="Age group") +
#   scale_x_continuous(breaks=c(2006,2010, 2015,2020,2023))
#   
# #2 plots
# vacc_price %>%
#   filter(age == "child") %>%
#   ggplot(aes(y=price.2023, x=year, color=short.name)) +
#   geom_line() +
#   geom_point(size =4) +
#   theme(axis.title = element_text(face="bold",  size=20),
#         axis.text  = element_text(face="bold",  size=16, color = "black"),
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=16)) +
#   ylab("Price adjusted by 2023 PCI") +
#   labs(color="Vaccine",
#        shape="Age group") +
#   scale_x_continuous(breaks=c(2006,2010, 2015,2020,2023))
# 
# 
# vacc_price %>%
#   filter(age == "adult") %>%
#   ggplot(aes(y=price.2023, x=year, color=short.name)) +
#   geom_line() +
#   geom_point(size =4) +
#   theme(axis.title = element_text(face="bold",  size=20),
#         axis.text  = element_text(face="bold",  size=16, color = "black"),
#         legend.title = element_text(size=16),
#         legend.text = element_text(size=16)) +
#   ylab("Price adjusted by 2023 PCI") +
#   labs(color="Vaccine",
#        shape="Age group") +
#   scale_x_continuous(breaks=c(2007,2010, 2015,2020,2023))


#Individual Vaccines ----

#PCV Prices ----
PCV_price <- vacc_price %>%
  filter(grepl("PCV", short.name)) %>%
  group_by(short.name) %>%
  filter(year == min(year)) %>%
  select(-pci.2022.factor)

write.csv(PCV_price, "data/Intro_PCV_price.csv")

#facet wrap
p <- 
  vacc_price %>%
  filter(grepl("PCV13", short.name)) %>%
  #filter(grepl("PCV", short.name)) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  select(vaccine, year, age, cdc.price.2022, private.price.2022) %>%
  pivot_longer(cols = c(cdc.price.2022, private.price.2022),names_to = "price.type") %>%
  mutate(price.type = ifelse(price.type == "cdc.price.2022", "CDC", "Private")) %>%
  filter(year < 2023) %>%
  mutate(age = factor(age, levels= c("child", "adult")),
         Payer = price.type) %>%
  ggplot(aes(y=value, x=year, color=Payer)) +
  geom_line() +
  geom_point(size =4) +
  facet_wrap(~age)  +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face="bold",  size=20),
        axis.text  = element_text(face="bold",  size=16, color = "black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size = 14, face="bold")) +
  labs(#shape="Vaccine",
    Color="Payer",
    y = "Cost per Dose \n(in US$2022 based on CPI medical)",
    x = "year",
    title = "PCV13 vaccine price increase over time") +
  scale_x_continuous(breaks=c(2006,2010, 2015,2020,2022)) +
  theme(panel.spacing.x = unit(2, "lines"))


ggsave('Figures/highres_PCV_price_evolution.tiff', p, device = "tiff", dpi = 200)



#HPV.9V ----
p <- vacc_price %>%
  filter(grepl("HPV.9v", short.name)) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  select(vaccine, year, age, cdc.price.2022, private.price.2022) %>%
  pivot_longer(cols = c(cdc.price.2022, private.price.2022),names_to = "price.type") %>%
  mutate(price.type = ifelse(price.type == "cdc.price.2022", "CDC", "Private")) %>%
  filter(year < 2023) %>%
  mutate(age = factor(age, levels= c("child", "adult")),
         Payer = price.type) %>%
  ggplot(aes(y=value, x=year, color=Payer)) +
  geom_line() +
  geom_point(size =4) +
  #facet_wrap(~age)  +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face="bold",  size=20),
        axis.text  = element_text(face="bold",  size=16, color = "black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size = 14, face="bold")) +
  labs(#shape="Vaccine",
    Color="Payer",
    y = "Cost per Dose \n(in US$2022 based on CPI medical)",
    x = "year",
    title = "HPV.9V vaccine price increase over time") +
  scale_x_continuous(breaks=c(2006,2010, 2015,2020,2022)) +
  theme(panel.spacing.x = unit(2, "lines"))


ggsave('Figures/highres_HPV9_price_evolution.tiff', p, device = "tiff", dpi = 200)

# RZV ----
p <- 
  vacc_price %>%
  filter(grepl("RZV", short.name)) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  select(vaccine, year, age, cdc.price.2022, private.price.2022) %>%
  pivot_longer(cols = c(cdc.price.2022, private.price.2022),names_to = "price.type") %>%
  mutate(price.type = ifelse(price.type == "cdc.price.2022", "CDC", "Private")) %>%
  filter(year < 2023) %>%
  mutate(age = factor(age, levels= c("child", "adult")),
         Payer = price.type) %>%
  ggplot(aes(y=value, x=year, color=Payer)) +
  geom_line() +
  geom_point(size =4) +
  #facet_wrap(~age)  +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face="bold",  size=20),
        axis.text  = element_text(face="bold",  size=16, color = "black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size = 14, face="bold")) +
  labs(#shape="Vaccine",
    Color="Payer",
    y = "Cost per Dose \n(in US$2022 based on CPI medical)",
    x = "year",
    title = "RZV (Shingrix) vaccine price increase over time") +
  scale_x_continuous(breaks=c(2006,2010, 2018,2020,2022)) +
  theme(panel.spacing.x = unit(2, "lines"))


ggsave('Figures/highres_RZV_price_evolution.tiff', p, device = "tiff", dpi = 200)

#Varivax ----

#are Adult and Child Prices the same? No. 
vacc_price %>%
  filter(grepl("varivax", short.name)) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  select(vaccine, year, age, private.price.2022) %>%
  pivot_wider(names_from = "age", values_from = "private.price.2022") %>%
  ggplot(aes(y=adult, x=child)) +
  geom_line() +
  geom_point(size =4)
  

p <- 
  vacc_price %>%
  filter(grepl("varivax", short.name)) %>%
  separate(short.name, into=c("vaccine","type"), sep="\\.(?=[^.]+$)") %>%
  #playing around with shape being the same for vaccine groups
  mutate(family = substr(vaccine, 1, 3)) %>%
  select(vaccine, year, age, cdc.price.2022, private.price.2022) %>%
  pivot_longer(cols = c(cdc.price.2022, private.price.2022),names_to = "price.type") %>%
  mutate(price.type = ifelse(price.type == "cdc.price.2022", "CDC", "Private")) %>%
  filter(year < 2023) %>%
  mutate(age = factor(age, levels= c("child", "adult")),
         Payer = price.type) %>%
  ggplot(aes(y=value, x=year, color=Payer)) +
  geom_line() +
  geom_point(size =4) +
  facet_wrap(~age)  +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face="bold",  size=20),
        axis.text  = element_text(face="bold",  size=16, color = "black"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        strip.text = element_text(size = 14, face="bold")) +
  labs(#shape="Vaccine",
    Color="Payer",
    y = "Cost per Dose \n(in US$2022 based on CPI medical)",
    x = "year",
    title = "Varivax vaccine price increase over time") +
  scale_x_continuous(breaks=c(2002,2007,2012, 2017,2022)) +
  theme(panel.spacing.x = unit(2, "lines"))


ggsave('Figures/highres_pox_price_evolution.tiff', p, device = "tiff", dpi = 200)

