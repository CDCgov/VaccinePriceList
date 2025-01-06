#load packages and other functions, set gloabl objects
pacman::p_load(tidyr, tidyverse, readxl, ggplot2,patchwork, xlsx, Polychrome, ggtext)
#Polychrome is used for the color generator

#Comparing PCE to CPI
pce <- read.csv("data_out/PCE_Tbl1_LastIntro2021.csv") %>%
  select(-X) %>%
  mutate(inflation.index = "PCE")

cpi <- read.csv("data_out/CPI_Tbl1_LastIntro2021.csv") %>%
  select(-X) %>%
  mutate(inflation.index = "CPI")


d <- bind_rows(cpi, pce) 


#How the results compare
d %>%
  select(name.age,age, cost_type, annual_pct_change, inflation.index) %>%
  pivot_wider( names_from = inflation.index, values_from = annual_pct_change) %>%
  ggplot(aes(x = CPI, y = PCE, shape= cost_type, color=age)) +
  geom_point(size= 2) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  labs(title="Annual Percent Change by Inflation Index") +
  theme_bw()
  
# calculate regression coefficients 
corr_coef <- 
  d %>%
  select(name.age,age, cost_type, annual_pct_change, inflation.index) %>%
  pivot_wider( names_from = inflation.index, values_from = annual_pct_change) %>%
  group_by(age, cost_type) %>%
  summarize(COR = stats::cor.test(CPI, PCE)$estimate,
            pval = stats::cor.test(CPI, PCE)$p.value
  ) 

write.csv(corr_coef, "data_out/supplement/index_correlation.csv")

#does the rank order change?
rank_change <- 
bind_cols(
pce %>%
  group_by(age, cost_type) %>%
  arrange(average_annual_change, .by_group = TRUE) %>%
  select(brandname_tradename, age, cost_type, average_annual_change) %>%
  mutate(pce_group = case_when(age == "adult" & cost_type == "cdc" ~ "a",
                           age == "ped" & cost_type == "cdc" ~ "b",
                           age == "adult" & cost_type == "private" ~ "c",
                           age == "ped" & cost_type == "private" ~ "d")) %>%
  rename(pce_order = brandname_tradename,
         pce_aac = average_annual_change,
         pce_age = age,
         pce_cost = cost_type),
cpi %>%
  group_by(age, cost_type) %>%
  arrange(average_annual_change, .by_group = TRUE) %>%
  select(brandname_tradename, age, cost_type, average_annual_change) %>%
  mutate(cpi_group = case_when(age == "adult" & cost_type == "cdc" ~ "a",
                           age == "ped" & cost_type == "cdc" ~ "b",
                           age == "adult" & cost_type == "private" ~ "c",
                           age == "ped" & cost_type == "private" ~ "d")) %>%
  rename(cpi_order = brandname_tradename,
         cpi_aac = average_annual_change,
         cpi_age = age,
         cpi_cost = cost_type)) %>%
  select(pce_order, cpi_order, pce_group, cpi_group, pce_aac, cpi_aac)
#What about the impact of introduction year?

txt <- 12
p <- 
  d %>%
  select(name.age,age, cost_type, annual_pct_change, inflation.index, year_intro) %>%
  mutate(age= ifelse(age == "ped","Pediatric", "Adult"),
         cost_type = ifelse(cost_type == "cdc", "CDC", "Private")) %>%
  mutate(across(c(inflation.index, age, cost_type), factor)) %>%
  mutate(age = fct_relevel(age, c("Pediatric", "Adult")),
         cost_type = fct_relevel(cost_type, c("Private", "CDC"))) %>% 
  ggplot(aes(x=year_intro, y= annual_pct_change*100,color=inflation.index)) +
  geom_jitter() +
  #geom_smooth(alpha = 0.15) +
  facet_grid(age~cost_type) +
  labs(y = "Annual Inflation Adjusted Price Change (%)", x="Vaccine Introduction Year", color ="Inflation Index") +
  theme_bw() + 
  theme(
    legend.background = element_blank(),
    #legend.box.background = element_rect(colour = "black"),
    #legend.spacing.y = unit(4, "mm"), 
    legend.key=element_rect(fill="white"),
    #panel.background = element_blank(),
    axis.line.x = element_line(color="black", linewidth = 1),
    axis.line.y = element_line(color="black", linewidth = 1),
    axis.ticks = element_line(colour = "black"), 
    axis.ticks.length = unit(6, "pt"),
    #axis.line.x.top  = element_line(color="black", size=3),
    #axis.text.x=element_text(angle=45, hjust=1), 
    axis.text = element_text(colour = "black",size=txt -2),
    text = element_text(face="bold",size=txt),
    legend.text  = element_text(size = txt - 1),
    legend.title = element_text(size = txt - 1),
    # panel.border = element_rect(colour = "black", fill=NA),
    #panel.background = element_rect(colour = "black", size=1),
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(
      fill = "grey80", 
      color = "black", 
      linewidth = 1
    ))
  #theme(strip.text = element_blank()) +

ggsave('Figures/highres_CPIvPCE.tiff', p, device = "tiff", height = 5, width = 10, units = "in")

