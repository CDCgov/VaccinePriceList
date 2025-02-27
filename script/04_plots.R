#load packages and other functions, set gloabl objects
pacman::p_load(tidyr, tidyverse, readxl, ggplot2,patchwork, xlsx, Polychrome, ggtext)
#Polychrome is used for the color generator

source("script/00_helpful_fncs.R") #ggplot theme 

#Default y and x axis labels
ytitle <- "Private Sector Cost per Dose (US$2023)"
xtitle <- "Year"

#define index to use for analysis: PCE or CPI. Run with both indices for 05_supplement.R. File names change with index_use. 
index_use <- "CPI"
vacc_price <-  read.csv("data_out/adjusted_annual_prices.csv") %>%
  select(-X) %>%
  filter(index == index_use)

# Data ----
#PCE adjusted data
#time series data
  vacc_price <-  
  read.csv("data_out/adjusted_annual_prices.csv") %>%
  select(-X) %>%
  filter(index == index_use) %>%
    separate_wider_delim(name.age, names=c("name","age"), delim=".", too_few = "align_start", cols_remove = FALSE) %>%
    #change cost category capitalization
    mutate(cost_type = ifelse(cost_type == "private", "Private", "CDC"),
           age = ifelse(age=="ped","Pediatric", "Adult")) %>%
    mutate(cost_type = factor(cost_type, levels= c("Private", "CDC")),
           age = fct_relevel(age, c("Pediatric", "Adult"))) 

#summary data
Tbl1 <-   
  read.csv(paste0("data_out/",index_use,"_Tbl1_LastIntro2021.csv")) %>%
  arrange(annual_pct_change) %>%
  filter(year_intro < 2021) %>%
  #change cost category capitalization
  mutate(cost_type = ifelse(cost_type == "private", "Private", "CDC"),
         age = ifelse(age=="ped","Pediatric", "Adult")) %>%
  mutate(cost_type = factor(cost_type, levels= c("Private", "CDC")),
         age = fct_relevel(age, c("Pediatric", "Adult")))

vKeep <-  Tbl1 %>%
  select(name.age, cost_type) %>%
  distinct()
  


# Figure 1. Model range and market price for RZV adults and PCV13 children ----
# read in data

#data used in model
model <- 
  read_excel('data/ACIP_model_inputs.xlsx', sheet = "model.input" ) %>%
  #pull out vacc of interest
  filter(short.name %in% c("PCV13.child", "RZV.adult")) %>%
  filter(input.base.2023 >155) %>%
  #manually add in year of ACIP vote
  mutate(year = ifelse(short.name == "RZV.adult", 2017.5, 2009.5)) %>%
  #just keep the good stuff
  select(c(short.name,year, public.private.price, model.team, input.base.2023, input.low.2023, input.high.2023)) %>%
  #harmonize names
  rename(value = input.base.2023,
         name.age = short.name) %>%
  mutate(name.age = ifelse(grepl("child", name.age, ignore.case = TRUE), "prevnar13.ped", "shingrix.adult")) %>%
  as_tibble()


#market price data
price <- 
  vacc_price %>%
  filter(cost_type == "Private") %>%
  #pull out vacc of interest
  filter(name.age %in% c("prevnar13.ped", "shingrix.adult")) %>%
  #just keep the good stuff
  select(year, name.age, price.2023 ) %>%
  mutate(year = as.numeric(year)) %>%
  rename(value = price.2023) %>%
  as_tibble()

#check col names match
# str(model)
# str(price)  

#Two panel plot

#build a df to pipe into ggplot with facet wrap
p <- 
  price %>%
  mutate(input.low.2023 = NA,
         input.high.2023 = NA,
         source = "Price List: Private Sector") %>%
  select(name.age, year, value, input.low.2023, input.high.2023, source) %>%
  rbind(model %>%
          select(name.age, year, value, input.low.2023, input.high.2023) %>%
          mutate(source = "Model Input")) %>% 
  #filter(short.name == "PCV13.child") %>%
  mutate(name.age = case_when(
    name.age == "prevnar13.ped" ~ "Pediatric PCV13&trade;", #add TM 
    name.age =="shingrix.adult" ~ "Adult RZV<sup>&reg;</sup>")) %>% #add (R)
  ggplot( aes(x= year, y = value, ymin=input.low.2023, ymax=input.high.2023, group = source)) +
  geom_point(aes(pch = source), size = 4, position = position_dodge2(0.25)) +
  geom_line() +
  geom_errorbar(width = 0.5, position = position_dodge2(0.25)) +
  labs(  y = ytitle,
         x = xtitle,
         pch = "Data Source") +
  get_theme(txt = 16) +
  theme(strip.text = element_markdown()) +
  theme(legend.position = c(0.82,0.2),
         legend.background = element_rect(color="black",size = 0.1))+
  scale_y_continuous(limits = c(125,225)) +
  facet_wrap(~name.age, scales = "free_x" )

ggsave(paste0('Figures/',index_use,'_highres_Fig1.tiff'), p, device = "tiff", height = 5, width = 10, units = "in")


# Figure 2. The 5 vaccines with the fastest/slowest change in price ----

#find the 5 fast and slowest changing price by age and cost type
vax.name <- 
  bind_rows(
    Tbl1 %>%
      arrange(annual_pct_change) %>%
      group_by(age, cost_type) %>%
      slice_head(n=5) %>%
      mutate(price_trend = "down"),
    Tbl1 %>%
      arrange(annual_pct_change) %>%
      group_by(age, cost_type) %>%
      slice_tail(n=5)%>%
      mutate(price_trend = "up")) %>%
  select(name.age, brandname_tradename, age, annual_pct_change, price_trend) %>%
  mutate(series = c(1:10))

#pull out time series of the 5 fast and slowest changing price
d <- 
  vacc_price %>%
  inner_join(vax.name) 

#pretty vaccine name

pNames <- read.csv("data/vacc_labels.csv") %>%
  #drop NA
  filter(!is.na(legend.label)) %>%
  #make age col for easy filtering
  separate_wider_delim(name.age, names=c("name","age"), delim=".", too_few = "align_start", cols_remove = FALSE) %>%
  select(!c(name, vaccine_group))

#Create aes map for plot 
ped.map <- 
  vax.name %>%
  # pull out ped unique names
  filter(age == "Pediatric") %>% 
  ungroup() %>%
  group_by(name.age) %>%
  summarise() %>%
  #add pretty names
  left_join(pNames %>% filter(age == "ped") %>% select(!age)) %>%
  #add html colors; target accounts for color blindness
  mutate(color = unname(createPalette(15,  c("#ff0000", "#00ff00", "#0000ff"))),
         colshape = c(0:14),
         bwshape = c(65:79)) %>%#lower case: c(97:111)) #
 select(name.age, legend.label, color, colshape, bwshape)

adult.map <- 
  vax.name %>%
  # pull out ped unique names
  filter(age == "Adult") %>% 
  ungroup() %>%
  group_by(name.age) %>%
  summarise() %>%
  #add pretty names
  left_join(pNames %>% filter(age == "adult") %>% select(!age)) %>%
  #add html colors; target accounts for color blindness
  mutate(color = unname(createPalette(14,  c("#ff0000", "#00ff00", "#0000ff"))), 
         colshape = c(0:13),
         bwshape = c(65:78)) %>%#lower case: c(97:111)) #
  select(name.age, legend.label, color, colshape, bwshape)

#plot axis title
ytitle <- "Cost per dose (US$2023)"

#black and White Version 
p3 <- 
  d %>%
  filter(age == "Pediatric" ) %>%
  ggplot( aes(x= year, y = price.2023,  group =name.age)) +
  geom_line(linetype ="dotted", alpha = 0.25) +
  #this size seems to find the balance between overlap and readability
  geom_point( aes(shape = name.age),size = 2.75) +
  labs(  y = ytitle,
         x = "Year") +
  scale_shape_manual(name ="Vaccine", values = ped.map$bwshape, labels = ped.map$legend.label) +
  get_theme() +
  theme(legend.text = element_markdown()) +
  #theme(strip.text = element_blank()) +
  facet_grid(~cost_type, scales = "free_y") 

ggsave(paste0('Figures/',index_use,'_highres_Fig2BW_Peds.tiff'), p3, device = "tiff", height = 5, width = 10, units = "in")

p4 <- 
  d %>%
  filter(age == "Adult" ) %>%
  ggplot( aes(x= year, y = price.2023,  group =name.age)) +
  geom_line(linetype ="dotted", alpha = 0.25) +
  #this size seems to find the balance between overlap and readability
  geom_point( aes(shape = name.age),size = 2.75) +
  labs(  y = ytitle,
         x = "Year") +
  scale_shape_manual(name ="Vaccine", values = adult.map$bwshape, labels = adult.map$legend.label) +
  get_theme() +
  theme(legend.text = element_markdown()) +
  #theme(strip.text = element_blank()) +
  facet_grid(~cost_type, scales = "free_y") 

ggsave(paste0('Figures/',index_use,'_highres_Fig2BW_Adults.tiff'), p4, device = "tiff", height = 5, width = 10, units = "in")

  
# Figure 2_All: Four panel with all vaccines. ----
  
  tb <- 
    bind_rows(
    Tbl1 %>%
      group_by(age, cost_type) %>%
      arrange(annual_pct_change) %>%
      slice_head(n=1) %>%
      mutate(legend = "increasing"),
    Tbl1 %>%
      group_by(age, cost_type) %>%
      arrange(annual_pct_change) %>%
      slice_tail(n=1) %>%
    mutate(legend = "decreasing")) %>%
    ungroup() %>%
    select(name.age, age,cost_type, legend) #%>%


  #timeseries with legend col for defining line type as needed
  d2 <- 
  vacc_price %>%
    left_join(tb) %>%
    mutate(legend = ifelse(is.na(legend), "std", legend)) %>%
    inner_join(vKeep) %>%
  #add html colors; target accounts for color blindness
    mutate(bwshape = case_when(legend == "std" ~21 ,
                               legend == "increasing" ~ 19,
                               legend == "decreasing" ~ 15),
         name.age.type = str_c(name.age, cost_type, sep ="."))

#create aes mapping   
  d2.map <- 
    d2 %>%
     select(name, age, cost_type, name.age.type, bwshape) %>%
     distinct() 

  
txt <- 12

#determine limits
d2 %>%
  group_by(cost_type, age) %>%
  filter(price.2023 == max(price.2023)) %>%
  select(name.age.type,year, price.2023) 

xmin <- 2000
xmax <- 2023
ymax <- 275
ylab1 <- "Private Sector cost \nper dose (US$2023)"
ylab2 <- "CDC cost \nper dose (US$2023)"
ylab1 <- ylab2 <- "Cost per dose (US$2023)"
xlab <- "Year"

#facet_grid ----
txt <- 16
pd <- position_dodge(0.5)

p2 <- ggplot(d2, aes(x = year, y = price.2023, group = name.age.type)) +
  geom_line(linewidth = .5,
            color = "grey85",
            position = pd) + #linetype ="dotted", alpha = 0.25) +
  geom_point(aes(shape = name.age.type),size=1.5, position = pd) +
  scale_shape_manual(
    values = d2.map$bwshape,
    labels = d2.map$name.age.type,
    guide = "none"
  ) +
  scale_y_continuous(breaks = seq(0, 250,50)) +
  scale_x_continuous(limits = c(2001, 2023)) +
  #xlim(xmin, xmax) +
  labs(y = ylab2,
       x = xlab) +
  get_theme() +
#  theme(strip.background = element_blank(), 
#        strip.placement = "outside")+
  facet_grid(cost_type~age, scales = "free_y") +
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
    axis.text = element_text(colour = "black",size=txt-1 ),
    axis.title = element_text(size =txt, face="bold"),
    text = element_text(face="bold",size=txt),
    # panel.border = element_rect(colour = "black", fill=NA),
    #panel.background = element_rect(colour = "black", size=1),
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(
      fill = "grey80", 
      color = "black", 
      linewidth = 1
    ))

ggsave(paste0('Figures/',index_use,'_highres_Fig2.tiff'), p2, device = "tiff", height = 6, width = 10, units = "in")

# elongated facet_grid ----



#Create aes map for plot 
whole.map <- 
  d %>% 
  select(brandname_tradename) %>%
  distinct() %>%
  #add pretty names
  left_join(pNames %>% 
              ungroup() %>%
              mutate(brandname_tradename = str_extract(name.age, "^[:alnum:]+")) %>%
              select(brandname_tradename, legend.label) %>%
              distinct()) %>%
  #add html colors; target accounts for color blindness
  mutate(color = unname(createPalette(19,  c("#ff0000", "#00ff00", "#0000ff"))),
         colshape = c(0:18),
         bwshape = c("A","B","C","D","E",
                     "F","G","H","I","J",
                     "K","L","M","N","O",
                     "P","R","S","T")) #c(65:84)) %>%#lower case: c(97:111)) #

ped.map <- 
  whole.map %>%
  filter(age == "Pediatric") %>%
  distinct()


adult.map <- 
  whole.map %>%
  filter(age == "Adult")

e1 <- 
d %>%
  select(age, cost_type, brandname_tradename, price.2023, year) %>%
  left_join(whole.map %>%
              select(brandname_tradename, legend.label, bwshape)) %>%
#  filter(age == "Pediatric" ) %>%
  ggplot( aes(x= year, y = price.2023,  group = brandname_tradename)) +
  geom_line(linetype ="dotted", alpha = 0.25) +
  #this size seems to find the balance between overlap and readability
  geom_point( aes(shape = bwshape),size = 3) +
  labs(  y = ytitle,
         x = "Year") +
 # ylim(c(0,100)) +
  scale_shape_manual(name ="Vaccine", values = whole.map$bwshape, labels = whole.map$legend.label) +
  get_theme() +
  theme(legend.text = element_markdown()) +
  #theme(strip.text = element_blank()) +
  facet_grid(cost_type~age, scales = "free_y") +
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
    axis.text = element_text(colour = "black",size=txt-1 ),
    axis.title = element_text(size =txt, face="bold"),
    text = element_text(face="bold",size=txt),
    # panel.border = element_rect(colour = "black", fill=NA),
    #panel.background = element_rect(colour = "black", size=1),
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(
      fill = "grey80", 
      color = "black", 
      linewidth = 1
    ))

ggsave(paste0('Figures/',index_use,'_highres_elongated_Fig2.tiff'), e1, device = "tiff", height = 15, width = 10, units = "in")

# NCIRD Seminar Figures ----
seminar <- 
price %>%
  mutate(input.low.2023 = NA,
         input.high.2023 = NA,
         source = "PCV13 Private Sector List Price") %>%
  select(name.age, year, value, input.low.2023, input.high.2023, source) %>%
  rbind(model %>%
          select(name.age, year, value, input.low.2023, input.high.2023) %>%
          mutate(source = "PCV13 Model Input")) %>% 
  filter(name.age == "prevnar13.ped") %>%
  mutate(name.age = case_when(
    name.age == "prevnar13.ped" ~ "Pediatric PCV13", #add TM 
    name.age =="shingrix.adult" ~ "Adult RZV<sup>&reg;</sup>")) %>% #add (R)
  #Add PCV20
  bind_rows(
  bind_cols(name.age = c("Pediatric PCV20&trade;"), year = c(2023), value= c(253.21),  source = "PCV20 Private Sector List Price")) %>%
  ggplot( aes(x= year, y = value, ymin=input.low.2023, ymax=input.high.2023, color = source)) +
  geom_point(aes(pch = source), size = 4, position = position_dodge2(0.25)) +
  geom_line() +
  geom_errorbar(width = 0.5, position = position_dodge2(0.25)) +
  scale_color_manual(values = c("black", "black", "blue")) +
  labs(  y = ytitle,
         x = xtitle) +
  get_theme(txt = 16) +
  theme(strip.text = element_markdown()) +
  theme(legend.position = c(0.72,0.25),
        legend.background = element_rect(color="black",size = 0.1),
        legend.title = element_blank())+
  scale_y_continuous(limits = c(125,270)) +
  scale_x_continuous(limits = c(2009,2025)) 

ggsave(paste0('Figures/',index_use,'_highres_NCIRD_Seminar_B.tiff'), seminar, device = "tiff", height = 10, width = 10, units = "in")

