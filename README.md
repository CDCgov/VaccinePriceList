# Vaccine Prices

This repository contains the code and data associated with Kaul, R., Leidner, A. J., & Chesson, H. W. (2025). Trends in costs of routinely recommended vaccines in the United States, 2001-2023. Vaccine, 47, 126667. https://doi.org/10.1016/j.vaccine.2024.126667 

## Project Description and Objectives
The purpose of this project is to describe the extent to which vaccine cost assumptions are stable, or indicative of the real world costs. Prices were collected from recent models presented to ACIP meetings, and the private sector cost per dose values found on the [CDC Vaccine Price List Archives](https://www.cdc.gov/vaccines/programs/vfc/awardees/vaccine-management/price-list/archive.html). 

The results are presented in a table comparing first reported cost per dose to cost per dose 5 years after introduction, and the yearly average cost per dose since 2006 or first introduction (which ever is later).  

**Main findings** were that cost per dose assumptions presented in economic models were very similar to the initial cost per dose found on the Price List. For many vaccines, cost per dose increased during the observation period, and increased at a rate that was faster than medical care inflation.

### Methods Used
* Web Scraping
* Data Wrangling
* Data Visualization

### Technologies
* R
* Powerpoint 

## File Description
1. Scripts
  - 01_price_scrape.R pulls html pages from archived CDC website and creates 'data/peds_raw.csv', 'data/adult_raw.csv', 'data/vacc_brandnames.csv'
  - 02_cleaning.R fixes spelling, duplicates, data types, etc. to create                
      - 'data_out/all_prices.csv' which lists all the prices listed in the archive.
      - 'data_out/vaccine_details.xlsx' which has a few sheets with vaccine details
      - 'data_out/ave_annual_prices.csv' annual price weighted by days at price
      - 'data_out/adjusted_annual_prices.csv' annual prices weighted by days at price and adjusted for inflation using the PCE and CPI index.
  - 03_analysis.R used 'data_out/adjusted_annual_prices.csv' to create inflation index specific summary statistics
      - 'data_out/PCE_Tbl1_LastIntro2021' summary statistics for PCE adjusted data and vaccines introduced no later than Dec 31, 2021. 
      - 'data_out/CPI_Tbl1_LastIntro2021' summary statistics for CPI adjusted data and vaccines introduced no later than Dec 31, 2021. 
  - 04_plots.R creates plots using in manuscripts. Saves output to Figures/
  - 05_supplement.R uses the PCE and CPI Table 1 files output by 03_analysis.R to compare how the results differ between the two inflation adjustment methods. 
2. Data
  - Raw data includes the html pages converted to  a csv
  - Data manually collected (ACIP_model_inputs, vacc_labels, etc.)
3. Data_out
  - Intermediate and final data analysis products. 
4. Documents 
  - NA
5. Figures
  - Figures used in manuscript. 


## Future needs of this project

- This prject can be revisited periodically to update or build on the price time series data. 

## Contributing Econ Team Members

|Name     |  Email   | 
|---------|-----------------|
|[Rajreni Kaul](https://gitlab.com/omc7)| omc7@cdc.goc    |
|[Andrew Leidner](https://gitlab.com/wqm6)| wqm6@cdc.gov    |
