library(tidyverse)
library(readxl)
library(dplyr)
library(forecast)
library(AER)
library(tsbox)
library(gridExtra)
library(tseries)
library(zoo)
library(vars)
library(lubridate)
library(mFilter)

# Load the specific sheet and skip header rows
GDP_pounds <- 
  read_excel("data/firstquarterlyestimatedatatables_UK_GDP_qtrly.xlsx", sheet = "A2 AGGREGATES", skip = 8)

GDP_index <- 
  read_excel("data/firstquarterlyestimatedatatables_UK_GDP_qtrly.xlsx", sheet = "A1 AGGREGATES", skip = 7)

view(GDP_pounds)
view(GDP_index)

# Process the Pounds data (ABMI)
gdp_pounds <- GDP_pounds  |> 
  dplyr::select(Date = 1, Real_GDP_Pounds = ABMI)  |>   # Select 1st col as Date and ABMI
  filter(grepl("Q", Date))  |>                   # Keep only rows with "Q" (Quarterly)
  mutate(Real_GDP_Pounds = as.numeric(Real_GDP_Pounds))
gdp_pounds <- gdp_pounds[-1, ]
view(gdp_pounds)

# Process the Index data (YBEZ)
gdp_index <- GDP_index  |> 
  dplyr::select(Date = 1, Real_GDP_Index = YBEZ) %>%   # Select 1st col as Date and YBEZ
  filter(grepl("Q", Date)) |>                   # Keep only rows with "Q"
  mutate(Real_GDP_Index = as.numeric(Real_GDP_Index))
gdp_index <- gdp_index[-1, ]




# Combine into one tibble
gdp_combined <- inner_join(gdp_pounds, gdp_index, by = "Date")

# Clean Pounds (ABMI)
pounds_final <- GDP_pounds %>%
  dplyr::select(Date = 1, Real_GDP_Pounds = ABMI) %>%
  filter(grepl("^[0-9]{4} Q[1-4]$", Date)) %>% # Only matches "YYYY QX"
  distinct(Date, .keep_all = TRUE)           # Removes duplicates

# Clean Index (YBEZ)
index_final <- GDP_index %>%
  dplyr::select(Date = 1, Real_GDP_Index = YBEZ) %>%
  filter(grepl("^[0-9]{4} Q[1-4]$", Date)) %>% # Only matches "YYYY QX"
  distinct(Date, .keep_all = TRUE)            # Removes duplicates

# Join - This should now have NO warning
gdp_combined <- inner_join(pounds_final, index_final, by = "Date") %>%
  mutate(across(starts_with("Real"), as.numeric))

# This should show the most recent quarter (e.g., 2023 Q4 or 2024 Q1)
tail(gdp_combined) 

# This should be around 270-280 rows for data starting in 1955
nrow(gdp_combined) 



UK_OBR_inflation <- read_excel("data/UK_OBR_inflation.xlsx", sheet = "Sheet2", skip = 3)
View(UK_OBR_inflation)
str(UK_OBR_inflation)

# Process the inflation data (CPI)
cpi_index <- UK_OBR_inflation  |> 
  dplyr::select(Date = 1, CPI_index = "CPI outturn index")  |>     
  mutate(Date = format(as.yearqtr(Date, format = "%YQ%q"), "%Y Q%q"))

view(cpi_index)
str(cpi_index)

# Merging CPI data with GDP data - separate dataframes
Y_P <- gdp_combined |> 
  left_join(cpi_index, by = "Date")

view(Y_P)

# To confine your table to the range 1996 Q1 to 2024 Q4, you should convert the Date column back 
#   into a yearqtr object within a filter() function. While the strings "1996 Q1" and "2024 Q4" look
#   like dates, R cannot naturally "rank" them as strings (e.g., "2000 Q1" would come after "1996 Q4" 
#   alphabetically, but this logic can fail with different naming conventions). By using the zoo 
#   package's as.yearqtr class, you can treat them as numeric values where 1996.0 is earlier than 2024.75. 

Y_P_modern <- Y_P |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1996 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

view(Y_P_modern)
str(Y_P_modern)


### BANK RATE


# 1. Process the Bank Rate (assuming column 1 is Date, column 2 is Rate)
library(tidyverse)
library(lubridate)
library(zoo)

# 1. Clean the Bank Rate data
BOE_rate <- BoE_bank_rate |>
  # Select the first two columns regardless of their names
  dplyr::select(RawDate = 1, BOE_rate = 2) |>
  mutate(
    # Convert "31 Mar 24" to a Date object
    temp_date = dmy(RawDate),
    # Convert to the ONS string format "2024 Q1"
    Date = format(as.yearqtr(temp_date), "%Y Q%q")
  ) |>
  # Keep only the cleaned Date and the Rate for the join
  dplyr::select(Date, BOE_rate)


# 2. Join with your existing Master Dataframe

Y_P_r <- Y_P_modern |>
  inner_join(BOE_rate, by = "Date")
view(Y_P_r)
str(BOE_rate)





### STERLING EFFECTIVE EXCHANGE RATE

str(series_090326)
GBP_EER <- series_090326 %>%
  dplyr::select(Date = 1, GBP_EER = "Sterling effective exchange rate index: Monthly average (Jan 2005=100)") %>%
  filter(grepl("^[0-9]{4} Q[1-4]$", Date)) %>% # Only matches "YYYY QX"
  distinct(Date, .keep_all = TRUE)            # Removes duplicates
GBP_EER_dates <- GBP_EER |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1996 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )
view(GBP_EER_dates)

Y_P_r_ex <- Y_P_r |>
  inner_join(GBP_EER_dates, by = "Date")
Y_P_r_stex <- Y_P_r_ex |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )
view(Y_P_r_stex)
str(Y_P_r_stex)



# 1. Clean the Exchange Rate and Create Structural Variables
model_ready_df <- Y_P_r_stex |>
  mutate(
    # Convert character to numeric
    GBP_EER = as.numeric(GBP_EER),
    
    # log-linearise for elasticity estimation
    log_y = log(Real_GDP_Pounds),
    log_er = log(GBP_EER),
    
    # Calculate the Output Gap (y_gap) using HP Filter (Quarterly lambda = 1600)
    # This separates the 'Cycle' from the 'Trend'
    y_gap = hpfilter(log_y, freq = 1600)$cycle,
    
    # Calculate Annualised Quarterly Inflation (pi)
    # Using 4 * log difference to get the yearly rate
    pi = 4 * (log(CPI_index) - lag(log(CPI_index), 1)),
    
    # Calculate the Real Interest Rate (r) using the Fisher link
    r_rate = BOE_rate - pi
  ) |>
  # Create a COVID Dummy to protect your structural coefficients
  mutate(covid_dummy = ifelse(Date %in% c("2020 Q1", "2020 Q2", "2020 Q3"), 1, 0)) |>
  drop_na() # Remove the first row where lag(inflation) is NA

view(model_ready_df)




















###### NOT SURE IF WE NEED UNEMPL DATA YET, BUT THIS IS THE CODE FOR INCLUDING MORE DATA IN ANY CASE.

UK_unempl <- read_excel("data/UK_unempl.xls")
str(UK_unempl)




# Process the unemployment data
unemp <- UK_unempl  |> 
  select(Date = 1, Unemp_rate = "Unemployment rate (aged 16 and over, seasonally adjusted): %") |> 
  filter(grepl("Q", Date))

unemp_modern <- unemp |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1996 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

# Merge unemployment data with GDP and CPI data
Y_P_U <- Y_P_modern |> 
  left_join(unemp_modern, by = "Date")

view(Y_P_U)
str(Y_P_U)

# WE NOW HAVE A DATASET CONTAINING GDP, CPI and UNEMPL OVER THE SAME PERIOD, BY QUARTER, FOR TIME SERIES ANALYSIS



