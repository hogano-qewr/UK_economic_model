library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(forecast)
library(AER)
library(tsbox)
library(gridExtra)
library(tseries)
library(zoo)
library(vars)
library(lubridate)
library(mFilter)
library(stringr)
library(systemfit)
library(tsibble)
select <- dplyr::select


UK_AGGS <- read_excel("data/firstquarterlyestimatedatatables_UK_GDP_qtrly.xlsx", 
                                                            sheet = "A2 AGGREGATES", skip = 8)
GDP <- UK_AGGS  |> 
  dplyr::select(Date = 1, GDP_mp = ABMI, GVA_bp = ABMM, indir_net_tax = NTAO)  |>   # Select 1st col as Date and ABMI
  filter(grepl("Q", Date))  |>                   # Keep only rows with "Q" (Quarterly)
  mutate(GDP_mp = as.numeric(GDP_mp), GVA_bp = as.numeric(GVA_bp), indir_net_tax = as.numeric(indir_net_tax))

### ADD CONSUMPTION, INVESTMENT, GOVERNMENT SPENDING, EXPORTS AND IMPORT

EXPEN <- read_excel("data/firstquarterlyestimatedatatables_UK_GDP_qtrly.xlsx", 
                    sheet = "C2 EXPENDITURE", skip = 8)

AGG_DEM <- EXPEN  |> 
  dplyr::select(Date = 1, 
                C = ABJR, 
                C_np = HAYO, 
                G = NMRY, 
                I_gcf = NPQT,
                I_inven = CAFU,
                I_acq = NPJR,
                dom_DEM = YBIM,
                EX = IKBK,
                IM = IKBL)  |>   # Select 1st col as Date and ABMI
  filter(grepl("Q", Date))  |>                   # Keep only rows with "Q" (Quarterly)
  mutate(C = as.numeric(C),
         C_np = as.numeric(C_np),
         G = as.numeric(G),
         I_gcf = as.numeric(I_gcf),
         I_inven = as.numeric(I_inven),
         I_acq = as.numeric(I_acq),
         dom_DEM = as.numeric(dom_DEM),
         EX = as.numeric(EX),
         IM = as.numeric(IM)
         )


# Clean GDP: remove ONS header and NA values
GDP <- GDP %>% filter(Date != "Table 2: Quarterly", !is.na(GDP_mp))
AGG_DEM <- AGG_DEM %>% filter(Date != "Table 2: Quarterly", !is.na(C))

# 1. Clean each dataframe to keep only the first record per Date
GDP_clean     <- GDP %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()
AGG_DEM_clean <- AGG_DEM %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()

# 2. Join them all together seamlessly
MODEL_DF <- list(GDP_clean, AGG_DEM_clean) %>% 
  reduce(left_join, by = "Date")

# To confine your table to the range 1997 Q1 to 2024 Q4, you should convert the Date column back 
#   into a yearqtr object within a filter() function. While the strings "1997 Q1" and "2024 Q4" look
#   like dates, R cannot naturally "rank" them as strings (e.g., "2000 Q1" would come after "1997 Q4" 
#   alphabetically, but this logic can fail with different naming conventions). By using the zoo 
#   package's as.yearqtr class, you can treat them as numeric values where 1996.0 is earlier than 2024.75.
#  filter down to our period

MODEL_DF <- MODEL_DF |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

#  CREATING A SINGLE INVESTMENT TOTAL VARIABLE:
MODEL_DF <- MODEL_DF |> 
  mutate(I_tot = (I_gcf + I_inven + I_acq),
         I_tot = as.numeric(I_tot),
  )
# Handle Inventory separately 
# (I_inven has negative numbers, so you CANNOT log it. Use it as a % of GDP instead)
MODEL_DF <- MODEL_DF %>%
  mutate(invtry_RAT = I_inven / GDP_mp) 



## ADD UNEMPLOYMENT DATA

UK_unempl <- read_excel("data/UK_unempl.xls")

# Process the unemployment data
unemp <- UK_unempl  |> 
  dplyr::select(Date = 1, u_RATE = "Unemployment rate (aged 16 and over, seasonally adjusted): %") |> 
  filter(grepl("Q", Date)) |> 
  group_by(Date)  |>  slice_head(n = 1)  |>  ungroup() |> 
  mutate(u_RATE = as.numeric(u_RATE)) |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, unemp, by = "Date")

view(MODEL_DF)

#### PRODUCTIVITY: OUTPUT PER HOUR WORKED  #####################################

library(readxl)
Productivity <- read_excel("data/prodbydivoph.xlsx", 
                           sheet = "Table_19",
                           skip = 3)
Productivity_Clean <- Productivity %>%
  # 1. Drop the first 3 rows of metadata
  slice(-c(1:3)) %>%
  # 2. Rename columns for easier use
  rename(date_raw = 1, prod_IDX = 2) %>%
  # 3. Convert the character index to a numeric number
  mutate(prod_IDX = as.numeric(prod_IDX)) %>%
  # 4. Convert "1997 Q1" into a proper R date
  mutate(date = as.Date(as.yearqtr(date_raw))) 


MODEL_DF <- MODEL_DF %>%
  mutate(date = as.Date(as.yearqtr(Date))) %>% 
  left_join(Productivity_Clean %>% select(date, prod_IDX), by = "date")

MODEL_DF <- MODEL_DF %>% 
  relocate(date)


############ WAGES: AVERAGE WEEKLY EARNINGS####################################

library(readxl)
average_weekly_earnings <- read_excel("data/average_weekly_earnings.xlsx")

Earnings_Qtr_Clean <- average_weekly_earnings %>%
  # 1. Rename columns (using A2F8: Real Terms Index)
  rename(date_raw = 1, wage_IDX = 2) %>%
  # 2. Keep ONLY the rows that look like "YYYY Q1" 
  # This automatically removes the monthly "2020 JAN" and metadata rows
  filter(grepl("Q[1-4]", date_raw)) %>%
  # 3. Convert to numeric and proper Date
  mutate(
    wage_IDX = as.numeric(wage_IDX),
    date = as.Date(as.yearqtr(date_raw))
  ) 

# 5. Join to your main dataframe
MODEL_DF <- MODEL_DF %>%
  left_join(Earnings_Qtr_Clean %>% select(date, wage_IDX), by = "date")

MODEL_DF$wage_IDX <- na.approx(MODEL_DF$wage_IDX, rule = 2)




##################################################################################################








## ADD INFLATION DATA

UK_OBR_inflation <- read_excel("data/UK_OBR_inflation.xlsx", sheet = "Sheet2", skip = 3)

# Process the inflation data (CPI)
cpi <- UK_OBR_inflation  |> 
  dplyr::select(
    Date = 1, 
    cpi_IDX = "CPI outturn index",
    dcpi_IDX = "GDP deflator outturn index"
  ) |>  # Close select() here
  mutate(
    Date = format(as.yearqtr(Date, format = "%YQ%q"), "%Y Q%q"),
    cpi_IDX = as.numeric(cpi_IDX),
    dcpi_IDX = as.numeric(dcpi_IDX)
  )

#  Clean the cpi df to keep only the first record per Date
cpi_clean <- cpi %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()

#  filter down to matching period
cpi_clean |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

# Merging CPI data with GDP and DEM data - separate dataframes
MODEL_DF <- left_join(MODEL_DF, cpi_clean, by = "Date")


#############    CPIs FOR REAL EXCHANGE RATE .... IMF...COMMON BASIS FOR UK AND TOP 5 TRADING PARTNERS ######

# 1. Get file paths
file_paths <- list.files(path = "data", pattern = "IMF.*\\.csv", full.names = TRUE)

# 2. Process with robust date handling
cpi_wide <- file_paths %>%
  map_df(~ read_csv(.x, show_col_types = FALSE)) %>%
  # Keep only Quarterly Index values (Standard reference period)
  # This prevents the "list-cols" by removing growth rate rows
  filter(str_detect(TIME_PERIOD, "Q"),
         str_detect(TYPE_OF_TRANSFORMATION, "Index")) %>%
  # Explicitly convert "1997-Q1" to yearqtr
  # If the format is "1997-Q1", format = "%Y-Q%q" helps
  mutate(TIME_PERIOD = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")) %>%
  # Drop any failed date conversions
  filter(!is.na(TIME_PERIOD)) %>%
  # Select only what we need
  dplyr::select(TIME_PERIOD, COUNTRY, OBS_VALUE) %>%
  # Force uniqueness just in case
  distinct(TIME_PERIOD, COUNTRY, .keep_all = TRUE) %>%
  # Pivot
  pivot_wider(names_from = COUNTRY, values_from = OBS_VALUE) %>%
  arrange(TIME_PERIOD)


# 1. Prepare cpi_wide: rename, format date, and add the suffix
cpi_prepared <- cpi_wide %>%
  rename(Date = TIME_PERIOD) %>%
  mutate(Date = format(Date, format = "%Y Q%q")) %>%
  # Add _CPI_imf to every column except "Date"
  rename_with(~ paste0(., "_CPI_imf"), -Date)

cpi_prepared <- cpi_prepared |> 
  rename(cpi_IDX_imf = `United Kingdom_CPI_imf`,
         USA_CPI_imf = `United States_CPI_imf`,
         China_CPI_imf = `China, People's Republic of_CPI_imf`,
         EURO_CPI_imf = `Euro Area (EA)_CPI_imf`,
         SWISS_CPI_imf = `Switzerland_CPI_imf`,
         NORW_CPI_imf = `Norway_CPI_imf`)

# 1. Define the weights as a named vector for clarity
# These match the 5 partners and sum to 1.0
weights <- c(
  usa   = 0.38,
  euro  = 0.42,
  china = 0.14,
  swiss = 0.04,
  nor   = 0.02
)

# 2. Calculate the Foreign Price Index (Geometric Mean)
cpi_prepared <- cpi_prepared %>%
  mutate(
    fcpi_WIDX = 
      (USA_CPI_imf ^ weights["usa"]) *
      (EURO_CPI_imf ^ weights["euro"]) *
      (China_CPI_imf ^ weights["china"]) *
      (SWISS_CPI_imf ^ weights["swiss"]) *
      (NORW_CPI_imf ^ weights["nor"])
  )


# 1. Select only Date and the final weighted rate
fcpi_WIDX_only <- cpi_prepared %>%
  dplyr::select(Date, cpi_IDX_imf, fcpi_WIDX)

# 2. Join it into your master dataframe
MODEL_DF <- MODEL_DF %>%
  left_join(fcpi_WIDX_only, by = "Date")






###################### END  #########################################################

### adding Imports of goods and services deflator:SA

import_price_deflator <- read_excel("data/import_price_deflator.xls", skip = 8)

# Process the Imports deflator (ONS series YBFZ)
IM_defl <- import_price_deflator  |> 
  dplyr::select(Date = 1, IMcpi_IDX = `Imports of goods and services deflator:SA`) |> 
  filter(grepl("Q", Date))  |>                  # Keep only rows with "Q" (Quarterly) 
  group_by(Date)  |>  slice_head(n = 1)  |>  ungroup() |> 
  mutate(IMcpi_IDX = as.numeric(IMcpi_IDX)) |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, IM_defl, by = "Date")


view(MODEL_DF)


##################################################################################################

## Adding CPI fuels component, 
#     (due to failure of Phillips Curve on 1st-pass 3SLS)
model_inputs_energy_prices <- read_excel("data/model_inputs_energy_prices.xlsx")

# 1. Clean the 'Date' strings in both dataframes (remove extra spaces)
MODEL_DF <- MODEL_DF %>%
  mutate(date = str_squish(as.character(Date)))

model_inputs_energy_prices <- model_inputs_energy_prices %>%
  mutate(date = str_squish(as.character(date)))

MODEL_DF <- MODEL_DF %>%
  # 1. Drop the superfluous lowercase 'date'
  select(-any_of("date")) %>% 
  # 2. Rename 'Date' to 'date' so it matches your other datasets
  rename(date = Date)

# 3. Now the join will be clean
MODEL_DF <- MODEL_DF %>%
  left_join(
    model_inputs_energy_prices %>% select(date, energy_cpi), 
    by = "date"
  )
MODEL_DF <- MODEL_DF |> 
  rename(ecpi_IDX = energy_cpi)






##### NEED A BROADER MEASURE OF FOREIGN INTEREST RATE AS ONLY ACCOUNTS FOR USA AT PRESENT #####


BIS_foreign_i <- read_excel("data/bis_dp_search_export_20260324-130420.xlsx", 
                                                   sheet = "timeseries observations", skip = 6)



# 1. Process the BIS monthly data into quarterly averages
BIS_quarterly <- BIS_foreign_i %>%
  mutate(
    # Convert string to date, then to "1997 Q1" character format
    Date = as.character(yearquarter(as.Date(Month)))
  ) %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  # NEW: Rename numeric columns to "CB_i_[Full Name]"
  rename_with(
    .fn = ~ paste0("CB_i_", sub(".*:", "", .x)), # Deletes everything before and including the colon
    .cols = where(is.numeric)
  )

# 1. Map your weights to the actual column names (adjusting for the new prefixes)
# Note: Ensure these names match your dataframe exactly (including spaces/case)
weights_map <- c(
  "CB_i_United States" = 0.38,
  "CB_i_Euro area"     = 0.42,
  "CB_i_China"         = 0.14,
  "CB_i_Switzerland"   = 0.04,
  "CB_i_Norway"        = 0.02
)

# 2. Calculate the weighted average
BIS_quarterly <- BIS_quarterly  |> 
  mutate(
    i_FOR = (
      (`CB_i_United States` * weights_map["CB_i_United States"]) +
        (`CB_i_Euro area`     * weights_map["CB_i_Euro area"]) +
        (`CB_i_China`         * weights_map["CB_i_China"]) +
        (`CB_i_Switzerland`   * weights_map["CB_i_Switzerland"]) +
        (`CB_i_Norway`        * weights_map["CB_i_Norway"])
    )
  )

# 1. Select only Date and the final weighted rate
Foreign_i_only <- BIS_quarterly %>%
  dplyr::select(Date, i_FOR)

# 2. Join it into your master dataframe
MODEL_DF <- MODEL_DF %>%
  left_join(Foreign_i_only, by = c("date" = "Date"))


###################################################################################################################


### BANK RATE

# 1. Process the Bank Rate (assuming column 1 is Date, column 2 is Rate)

BoE_bank_rate <- read_excel("data/BoE_bank_rate.xlsx", skip = 1)

# 1. Clean the Bank Rate data
BOE_rate <- BoE_bank_rate |>
  # Select the first two columns regardless of their names
  dplyr::select(RawDate = 1, i_UK = 2) |>
  mutate(
    # Convert "31 Mar 24" to a Date object
    temp_date = dmy(RawDate),
    # Convert to the ONS string format "2024 Q1"
    Date = format(as.yearqtr(temp_date), "%Y Q%q")
  ) |>
  # Keep only the cleaned Date and the Rate for the join
  dplyr::select(Date, i_UK)

#  Clean the cpi df to keep only the first record per Date
BOE_rate_clean <- BOE_rate %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()

#  filter down to matching period
BOE_rate_clean |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

# 2. Join with your existing Master Dataframe
MODEL_DF <- left_join(MODEL_DF, BOE_rate, by = c("date" = "Date"))



# MARKET INTEREST RATE: (for market premium over BOE risk-free rate) Pull 5-year Gilt Yield (Market-based interest rate)
gilt_5y <- read_excel("data/BoE_gilt_5y_nom_par_rate.xlsx", 
                                       skip = 1) |> 
  dplyr::select(RawDate = 1, i_g5y = 2) |>            # Select the first two columns regardless of their names
  mutate(
    # Convert "31 Mar 24" to a Date object
    temp_date = dmy(RawDate),
    # Convert to the ONS string format "2024 Q1"
    Date = format(as.yearqtr(temp_date), "%Y Q%q")
  ) |>
  # Keep only the cleaned Date and the Rate for the join
  dplyr::select(Date, i_g5y)

#  Clean the gilt yield df to keep only the first record per Date
gilt_5y <- gilt_5y  |>  
  group_by(Date)  |>  
  slice_head(n = 1)  |>  
  ungroup()

#  filter down to matching period
gilt_5y |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, gilt_5y, by = c("date" = "Date"))




### STERLING EFFECTIVE EXCHANGE RATE
sterling_eff_exch_rate <- read_excel("data/sterling_eff_exch_rate.xls", 
                                     skip = 8)

GBP_EER <- sterling_eff_exch_rate %>%
  dplyr::select(Date = 1, er_IDX = "Sterling effective exchange rate index: Monthly average (Jan 2005=100)") %>%
  filter(grepl("^[0-9]{4} Q[1-4]$", Date)) %>% # Only matches "YYYY QX"
  distinct(Date, .keep_all = TRUE)            # Removes duplicates

GBP_EER_final <- GBP_EER |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2025 Q4")
  ) |> 
  group_by(Date) |>  
  slice_head(n = 1)  |>  
  ungroup() |> 
  mutate(
    # Convert list to vector, then to numeric
  er_IDX = as.numeric(unlist(er_IDX))
  )


MODEL_DF <- left_join(MODEL_DF, GBP_EER_final, by = c("date" = "Date"))


MODEL_DF <- MODEL_DF %>%
  mutate(date = as.Date(as.yearqtr(date, format = "%Y Q%q")))




#### convert the data we need (logs) ###############################

# 1. Define your groups
vars_to_log <- c("GDP_mp", "GVA_bp", "indir_net_tax", "C", "C_np", "G", 
                 "I_gcf", "dom_DEM", "EX", "IM", "I_tot", "prod_IDX", "wage_IDX",
                 "cpi_IDX", "dcpi_IDX", "cpi_IDX_imf", "fcpi_WIDX", "IMcpi_IDX", 
                 "ecpi_IDX", "er_IDX")

# 2. Apply the transformations
MODEL_READY <- MODEL_DF %>%
  mutate(
    # Turn 429,418 into 12.97 (logs)
    across(all_of(vars_to_log), ~ log(.x))
  )





view(MODEL_READY)
str(MODEL_READY)



##########################################################################################
########### tried Christiano-Fitzgerald (CF) FILTER FOR OUTPUT AND UNEMPLOYMENT GAPS #####
### BUT THE 3SLS RESULTS PRODUCED SIGNIFICANT AUTOCORRELATION, SO UNIFYING THE APPROACH ##

# 2. Apply HP Filter with Lambda (1600) for a "Neutral Rate"

hp_Y <- hpfilter(MODEL_READY$GVA_bp, freq = 1600)

# 3. Map back to the subset
MODEL_READY <- MODEL_READY %>%
  mutate(
    Y_star = as.numeric(hp_Y$trend),
    Y_GAP = as.numeric(hp_Y$cycle) * 100
  )

# 3. Quick check: Is the 2024 Q4 gap reasonable?
tail(MODEL_READY %>% select(date, GVA_bp, Y_star, Y_GAP), 5)

### REPEAT FOR UNEMPLOYMENT GAP ################

hp_U <- hpfilter(MODEL_READY$u_RATE, freq = 1600)

# 3. Map back to the subset
MODEL_READY <- MODEL_READY %>%
  mutate(
    u_STAR = as.numeric(hp_U$trend),
    u_GAP = as.numeric(hp_U$cycle)
  )

# 3. Quick check: Is the 2024 Q4 gap reasonable?
tail(MODEL_READY %>% select(date, u_RATE, u_STAR, u_GAP), 5)


# 4. Verify comparability between output and unemployment gaps 
MODEL_READY %>% 
  select(date, GVA_bp, Y_GAP, u_RATE, u_GAP) %>% 
  tail(5)


#### SO, ABOVE WE HAVE NEW OUTPUT AND UNEMPLOYMENT GAPS
################################################################################################################

########## NEW INTEREST RATE GAP  #########################################
# Wald test showed the expectations weights don't sum to 1. This usually means lead_pi (expectations) 
#   is "stealing" the explanatory power of the other variables because it is so highly correlated with them.
# The Fix: Try a "Target-Consistent" Phillips Curve. Instead of using lead_pi (which is just a lead of your 
#   own data), use the deviation from the 2% target.

MODEL_READY <- MODEL_READY %>%
  mutate(
    # INFLATION, INFLATION "GAP" (deviation from BOE target), REAL INTEREST RATE, GILT SPREAD (5-YR) 
    # INFL_yoy: (log(now) - log(4 quarters ago)) gives a decimal growth rate
    # NEED TO BASE THIS ON IMF VERSION OF UK CPI FOR MODEL CONSISTENCY AND RESOLVE AUTOCORREL.
    cpi_INFL_imf = (cpi_IDX_imf - lag(cpi_IDX_imf, 4)) * 100,
    dcpi_INFL = (dcpi_IDX - lag(dcpi_IDX, 4)) * 100,
    # 1. Inflation Deviation from Target (2.25% is value for GDP defl that is equiv to CPI 2% target)
    dcpi_DEV = dcpi_INFL - 2.25,
    
    r_UK = i_UK - dcpi_DEV,
    
    i5y_SPR = i_g5y - i_UK)

# IMPORTS / ENERGY PRICE INFLATION 

MODEL_READY <- MODEL_READY |> 
  mutate(
    IMcpi_INFL  = (IMcpi_IDX - lag(IMcpi_IDX, 4)) * 100,
    IMcpi_INFL_adj = IMcpi_IDX / er_IDX,
    ecpi_INFL = (ecpi_IDX - lag(ecpi_IDX, 4)) * 100
  )

#################### NET EXPORT GROWTH ########################################

# Net Exports (NX) growth can be volatile, so we often use the 
MODEL_READY <- MODEL_READY %>%
  mutate(
    # 1. Calculate the Net Export Log-Ratio (Level)
    # Since EX and IM are already logs in your dataset:
    NX_ratio = EX - IM,
    
    # 2. Calculate the Change (Growth Contribution)
    # This stays in decimal scale (e.g., 0.005 = 0.5% contribution)
    dNX = (NX_ratio - lag(NX_ratio)) * 100,
    
    # 3. Create a lag for the regression
    dNX_lag1 = lag(dNX, 1)
  )


# Remove the first 4 rows and the last 4 rows
MODEL_READY <- MODEL_READY %>%
  slice(5:(n() - 4))






## NOMINAL RATE GAP
hp_i_UK <- hpfilter(MODEL_READY$i_UK, freq = 1600)

# 3. Map back to the subset
MODEL_READY <- MODEL_READY %>%
  mutate(
    i_STAR = as.numeric(hp_i_UK$trend),
    i_GAP = as.numeric(hp_i_UK$cycle)
  )

## REAL RATE GAP
#### real interest rate GAP = nominal interest rate gap less the inflation deviation ##########
MODEL_READY <- MODEL_READY %>%
  mutate(
    r_GAP = i_GAP - dcpi_DEV)

## INT'L RATE DIFFERENTIAL
MODEL_READY <- MODEL_READY %>%
  mutate(
    i_DIFFL = (i_UK - i_FOR)
  )

MODEL_READY <- MODEL_READY |> 
  mutate(
    di_DIFFL = i_DIFFL - lag(i_DIFFL, 1)
  )

################################################################################




####################################################################################################

### CONVERT NOMINAL EXCHANGE RATE TO REAL EXCHANGE RATE###########

MODEL_READY <- MODEL_READY %>%
  mutate(rer = er_IDX + cpi_IDX_imf - fcpi_WIDX)

MODEL_READY <- MODEL_READY |> 
  mutate(
    drer = (rer - lag(rer, 1)) * 100
  )

#  NOTE ON RER ####

# RER rising indicates a Real Appreciation of the Pound—either because the nominal exchange rate 
#   strengthened or UK prices rose faster than foreign prices.
# Coefficient Impact: In your equations (like an export or output gap equation), RER 
#   should typically have a negative coefficient, as a higher RER represents a loss of international 
#   competitiveness



### QUICK NOTE ON APPROACH TO PRODUCTIVITY AND WAGES #########
# For a consistent New Keynesian / Structural model: 
#   Productivity: Use the Gap. It represents "Supply-side slack." If productivity is below trend 
#     (negative gap), it puts upward pressure on unit labor costs and inflation.
#   Real Wages: Use Real Wage Growth. In the UK, wages are often "sticky." Modeling the growth of 
#     wages relative to the gap in productivity is a classic way to show how "Wage-Push Inflation" 
#     works.
# N.B. FOUND THAT REAL WAGE GROWTH FAILS STATIONARITY TESTS


##### define PRODUCTIVITY GAP so it represents supply-side slack ###################################
#                      ######################

# 2. Apply HP Filter (lambda = 1600 for quarterly data)
hp_prod <- hpfilter(MODEL_READY$prod_IDX, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$prod_GAP <- hp_prod$cycle * 100

MODEL_READY %>% 
  select(date, prod_IDX, prod_GAP) |>  
  tail(5)

# Direct Elasticity: In your 3SLS, if you regressed Inflation on this PROD_GAP, a coefficient of 
#   -0.2 would mean: "A 1 percentage point drop in the productivity gap (productivity falling 
#   further below trend) is associated with a 0.2 percentage point increase in inflation."
# Middle East Crisis Link: In a supply shock scenario (like the Middle East crisis), energy costs
#   spike, which often causes real productivity to drop relative to its trend. Your model can now 
#   capture this "negative productivity shock" on the same 1:1 scale as your interest rate hikes.


##### REAL WAGE GAP ##############################################################

hp_wage <- hpfilter(MODEL_READY$wage_IDX, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$wage_GAP <- hp_wage$cycle * 100
#####################################################################################




# Handling Supply Shocks: The Middle East crisis is a classic supply shock. When energy prices spike,
#   it creates a "wedge" between productivity and wages. By using Real Wage Growth, you can observe 
#   if wages are "sticky" (rising despite the productivity gap falling) or if they are "flexible" 
#   and drop to match the new economic reality.
# The "Wage-Push" Channel: In the UK, recent data from 2024 and early 2025 shows real wages growing 
#   by approximately 2% even as output per worker fell. Modeling this as growth allows you to see 
#   how much "catch-up" pressure exists from previous high-inflation periods. 


str(MODEL_READY)
################################################################################




hp_IMcpi_INFL<- hpfilter(MODEL_READY$IMcpi_INFL, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$IMcpi_GAP <- hp_IMcpi_INFL$cycle

hp_ecpi_INFL<- hpfilter(MODEL_READY$ecpi_INFL, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$ecpi_GAP <- hp_ecpi_INFL$cycle


hp_i5y_SPR<- hpfilter(MODEL_READY$i5y_SPR, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$i5y_GAP <- hp_i5y_SPR$cycle

# IF REQUIRED...
# 3. ZLB Interaction (Zero Lower Bound)
# # Zero lower bound dummy for Bank Rate
MODEL_READY$zlb <- if_else(MODEL_READY$i_UK <= 0.25, 1, 0)


str(MODEL_READY)
view(MODEL_READY)


MODEL_READY <- MODEL_READY %>%
  slice(-1)



####################################################
### FOR QUARTO REPORT
library(readr)
write_csv(MODEL_READY, "reports/MODEL_READY.csv")
####################################################



################################################################################

###   TESTING    ################


vars_to_test <- c("Y_GAP", "u_GAP", "dcpi_DEV", "r_GAP", "drer", "prod_GAP", "wage_GAP", 
                  "i5y_GAP", "di_DIFFL","IMcpi_GAP", "ecpi_GAP", "dNX")
# Loop through and print p-values
for (v in vars_to_test) {
  # Remove NAs to avoid errors
  clean_data <- na.omit(MODEL_READY[[v]])
  
  # Run ADF test
  test_result <- adf.test(clean_data)
  
  # Print result
  cat(v, "ADF p-value:", round(test_result$p.value, 4), "\n")
}

### STATIONARITY AUDIT VARIABLES #########################################################
# Y_GAP ADF p-value: 0.01 
# u_GAP ADF p-value: 0.0275 
# dcpi_DEV ADF p-value: 0.0437 
# r_GAP ADF p-value: 0.0709 
# drer ADF p-value: 0.01 
# prod_GAP ADF p-value: 0.01 
# wage_GAP ADF p-value: 0.0118 
# i5y_GAP ADF p-value: 0.01 
# di_DIFFL ADF p-value: 0.0128 
# IMcpi_GAP ADF p-value: 0.0486 
# ecpi_GAP ADF p-value: 0.01 
# dNX ADF p-value: 0.01 

MODEL_READY %>% 
  select(date, Y_GAP, u_GAP, dcpi_DEV, r_GAP, drer, prod_GAP, wage_GAP, 
        i5y_GAP, di_DIFFL, IMcpi_GAP, ecpi_GAP, dNX) |>  
  tail(25)

# ZIVOT-ANDREWS TEST FOR STATIONARITY WITH STRUCTURAL BREAKS

library(urca)

# ENDOGENOUS VARIABLES

# for both intercept and trend ('both')
za_Y_GAP <- ur.za(MODEL_READY$Y_GAP, model = "both", lag = NULL)
summary(za_Y_GAP)
# Y_GAP is stationary with a break (position 2020 Q1)
struc <- breakpoints(MODEL_READY$Y_GAP ~ 1)
summary(struc)
# case not strong for break...BIC minimised at m = 0 ; is it worth it for RSS reduction?


za_u_GAP <- ur.za(MODEL_READY$u_GAP, model = "both", lag = NULL)
summary(za_u_GAP)
# Zivot-Andrews only allows for one structural break, but there are several in U_GAP

# Bai–Perron multiple breakpoints test
struc <- breakpoints(MODEL_READY$u_GAP ~ 1)
summary(struc)
# U_GAP has for major structural breaks (@ 30: 2004 Q2; @48: 2008 Q4; @68: 2013 Q4; @92: 2019 Q4)
MODEL_READY$date[c(25, 43, 63, 87, 88)]
# Here’s the key insight:
# 👉 Standard unit‑root tests (ADF, PP, ZA) assume 0 or 1 break.
# 👉 U_GAP clearly has 4 breaks.
# 👉 So these tests incorrectly classify U_GAP as non‑stationary.
# This is not a real non‑stationarity problem.
# It’s simply a multiple‑break problem, which is normal in macro labour market data.
# 💡 4. The correct conclusion: U_GAP is stationary Because:  
# It has a stable mean-reverting structure within regimes.
# Labour gaps are constructed to be I(0).
# Bai–Perron shows clear discrete regime shifts.
# ZA fails because it only handles 1 break.
# ADF fails because it assumes no breaks at all.
#So the correct interpretation is:
#  ✔️ U_GAP is I(0) with multiple structural breaks
#❗ It is not I(1).
#❗ No differencing is needed.
#❗ This is fully normal in real macro models.


# for both intercept and trend ('both')
za_dcpi_DEV <- ur.za(MODEL_READY$dcpi_DEV, model = "both", lag = NULL)
summary(za_dcpi_DEV)
# DEFL_dev is stationary with a break at position 96 (2022 Q1)
struc <- breakpoints(MODEL_READY$dcpi_DEV ~ 1)
summary(struc)
# Inflation deviation exhibits a single structural break @ 88 2020Q1
# 2022Q1 (ZA) or 2020 Q1 (BP), 
# corresponding to the onset of the global inflation shock. Multiple-break models were rejected 
# by BIC, confirming that only one break is needed to stabilise the inflation equation.
# The UK inflation process did not structurally break in 2019. Inflation was still very subdued in 2019.
# The actual break is the global inflation shock of 2021. Supply chains, energy markets, markups, 
# import prices, and expectations all changed. ZA is more aligned with inflation‑process breaks
# ZA is designed to detect shifts in the trend and drift, not just mean shifts. Your structural model 
# needs the big break, not the early drift Using the early‑drift BP break would: overcorrect the Phillips 
# Curve, reduce parsimony, distort the inflation slope, introduce unnecessary dummy structure.


za_r_GAP <- ur.za(MODEL_READY$r_GAP, model = "both", lag = NULL)
summary(za_r_GAP)
# r_GAP is stationary with a break at position 96 (2022 Q1) [same as DEFL_dev]
# Bai–Perron multiple breakpoints test
struc <- breakpoints(MODEL_READY$r_GAP ~ 1)
summary(struc)
# again, suggests break @ 88

za_drer <- ur.za(MODEL_READY$drer, model = "both", lag = NULL)
summary(za_drer)
# D_RER is stationary with a potential break at position 48 (2008 Q4)
# Bai–Perron multiple breakpoints test
struc <- breakpoints(MODEL_READY$drer ~ 1)
summary(struc)
# suggests that no breakpoint is required. but, if needed for robustness, include at 48 (what ZA suggests)
# Interpretation: The ZA statistic is strongly significant → D_RER is stationary (I(0)) even with a break.
# ZA’s estimated break date is position 48, which corresponds to 2008 Q4. This makes perfect economic sense: 
# 2008Q4 was the height of the global financial crisis, exchange rates worldwide experienced huge swings and 
# volatility regime‑shifts. So ZA is telling you: If D_RER has a break, it is only the 2008Q4 financial crisis.
# Reconciling the Two Tests (ZA vs BP): A (unit-root + break test): Designed to detect a single break in a 
#   trending / near-unit-root variable. Finds break at 48 = 2008Q4. Tells you: “D_RER is stationary, but the 
#   best single-break specification is 2008Q4.” BP (multiple-structural-break segmentation): Designed to detect 
#   regime shifts in the mean. Prefers zero breaks for D_RER. Adding breaks reduces RSS but worsens BIC 
#   (overfits noise). Thus the two tests are telling you: D_RER is stationary — good for your UIP equation. 
#   No break is required for stability. The only plausible break is 2008Q4 (if you want a robustness dummy)


# note that WAGE_GAP passed the ADF test
za_wage_GAP <- ur.za(MODEL_READY$wage_GAP, model = "both", lag = NULL)
summary(za_wage_GAP)
# fails to reject null hypothesis that WAGE_GAP has a unit root and suggests break at position 101 (2022 Q1)
# ...very late in sample and doesn't make much sense 
# Bai–Perron multiple breakpoints test
struc <- breakpoints(MODEL_READY$wage_GAP ~ 1)
summary(struc)
# Statistically, no breaks required. BIC lowest with no breaks, despite dip at m=2 and m=3. 
# Why wage gap can be stationary without clear breakpoints. Wage behaviour in the UK changed over time due to:
#   post‑GFC low productivity; Brexit labour‑supply shifts; post‑Covid tight labour market; 2022–23 wage–price 
#   dynamics.
# BUT these changes were gradual, not sudden: firms adjust wages with delays; contracts are staggered; 
#   expectations adapt slowly; bargaining power shifts smoothly.
# Thus: ADF sees stable mean reversion → stationary. ZA cannot detect one dominant crash or spike → no strong 
#   break. BP detects lots of small, unimportant changes → no break optimal. 
# This is consistent with how wage gaps tend to behave in real semi‑structural models.


za_prod_GAP <- ur.za(MODEL_READY$prod_GAP, model = "both", lag = NULL)
summary(za_prod_GAP)
struc <- breakpoints(MODEL_READY$prod_GAP ~ 1)
summary(struc)
# “ADF confirms that the productivity gap is stationary. Zivot–Andrews suggests one break in late 2020/early 
#   2021, consistent with COVID distortions. Bai–Perron BIC clearly rejects the inclusion of breaks, making 
#   a zero‑break specification optimal. Therefore, no break dummies are required in the productivity‑gap 
#   equation.”


# EXOGENOUS VARIABLES
za_i5y_GAP <- ur.za(MODEL_READY$i5y_GAP, model = "both", lag = NULL)
summary(za_i5y_GAP)
struc <- breakpoints(MODEL_READY$i5y_GAP ~ 1)
summary(struc)

za_di_DIFFL <- ur.za(MODEL_READY$di_DIFFL, model = "both", lag = NULL)
summary(za_di_DIFFL)
struc <- breakpoints(MODEL_READY$di_DIFFL ~ 1)
summary(struc)

za_IMcpi_GAP <- ur.za(MODEL_READY$IMcpi_GAP, model = "both", lag = NULL)
summary(za_IMcpi_GAP)
struc <- breakpoints(MODEL_READY$IMcpi_GAP ~ 1)
summary(struc)

za_ecpi_GAP <- ur.za(MODEL_READY$ecpi_GAP, model = "both", lag = NULL)
summary(za_ecpi_GAP)
struc <- breakpoints(MODEL_READY$ecpi_GAP ~ 1)
summary(struc)

za_dNX <- ur.za(MODEL_READY$dNX, model = "both", lag = NULL)
summary(za_dNX)
struc <- breakpoints(MODEL_READY$dNX ~ 1)
summary(struc)




###  STATIONARITY OF ALL ENDOGENDOUS AND EXOGENOUS VARIABLES CONFIRMED  ########

########## BREAK DUMMIES #####################################
# THESE ARE WHERE THE STRUCTURAL BREAKDS OCCUR IN U_GAP ######

bp <- c(25, 43, 63, 87, 88, 96)
# new Y_GAP @ 88 (2020Q1) 
#     u_GAP @ 87 (2019Q4) 
#           @ 63 (2013Q4) 
#           @ 43 (2008Q4) 
#           @ 25 (2004Q2)
# dcpi_DEV  @ 96 (2022Q1)
#     r_GAP @ 96 also

# Unemployment gap breaks (Okun equation only)
MODEL_READY$bpu1 <- seq_len(nrow(MODEL_READY)) >= bp[1]
MODEL_READY$bpu2 <- seq_len(nrow(MODEL_READY)) >= bp[2]
MODEL_READY$bpu3 <- seq_len(nrow(MODEL_READY)) >= bp[3]
MODEL_READY$bpu4 <- seq_len(nrow(MODEL_READY)) >= bp[4]

# Output gap break (IS curve only)
MODEL_READY$bpy  <- seq_len(nrow(MODEL_READY)) >= bp[5]

# Inflation deviation break (Phillips curve only)
MODEL_READY$bpp  <- seq_len(nrow(MODEL_READY)) >= bp[6]

# Real interest-rate gap break (IS curve only)
MODEL_READY$bpr  <- seq_len(nrow(MODEL_READY)) >= bp[6]
# THERE ARE ALSO SINGLE STRUCTURAL BREAKS IN Y_GAP (@93) AND DEFL_dev (@97)
# continue with ZA and BP tests on the other variables that have passed ADF
# THINK ABOUT WHETHER SEPARATE STRUCTURAL BREAK DUMMIES ARE REQUIRED FOR THE DIFFERENT EQUATIONS. 
# because CO-PILOT recommends including the four above (30, 48, 68, 92) in the: Okun equation, 
#     Wage Phillips Curve, Phillips Curve, IS curve, Taylor rule (optional)




