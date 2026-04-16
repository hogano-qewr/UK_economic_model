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
library(stats)
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
#   package's as.yearqtr class, you can treat them as numeric values


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
# (I_inven has negative numbers, so you CANNOT log it. Use it as a % of GDP instead) (*100 to state as %)
MODEL_DF <- MODEL_DF %>%
  mutate(invtry_RAT = (I_inven / GDP_mp) * 100)  



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

view(MODEL_DF)


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


MODEL_READY <- MODEL_READY %>%
  mutate(
    Y_GAP_L1    = lag(Y_GAP, 1),
    Y_GAP_L2    = lag(Y_GAP, 2),
    Y_GAP_L3    = lag(Y_GAP, 3),
    u_GAP_L1    = lag(u_GAP, 1),
    u_GAP_L2    = lag(u_GAP, 2),
    u_GAP_L3    = lag(u_GAP, 3)
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

### CONVERT NOMINAL EXCHANGE RATE INDEX TO REAL EXCHANGE RATE INDEX and ESTABLISH FIRST DIFFERENCES ###########

MODEL_READY <- MODEL_READY %>%
  mutate(rer = er_IDX + cpi_IDX_imf - fcpi_WIDX)

MODEL_READY <- MODEL_READY |> 
  mutate(
    drer = (rer - lag(rer, 1)) * 100
  )

MODEL_READY <- MODEL_READY %>%
  mutate(
    drer_L1     = lag(drer, 1)
  )
MODEL_DF %>% 
  select(date, er_IDX) %>% 
  tail(5)
MODEL_READY %>% 
  select(date, er_IDX, cpi_IDX_imf, fcpi_WIDX, drer, i_UK, i_FOR) %>% 
  tail(5)
#  NOTE ON RER ####

# RER rising indicates a Real Appreciation of the Pound—either because the nominal exchange rate 
#   strengthened or UK prices rose faster than foreign prices.
# Coefficient Impact: In your equations (like an export or output gap equation), RER 
#   should typically have a negative coefficient, as a higher RER represents a loss of international 
#   competitiveness



##### REAL WAGE GAP and LAG ##############################################################

hp_wage <- hpfilter(MODEL_READY$wage_IDX, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$wage_GAP <- hp_wage$cycle * 100
#####################################################################################

MODEL_READY <- MODEL_READY %>%
  mutate(
    wage_GAP_L1 = lag(wage_GAP, 1)
  )
##########################################################################################

# SLICE OUT LAST 4 ROWS FOR MISSING DATA #################################################
library(dplyr)
MODEL_READY <- MODEL_READY %>% slice_head(n = -4)


##### define PRODUCTIVITY GAP so it represents supply-side slack ###################################
#                      ######################

# 2. Apply HP Filter (lambda = 1600 for quarterly data)
hp_prod <- hpfilter(MODEL_READY$prod_IDX, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$prod_GAP <- hp_prod$cycle * 100


# CHECK ###
MODEL_READY %>% 
  select(date, prod_IDX, prod_GAP) |>  
  tail(5)

MODEL_READY <- MODEL_READY %>%
  mutate(
    prod_GAP_L1 = lag(prod_GAP, 1)
  )

# Direct Elasticity: In your 3SLS, if you regressed Inflation on this PROD_GAP, a coefficient of 
#   -0.2 would mean: "A 1 percentage point drop in the productivity gap (productivity falling 
#   further below trend) is associated with a 0.2 percentage point increase in inflation."
# Middle East Crisis Link: In a supply shock scenario (like the Middle East crisis), energy costs
#   spike, which often causes real productivity to drop relative to its trend. Your model can now 
#   capture this "negative productivity shock" on the same 1:1 scale as your interest rate hikes.

# Handling Supply Shocks: The Middle East crisis is a classic supply shock. When energy prices spike,
#   it creates a "wedge" between productivity and wages. By using Real Wage Growth, you can observe 
#   if wages are "sticky" (rising despite the productivity gap falling) or if they are "flexible" 
#   and drop to match the new economic reality.
# The "Wage-Push" Channel: In the UK, recent data from 2024 and early 2025 shows real wages growing 
#   by approximately 2% even as output per worker fell. Modeling this as growth allows you to see 
#   how much "catch-up" pressure exists from previous high-inflation periods. 
# N.B. FOUND THAT REAL WAGE GROWTH FAILS STATIONARITY TESTS




### HYBRID G ONLY ##########################################################

# need one new state                Y_POT_G
# Y_gPOT = productivity‑driven potential output growth component. It is the he flow component of 
#   output growth that the economy can absorb without generating inflationary pressure. 
#   Equivalently, Y_gPOT is the part of Y_GAP that should not trigger policy or inflation responses.
#  The long-run anchor we're missing from the SOE Hybrids (D,E,F,G)
# Levels vs growth: what should Y_gPOT respond to?
#   Option A: Y_gPOT responds to levels of productivity: Y_gPOT_t = ρ · Y_gPOT_{t−1} + φ · prod_GAP_t
#         Interpretation: A permanent productivity improvement permanently raises potential growth.
#         Problems: Over‑amplifies productivity shocks. Blurs level vs growth effects. Leads to 
#         “double counting” because you already have prod_GAP in levels. Hard to stabilise numerically 
#         in IRFs. Not recommended in a gap‑based semi‑structural model
#   Option B (Best practice): Y_gPOT responds to changes in productivity. 
#         Y_gPOT_t = ρ · Y_gPOT_{t−1} + φ · Δprod_GAP_t in which 
#         Δprod_GAP_t = prod_GAP_t − prod_GAP_{t−1}
#         Interpretation: Acceleration in productivity growth raises potential growth. Level shifts 
#         do not permanently push potential growth higher. Balanced growth stabilises naturally.
#          CB practice for semi-struc models. Integrates smoothly with your existing OLP block
#          Should fix the pathology we're seeing
#   RECOMMENDATION: Use Δprod_GAP, not prod_GAP levels, to drive Y_gPOT. Best for Hybrid G given: 
#         gap‑based structure, no expectations, strong desire for long‑run neutrality, open‑economy
#         + UIP dynamics.

MODEL_READY <- MODEL_READY %>%
  mutate(
    dprod_GAP = prod_GAP - prod_GAP_L1,
    )
rho_gpot <- 0.7   # persistence of potential growth; productivity growth affects potential growth for 2–3 years
phi_gpot <- 0.5   # responsiveness to productivity growth; partial pass‑through (not one‑for‑one)
MODEL_READY$Y_gPOT <- NA_real_

# initialise at zero (balanced growth at sample start)
MODEL_READY$Y_gPOT[1] <- 0

for (t in 2:nrow(MODEL_READY)) {
  MODEL_READY$Y_gPOT[t] <-
    rho_gpot * MODEL_READY$Y_gPOT[t - 1] +
    phi_gpot * MODEL_READY$dprod_GAP[t]
}
# CHECK: Does it move only when productivity growth changes? You should see: spikes around productivity 
#   accelerations, gradual decay back toward zero, no permanent drift.
plot(MODEL_READY$Y_gPOT, type = "l",
     main = "Potential output growth component (Y_gPOT)")
mean(MODEL_READY$Y_gPOT, na.rm = TRUE)
cor(cumsum(MODEL_READY$Y_gPOT), 1:nrow(MODEL_READY), use = "complete.obs")

# Does it correlate with productivity growth but not levels? Expected: high-ish correlation with dprod_GAP
#   much lower correlation with prod_GAP. If it tracks levels too closely, rho is too high.
cor(MODEL_READY$Y_gPOT, MODEL_READY$dprod_GAP, use = "complete.obs")
cor(MODEL_READY$Y_gPOT, MODEL_READY$prod_GAP, use = "complete.obs")



# need one transformed variable     YGAP_EFF = Y_GAP − κ · Y_POT_G    
#                         where κ is a calibration parameter (typically 0.7–1.0)

# Construct the effective output gap
kappa <- 1.0   # full removal of potential growth from the gap
MODEL_READY$YGAP_EFF <-
  MODEL_READY$Y_GAP - kappa * MODEL_READY$Y_gPOT
# THIS IS THE VARIABLE WE'LL SUB Phillips curve and Taylor rule. Everything else still uses Y_GAP.
# So, we won't be changing IS curve, Okun’s law, Wage Phillips curve, OLP equation, UIP, PLG error 
#   correction
plot(MODEL_READY$YGAP_EFF, type = "l",
     main = "Hybrid G: Effective output gap (YGAP_EFF)")



MODEL_READY %>% 
  select(date, Y_GAP, prod_GAP, dprod_GAP, Y_gPOT, YGAP_EFF) |>  
  tail(10)


MODEL_READY <- MODEL_READY %>%
  mutate(
    dprod_GAP_L1 = lag(dprod_GAP, 1),
    Y_gPOT_L1    = lag(Y_gPOT, 1)
  )




###  HYBRID H ONLY - BUILDS ON HYBRID G ######################################

# Hybrid H closes the real system in levels, not just growth, so that permanent productivity level 
#   shocks are fully accommodated and the output gap converges to zero in the long run — giving you 
#   a credible SOE over all timeframes. Hybrid G fixed growth misclassification. Hybrid H fixes level 
#   mis‑anchoring.
# The principle (very important): Permanent productivity levels must shift potential output levels, 
#   not generate permanent output gaps. Formally, Hybrid H enforces: as t approaches infinity, limit of 
#   Y_GAP_t =0. In other words, after a permanent productivity shock. The missing SOE equilibrium condition.
# 🔧 New variable (one only): Potential output level gap.  Call it: Y_potL
# Interpretation: slow‑moving level of potential output relative to baseline. Absorbs permanent productivity 
#   shifts. distinct from Y_gPOT (which absorbs growth)
# Dynamics of the new variable. Simplest credible closure: Y_potL_t = Y_potL_t-1 + ψ . prod_GAP_t in which
#   ψ ∈ (0,1) (start with 0.05–0.10 quarterly). Permanent productivity raises potential levels gradually
# ✅ Balanced growth✅ Long‑run neutrality✅ No explosions✅ No DSGE machinery
# Redefine the effective output gap (Hybrid H)
#   Currently (Hybrid G): YGAP_EFF = Y_GAP − Y_gPOT
#   Hybrid H            : YGAP_EFF = Y_GAP − Y_gPOT − Y_potL

# CREATE NEW VARIABLE HERE AND NEW YGAP_EFF HERE => MINIMAL CODE CHANGES

psi_potL <- 0.07   # start conservative

MODEL_READY$Y_potL <- NA_real_
MODEL_READY$Y_potL[1] <- 0

for (t in 2:nrow(MODEL_READY)) {
  MODEL_READY$Y_potL[t] <-
    MODEL_READY$Y_potL[t-1] +
    psi_potL * MODEL_READY$prod_GAP[t]
}

plot(MODEL_READY$Y_potL, type = "l",
     main = "Potential output gap level (Y_potL)")
mean(MODEL_READY$Y_potL, na.rm = TRUE)
cor(cumsum(MODEL_READY$Y_potL), 1:nrow(MODEL_READY), use = "complete.obs")

MODEL_READY <- MODEL_READY |> 
  mutate(
    YGAP_EFF_H = Y_GAP - Y_gPOT - Y_potL
  )
plot(MODEL_READY$YGAP_EFF_H, type = "l",
     main = "Hybrid H: Effective output gap (YGAP_EFF_H)")



MODEL_READY <- MODEL_READY |> 
  mutate(
    Y_potL_L1 = lag(Y_potL, 1)
  )

MODEL_READY %>% 
  select(date, Y_GAP, prod_GAP, dprod_GAP, Y_gPOT, YGAP_EFF, Y_potL, YGAP_EFF_H) |>  
  tail(10)


####################################################################################################





########## INFLATION AND INTEREST RATES (PREP FOR RATE GAPS) #########################################
# Wald test showed the expectations weights don't sum to 1. This usually means lead_pi (expectations) 
#   is "stealing" the explanatory power of the other variables because it is so highly correlated with them.
# The Fix: Try a "Target-Consistent" Phillips Curve. Instead of using lead_pi (which is just a lead of your 
#   own data), use the deviation from the 2% target.

MODEL_READY <- MODEL_READY %>%
  mutate(
    # INFLATION, INFLATION "DEV" (deviation from BOE target), REAL INTEREST RATE, GILT SPREAD (5-YR) 
    # INFL_yoy: (log(now) - log(4 quarters ago)) gives a decimal growth rate
    # NEED TO BASE THIS ON IMF VERSION OF UK CPI FOR MODEL CONSISTENCY AND RESOLVE AUTOCORREL.
    cpi_INFL_imf = (cpi_IDX_imf - lag(cpi_IDX_imf, 4)) * 100,
    dcpi_INFL = (dcpi_IDX - lag(dcpi_IDX, 4)) * 100,
    # 1. Inflation Deviation from Target (2.25% is value for GDP defl that is equiv to CPI 2% target)
    dcpi_DEV = dcpi_INFL - 2.25,
    
    r_UK = i_UK - dcpi_INFL,
    
    i5y_SPR = i_g5y - i_UK)

# IMPORTS / ENERGY PRICE INFLATION 

MODEL_READY <- MODEL_READY |> 
  mutate(
    IMcpi_INFL  = (IMcpi_IDX - lag(IMcpi_IDX, 4)) * 100,
    ecpi_INFL = (ecpi_IDX - lag(ecpi_IDX, 4)) * 100
  )

## INT'L RATE DIFFERENTIAL
MODEL_READY <- MODEL_READY %>%
  mutate(
    i_DIFFL = (i_UK - i_FOR)
  )

MODEL_READY <- MODEL_READY %>%
  mutate(
    dcpi_DEV_L1 = lag(dcpi_DEV, 1),
    dcpi_DEV_L2 = lag(dcpi_DEV,2),
    i_UK_L1     = lag(i_UK, 1),
    )




# REMOVE FIRST 4 ROWS (now NA due to df loss)
MODEL_READY <- MODEL_READY %>%
  slice(-(1:4))

MODEL_READY <- MODEL_READY %>%
  mutate(
    PLG = cumsum(dcpi_DEV),
    PLG_L1 = lag(PLG, 1)
  )

plot(MODEL_READY$PLG, type = "l",
     main = "Price level GAP (PLG)")


MODEL_READY %>% 
  select(date, dcpi_DEV, PLG) |>  
  tail(10)







## REAL RATE GAP
#### real interest rate GAP 
hp_r_UK <- hpfilter(MODEL_READY$r_UK, freq = 1600)

MODEL_READY <- MODEL_READY %>%
  mutate(
    r_STAR = as.numeric(hp_r_UK$trend),
    r_GAP = as.numeric(hp_r_UK$cycle)
  )

MODEL_READY <- MODEL_READY |> 
  mutate(
    r_STAR_cal = 0.5,
    r_GAP_cal  = r_UK - r_STAR_cal
  )

plot(MODEL_READY$r_GAP_cal, type = "l",
     main = "Real interest rate GAP (calibrated =>")


MODEL_READY %>% 
  select(date, i_UK, dcpi_INFL, r_UK, r_STAR_cal, r_GAP_cal) %>% 
  tail(5)


hp_i_DIFFL <- hpfilter(MODEL_READY$i_DIFFL, freq = 1600)

MODEL_READY <- MODEL_READY %>%
  mutate(
    i_DIFFL_STAR = as.numeric(hp_i_DIFFL$trend),
    i_DIFFL_GAP = as.numeric(hp_i_DIFFL$cycle)
  )

################################################################################


hp_IMcpi_INFL<- hpfilter(MODEL_READY$IMcpi_INFL, freq = 1600)

# 3. Extract the 'cycle' as your PROD_GAP
MODEL_READY$IMcpi_GAP <- hp_IMcpi_INFL$cycle

### FOR HYBRID E ONLY  ###################
MODEL_READY <- MODEL_READY %>%
  mutate(
    IMcpi_GAP_L1 = lag(IMcpi_GAP, 1)
  )





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



# REMOVE FIRST 2 ROWS (now NA due to df losses on dcpi_DEV lags)
MODEL_READY <- MODEL_READY %>%
  slice(-(1:2))

####################################################################################################



str(MODEL_READY)
################################################################################




################################################################################

###   TESTING    ################


vars_to_test <- c("Y_GAP", "u_GAP", "dNX", "drer", "wage_GAP", "prod_GAP", "dprod_GAP", "Y_gPOT", "YGAP_EFF",
                  "Y_potL", "YGAP_EFF_H", "dcpi_DEV", "r_GAP", "i_DIFFL_GAP", "i5y_GAP", "IMcpi_GAP", "ecpi_GAP")
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
# r_GAP ADF p-value: 0.01 
# drer ADF p-value: 0.01 
# prod_GAP ADF p-value: 0.01 
# wage_GAP ADF p-value: 0.0118 
# i5y_GAP ADF p-value: 0.01 
# i_DIFFL_GAP ADF p-value: 0.0347 
# IMcpi_GAP ADF p-value: 0.0486 
# ecpi_GAP ADF p-value: 0.01 
# dNX ADF p-value: 0.01 
 
### re-ren post re-organ of data script to lose fewer degrees of freedom APRIL 5TH 2026
# Y_GAP ADF p-value: 0.01 
# u_GAP ADF p-value: 0.0293 
# dcpi_DEV ADF p-value: 0.0453 
# r_GAP ADF p-value: 0.01 
# drer ADF p-value: 0.01 
# prod_GAP ADF p-value: 0.01 
# wage_GAP ADF p-value: 0.0225 
# i5y_GAP ADF p-value: 0.01 
# i_DIFFL_GAP ADF p-value: 0.0406 
# IMcpi_GAP ADF p-value: 0.0522 
# ecpi_GAP ADF p-value: 0.01 
# dNX ADF p-value: 0.01 

# re-run post-Hybrid E 13/04/2026
#Y_GAP ADF p-value: 0.01 
#u_GAP ADF p-value: 0.0293 
#dNX ADF p-value: 0.01 
#drer ADF p-value: 0.01 
#wage_GAP ADF p-value: 0.0225 
#prod_GAP ADF p-value: 0.01 
#dprod_GAP ADF p-value: 0.01 
#Y_gPOT ADF p-value: 0.01 
#YGAP_EFF ADF p-value: 0.01 
#Y_potL ADF p-value: 0.0209 
#YGAP_EFF_H ADF p-value: 0.01 
#dcpi_DEV ADF p-value: 0.0453 
#r_GAP ADF p-value: 0.01 
#i_DIFFL_GAP ADF p-value: 0.0406 
#i5y_GAP ADF p-value: 0.01 
#IMcpi_GAP ADF p-value: 0.0522 
#ecpi_GAP ADF p-value: 0.01 

# All equilibrium‑relevant gap variables are stationary. Non‑stationarity in price‑level and 
#   interest‑rate stock variables is intentional and disciplined via error‑correction terms.”

MODEL_READY %>% 
  select(date, Y_GAP, u_GAP, dcpi_DEV, r_GAP, drer, prod_GAP, wage_GAP, 
        i5y_GAP, i_DIFFL_GAP, IMcpi_GAP, ecpi_GAP, dNX) |>  
  tail(25)

# ZIVOT-ANDREWS TEST FOR STATIONARITY WITH STRUCTURAL BREAKS

library(urca)

# ENDOGENOUS VARIABLES
za_Y_GAP <- ur.za(MODEL_READY$Y_GAP, model = "both", lag = NULL)
summary(za_Y_GAP)

struc <- breakpoints(MODEL_READY$Y_GAP ~ 1)
summary(struc)



za_u_GAP <- ur.za(MODEL_READY$u_GAP, model = "both", lag = NULL)
summary(za_u_GAP)

struc <- breakpoints(MODEL_READY$u_GAP ~ 1)
summary(struc)
MODEL_READY$date[c(24, 42, 62, 86)] # = ["2004-04-01" "2008-10-01" "2013-10-01" "2019-10-01"]




za_dcpi_DEV <- ur.za(MODEL_READY$dcpi_DEV, model = "both", lag = NULL)
summary(za_dcpi_DEV)

struc <- breakpoints(MODEL_READY$dcpi_DEV ~ 1)
summary(struc)




za_r_GAP <- ur.za(MODEL_READY$r_GAP, model = "both", lag = NULL)
summary(za_r_GAP)

struc <- breakpoints(MODEL_READY$r_GAP ~ 1)
summary(struc)





za_drer <- ur.za(MODEL_READY$drer, model = "both", lag = NULL)
summary(za_drer)

struc <- breakpoints(MODEL_READY$drer ~ 1)
summary(struc)





za_wage_GAP <- ur.za(MODEL_READY$wage_GAP, model = "both", lag = NULL)
summary(za_wage_GAP)

struc <- breakpoints(MODEL_READY$wage_GAP ~ 1)
summary(struc)





za_prod_GAP <- ur.za(MODEL_READY$prod_GAP, model = "both", lag = NULL)
summary(za_prod_GAP)

struc <- breakpoints(MODEL_READY$prod_GAP ~ 1)
summary(struc)






# EXOGENOUS VARIABLES
za_i5y_GAP <- ur.za(MODEL_READY$i5y_GAP, model = "both", lag = NULL)
summary(za_i5y_GAP)

struc <- breakpoints(MODEL_READY$i5y_GAP ~ 1)
summary(struc)




za_i_DIFFL_GAP <- ur.za(MODEL_READY$i_DIFFL_GAP, model = "both", lag = NULL)
summary(za_i_DIFFL_GAP)

struc <- breakpoints(MODEL_READY$i_DIFFL_GAP ~ 1)
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




########## BREAK DUMMIES ######################################
# SEPARATE STRUCTURAL BREAK DUMMIES FOR THE DIFFERENT EQUATIONS
# APRIL 5 REVSED POSITION -> BETTER DATA -> MODEL DESIGN motivate MINIMAL BREAKS & MINIMAL application in EQNS

# new Y_GAP @ 87 (2020Q1)  # cancelled for Apr 5th run
#     u_GAP @ 86 (2019Q4)  # cancelled for Apr 5th run
#           @ 62 (2013Q4)      # RETAINED
#           @ 42 (2008Q4)      # RETAINED
#           @ 24 (2004Q2)  # cancelled for Apr 5th run
# dcpi_DEV  @ 87 (2020Q1)      # RETAINED BUT CHANGED FROM 2022Q1 (THE PEAK)
#           @ 91 (2021Q1)      # NEW but only as an alternative to 87 for robustness testing


bpu <- c(42, 62)
# Unemployment gap breaks (Okun equation only OR OKUN and WAGE-UNEMP BLOCK?)
# Keep the dummy narrowly targeted unless evidence tells you otherwise.
MODEL_READY$bpu1_GFC  <- seq_len(nrow(MODEL_READY)) >= bpu[1]  
MODEL_READY$bpu2_POST <- seq_len(nrow(MODEL_READY)) >= bpu[2]  # robustness test for inclusion / exclusion...

bpp <- c(87, 91) # inflation regime start alternatives
# Inflation deviation break (Phillips curve only)
MODEL_READY$bpp_BEG  <- seq_len(nrow(MODEL_READY)) >= bpp[1]
MODEL_READY$bpp_LAT  <- seq_len(nrow(MODEL_READY)) >= bpp[2]  # for robustness testing as alternative


# NOTE: Only one regime dummy per block is included in the baseline.
# Secondary dummies are used exclusively for sensitivity analysis.




####################################################
### FOR QUARTO REPORT
library(readr)
write_csv(MODEL_READY, "reports/MODEL_READY.csv")
####################################################


## MULTICOLLINEARITY TESTS
# N.B library(car) / vif(lm(Y_GAP ~ ...)) once OLS run on individ equations

# IS relation
cor(MODEL_READY[, c("Y_GAP_L1", "r_GAP", "r_GAP_cal", "i5y_GAP","drer","dNX")], use = "pairwise.complete.obs")
# PC relation
cor(MODEL_READY[, c("dcpi_DEV_L1", "dcpi_DEV_L2", "Y_GAP", "IMcpi_GAP", "ecpi_GAP")], 
    use = "pairwise.complete.obs")
# TR relation
cor(MODEL_READY[, c("i_UK_L1", "dcpi_DEV", "Y_GAP")], use = "pairwise.complete.obs")
# OLU 
cor(MODEL_READY[, c("u_GAP_L1", "Y_GAP")], use = "pairwise.complete.obs")
# WPC
cor(MODEL_READY[, c("wage_GAP_L1", "dcpi_DEV_L1", "u_GAP", "prod_GAP")], use = "pairwise.complete.obs")
# OLP
cor(MODEL_READY[, c("prod_GAP_L1", "Y_GAP")], use = "pairwise.complete.obs")
# UIP
cor(MODEL_READY[, c("drer_L1", "i_DIFFL_GAP", "dNX", "IMcpi_GAP")], use = "pairwise.complete.obs")


## ACF, PACF etc. ### lag structures

# Y_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$Y_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$Y_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))


# u_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$u_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$u_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))


# dcpi_DEV ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$dcpi_DEV, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$dcpi_DEV, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))


# r_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$r_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$r_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))


# drer ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$drer, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$drer, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# wage_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$wage_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$wage_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# prod_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$prod_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$prod_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# i_DIFFL_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$i_DIFFL_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$i_DIFFL_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# dNX ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$dNX, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$dNX, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# IMcpi_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$IMcpi_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$IMcpi_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))



# ecpi_GAP ##############################
# 1. Set up the plotting area (1 row, 2 columns)
par(mfrow = c(1, 2))

# 2. Plot Autocorrelation Function
acf(MODEL_READY$ecpi_GAP, main = "ACF Plot", na.action = na.pass)

# 3. Plot Partial Autocorrelation Function
pacf(MODEL_READY$ecpi_GAP, main = "PACF Plot")

# Reset plotting area
par(mfrow = c(1, 1))