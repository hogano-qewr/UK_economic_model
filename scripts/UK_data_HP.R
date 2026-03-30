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
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )


## ADD INFLATION DATA

UK_OBR_inflation <- read_excel("data/UK_OBR_inflation.xlsx", sheet = "Sheet2", skip = 3)

# Process the inflation data (CPI)
cpi <- UK_OBR_inflation  |> 
  dplyr::select(Date = 1, CPI_index = "CPI outturn index")  |>     
  mutate(Date = format(as.yearqtr(Date, format = "%YQ%q"), "%Y Q%q"),
         CPI_index = as.numeric(CPI_index))

#  Clean the cpi df to keep only the first record per Date
cpi_clean <- cpi %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()

#  filter down to matching period
cpi_clean |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
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
  rename(UK_CPI_imf = `United Kingdom_CPI_imf`,
         USA_CPI_imf = `United States_CPI_imf`,
         China_CPI_imf = `China, People's Republic of_CPI_imf`,
         EURO_CPI_imf = `Euro Area (EA)_CPI_imf`,
         SWISS_CPI_imf = `Switzerland_CPI_imf`,
         NORW_CPI_imf = `Norway_CPI_imf`)





# 2. Join with your main dataset
MODEL_DF <- left_join(MODEL_DF, cpi_prepared, by = "Date")




library(tidyverse)

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
MODEL_DF <- MODEL_DF %>%
  mutate(
    Foreign_CPI_imf = 
      (USA_CPI_imf ^ weights["usa"]) *
      (EURO_CPI_imf ^ weights["euro"]) *
      (China_CPI_imf ^ weights["china"]) *
      (SWISS_CPI_imf ^ weights["swiss"]) *
      (NORW_CPI_imf ^ weights["nor"])
  )

# View the first few rows of the new index
# Use the explicit package name to avoid the conflict
MODEL_DF %>% 
  dplyr::select(Date, Foreign_CPI_imf) %>% 
  head()

###################### END  ######










###################################################################################################################










### BANK RATE

# 1. Process the Bank Rate (assuming column 1 is Date, column 2 is Rate)

BoE_bank_rate <- read_excel("data/BoE_bank_rate.xlsx", skip = 1)

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

#  Clean the cpi df to keep only the first record per Date
BOE_rate_clean <- BOE_rate %>% group_by(Date) %>% slice_head(n = 1) %>% ungroup()

#  filter down to matching period
BOE_rate_clean |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

# 2. Join with your existing Master Dataframe
MODEL_DF <- left_join(MODEL_DF, BOE_rate, by = "Date")



# MARKET INTEREST RATE: (for market premium over BOE risk-free rate) Pull 5-year Gilt Yield (Market-based interest rate)
gilt_5y <- read_excel("data/BoE_gilt_5y_nom_par_rate.xlsx", 
                                       skip = 1) |> 
  dplyr::select(RawDate = 1, gilt_5y = 2) |>            # Select the first two columns regardless of their names
  mutate(
    # Convert "31 Mar 24" to a Date object
    temp_date = dmy(RawDate),
    # Convert to the ONS string format "2024 Q1"
    Date = format(as.yearqtr(temp_date), "%Y Q%q")
  ) |>
  # Keep only the cleaned Date and the Rate for the join
  dplyr::select(Date, gilt_5y)

#  Clean the gilt yield df to keep only the first record per Date
gilt_5y <- gilt_5y  |>  
  group_by(Date)  |>  
  slice_head(n = 1)  |>  
  ungroup()

#  filter down to matching period
gilt_5y |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, gilt_5y, by = "Date")




### STERLING EFFECTIVE EXCHANGE RATE
sterling_eff_exch_rate <- read_excel("data/sterling_eff_exch_rate.xls", 
                                     skip = 8)

GBP_EER <- sterling_eff_exch_rate %>%
  dplyr::select(Date = 1, GBP_EER = "Sterling effective exchange rate index: Monthly average (Jan 2005=100)") %>%
  filter(grepl("^[0-9]{4} Q[1-4]$", Date)) %>% # Only matches "YYYY QX"
  distinct(Date, .keep_all = TRUE)            # Removes duplicates

GBP_EER_final <- GBP_EER |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  ) |> 
  group_by(Date) |>  
  slice_head(n = 1)  |>  
  ungroup() |> 
  mutate(
    # Convert list to vector, then to numeric
  GBP_EER = as.numeric(unlist(GBP_EER))
  )


MODEL_DF <- left_join(MODEL_DF, GBP_EER_final, by = "Date")








### adding Imports of goods and services deflator:SA

import_price_deflator <- read_excel("data/import_price_deflator.xls", skip = 8)

# Process the Imports deflator (ONS series YBFZ)
IM_defl <- import_price_deflator  |> 
  dplyr::select(Date = 1, IM_defl = `Imports of goods and services deflator:SA`) |> 
  filter(grepl("Q", Date))  |>                  # Keep only rows with "Q" (Quarterly) 
  group_by(Date)  |>  slice_head(n = 1)  |>  ungroup() |> 
  mutate(IM_defl = as.numeric(IM_defl)) |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, IM_defl, by = "Date")





## ADD UNEMPLOYMENT DATA

UK_unempl <- read_excel("data/UK_unempl.xls")

# Process the unemployment data
unemp <- UK_unempl  |> 
  dplyr::select(Date = 1, Unemp_rate = "Unemployment rate (aged 16 and over, seasonally adjusted): %") |> 
  filter(grepl("Q", Date)) |> 
  group_by(Date)  |>  slice_head(n = 1)  |>  ungroup() |> 
  mutate(Unemp_rate = as.numeric(Unemp_rate)) |> 
  filter(
    as.yearqtr(Date) >= as.yearqtr("1997 Q1") & 
      as.yearqtr(Date) <= as.yearqtr("2024 Q4")
  )

MODEL_DF <- left_join(MODEL_DF, unemp, by = "Date")




MODEL_DF <- MODEL_DF %>%
  mutate(
    # Pulse dummies to 'neutralise' the 2020 volatility
    dummy_2020Q2 = if_else(Date == "2020 Q2", 1, 0),
    dummy_2020Q3 = if_else(Date == "2020 Q3", 1, 0),
    
    # Optional: Step dummy if you believe in a post-2020 structural shift
    post_covid = if_else(as.yearqtr(Date) >= as.yearqtr("2020 Q1"), 1, 0)
  )


## Adding CPI fuels component, system price for electricity and Ofgem price cap (dual fuel) 
#     (due to failure of Phillips Curve on 1st-pass 3SLS)


model_inputs_energy_prices <- read_excel("data/model_inputs_energy_prices.xlsx")



# 1. Clean the 'Date' strings in both dataframes (remove extra spaces)
MODEL_DF <- MODEL_DF %>%
  mutate(Date = str_squish(as.character(Date)))

model_inputs_energy_prices <- model_inputs_energy_prices %>%
  mutate(Date = str_squish(as.character(Date)))

# 2. Re-join
MODEL_DF <- left_join(MODEL_DF, model_inputs_energy_prices, by = "Date")

# 3. Check if it worked
sum(!is.na(MODEL_DF$energy_cpi))


#  CREATING A SINGLE INVESTMENT TOTAL VARIABLE:
MODEL_DF <- MODEL_DF |> 
  mutate(I_total = (I_gcf + I_inven + I_acq),
         I_total = as.numeric(I_total),
         )



library(dplyr)

# 1. Define your groups
vars_to_log <- c("GDP_mp", "GVA_bp", "indir_net_tax", "C", "C_np", "G", 
                 "I_gcf", "I_total", "EX", "IM", "dom_DEM",
                 "CPI_index", "China_CPI_imf", "NORW_CPI_imf", "SWISS_CPI_imf", 
                 "UK_CPI_imf", "USA_CPI_imf", "EURO_CPI_imf", "Foreign_CPI_imf",
                 "GBP_EER", "IM_defl", "energy_cpi")

vars_to_decimal <- c("BOE_rate", "gilt_5y", "Unemp_rate")

# 2. Apply the transformations
MODEL_READY <- MODEL_DF %>%
  mutate(
    # Turn 7.5% into 0.075
    across(all_of(vars_to_decimal), ~ .x / 100),
    
    # Turn 429,418 into 12.97 (logs)
    across(all_of(vars_to_log), ~ log(.x))
  )

# 3. Handle Inventory separately 
# (I_inven has negative numbers, so you CANNOT log it. Use it as a % of GDP instead)
MODEL_READY <- MODEL_READY %>%
  mutate(I_inven_ratio = I_inven / exp(GDP_mp)) 





#################### DATA PREP ; DIFFS, LOG-DIFFS, LEADS, LAGS ETC ###################################

# NEED TO REVIEW IN LIGHT OF CHANGES TO METHOD FOR REAL INTEREST RATE GAP AND INCORPORATING NEW FOREIGN CPI (AND MATCHING
#   UK CPI FROM IMF) INTO CALCULATION OF REAL EXCHANGE RATE# #######

# 1. Define variables that are naturally rates/ratios (0-100 scale)
rate_vars <- c("BOE_rate", "gilt_5y", "Unemp_rate")


# 2. Transform the data

MODEL_READY <- MODEL_READY %>%
  arrange(Date) %>%
  mutate(
    # A. LOG-DIFFERENCES (Calculates % growth)
    # Use for levels/indices: GDP, Consumption, CPI, Energy, Import Prices
    across(c(GDP_mp, GVA_bp, indir_net_tax, C, C_np, I_total, G, EX, IM, CPI_index, Foreign_CPI_imf, UK_CPI_imf, IM_defl, energy_cpi), 
           ~ log(.) - log(lag(.)), 
           .names = "dlog_{.col}"),
    
    # B. SIMPLE DIFFERENCES (Calculates percentage point change)
    # Use for interest rates and unemployment rate
    across(all_of(rate_vars), 
           ~ . - lag(.), 
           .names = "d_{.col}"),
    
    # C. ENERGY SHIFTERS (Log-diffs, filling NAs with 0 for early years)
    dlog_w_sale = coalesce(log(w_sale_elec_ppkWh / lag(w_sale_elec_ppkWh)), 0),
    dlog_cap    = coalesce(log(dual_price_cap_GBPpm / lag(dual_price_cap_GBPpm)), 0)
  )

# Net Exports (NX) growth can be volatile, so we often use the 
MODEL_READY <- MODEL_READY %>%
  mutate(
    # 1. Calculate the Net Export Log-Ratio (Level)
    # Since EX and IM are already logs in your dataset:
    NX_ratio = EX - IM,
    
    # 2. Calculate the Change (Growth Contribution)
    # This stays in decimal scale (e.g., 0.005 = 0.5% contribution)
    dNX = NX_ratio - lag(NX_ratio),
    
    # 3. Create a lag for the regression
    dNX_lag1 = lag(dNX, 1)
  )



# Wald test showed the expectations weights don't sum to 1. This usually means lead_pi (expectations) 
#   is "stealing" the explanatory power of the other variables because it is so highly correlated with them.
# The Fix: Try a "Target-Consistent" Phillips Curve. Instead of using lead_pi (which is just a lead of your 
#   own data), use the deviation from the 2% target.
MODEL_READY <- MODEL_READY %>%
  mutate(
    
    # 3. REAL INTEREST RATE GAP (R_GAP)
    # INFL_yoy: (log(now) - log(4 quarters ago)) gives a decimal growth rate
    INFL_yoy = log(CPI_index) - log(lag(CPI_index, 4)),
    # 1. Inflation Deviation from Target
    # Use 0.02 (decimal) instead of 2.0 (whole number)
    # This keeps it consistent with your other gap variables
    Infl_dev = INFL_yoy - 0.02,
    
    # 2. Market Spread Change
    # d_gilt_5y and d_BOE_rate are already in decimals (e.g., 0.0025 for 25bps)
    # We calculate the change (difference) here:
    d_gilt_5y = gilt_5y - lag(gilt_5y),
    d_BOE_rate = BOE_rate - lag(BOE_rate),
    d_mkt_spr = d_gilt_5y - d_BOE_rate,
    
    # 3. ZLB Interaction (Zero Lower Bound)
    # Use 0.005 (0.5%) as the threshold to match your decimal BOE_rate
    zlb_dummy = if_else(BOE_rate <= 0.005, 1, 0),
    zlb_interaction = zlb_dummy * d_mkt_spr
  )





#######  NEW GAPS   ################


library(mFilter)
library(dplyr)

# Standard HP frequency for quarterly data
hp_lambda <- 1600 

MODEL_READY <- MODEL_READY %>%
  mutate(
        # 2. UNEMPLOYMENT GAP (U_GAP)
    # Unemp_rate is already 0.073 (decimal). Cycle will be e.g., +0.005
    U_GAP = as.numeric(mFilter::hpfilter(Unemp_rate, freq = hp_lambda)$cycle),
    
    # Real Rate = Nominal Decimal Rate - Decimal Inflation
    real_BOE_r = BOE_rate - INFL_yoy,
    
    # r_star: Trend of the real interest rate is a better proxy for 3SLS 
    # than trying to scale dlog growth by 4.
    r_star = as.numeric(mFilter::hpfilter(na.omit(real_BOE_r), freq = hp_lambda)$trend) %>%
      { c(rep(NA, 4), .) }, # Pad for the 4-quarter lag in INFL_yoy
    
    R_GAP = real_BOE_r - r_star,
    
    # 4. REAL EXCHANGE RATE GAP (RER_GAP)
    # Construct REER (already in logs from previous step's 'vars_to_log')
    # log(reer) = log(EER) + log(UK_CPI) - log(Foreign_CPI)
    reer_log = GBP_EER + UK_CPI_imf - Foreign_CPI_imf,
    
    # Gap is the cyclical deviation in decimals
    RER_GAP = as.numeric(mFilter::hpfilter(reer_log, freq = hp_lambda)$cycle)
  )


########### look at HAMILTON FILTER which looks more robust than HP FILTER ###################################

####  below is our new output gap Y_GAP .... 
# Install the package if you haven't

library(tidyverse)
library(timetk)
library(neverhpfilter)
library(zoo) # Specifically for quarterly date handling

MODEL_READY <- MODEL_READY %>%
  mutate(
    # Converts "1997Q1" or similar into a Date object at the start of the quarter
    date = as.Date(as.yearqtr(date))
  ) %>% 
  tk_xts(select = GDP_mp, date_var = date) %>% 
  yth_filter(h = 8, p = 4) %>%
  tk_tbl(rename_index = "date") %>%
  right_join(
    MODEL_READY %>% mutate(date = as.Date(as.yearqtr(date))), 
    by = "date"
  ) %>%
  rename(
    potential_gdp = yth.trend,
    output_gap    = yth.cycle
  )


library(tidyverse)
library(timetk)
library(neverhpfilter)
library(zoo)

# Re-run the filter to get the clean columns back
HAMILTON_RESULTS <- MODEL_READY %>%
  mutate(date_clean = as.Date(as.yearqtr(.data$date))) %>%
  tk_xts(select = GDP_mp, date_var = date_clean) %>% 
  yth_filter(h = 8, p = 4) %>%
  tk_tbl(rename_index = "date_clean")

# Explicitly name the columns based on the yth_filter structure
# 1st: date_clean, 2nd: GDP_mp (original), 3rd: potential, 4th: gap
colnames(HAMILTON_RESULTS) <- c("date_clean", "GDP_mp_orig", "potential_gdp", "output_gap")

# Now merge it back correctly
MODEL_READY <- MODEL_READY %>%
  mutate(date_clean = as.Date(as.yearqtr(.data$date))) %>%
  # Remove the old (broken) columns if they exist
  select(-any_of(c("potential_gdp", "output_gap"))) %>% 
  left_join(HAMILTON_RESULTS %>% select(date_clean, potential_gdp, output_gap), 
            by = "date_clean")

# Check the results again
MODEL_READY %>% 
  select(date, GDP_mp, potential_gdp, output_gap) %>% 
  slice(50:60)

# Check the most recent 8 quarters
MODEL_READY %>% 
  select(date, GDP_mp, potential_gdp, output_gap) %>% 
  tail(8)


library(tidyverse)
library(patchwork)
library(zoo)

# 1. Prepare a clean plotting data frame
# We filter out the first 12 rows (NAs) so the scales look correct
plot_df <- MODEL_READY %>%
  mutate(date_plot = as.Date(as.yearqtr(date))) %>%
  filter(!is.na(output_gap))

# 2. Top Plot: Actual vs Potential (Levels)
p1 <- ggplot(plot_df, aes(x = date_plot)) +
  geom_line(aes(y = GDP_mp, color = "Actual Log GDP"), linewidth = 1) +
  geom_line(aes(y = potential_gdp, color = "Potential (Hamilton Trend)"), 
            linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Actual Log GDP" = "black", 
                                "Potential (Hamilton Trend)" = "firebrick")) +
  labs(title = "GDP Decomposition: 1997 - 2024",
       subtitle = "Decomposing Log GDP into Trend and Cycle",
       y = "Log Level", x = NULL, color = "") +
  theme_minimal() +
  theme(legend.position = "top")

# 3. Bottom Plot: The Output Gap (The Cycle)
p2 <- ggplot(plot_df, aes(x = date_plot, y = output_gap)) +
  # Adds a shaded area for visual impact
  geom_ribbon(aes(ymin = 0, ymax = output_gap), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  labs(title = "Output Gap",
       subtitle = "Percentage Deviation from Potential",
       y = "Gap (Decimal)", x = "Year",
       caption = "Method: Hamilton Regression Filter (h=8, p=4)") +
  theme_minimal()

# 4. Combine and display
p1 / p2




################################################################################################################



view(MODEL_READY)




# LEADS AND LAGS: EXPECTATIONS AND PERSISTENCE

library(dplyr)

# 1. Define the core variables that need leads/lags
# These are usually your endogenous 'gap' and 'rate' variables
core_vars <- c("Y_GAP", "U_GAP", "R_GAP", "RER_GAP", "INFL_yoy", "Infl_dev", "BOE_rate")

MODEL_READY <- MODEL_READY %>%
  # Create Lags (t-1 to t-4) for persistence
  mutate(across(all_of(core_vars), 
                list(lag1 = ~lag(.x, 1), 
                     lag2 = ~lag(.x, 2),
                     lag4 = ~lag(.x, 4)), 
                .names = "{.col}_{.fn}")) %>%
  
  # Create Leads (t+1) for forward-looking expectations
  # Note: 3SLS will use instruments to handle the endogeneity of these leads
  mutate(across(all_of(core_vars), 
                list(lead1 = ~lead(.x, 1)), 
                .names = "{.col}_{.fn}")) %>%
  
  # Optional: Create a 'Change' variable for the exchange rate gap to capture momentum
  mutate(dRER_GAP = RER_GAP - lag(RER_GAP),
         RER_GAP_lag2 = lag(RER_GAP, 2),
         dNX_lag2      = lag(dNX, 2)
  )

MODEL_READY <- MODEL_READY %>%
  mutate(
    # Change in Import Prices (already logged in previous steps)
    dIM_defl     = IM_defl - lag(IM_defl),
    dIM_defl_lag1 = lag(dIM_defl, 1),
    
    # Change in Energy Prices (already logged)
    dEnergy      = energy_cpi - lag(energy_cpi),
    dEnergy_lag1 = lag(dEnergy, 1)
  )


MODEL_READY <- MODEL_READY %>%
  mutate(
    # 1. Calculate the change (will be NA for early years)
    dlog_wholesale = log(w_sale_elec_ppkWh) - lag(log(w_sale_elec_ppkWh)),
    dlog_price_cap = log(dual_price_cap_GBPpm) - lag(log(dual_price_cap_GBPpm)),
    
    # 2. THE FIX: Replace NAs with 0
    # This keeps the rows in the regression instead of dropping them
    dlog_wholesale_lag1 = coalesce(lag(dlog_wholesale, 1), 0),
    dlog_price_cap_lag1 = coalesce(lag(dlog_price_cap, 1), 0)
  )


view(MODEL_READY)
str(MODEL_READY)



# Remove the first 5 rows and the last row
MODEL_READY <- MODEL_READY  |>  
  slice(-(1:4), -n())
 



####################################################
### FOR QUARTO REPORT
library(readr)
write_csv(MODEL_READY, "reports/MODEL_READY.csv")
####################################################





















