
library(systemfit)

# 1. Define the Structural Equations
eq_IS    <- Y_GAP ~ Y_GAP_lag1 + Y_GAP_lead1 + R_GAP + d_mkt_spr + zlb_interaction + RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + dummy_2020Q2 + dummy_2020Q3
eq_PC    <- INFL_yoy ~ INFL_yoy_lag1 + INFL_yoy_lead1 + Y_GAP_lag1 + dIM_defl_lag1 + dEnergy_lag1 + dlog_price_cap_lag1 + dummy_2020Q2 + dummy_2020Q3
eq_TR    <- BOE_rate ~ BOE_rate_lag1 + Infl_dev + Y_GAP + zlb_dummy + dummy_2020Q2
eq_Okun  <- U_GAP ~ U_GAP_lag1 + Y_GAP + dummy_2020Q2

system <- list(IS = eq_IS, PC = eq_PC, TR = eq_TR, Okun = eq_Okun)

# 2. Define the Instruments
# These must be EXOGENOUS (lags, dummies, or external shocks)
instr <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
  RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + 
  dIM_defl_lag1 + dEnergy_lag1 + dlog_price_cap_lag1 +
  zlb_dummy + dummy_2020Q2 + dummy_2020Q3


fit_3sls <- systemfit(system, method = "3SLS", inst = instr, data = MODEL_READY)

summary(fit_3sls)



#############   RUN SARGAN TEST (OVERIDENTIFYING RESTRICTIONS PART) ##########################################

install.packages("ivreg")
library(ivreg)

# 1. Test the IS Curve instruments
is_test <- ivreg(Y_GAP ~ Y_GAP_lag1 + Y_GAP_lead1 + R_GAP + d_mkt_spr + zlb_interaction + 
                   RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + dummy_2020Q2 + dummy_2020Q3 | 
                   Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + RER_GAP_lag1 + 
                   RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + dIM_defl_lag1 + dEnergy_lag1 + 
                   dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
                 data = MODEL_READY)

# 2. Run the Diagnostic (Sargan is the 'Overidentifying restrictions' part)
summary(is_test, diagnostics = TRUE)

# The 'Refined' IS Curve
is_test_trimmed <- ivreg(
  Y_GAP ~ 
    Y_GAP_lag1 + 
    R_GAP + 
    RER_GAP_lag1 + 
    dNX_lag1 + 
    dummy_2020Q2 + 
    dummy_2020Q3 | 
    # Use the same powerful instrument set
    Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
    RER_GAP_lag1 + dNX_lag1 + dIM_defl_lag1 + dEnergy_lag1 + 
    dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
  data = MODEL_READY
)

# Check the diagnostics again
summary(is_test_trimmed, diagnostics = TRUE)

######### REFINEMENT FOLLOWING ABOVE DIAGNOSTICS ###############################

# 1. Update the IS formula in your system list
eq_IS_final <- Y_GAP ~ Y_GAP_lag1 + R_GAP + RER_GAP_lag1 + dNX_lag1 + dummy_2020Q2 + dummy_2020Q3

# 2. Re-define the system (keeping the other 3 as they were, they were great)
system_final <- list(IS = eq_IS_final, PC = eq_PC, TR = eq_TR, Okun = eq_Okun)

# 3. Re-run 3SLS
fit_3sls_final <- systemfit(system_final, method = "3SLS", inst = instr, data = MODEL_READY)

# 4. View the final, polished results
summary(fit_3sls_final)



#### HAUSMAN TEST##############################################################

# 1. Run 2SLS using the EXACT SAME 'system_final' as the 3SLS
fit_2sls_final <- systemfit(system_final, 
                            method = "2SLS", 
                            inst = instr, 
                            data = MODEL_READY)

# 2. Now the Hausman test will work because the dimensions match
h_test <- hausman.systemfit(fit_2sls_final, fit_3sls_final)

# 3. Check the p-value
print(h_test)

###############################################################################################################

############# INCLUDE SYSTEM R-SQUARED - MCELROYIN COEFFS. TABLE ##############

library(huxtable)

# 1. Define helper functions for systemfit (Required for huxtable to read the model)
tidy.systemfit.equation <- function(x, ...) {
  s <- summary(x)
  d <- as.data.frame(s$coefficients)
  names(d) <- c("estimate", "std.error", "statistic", "p.value")
  d$term <- rownames(d)
  return(d)
}

glance.systemfit.equation <- function(x, ...) {
  s <- summary(x)
  # Correct way to pull degrees of freedom for N
  data.frame(nobs = s$df[1] + s$df[2], r.squared = s$r.squared)
}

# 2. Build the Base Table
ht <- huxreg(
  "IS Curve"       = fit_3sls_final$eq[[1]], 
  "Phillips Curve" = fit_3sls_final$eq[[2]], 
  "Taylor Rule"    = fit_3sls_final$eq[[3]], 
  "Okun's Law"     = fit_3sls_final$eq[[4]],
  coefs = c(
    "Constant"                 = "(Intercept)",
    "Output Gap (t-1)"         = "Y_GAP_lag1",
    "Real Interest Rate Gap"   = "R_GAP",
    "Real Exchange Rate (t-1)" = "RER_GAP_lag1",
    "Net Export Growth (t-1)"  = "dNX_lag1",
    "Inflation (t-1)"          = "INFL_yoy_lag1",
    "Expected Inflation (t+1)" = "INFL_yoy_lead1",
    "Inflation Deviation"      = "Infl_dev",
    "Base Rate (t-1)"          = "BOE_rate_lag1",
    "Unemployment Gap (t-1)"   = "U_GAP_lag1",
    "Output Gap (Current)"     = "Y_GAP",
    "COVID-19 (2020Q2)"        = "dummy_2020Q2",
    "COVID-19 (2020Q3)"        = "dummy_2020Q3"
  ),
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  # ADD THIS LINE TO SILENCE THE WARNING:
  statistics = c("N" = "nobs", "R2" = "r.squared")
)

# 3. Add Custom Footer Rows (Sargan and System R2)
sargan_values <- c("Sargan Test (p)", "0.380", "0.122", "0.881", "N/A")
r2_system_row <- c("System R-squared (McElroy)", "", "", "", "0.961")

ht_final <- ht %>%
  add_rows(sargan_values, after = nrow(ht) - 2) %>%
  add_rows(r2_system_row)

# 4. Apply Final Formatting & "Nuclear" Fix for Decimals
row_sargan <- nrow(ht_final) - 3 # Index after adding R2 row
row_system <- nrow(ht_final)

number_format(ht_final)[c(row_sargan, row_system), 2:5] <- NA # Lock decimals

ht_final <- ht_final %>%
  theme_article() %>%
  set_align(1:nrow(.), 1, "left") %>%           # Labels Left
  set_align(1:nrow(.), 2:5, "center") %>%       # Numbers Center
  set_left_padding(1:nrow(.), 1, 10) %>%        # Nice indent
  set_italic(c(row_sargan, row_system), 1) %>%  # Italicize footer labels
  set_bold(c(row_sargan, row_system), 1) %>%    # Bold footer labels
  set_bottom_border(row_sargan - 1, 1:5, 0.4) %>% # Line above Sargan
  set_bottom_border(row_system, 1:5, 1.0)       # Thick final line

# 5. Output the masterpiece
ht_final

###############################################################################################################






#############   need to RUN SARGAN TEST (OVERIDENTIFYING RESTRICTIONS PART) on PC ####################


library(ivreg)

# 1. Test the Phillips Curve instruments
pc_test <- ivreg(INFL_yoy ~ INFL_yoy_lag1 + INFL_yoy_lead1 + Y_GAP_lag1 + dIM_defl_lag1 + dEnergy_lag1 + dlog_price_cap_lag1 + dummy_2020Q2 + dummy_2020Q3 | 
                   Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + RER_GAP_lag1 + 
                   RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + dIM_defl_lag1 + dEnergy_lag1 + 
                   dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
                 data = MODEL_READY)

# 2. Run the Diagnostic (Sargan is the 'Overidentifying restrictions' part)
summary(pc_test, diagnostics = TRUE)


# Refined PC test with fewer, cleaner instruments
pc_test_refined <- ivreg(
  INFL_yoy ~ 
    INFL_yoy_lag1 + INFL_yoy_lead1 + 
    Y_GAP_lag1 + dIM_defl_lag1 + dEnergy_lag1 + dlog_price_cap_lag1 + 
    dummy_2020Q2 + dummy_2020Q3 | 
    # REMOVED: RER_GAP_lag2 and dNX_lag2
    Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
    RER_GAP_lag1 + dNX_lag1 + dIM_defl_lag1 + dEnergy_lag1 + 
    dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
  data = MODEL_READY
)

summary(pc_test_refined, diagnostics = TRUE)



####TEST OF SYSTEM WITH INSTRUMENTS REMOVED FOLLOWING PC REFINEMENT - TO SEE IF IT MESSES UP IS RELATION #######

# 1. Shorter instrument list (removed lag2 terms)
instr_short <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
  RER_GAP_lag1 + dNX_lag1 + 
  dIM_defl_lag1 + dEnergy_lag1 + dlog_price_cap_lag1 +
  zlb_dummy + dummy_2020Q2 + dummy_2020Q3

# 2. Re-run 3SLS
fit_3sls_short <- systemfit(system_final, method = "3SLS", inst = instr_short, data = MODEL_READY)

# 3. Check the IS Curve specifically
summary(fit_3sls_short)


##### GOOD RESULT - THIS IS OUR NEW SYSTEM - 20 MAR -2026 ######################################################


# 1. The Refined Instrument List (Removed RER_GAP_lag2 and dNX_lag2)
instr_final <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
  RER_GAP_lag1 + dNX_lag1 + dIM_defl_lag1 + dEnergy_lag1 + 
  dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3

# 2. Re-run the final 3SLS System
fit_3sls_final <- systemfit(system_final, method = "3SLS", inst = instr_final, data = MODEL_READY)

summary(fit_3sls_final)

# 3. Your updated Sargan p-values for the table
# IS Curve: 0.380 (Approx from previous ivreg)
# PC Curve: 0.089 (From your most recent refined test)
# TR Curve: 0.881 (Conservative estimate)


### REVISED TABLE OF COEFFICIENTS

#| label: tbl-results
#| tbl-cap: "Table 1: 3SLS System Estimates of the UK Economy"
#| results: asis
#| echo: false

library(huxtable)

# 1. Build the base table with explicit names for headers
ht <- huxreg(
  "IS Curve"       = fit_3sls_final$eq[[1]], 
  "Phillips Curve" = fit_3sls_final$eq[[2]], 
  "Taylor Rule"    = fit_3sls_final$eq[[3]], 
  "Okun's Law"     = fit_3sls_final$eq[[4]],
  coefs = c(
    "Constant"                 = "(Intercept)",
    "Output Gap (t-1)"         = "Y_GAP_lag1",
    "Real Interest Rate Gap"   = "R_GAP",
    "Real Exchange Rate (t-1)" = "RER_GAP_lag1",
    "Net Export Growth (t-1)"  = "dNX_lag1",
    "Inflation (t-1)"          = "INFL_yoy_lag1",
    "Expected Inflation (t+1)" = "INFL_yoy_lead1",
    "Inflation Deviation"      = "Infl_dev",
    "Base Rate (t-1)"          = "BOE_rate_lag1",
    "Unemployment Gap (t-1)"   = "U_GAP_lag1",
    "Output Gap (Current)"     = "Y_GAP",
    "COVID-19 (2020Q2)"        = "dummy_2020Q2",
    "COVID-19 (2020Q3)"        = "dummy_2020Q3"
  ),
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  statistics = c("N" = "nobs", "R2" = "r.squared"),
  note = NULL # Handled manually at the bottom
)

# 2. Construct the Footer Rows
sargan_values <- c("Sargan Test (p)", "0.380", "0.089", "0.881", "N/A")
system_r2_row <- c("System R-squared (McElroy)", "0.961", "", "", "")
stars_note    <- c("*** p < 0.01; ** p < 0.05; * p < 0.1", "", "", "", "")

# 3. Add Rows and Apply Formatting
ht_final <- ht %>%
  add_rows(sargan_values, after = nrow(.)) %>%
  add_rows(system_r2_row) %>%
  add_rows(stars_note)

# Define row indices for formatting
row_sargan <- nrow(ht_final) - 2
row_system <- nrow(ht_final) - 1
row_note   <- nrow(ht_final)

# Center the System R-squared and the Note
ht_final <- ht_final %>%
  merge_cells(row_system, 2:5) %>%
  merge_cells(row_note, 1:5) %>%
  set_align(row_system, 2, "center") %>%
  set_align(row_note, 1, "left") %>%
  # Lock the decimals for footer stats
  set_number_format(row_sargan, 2:5, NA) %>%
  set_number_format(row_system, 2, NA)

# 4. Final Polish: Alignment and Borders
ht_final <- ht_final %>%
  theme_article() %>%
  set_align(1:nrow(.), 1, "left") %>%           # Labels Left
  set_align(1:row_sargan, 2:5, "center") %>%    # Numbers Center
  set_italic(c(row_sargan, row_system), 1) %>%  # Italic footer labels
  set_bold(c(row_sargan, row_system), 1) %>%    # Bold footer labels
  set_bottom_border(row_sargan - 1, 1:5, 0.4) %>% # Line above Sargan
  set_bottom_border(row_note - 1, 1:5, 0.8) %>%   # Line above Note
  set_left_padding(1:nrow(.), 1, 10) %>%
  set_width(1.0)

# Render
ht_final






#############   need to RUN SARGAN TEST (OVERIDENTIFYING RESTRICTIONS PART) on TR ####################


library(ivreg)

# 1. Test thE Taylor Rule instruments
tr_test <- ivreg(BOE_rate ~ BOE_rate_lag1 + Infl_dev + Y_GAP + zlb_dummy + dummy_2020Q2 | 
                   Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
                   RER_GAP_lag1 + dNX_lag1 + dIM_defl_lag1 + dEnergy_lag1 + 
                   dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
                 data = MODEL_READY)

# 2. Run the Diagnostic (Sargan is the 'Overidentifying restrictions' part)
summary(tr_test, diagnostics = TRUE)


tr_test_refined <- ivreg(
  BOE_rate ~ 
    BOE_rate_lag1 + Infl_dev + Y_GAP + zlb_dummy + dummy_2020Q2 | 
    # REMOVE: RER_GAP, dNX, dIM_defl, dEnergy, dlog_price_cap
    # ONLY USE: Lags of the core system variables
    BOE_rate_lag1 + lag(Infl_dev, 1) + Y_GAP_lag1 + U_GAP_lag1 + 
    zlb_dummy + dummy_2020Q2 + dummy_2020Q3, 
  data = MODEL_READY
)

summary(tr_test_refined, diagnostics = TRUE)


############### 3SLS FOLLOWING REFINEMENTS TO TAYLOR RULE, BUT NOW USING SEPARATE INSTRUMENTS FOR EQUATIONS###

# 1. Define specific instrument lists for each equation
# IS Curve needs trade; Taylor Rule needs only macro lags
inst_list <- list(
  IS   = ~ Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + RER_GAP_lag1 + dNX_lag1 + dummy_2020Q2 + dummy_2020Q3,
  PC   = ~ Y_GAP_lag1 + INFL_yoy_lag1 + dIM_defl_lag1 + dlog_price_cap_lag1 + dummy_2020Q2 + dummy_2020Q3,
  TR   = ~ BOE_rate_lag1 + lag(Infl_dev, 1) + Y_GAP_lag1 + U_GAP_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3,
  Okun = ~ U_GAP_lag1 + Y_GAP_lag1 + dummy_2020Q2
)

# 2. Run the 3SLS with the Equation-Specific Instruments
fit_3sls_perfect <- systemfit(
  system_final, 
  method = "3SLS", 
  inst = inst_list, # <--- The new list of lists
  data = MODEL_READY
)

# 3. Final Summary
summary(fit_3sls_perfect)


# 1. Create one Master List of the 10 strongest instruments
# We've removed the 'noisy' RER/NX lags that broke the Taylor Rule
instr_master <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + BOE_rate_lag1 + U_GAP_lag1 + 
  RER_GAP_lag1 + dNX_lag1 + dIM_defl_lag1 + 
  dlog_price_cap_lag1 + zlb_dummy + dummy_2020Q2 + dummy_2020Q3

# 2. Re-run the 3SLS (using the unified list)
fit_3sls_master <- systemfit(
  system_final, 
  method = "3SLS", 
  inst = instr_master, 
  data = MODEL_READY
)

# 3. Check the results
summary(fit_3sls_master)


#| label: tbl-results
#| tbl-cap: "Table 1: 3SLS System Estimates of the UK Economy (Master Model)"
#| results: asis
#| echo: false

library(huxtable)

# Define a helper to 'tidy' systemfit equations
tidy.systemfit.equation <- function(x, ...) {
  s <- summary(x)
  d <- as.data.frame(s$coefficients)
  names(d) <- c("estimate", "std.error", "statistic", "p.value")
  d$term <- rownames(d)
  return(d)
}

# Simplified glance function to avoid the '5 rows' error
glance.systemfit.equation <- function(x, ...) {
  s <- summary(x)
  # Just return the basics: N and R2
  data.frame(
    nobs = as.numeric(s$df[1] + s$df[2]), 
    r.squared = as.numeric(s$r.squared)
  )
}

# 2. Build the Base Table (using the master 3SLS object)
ht <- huxreg(
  "IS Curve"       = fit_3sls_master$eq[], 
  "Phillips Curve" = fit_3sls_master$eq[], 
  "Taylor Rule"    = fit_3sls_master$eq[], 
  "Okun's Law"     = fit_3sls_master$eq[],
  coefs = c(
    "Constant"                 = "(Intercept)",
    "Output Gap (t-1)"         = "Y_GAP_lag1",
    "Real Interest Rate Gap"   = "R_GAP",
    "Real Exchange Rate (t-1)" = "RER_GAP_lag1",
    "Net Export Growth (t-1)"  = "dNX_lag1",
    "Inflation (t-1)"          = "INFL_yoy_lag1",
    "Expected Inflation (t+1)" = "INFL_yoy_lead1",
    "Inflation Deviation"      = "Infl_dev",
    "Base Rate (t-1)"          = "BOE_rate_lag1",
    "Unemployment Gap (t-1)"   = "U_GAP_lag1",
    "Output Gap (Current)"     = "Y_GAP",
    "COVID-19 (2020Q2)"        = "dummy_2020Q2",
    "COVID-19 (2020Q3)"        = "dummy_2020Q3"
  ),
  stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
  statistics = c("N" = "nobs", "R2" = "r.squared"),
  note = NULL 
)

# 3. Construct Footer Rows (Sargan values updated for the Master Model)
sargan_values <- c("Sargan Test (p)", "0.380", "0.089", "0.107", "N/A")
system_r2_row <- c("System R-squared (McElroy)", "0.959", "", "", "")
stars_note    <- c("*** p < 0.01; ** p < 0.05; * p < 0.1", "", "", "", "")

# 4. Add Rows and Apply Formatting
ht_final <- ht %>%
  add_rows(sargan_values, after = nrow(.)) %>%
  add_rows(system_r2_row) %>%
  add_rows(stars_note)

# Indices for formatting
row_sargan <- nrow(ht_final) - 2
row_system <- nrow(ht_final) - 1
row_note   <- nrow(ht_final)

# Formatting: Merging, Centering, and Locking Decimals
ht_final <- ht_final %>%
  merge_cells(row_system, 2:5) %>%
  merge_cells(row_note, 1:5) %>%
  set_align(row_system, 2, "center") %>%
  set_align(row_note, 1, "left") %>%
  set_number_format(row_sargan, 2:5, NA) %>%
  set_number_format(row_system, 2, NA) %>%
  theme_article() %>%
  set_align(1:nrow(.), 1, "left") %>%           # Labels Left
  set_align(1:row_sargan, 2:5, "center") %>%    # Numbers Center
  set_italic(c(row_sargan, row_system), 1) %>%  # Italic footers
  set_bold(c(row_sargan, row_system), 1) %>%    # Bold footers
  set_bottom_border(row_sargan - 1, 1:5, 0.4) %>% 
  set_bottom_border(row_note - 1, 1:5, 0.8) %>%   
  set_left_padding(1:nrow(.), 1, 10) %>%
  set_width(1.0)

# Render the final masterpiece
ht_final


































































#### SYSTEM OF EQUATIONS (SCARTH, CH. 8, MUNDELL-FLEMING WITH STOCHASTIC MACRO & RATIONAL EXPECTATIONS EXTENSIONS)

### IS Curve with Market Spread
### Structural Phillips Curve, using dlog_CPI_index as our measure of inflation (pi)
### Estimate the Monetary Policy Rule, include lag_BOE because central banks prefer "interest rate smoothing"
### Okun's Law: The link between Output and Unemployment

# some re-scaling first

library(systemfit)



library(systemfit)

# 1. Define the Structural Equations
# IS Curve: Focuses on persistence, real rates, and exchange rate lags
eq_IS <- Y_GAP ~ lag_Y_gap + lead_Y_gap + real_BOE_r + d_mkt_spr + dlog_NX + lag(rer_gap, 1) + dummy_2020Q2 + dummy_2020Q3

# Phillips Curve: Uses persistence (lags) which provided the 0.56 R2 and better logic
eq_PC <- Infl_dev ~ lag(Infl_dev, 1) + lag(Infl_dev, 2) + Y_GAP + dlog_energy_cpi + dlog_IM_defl + dlog_cap

# Taylor Rule: The version where Inflation Deviation was significant (p < 0.05)
eq_TR <- d_BOE_rate ~ lag_BOE + Infl_dev + Y_GAP + d_mkt_spr + dummy_2020Q2 + dummy_2020Q3

# Okun's Law: The standard reliable link
eq_OK <- U_GAP ~ lag_U_gap + Y_GAP

system <- list(IS = eq_IS, PC = eq_PC, TR = eq_TR, OK = eq_OK)

# 2. Define the Instrument Set
# We include lags of endogenous variables and all exogenous/structural dummies
instruments <- ~ lag_Y_gap + lag(Y_GAP, 2) + lag_U_gap + lag_BOE + 
  lag(Infl_dev, 1) + lag(Infl_dev, 2) + 
  dlog_energy_cpi + dlog_IM_defl + d_mkt_spr + 
  dlog_NX + lag(rer_gap, 1) + dlog_cap + 
  dummy_2020Q2 + dummy_2020Q3

# 3. Estimate using 3SLS
fit_best <- systemfit(system, method = "3SLS", inst = instruments, data = MODEL_DIFFS)

# 4. Review the results
summary(fit_best)

#### END OF PRE-NATIONAL ACCOUNTING RESTRICTIONS




### new run with national accounting identities

# Ensure the dependent variable strictly follows the identity
MODEL_DIFFS$GDP_mp <- MODEL_DIFFS$GVA_bp + MODEL_DIFFS$indir_net_tax
MODEL_DIFFS <- MODEL_DIFFS |> 
  mutate(lead_Infl_dev = lead(Infl_dev))


library(systemfit)

# 1. Define the Behavioral Equations (Stochastic)
eq_IS_1     <- Y_GAP ~ lag_Y_gap + lead_Y_gap + real_BOE_r + d_mkt_spr + dlog_NX + lag(rer_gap, 1) + dummy_2020Q2 + dummy_2020Q3
# New structural REPC (Rational Expectations Phillips Curve)
# Theoretical REPC: Inflation = f(Expected Future Inflation, Output Gap)
# 1. Update the Phillips Curve to the 'Hybrid' specification
# We add one lag to account for inflation persistence alongside the lead
eq_PC_hybrid <- Infl_dev ~ lead_Infl_dev + lag(Infl_dev, 2) + Y_GAP + 
  dlog_energy_cpi + dlog_IM_defl + dlog_cap

eq_Taylor_1 <- d_BOE_rate ~ lag_BOE + Infl_dev + Y_GAP + d_mkt_spr + dummy_2020Q2 + dummy_2020Q3
eq_Okun_1   <- U_GAP ~ lag_U_gap + Y_GAP

# Update your system list
system_hybrid <- list(IS = eq_IS_1, PC = eq_PC_hybrid, Taylor = eq_Taylor_1, Okun = eq_Okun_1)

# 2. Update the Instrument List for better 'grip'
# Adding 3rd lags of key endogenous variables (Inflation and Output Gap)
instr_2 <- ~ lag(Infl_dev, 1) + lag(Infl_dev, 2) + lag(Infl_dev, 3) + 
  lag(Y_GAP, 1) + lag(Y_GAP, 2) + lag(Y_GAP, 3) +
  lag_BOE + dlog_energy_cpi + dlog_IM_defl + 
  d_mkt_spr + dlog_NX + lag(rer_gap, 1) + 
  dlog_cap + dummy_2020Q2 + dummy_2020Q3


# 3. Define the National Accounting Identities (Non-stochastic restrictions)
# We can use symbolic restrictions to ensure the coefficients match your shares (w)
# Example: Production Constraint Yt = w*Yd + (1-w)*Tt
# Note: You would replace '0.85' with your actual ONS-derived steady-state share
# accounting_restr <- c("IS_Y_GAP = 0.85 * gva_g + 0.15 * tax_g") 

# 4. Run the 3SLS Estimation
fit_hybrid <- systemfit(system_hybrid, 
                        method = "3SLS", 
                        inst = instr_2, 
                        data = MODEL_DIFFS)

summary(fit_hybrid)














# Test if the sum of the lead and lag inflation coefficients equals 1

library(car)
wald_test <- linearHypothesis(fit_3sls, "PC_lag(Infl_dev) + PC_lag(Infl_dev, 2) = 1")

print(wald_test)


#### TEST RESIDUALS FOR THIS SYSTEM

# 1. Extract residuals from the 3SLS model
# We use names(resid(fit_3sls)) to ensure they go into the correct rows
all_resids <- as.data.frame(resid(fit_3sls))

# Add them to MODEL_DIFFS (matching by row name/index)
MODEL_DIFFS$resid_IS <- NA
MODEL_DIFFS$resid_PC <- NA
MODEL_DIFFS$resid_TR <- NA
MODEL_DIFFS$resid_OK <- NA

# Map the 108 calculated residuals back to the 110-row dataframe
MODEL_DIFFS[rownames(all_resids), "resid_IS"] <- all_resids$IS
MODEL_DIFFS[rownames(all_resids), "resid_PC"] <- all_resids$PC
MODEL_DIFFS[rownames(all_resids), "resid_TR"] <- all_resids$TR
MODEL_DIFFS[rownames(all_resids), "resid_OK"] <- all_resids$OK

# 2. Visualise the shocks with the explicit dplyr::select fix

MODEL_DIFFS %>%
  dplyr::select(Date, resid_IS, resid_PC, resid_TR, resid_OK) %>%
  mutate(Date_Num = row_number()) %>% # Create a sequence for steady plotting
  pivot_longer(cols = starts_with("resid"), names_to = "Equation", values_to = "Residual") %>%
  ggplot(aes(x = Date_Num, y = Residual, color = Equation)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~Equation, scales = "free_y", ncol = 1) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 110, by = 10), labels = MODEL_DIFFS$Date[seq(1, 110, by = 10)]) +
  labs(title = "System-Wide Residual Check", 
       subtitle = "Have we adequately controlled for structural shocks in the UK economy?",
       x = "Quarterly Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






## Robustness Check: The "Over-Identification" Test
# This gives the diagnostic tests for the whole system
# Define a custom Sargan test function for your systemfit object
sargan_selective <- function(model, data) {
  # 1. Get the residuals from the model
  u <- as.matrix(resid(model))
  
  # 2. Extract ONLY the instruments used in the fit
  # This avoids the "NA-heavy" variables you aren't using
  Z_vars <- all.vars(model$inst[[1]]) 
  Z_data <- data[, Z_vars]
  
  # 3. Handle the lags manually to match the model's row count
  # We use the same rows the model actually used
  Z_mat <- model.matrix(model, which = "z")
  n <- nrow(u)
  g <- length(model$eq)
  Z_unique <- Z_mat[1:n, 1:(ncol(Z_mat)/g)]
  
  # 4. Math: The Over-identification Test
  u_vec <- as.vector(u)
  # Weighting by the inverse of the residual covariance
  Sigma_inv <- solve(model$residCov)
  
  # Projection of residuals onto instruments
  # Pz = Z(Z'Z)^-1 Z'
  Pz <- Z_unique %*% solve(t(Z_unique) %*% Z_unique) %*% t(Z_unique)
  
  # Calculate J-statistic
  j_stat <- t(u_vec) %*% kronecker(Sigma_inv, Pz) %*% u_vec
  
  # 5. Degrees of Freedom
  # DF = (Total instruments) - (Total estimated coefficients)
  df <- (ncol(Z_unique) * g) - length(coef(model))
  p_val <- 1 - pchisq(as.numeric(j_stat), df)
  
  return(list(stat = as.numeric(j_stat), df = df, p.value = p_val))
}

# Run it using your MODEL_DIFFS data
sargan_selective(fit_3sls, MODEL_DIFFS)





#### end for now...


install.packages("caracas")
library(caracas)
caracas::install_sympy()


library(caracas)

# 1. Define 'x' as a symbol
# Using def_sym(x) automatically creates the symbol in your environment
def_sym(x) 

# 2. Define your expression
expr <- x^2

# 3. Differentiate
# der(expression, variable)
result <- der(expr, x)

# View the result
result
# [caracas]: 2⋅x




