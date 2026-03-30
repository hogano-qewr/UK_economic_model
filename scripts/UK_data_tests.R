# 1. Calculate Correlation
# Use 'use = "complete.obs"' to handle any missing values (NAs)
cor_value <- cor(MODEL_READY$CPI_index, MODEL_READY$UK_CPI_imf, use = "complete.obs")
print(paste("Correlation:", round(cor_value, 4)))

# 2. Rebase to compare the "Wedge"
# Picking the first observation as the common base (100)
MODEL_READY$ONS_rebased <- (MODEL_READY$CPI_index / MODEL_READY$CPI_index[1]) * 100
MODEL_READY$IMF_rebased <- (MODEL_READY$UK_CPI_imf / MODEL_READY$UK_CPI_imf[1]) * 100

# 3. Calculate the difference (The potential source of autocorrelation)
MODEL_READY$wedge <- MODEL_READY$ONS_rebased - MODEL_READY$IMF_rebased

# 4. Quick Plot to see if the error is persistent
plot(MODEL_READY$wedge, type="l", col="blue", 
     main="Wedge between Rebased ONS and IMF CPI",
     ylab="Percentage Point Difference", xlab="Time")
abline(h=0, lty=2)




### QUICK TEST RUN ON REVISED DATA

library(systemfit)

# 1. Define the Structural Equations
# We use lags (e.g., Y_GAP_lag1) to soak up autocorrelation
eq_output <- Y_GAP ~ lag(Y_GAP, 1) + re_real_BOE_r + RER + dNX
eq_infl   <- Infl_dev ~ lag(Infl_dev, 1) + PROD_GAP + WAGE_GROWTH + Energy_infl
eq_rates  <- BOE_rate ~ lag(BOE_rate, 1) + Infl_dev + Y_GAP + zlb_interaction
eq_unemp  <- U_GAP ~ lag(U_GAP, 1) + Y_GAP
eq_wages  <- WAGE_GROWTH ~ lag(WAGE_GROWTH, 1) + Infl_exp + PROD_GAP
eq_rer    <- RER ~ lag(RER, 1) + rate_diffl + dNX
eq_trade  <- dNX ~ lag(dNX, 1) + RER + Y_GAP

system_eqs <- list(
  output = eq_output, infl = eq_infl, rates = eq_rates, 
  unemp = eq_unemp, wages = eq_wages, rer = eq_rer, trade = eq_trade
)

# 2. Define Instruments
# In 3SLS, you need exogenous variables and lags of endogenous ones
instrum <- ~ Foreign_i + Foreign_CPI + Energy_infl + IMP_infl + 
  lag(Y_GAP, 1) + lag(Infl_dev, 1) + lag(BOE_rate, 1) + 
  lag(U_GAP, 1) + lag(WAGE_GROWTH, 1) + lag(RER, 1) + lag(dNX, 1)

# 3. Run the 3SLS Model
fit_3sls <- systemfit(system_eqs, method = "3SLS", inst = instrum, data = MODEL_READY)

summary(fit_3sls)
