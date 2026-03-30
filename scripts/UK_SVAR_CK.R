library(vars)
library(tidyverse)

# 1. Define the Endogenous Variables (The Internal System)
svar_data <- MODEL_READY %>%
  select(
    dlog_energy_cpi,   # 1. Supply Shock
    Y_GAP,             # 2. Activity
    U_GAP,             # 3. Labour Market
    dlog_CPI_index,    # 4. Prices
    R_GAP,             # 5. Monetary Policy
    i_spr_GAP,         # 6. Financial Spread
    RER_GAP            # 7. Exchange Rate
  )

# 2. Define the Exogenous Variables (External Drivers)
exog_data <- MODEL_READY %>%
  select(R_GAP_for, dlog_IM_defl)

# 3. Estimate the VAR
# We use p = 2 to capture enough momentum without exhausting degrees of freedom
var_model <- VAR(svar_data, p = 2, type = "const", exogen = exog_data)

# 4. Final Sanity Check: Stability
roots(var_model)





############################################


library(tidyverse)
library(reshape2) # To pivot the data
library(RColorBrewer)

# 1. Extract the FEVD data for the Output Gap
# (Make sure to use the exact name: 'output_gap_structural' or 'Y_GAP')
fevd_matrix <- fevd_res$Y_GAP 

# 2. Convert to a "Long" format for ggplot
fevd_df <- as.data.frame(fevd_matrix) %>%
  mutate(Horizon = 1:n()) %>%
  pivot_longer(-Horizon, names_to = "Shock", values_to = "Contribution")

# 3. Create the Stacked Bar Chart
ggplot(fevd_df, aes(x = factor(Horizon), y = Contribution, fill = Shock)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Set3") + # Professional color palette
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Variance Decomposition: What Drives the UK Output Gap?",
       subtitle = "Percentage of variance explained by each structural shock (1997-2024)",
       x = "Quarters Ahead",
       y = "Contribution (%)",
       fill = "Type of Shock") +
  theme_minimal() +
  theme(legend.position = "right")


# 1. Extract the FEVD data for inflation
# (Make sure to use the exact name: dlog_CPI_index)
fevd_matrix <- fevd_res$dlog_CPI_index 

# 2. Convert to a "Long" format for ggplot
fevd_df <- as.data.frame(fevd_matrix) %>%
  mutate(Horizon = 1:n()) %>%
  pivot_longer(-Horizon, names_to = "Shock", values_to = "Contribution")

# 3. Create the Stacked Bar Chart
ggplot(fevd_df, aes(x = factor(Horizon), y = Contribution, fill = Shock)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_brewer(palette = "Set3") + # Professional color palette
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Variance Decomposition: What Drives UK Inflation?",
       subtitle = "Percentage of variance explained by each structural shock (1997-2024)",
       x = "Quarters Ahead",
       y = "Contribution (%)",
       fill = "Type of Shock") +
  theme_minimal() +
  theme(legend.position = "right")

####### HISTORICAL DECOMPOSITION OF INFLATION SHOCKS ###########################

# 1. Get the residuals and the Cholesky decomposition (P matrix)
resids <- as.matrix(residuals(var_model))
P <- t(chol(cov(resids))) # The Lower Triangular Matrix

# 2. Convert reduced-form shocks to structural shocks (Corrected operator)
# Structural Shocks = Inverse(P) %*% Reduced-Form Residuals
struct_shocks <- resids %*% t(solve(P))
colnames(struct_shocks) <- colnames(svar_data)

# 3. Get the VMA coefficients (the impulse responses)
n_obs <- nrow(resids)
Phi <- Phi(var_model, nstep = n_obs)

# 4. Decompose dlog_CPI_index (Variable #4 in your svar_data)
target_var_idx <- 4 
hd_matrix <- matrix(0, nrow = n_obs, ncol = ncol(svar_data))
colnames(hd_matrix) <- colnames(svar_data)

for(t in 1:n_obs) {
  for(j in 0:(t-1)) {
    # Matrix multiplication: Phi_j * P * struct_shock_{t-j}
    # This identifies the contribution of each structural shock to the variable
    impact <- Phi[,,j+1] %*% P %*% struct_shocks[t-j, ]
    hd_matrix[t, ] <- hd_matrix[t, ] + impact[target_var_idx, ]
  }
}

# 5. Tidy the data for plotting
hd_df <- as.data.frame(hd_matrix) %>%
  mutate(
    # Align dates: residuals start at (original_start + lags)
    Date = MODEL_READY$date[(nrow(MODEL_READY) - n_obs + 1):nrow(MODEL_READY)],
    Actual = resids[, target_var_idx]
  ) %>%
  pivot_longer(cols = -c(Date, Actual), names_to = "Shock", values_to = "Value")

ggplot(hd_df, aes(x = Date)) +
  geom_bar(aes(y = Value, fill = Shock), stat = "identity", alpha = 0.8) +
  geom_line(aes(y = Actual, group = 1), color = "black", linewidth = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Historical Decomposition: What caused UK Inflation?",
       subtitle = "Black line = Deviation from trend; Bars = Contribution of each shock",
       x = "Quarter", y = "Log Points", fill = "Shock Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Re-run the VAR with p = 4
var_model_p4 <- VAR(svar_data, p = 4, type = "const", exogen = exog_data)

# Check stability - with p=4, we now have 28 roots (7 variables * 4 lags)
# Ensure the largest is still < 1.0
max(roots(var_model_p4))

# 1. Setup
resids_p4 <- as.matrix(residuals(var_model_p4))
P_p4 <- t(chol(cov(resids_p4)))
struct_shocks_p4 <- resids_p4 %*% t(solve(P_p4))
colnames(struct_shocks_p4) <- colnames(svar_data)

n_obs_p4 <- nrow(resids_p4)
Phi_p4 <- Phi(var_model_p4, nstep = n_obs_p4)

# 2. Decompose Inflation (Variable #4)
target_var_idx <- 4 
hd_matrix_p4 <- matrix(0, nrow = n_obs_p4, ncol = ncol(svar_data))
colnames(hd_matrix_p4) <- colnames(svar_data)

for(t in 1:n_obs_p4) {
  for(j in 0:(t-1)) {
    impact <- Phi_p4[,,j+1] %*% P_p4 %*% struct_shocks_p4[t-j, ]
    hd_matrix_p4[t, ] <- hd_matrix_p4[t, ] + impact[target_var_idx, ]
  }
}

# 3. Align Dates and Plot
hd_df_p4 <- as.data.frame(hd_matrix_p4) %>%
  mutate(
    # Adjusted date indexing for 4 lags
    Date = MODEL_READY$date[(nrow(MODEL_READY) - n_obs_p4 + 1):nrow(MODEL_READY)],
    Actual = resids_p4[, target_var_idx]
  ) %>%
  pivot_longer(cols = -c(Date, Actual), names_to = "Shock", values_to = "Value")

ggplot(hd_df_p4, aes(x = Date)) +
  geom_bar(aes(y = Value, fill = Shock), stat = "identity", alpha = 0.8) +
  geom_line(aes(y = Actual, group = 1), color = "black", linewidth = 0.8) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(title = "Historical Decomposition (p=4): What caused UK Inflation?",
       subtitle = "With 4 lags, we are looking for more 'Green' and 'Orange' influence.",
       x = "", y = "Log Points", fill = "Shock Source") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1. Calculate the IRF for a Monetary Policy Shock (R_GAP)
# n.ahead = 20 quarters (5 years) to account for high persistence
# boot = TRUE creates the 95% confidence intervals (the "shaded" areas)
irf_policy <- irf(var_model_p4, 
                  impulse = "R_GAP", 
                  response = c("Y_GAP", "dlog_CPI_index", "RER_GAP"), 
                  n.ahead = 20, 
                  ortho = TRUE, 
                  boot = TRUE, 
                  runs = 100) # 100 runs is fast; use 500 for final paper quality

# 2. Plot the results
plot(irf_policy)

# CONCLUSION....MODEL WITH P=2 IS MORE STABLE / ROBUST

################################################################################






# A. The Monetary Policy Shock (A 1% hike in the UK Interest Rate Gap)
irf_monetary <- irf(var_model, impulse = "R_GAP", 
                    response = c("Y_GAP", "dlog_CPI_index", "RER_GAP"), 
                    n.ahead = 25, boot = TRUE, cumulative = FALSE)

# B. The Energy Supply Shock (A spike in the CPI Fuel Component)
# (Assuming your variable name is 'energy_cpi' or similar)
irf_energy <- irf(var_model, impulse = "dlog_energy_cpi", 
                  response = c("dlog_CPI_index", "Y_GAP"), 
                  n.ahead = 25, boot = TRUE)

# Plot them
plot(irf_monetary, main = "Shock to UK Interest Rates")
plot(irf_energy, main = "Shock to Energy Prices")

# Impact of a Financial Spread Shock (Credit Tightening)
irf_spread <- irf(var_model, impulse = "i_spr_GAP", 
                  response = c("Y_GAP", "dlog_CPI_index", "RER_GAP"), 
                  n.ahead = 20, boot = TRUE)

plot(irf_spread, main = "Shock to Gilt-Base Rate Spread")




