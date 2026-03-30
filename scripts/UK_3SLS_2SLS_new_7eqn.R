

################################################################################


library(ggplot2)
ggplot(MODEL_READY, aes(x = U_GAP, y = WAGE_GAP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wage Phillips Curve: Unemployment vs Wage Pressure",
       x = "Unemployment Gap (Slack)",
       y = "Wage Gap (Pressure)") +
  theme_minimal()

################################################################################

######  NEW STRUCTURAL EQUATIONS ###############################################

library(systemfit)

# 1. Define the Structural Equations
eq_IS    <- Y_GAP ~ 
  Y_GAP_lag1 +  
  R_GAP + i_spr_GAP +  
  RER_GAP_lag1 + # Test both
  dNX_lag1 +        # Test both
  dummy_2020Q2 + dummy_2020Q3
eq_PC    <- INFL_yoy ~ 
  INFL_yoy_lag1 +   # Persistence
  INFL_yoy_lead1 +  # Expectations (Forward-looking)
  WAGE_GAP +
  RER_GAP_lag1+
  dlog_IM_defl_lag1 +   # Imported Inflation (OBR focus)
  dlog_energy_cpi_lag1 +    # Energy shock (OBR focus)
  dummy_2020Q2 +    # COVID control
  dummy_2020Q3
eq_TR    <- i_GAP ~ 
  lag(i_GAP, 1) +   # Interest rate smoothing
  Infl_dev +        # Reaction to inflation gap
  Y_GAP +           # Reaction to output gap
  dummy_2020Q2
eq_Okun <- U_GAP ~ 
  U_GAP_lag1 +  # Persistence (Labour market stickiness)
  Y_GAP +       # The Output Gap effect
  dummy_2020Q2
eq_UIP <- RER_GAP ~ 
  RER_GAP_lag1 + 
  lag(R_GAP_diff, 1)
eq_WPC <- WAGE_GAP ~ 
  lag(WAGE_GAP,1) + 
  INFL_yoy_lead1 + 
  U_GAP_lag1 + 
  PROD_GAP
eq_Okun_Prod <- PROD_GAP ~ 
  lag(PROD_GAP, 1) + 
  Y_GAP + 
  dummy_2020Q2

system7 <- list(IS = eq_IS, 
               PC = eq_PC, 
               TR = eq_TR, 
               Okun = eq_Okun, 
               UIP = eq_UIP, 
               WPC = eq_WPC,
               OkunProd = eq_Okun_Prod)

# 2. Define the Instruments
# These must be EXOGENOUS (lags, dummies, or external shocks)
instr_7eqn <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + lag(WAGE_GAP, 1) + lag(PROD_GAP,1) + lag(i_GAP, 1) + U_GAP_lag1 + 
  RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + 
  dlog_IM_defl_lag1 + dlog_energy_cpi_lag1 + dummy_2020Q2 + dummy_2020Q3

fit_3sls_7eqn <- systemfit(system7, method = "3SLS", inst = instr_7eqn, data = MODEL_READY)

summary(fit_3sls_7eqn)



library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df1 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_7eqn)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df1, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()


library(car)

# Testing the joint significance of Supply/Cost shocks in the PC equation
linearHypothesis(fit_3sls_7eqn, 
                 c("PC_WAGE_GAP = 0", 
                   "PC_dlog_IM_defl_lag1 = 0", 
                   "PC_dlog_energy_cpi_lag1 = 0"))

### consistent with expectations story but unsatisfactory

################################################################################


##### RE-RUN SYSTEM WITHOUT INFLATION EXPECTATIONS#############################

# 1. Define the Structural Equations
eq_IS    <- Y_GAP ~ 
  Y_GAP_lag1 +  
  R_GAP + i_spr_GAP +  
  RER_GAP_lag1 + # Test both
  dNX_lag1 +        # Test both
  dummy_2020Q2 + dummy_2020Q3
eq_PC1    <- INFL_yoy ~ 
  INFL_yoy_lag1 +   # Persistence
  WAGE_GAP +
  RER_GAP_lag1+
  dlog_IM_defl_lag1 +   # Imported Inflation (OBR focus)
  dlog_energy_cpi_lag1 +    # Energy shock (OBR focus)
  dummy_2020Q2 +    # COVID control
  dummy_2020Q3
eq_TR    <- i_GAP ~ 
  lag(i_GAP, 1) +   # Interest rate smoothing
  Infl_dev +        # Reaction to inflation gap
  Y_GAP +           # Reaction to output gap
  dummy_2020Q2
eq_Okun <- U_GAP ~ 
  U_GAP_lag1 +  # Persistence (Labour market stickiness)
  Y_GAP +       # The Output Gap effect
  dummy_2020Q2
eq_UIP <- RER_GAP ~ 
  RER_GAP_lag1 + 
  lag(R_GAP_diff, 1)
eq_WPC1 <- WAGE_GAP ~ 
  lag(WAGE_GAP,1) + 
  INFL_yoy_lag1 + 
  U_GAP_lag1 + 
  PROD_GAP
eq_Okun_Prod <- PROD_GAP ~ 
  lag(PROD_GAP, 1) + 
  Y_GAP + 
  dummy_2020Q2

system71 <- list(IS = eq_IS, 
                PC = eq_PC1, 
                TR = eq_TR, 
                Okun = eq_Okun, 
                UIP = eq_UIP, 
                WPC = eq_WPC1,
                OkunProd = eq_Okun_Prod)

# 2. Define the Instruments
# These must be EXOGENOUS (lags, dummies, or external shocks)
instr_7eqn1 <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + lag(WAGE_GAP, 1) + lag(PROD_GAP,1) + lag(i_GAP, 1) + U_GAP_lag1 + 
  RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + 
  dlog_IM_defl_lag1 + dlog_energy_cpi_lag1 + dummy_2020Q2 + dummy_2020Q3

fit_3sls_7eqn1 <- systemfit(system71, method = "3SLS", inst = instr_7eqn1, data = MODEL_READY)

summary(fit_3sls_7eqn1)



library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df71 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_7eqn1)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df71, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()


library(ggplot2)
library(zoo)

plot_pc_df <- data.frame(
  Date = as.Date(as.yearqtr(MODEL_READY$date)),
  Actual = as.numeric(MODEL_READY$INFL_yoy),
  Fitted = as.numeric(fitted(fit_3sls_7eqn1)$PC) # Note: 'PC' must match the name in your list
)

ggplot(plot_pc_df, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "UK Inflation: Actual vs. 3SLS System Fit",
       subtitle = "Model captures 2022-2023 surge via Wages and Energy",
       x = "Year",
       y = "CPI Inflation (YoY)") +
  theme_minimal()


#### TEST THIS 3SLS SYSTEM - HAUSMAN ####################################################### 

# 1. Estimate the system using 2SLS
fit_2sls_7eqn1 <- systemfit(system71, method = "2SLS", inst = instr_7eqn1, data = MODEL_READY)

# 2. Run the Hausman test by providing BOTH objects
hausman_results <- hausman.systemfit(fit_2sls_7eqn1, fit_3sls_7eqn1)

# 3. View the results
print(hausman_results)

################# THE 3SLS SYSTEM FAILED THE HAUSMAN TEST ####################################

# Estimate using 2SLS (Robust to the Hausman failure)
fit_2sls_7eqn1 <- systemfit(system71, method = "2SLS", inst = instr_7eqn1, data = MODEL_READY)

summary(fit_2sls_7eqn1)


# This extracts the first-stage diagnostics for each endogenous variable
summary(fit_2sls_7eqn1, diagnostics = TRUE)


library(car)

# Testing the joint significance of Supply/Cost shocks in the PC equation
linearHypothesis(fit_2sls_7eqn1, 
                 c("PC_WAGE_GAP = 0", 
                   "PC_dlog_IM_defl_lag1 = 0", 
                   "PC_dlog_energy_cpi_lag1 = 0"))
########### THIS PRODUCED A STRONG RESULT

# Testing the joint significance of External Sector variables
linearHypothesis(fit_2sls_7eqn1, 
                 c("IS_RER_GAP_lag1 = 0", 
                   "IS_dNX_lag1 = 0", 
                   "PC_RER_GAP_lag1 = 0", 
                   "PC_dlog_IM_defl_lag1 = 0"))



##### RE-RUN SYSTEM WITH REAL EXCHANGE RATE IN TAYLOR RULE #############################
##### ALSO ADDING PROD_GAP TO PHILLIPS CURVE ###########################################

# 1. Define the Structural Equations
eq_IS    <- Y_GAP ~ 
  Y_GAP_lag1 +  
  R_GAP + i_spr_GAP +  
  RER_GAP_lag1 + # Test both
  dNX_lag1 +        # Test both
  dummy_2020Q2 + dummy_2020Q3
eq_PC1    <- INFL_yoy ~ 
  INFL_yoy_lag1 +   # Persistence
  WAGE_GAP +
  PROD_GAP +
  RER_GAP_lag1+
  dlog_IM_defl_lag1 +   # Imported Inflation (OBR focus)
  dlog_energy_cpi_lag1 +    # Energy shock (OBR focus)
  dummy_2020Q2 +    # COVID control
  dummy_2020Q3
eq_TR1    <- i_GAP ~ 
  lag(i_GAP, 1) +   # Interest rate smoothing
  Infl_dev +        # Reaction to inflation gap
  Y_GAP +           # Reaction to output gap
  RER_GAP_lag1 +
  dummy_2020Q2
eq_Okun <- U_GAP ~ 
  U_GAP_lag1 +  # Persistence (Labour market stickiness)
  Y_GAP +       # The Output Gap effect
  dummy_2020Q2
eq_UIP <- RER_GAP ~ 
  RER_GAP_lag1 + 
  lag(R_GAP_diff, 1)
eq_WPC1 <- WAGE_GAP ~ 
  lag(WAGE_GAP,1) + 
  INFL_yoy_lag1 + 
  U_GAP_lag1 + 
  PROD_GAP
eq_Okun_Prod <- PROD_GAP ~ 
  lag(PROD_GAP, 1) + 
  Y_GAP + 
  dummy_2020Q2

system72 <- list(IS = eq_IS, 
                 PC = eq_PC1, 
                 TR = eq_TR1, 
                 Okun = eq_Okun, 
                 UIP = eq_UIP, 
                 WPC = eq_WPC1,
                 OkunProd = eq_Okun_Prod)

# 2. Define the Instruments
# These must be EXOGENOUS (lags, dummies, or external shocks)
instr_7eqn2 <- ~ Y_GAP_lag1 + INFL_yoy_lag1 + lag(WAGE_GAP, 1) + lag(PROD_GAP,1) + lag(i_GAP, 1) + U_GAP_lag1 + 
  RER_GAP_lag1 + RER_GAP_lag2 + dNX_lag1 + dNX_lag2 + 
  dlog_IM_defl_lag1 + dlog_energy_cpi_lag1 + dummy_2020Q2 + dummy_2020Q3

fit_3sls_7eqn2 <- systemfit(system72, method = "3SLS", inst = instr_7eqn2, data = MODEL_READY)

summary(fit_3sls_7eqn2)

#### TEST THIS 3SLS SYSTEM - HAUSMAN ####################################################### 

# 1. Estimate the system using 2SLS
fit_2sls_7eqn2 <- systemfit(system72, method = "2SLS", inst = instr_7eqn2, data = MODEL_READY)

# 2. Run the Hausman test by providing BOTH objects
hausman_results <- hausman.systemfit(fit_2sls_7eqn2, fit_3sls_7eqn2)

# 3. View the results
print(hausman_results)

#################  FAILED to REJECT THE HAUSMAN TEST (p=1.0) ####################################

# Testing the joint significance of External Sector variables
linearHypothesis(fit_3sls_7eqn2, 
                 c("IS_RER_GAP_lag1 = 0", 
                   "IS_dNX_lag1 = 0", 
                   "PC_RER_GAP_lag1 = 0", 
                   "PC_dlog_IM_defl_lag1 = 0"))
# Testing the joint significance of Internal Supply (Wages + Productivity) in the PC
linearHypothesis(fit_3sls_7eqn2, 
                 c("PC_WAGE_GAP = 0", 
                   "PC_PROD_GAP = 0"))



##### plot the 3SLS results

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df72 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_7eqn2)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df72, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()


plot_pc_df <- data.frame(
  Date = as.Date(as.yearqtr(MODEL_READY$date)),
  Actual = as.numeric(MODEL_READY$INFL_yoy),
  Fitted = as.numeric(fitted(fit_3sls_7eqn2)$PC) # Note: 'PC' must match the name in your list
)

ggplot(plot_pc_df, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1, linetype = "dashed") +
  labs(title = "UK Inflation: Actual vs. 3SLS System Fit",
       subtitle = "Model captures 2022-2023 surge via Wages and Energy",
       x = "Year",
       y = "CPI Inflation (YoY)") +
  theme_minimal()


### test output gap in Taylor Rule - WALD TEST #################################
# Testing if Y_GAP can be removed from the Taylor Rule (Equation 3)
linearHypothesis(fit_3sls_7eqn2, "TR_Y_GAP = 0")




############### 




