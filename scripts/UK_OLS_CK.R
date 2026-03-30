############## IS RELATION #####################################################

IS_fit <- lm(
  Y_GAP ~ 
    Y_GAP_lag1 + Y_GAP_lead1 + 
    R_GAP + i_spr_GAP + zlb_interaction + 
    RER_GAP_lag1 + RER_GAP_lag2 + # Test both
    dNX_lag1 + dNX_lag2 +         # Test both
    dummy_2020Q2 + dummy_2020Q3, 
  data = MODEL_READY
)

summary(IS_fit)



# CORRELATION CHECKS

# This should give a strong negative correlation (e.g., -0.5 to -0.8)
cor(MODEL_READY$Y_GAP, MODEL_READY$U_GAP, use = "complete.obs")

cor(MODEL_READY$dNX_lag1, MODEL_READY$RER_GAP_lag1, use = "complete.obs")



# DIAGNOSTICS

# 1. Generate the Diagnostic Plots
# 'which = 1' focuses on the Residuals vs Fitted plot
plot(IS_fit, which = 1, col = "royalblue", pch = 20, 
     main = "Residuals vs Fitted")

# 2. Check for Normality (Optional but recommended)
# This confirms if your errors follow a bell curve
plot(IS_fit, which = 2, col = "darkgreen", pch = 20)

# Final check for 'clean' residuals
car::durbinWatsonTest(IS_fit)

################################################################################

################ PHILLIPS CURVE RELATION #######################################


## this is best performing model so far

PC_fit <- lm(
  INFL_yoy ~ 
    INFL_yoy_lag1 +   # Persistence
    INFL_yoy_lead1 +  # Expectations (Forward-looking)
    Y_GAP_lag1 +      # The "Slack" term (Domestic demand)
    dlog_IM_defl_lag1 +   # Imported Inflation (OBR focus)
    dlog_energy_cpi_lag1 +    # Energy shock (OBR focus)
    dummy_2020Q2 +    # COVID control
    dummy_2020Q3, 
  data = MODEL_READY
)

summary(PC_fit)



## this alternative was tested but adding wholesale electricity & ofgem price cap doesn't change anything

PC_fit_energy <- lm(
  INFL_yoy ~ 
    INFL_yoy_lag1 + INFL_yoy_lead1 + 
    Y_GAP_lag1 + 
    dlog_IM_defl_lag1 + 
    dlog_energy_cpi_lag1 + 
    dummy_2020Q2 + dummy_2020Q3, 
  data = MODEL_READY
)

summary(PC_fit_energy)

################################################################################


#####################  TAYLOR RULE  ############################################

# The Taylor Rule: BoE Reaction Function
TR_fit <- lm(
  i_GAP ~ 
    lag(i_GAP,1) +   # Interest rate smoothing
    Infl_dev +        # Reaction to inflation gap
    Y_GAP +           # Reaction to output gap
    dummy_2020Q2,     # Emergency COVID cut
  data = MODEL_READY
)

summary(TR_fit)

################################################################################


####################  OKUN'S LAW ###############################################

# Okun's Law: Linking Growth to Jobs
OKUN_fit <- lm(
  U_GAP ~ 
    U_GAP_lag1 +  # Persistence (Labour market stickiness)
    Y_GAP +       # The Output Gap effect
    dummy_2020Q2, # Furlough/COVID shock
  data = MODEL_READY
)

summary(OKUN_fit)



################################################################################

################# UNCOVERED INTEREST PARITY#####################################

UIP_fit <- lm(
  RER_GAP ~ 
    lag(RER_GAP, 1) + 
    (R_GAP - R_GAP_for),
  data = MODEL_READY
)

summary(UIP_fit)