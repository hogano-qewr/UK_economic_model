# PROPOSED WORKFLOW FOR HYBRID MODEL

# SEE WORD NOTES "UK_model_HYBRID" for
#  STAGE 1: deciding what should be estimated vs. calibrated

#  STAGE 2: Rewrite equations in hybrid form

############# IS RELATION #####################################
# ESTIMATE
# Y_GAP_t = a1 * Y_GAP_{t-1} + a2 * ΔNX_t + a3 * RER_{t-1} + dummy terms + ε
# that implies this to start with...
IS_fit <- lm(Y_GAP ~ lag(Y_GAP, 1) + dNX + lag(RER, 1))   
# ... but dummies for structural breaks still to be added

# CALIBRATE (not sure what to with the code suggested here yet...)
# - φ_r * r_GAP_t    # real rate gap effect (calibrated)
# - φ_s * spr_GAP_t  # spread effect (calibrated)




############# PHILLIPS CURVE (INFLATION EQUATION) #############
# ESTIMATE
# Infl_dev_t = b1*Infl_dev_{t-1} + b2*Infl_dev_{t-2} + b3*IM_infl_GAP + b4*Energy_infl + dummies + ε

# CALIBRATE
# + κ_y * Y_GAP_t           # slope term (calibrated)
# + κ_p * PROD_GAP_t        # small role in UK data




############# TAYLOR RULE #####################################
# ESTIMATE
# i_t = ρ*i_{t-1} + ε_t

# CALIBRATE 
# + (1-ρ)*( r* + π_target + τ_π*Infl_dev + τ_y*Y_GAP )

# where
# ρ estimated (policy inertia)
# τ_π (inflation reaction) calibrated
# τ_y (output gap reaction) calibrated




############ OKUN'S LAW (UNEMPLOYMENT GAP) ####################
# ESTIMATE
# U_GAP_t = c1*U_GAP_{t-1} + c2*Y_GAP_t + break_dummies + ε

# NO CALIBRATION REQUIRED




########### WAGE PHILLIPS CURVE ###############################
# ESTIMATE
# WAGE_GAP_t = d1*WAGE_GAP_{t-1} + dummies + ε

# CALIBRATE
# + λ_u * U_GAP_t  
# + λ_π * Infl_gap_t    # inflation expectations gap




########### PRODUCTIVITY GAP EQUATION ########################
# ESTIMATE AS BEFORE
# NO CALIBRATION NEEDED




########### UIP EQUATION #####################################
# ESTIMATE
# D_RER_t = f1*D_RER_{t-1} + f2*dNX + f3*dummies + ε

# CALIBRATE
# + ψ * di_DIFFL    # UIP term is notoriously unstable



# STAGE 3: create structural break dummies

# see data script



# STAGE 4: build 3SLS

########## BUILD 3SLS SYSTEM #################################

# We proceed in two blocks: 
#  A. Dynamics to estimate
#       lag terms
#       shock pass‑through
#       persistence
#       break‑dummy adjustments

#  B. Structural slopes calibrated as constants

φ_r  <- 0.15      # IS curve real-rate slope
τ_π  <- 1.5       # Taylor inflation response
λ_π  <- 0.25      # Wage PC slope
κ_y  <- 0.10      # Phillips curve slope
rstar <- 0.0      # neutral real rate

MODEL_READY$calibrated_taylor <- (1-rho)*(rstar + MODEL_READY$Infl_dev*τ_π + MODEL_READY$Y_GAP*τ_y)

# Then add these calibrated components to the LHS of the 3SLS equations.


# STAGE 5: testing
########## CHECK SYSTEM for STABILITY and REALISM #############
# We test:
#   residual autocorrelation (BG test)
#   residual variance stability (ARCH tests)
#   parameter stability (recursive estimates)
#   impulse response stability (if desired)
