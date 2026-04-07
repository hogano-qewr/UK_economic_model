



############# IS RELATION #####################################
IS_formula <- Y_GAP ~
  Y_GAP_L1 + 
  r_GAP + 
  i5y_GAP +
  dNX + 
  drer_L1 +
  bpy + 
  bpr

IS_ols <- lm(IS_formula, data = MODEL_READY)
summary(IS_ols)

lmtest::bgtest(IS_ols, order = 4)
lmtest::bptest(IS_ols)
checkresiduals(IS_ols)


# CALIBRATE (not sure what to with the code suggested here yet...)
# - φ_r * r_GAP_t    # real rate gap effect (calibrated)
# - φ_s * spr_GAP_t  # spread effect (calibrated)


############# PHILLIPS CURVE (INFLATION EQUATION) #############
# ESTIMATE
# Infl_dev_t = b1*Infl_dev_{t-1} + b2*Infl_dev_{t-2} + b3*IM_infl_GAP + b4*Energy_infl + dummies + ε

PC_formula <- dcpi_DEV ~
  dcpi_DEV_L1 +
  dcpi_DEV_L2 +
  Y_GAP +
  IMcpi_GAP +
  ecpi_GAP +
  bpp

PC_ols <- lm(PC_formula, data = MODEL_READY)
summary(PC_ols)

lmtest::bgtest(PC_ols, order = 4)
lmtest::bptest(PC_ols)
checkresiduals(PC_ols)


# CALIBRATE
# + κ_y * Y_GAP_t           # slope term (calibrated)
# + κ_p * PROD_GAP_t        # small role in UK data


############# TAYLOR RULE #####################################
# ESTIMATE
# i_t = ρ*i_{t-1} + ε_t

# estimate the reduced‑form hybrid rule:
# i_t=γ_0 +ρ(i_t−1) + γ_π(π_t) + γ_Y(Y_t) + ε_t

TR_formula <- i_UK ~
  i_UK_L1 +
  dcpi_DEV +
  Y_GAP

TR_ols <- lm(TR_formula, data = MODEL_READY)
summary(TR_ols)

lmtest::bgtest(TR_ols, order = 4)
lmtest::bptest(TR_ols)
checkresiduals(TR_ols)

# CALIBRATE 
# + (1-ρ)*( r* + π_target + τ_π*Infl_dev + τ_y*Y_GAP )

# where
# ρ estimated (policy inertia)
# τ_π (inflation reaction) calibrated
# τ_y (output gap reaction) calibrated


############ OKUN'S LAW (UNEMPLOYMENT GAP) ####################
# ESTIMATE
# U_GAP_t = c1*U_GAP_{t-1} + c2*Y_GAP_t + break_dummies + ε

OKUN_formula <- u_GAP ~
  u_GAP_L1 +
  Y_GAP +
  bpu1 +
  bpu2 +
  bpu3 +
  bpu4

OKUN_ols <- lm(OKUN_formula, data = MODEL_READY)
summary(OKUN_ols)

lmtest::bgtest(OKUN_ols, order = 4)
lmtest::bptest(OKUN_ols)
checkresiduals(OKUN_ols)
# NO CALIBRATION REQUIRED


########### WAGE PHILLIPS CURVE ###############################
# ESTIMATE
# WAGE_GAP_t = d1*WAGE_GAP_{t-1} + dummies + ε

WPC_formula <- wage_GAP ~
  wage_GAP_L1 +
  dcpi_DEV_L1 +
  u_GAP +
  prod_GAP

WPC_ols <- lm(WPC_formula, data = MODEL_READY)
summary(WPC_ols)

lmtest::bgtest(WPC_ols, order = 4)
lmtest::bptest(WPC_ols)
checkresiduals(WPC_ols)

# CALIBRATE
# + λ_u * U_GAP_t  
# + λ_π * Infl_gap_t    # inflation expectations gap


########### PRODUCTIVITY GAP EQUATION ########################
# ESTIMATE AS BEFORE
# NO CALIBRATION NEEDED
OLP_formula <- prod_GAP ~
  prod_GAP_L1 +
  Y_GAP

OLP_ols <- lm(OLP_formula, data = MODEL_READY)
summary(OLP_ols)

lmtest::bgtest(OLP_ols, order = 4)
lmtest::bptest(OLP_ols)
checkresiduals(OLP_ols)


########### UIP EQUATION #####################################
# ESTIMATE
# D_RER_t = f1*D_RER_{t-1} + f2*dNX + f3*dummies + ε
UIP_formula <- drer ~
  drer_L1 +
  i_DIFFL_GAP +
  dNX +
  IMcpi_GAP

UIP_ols <- lm(UIP_formula, data = MODEL_READY)
summary(UIP_ols)

lmtest::bgtest(UIP_ols, order = 4)
lmtest::bptest(UIP_ols)
checkresiduals(UIP_ols)

# CALIBRATE
# + ψ * di_DIFFL    # UIP term is notoriously unstable


################################################################################

# 2SLS on equations suffering simultaneity (due to endogenous regressors) and other
#         issues (PC and WPC)
library(AER)

PC_2sls <- ivreg(
  dcpi_DEV ~ 
    dcpi_DEV_L1 + 
    dcpi_DEV_L2 + 
    Y_GAP + 
    IMcpi_GAP + 
    ecpi_GAP + 
    bpp |
    
    dcpi_DEV_L1 + 
    dcpi_DEV_L2 + 
    Y_GAP_L2 + 
    Y_GAP_L3 +
    r_GAP + 
    i5y_GAP +
    drer_L1 +
    IMcpi_GAP + 
    ecpi_GAP + 
    bpp,
  data = MODEL_READY
)

summary(PC_2sls, diagnostics = TRUE)

lmtest::bgtest(PC_ols, order = 4)
lmtest::bptest(PC_ols)
checkresiduals(PC_ols)


library(AER)

WPC_2sls <- ivreg(
  wage_GAP ~ 
    wage_GAP_L1 +
    dcpi_DEV_L1 +
    u_GAP +
    prod_GAP |
    
    wage_GAP_L1 +
    dcpi_DEV_L1 +
    prod_GAP +
    
    u_GAP_L2 +
    u_GAP_L3 +
    Y_GAP +
    r_GAP +
    prod_GAP_L1,
  
  data = MODEL_READY
)

summary(WPC_2sls, diagnostics = TRUE)



# STAGE 4: build 3SLS system (fully free)

model.matrix(inst_system, data = MODEL_READY) %>%
  qr() %>%
  `[[`("rank")
ncol(model.matrix(inst_system, data = MODEL_READY))

mm <- model.matrix(inst_3sls, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))


# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP + i5y_GAP + dNX + drer_L1 + bpy + bpr

# Phillips Curve
eq_PC <- dcpi_DEV ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + Y_GAP + IMcpi_GAP + ecpi_GAP + bpp

# Wage Phillips Curve
eq_WPC <- wage_GAP ~ 
  wage_GAP_L1 + dcpi_DEV_L1 + u_GAP + prod_GAP

# Taylor Rule
eq_TR <- i_UK ~ 
  i_UK_L1 + dcpi_DEV + Y_GAP

# Okun equation
eq_OKUN <- u_GAP ~ 
  u_GAP_L1 + Y_GAP + bpu1 + bpu2 + bpu3 + bpu4

# Productivity
eq_OLP <- prod_GAP ~ 
  prod_GAP_L1 + Y_GAP

# UIP
eq_UIP <- drer ~ 
  drer_L1 + i_DIFFL_GAP + dNX + IMcpi_GAP

system_eqs <- list(
  IS   = eq_IS,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP + i5y_GAP +
  drer_L1 +
  bpp + bpy +
  bpu1 + bpu2 + bpu3 + bpu4

fit_3sls <- systemfit(
  system_eqs,
  method = "3SLS",
  inst = inst_3sls,
  data = MODEL_READY
)

summary(fit_3sls)




########## HYBRID A: FIRST-STAGE CALIBRATION (PC and WPC slopes) #################################
# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP + i5y_GAP + dNX + drer_L1 + bpy + bpr

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + IMcpi_GAP + ecpi_GAP + bpp

# Wage Phillips Curve
eq_WPC <- wage_GAP + 0.05 * u_GAP ~ 
  wage_GAP_L1 + dcpi_DEV_L1 + prod_GAP

# Taylor Rule
eq_TR <- i_UK ~ 
  i_UK_L1 + dcpi_DEV + Y_GAP

# Okun equation
eq_OKUN <- u_GAP ~ 
  u_GAP_L1 + Y_GAP + bpu1 + bpu2 + bpu3 + bpu4

# Productivity
eq_OLP <- prod_GAP ~ 
  prod_GAP_L1 + Y_GAP

# UIP
eq_UIP <- drer ~ 
  drer_L1 + i_DIFFL_GAP + dNX + IMcpi_GAP

system_eqs_HA <- list(
  IS   = eq_IS,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP + i5y_GAP +
  drer_L1 +
  bpp + bpy +
  bpu1 + bpu2 + bpu3 + bpu4

fit_3sls_HA <- systemfit(
  system_eqs_HA,
  method = "3SLS",
  inst = inst_3sls,
  data = MODEL_READY
)

summary(fit_3sls_HA)


########## HYBRID B: second-STAGE CALIBRATION #################################
mm <- model.matrix(inst_3sls_HB, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))



# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP_cal + i5y_GAP + dNX + drer_L1 + bpy + bpr

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + IMcpi_GAP + ecpi_GAP + bpp

# Wage Phillips Curve
eq_WPC <- wage_GAP + 0.05 * u_GAP ~ 
  wage_GAP_L1 + dcpi_DEV_L1 + prod_GAP

# Taylor Rule
eq_TR <- i_UK ~ 
  i_UK_L1 + dcpi_DEV + Y_GAP

# Okun equation
eq_OKUN <- u_GAP ~ 
  u_GAP_L1 + Y_GAP + bpu1 + bpu2 + bpu3 + bpu4

# Productivity
eq_OLP <- prod_GAP ~ 
  prod_GAP_L1 + Y_GAP

# UIP
eq_UIP <- drer ~ 
  drer_L1 + i_DIFFL_GAP + dNX + IMcpi_GAP

system_eqs_HB <- list(
  IS   = eq_IS,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls_HB <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP_cal + i5y_GAP +
  drer_L1 +
  bpp + bpy +
  bpu1 + bpu2 + bpu3 + bpu4

fit_3sls_HB <- systemfit(
  system_eqs_HB,
  method = "3SLS",
  inst = inst_3sls_HB,
  data = MODEL_READY
)

summary(fit_3sls_HB)

# EXAMINE SOME IRFs TO SEE IF HYBRID C ACTUALLY REQUIRED #######################
coef_sys <- coef(fit_3sls_HB)
print(coef_sys)

state_names <- c(
  "Y_GAP",
  "dcpi_DEV",
  "wage_GAP",
  "u_GAP",
  "i_UK",
  "prod_GAP",
  "drer"
)

shock_names <- c("IS", "PC", "WPC", "TR", "OKUN", "OLP", "UIP")

r_star <- 0.5   # same units you used for r_GAP_cal

b <- function(name) {
  if (name %in% names(coef_sys))
    as.numeric(coef_sys[name])
  else
    0
}

step_system <- function(state, shock) {
  
  with(as.list(state), {
    
    # Real rate gap (Hybrid B)
    r_GAP_cal <- i_UK - dcpi_DEV - r_star
    
    Y_GAP_new <-
      b("IS_Y_GAP_L1")     * Y_GAP +
      b("IS_r_GAP_cal")    * r_GAP_cal +
      shock["IS"]
    
    dcpi_DEV_new <-
      b("PC_dcpi_DEV_L1")  * dcpi_DEV +
      shock["PC"]
    
    wage_GAP_new <-
      b("WPC_wage_GAP_L1") * wage_GAP +
      b("WPC_prod_GAP")    * prod_GAP +
      shock["WPC"]
    
    u_GAP_new <-
      b("OKUN_u_GAP_L1")   * u_GAP +
      b("OKUN_Y_GAP")      * Y_GAP +
      shock["OKUN"]
    
    i_UK_new <-
      b("TR_i_UK_L1")      * i_UK +
      b("TR_dcpi_DEV")     * dcpi_DEV +
      b("TR_Y_GAP")        * Y_GAP +
      shock["TR"]
    
    prod_GAP_new <-
      b("OLP_prod_GAP_L1") * prod_GAP +
      b("OLP_Y_GAP")       * Y_GAP +
      shock["OLP"]
    
    drer_new <-
      b("UIP_drer_L1")     * drer +
      shock["UIP"]
    
    c(
      Y_GAP    = Y_GAP_new,
      dcpi_DEV = dcpi_DEV_new,
      wage_GAP = wage_GAP_new,
      u_GAP    = u_GAP_new,
      i_UK     = i_UK_new,
      prod_GAP = prod_GAP_new,
      drer     = drer_new
    )
  })
}

simulate_irf <- function(shock_name, shock_size = 1, H = 20) {
  
  state0 <- setNames(rep(0, length(state_names)), state_names)
  
  responses <- matrix(0, H + 1, length(state_names))
  colnames(responses) <- state_names
  responses[1, ] <- state0
  
  shock_vec <- setNames(rep(0, length(shock_names)), shock_names)
  shock_vec[shock_name] <- shock_size
  
  zero_shock <- setNames(rep(0, length(shock_names)), shock_names)
  
  for (h in 1:H) {
    responses[h + 1, ] <-
      step_system(
        responses[h, ],
        shock = if (h == 1) shock_vec else zero_shock
      )
  }
  
  responses
}


grep("r_GAP", names(coef_sys), value = TRUE)
"IS_r_GAP_cal" %in% names(coef_sys)
# should be TRUE

exists("r_star")
# should be TRUE

irf_mp     <- simulate_irf("TR", 1.0, 20)   # Monetary policy shock (100bp)
irf_demand <- simulate_irf("IS", 1.0, 20)   # Demand shock
irf_supply <- simulate_irf("PC", 1.0, 20)   # Supply shock

head(irf_mp)
tail(irf_mp)

head(irf_demand)
head(irf_supply)



plot_irf <- function(irf, vars, title = "") {
  matplot(
    irf[, vars],
    type = "l",
    lty = 1,
    lwd = 2,
    col = seq_along(vars),
    main = title,
    xlab = "Horizon",
    ylab = "Deviation from baseline"
  )
  legend(
    "topright",
    legend = vars,
    col = seq_along(vars),
    lwd = 2,
    bty = "n"
  )
}

plot_irf(irf_mp,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Monetary Policy Shock – Hybrid B")


########## HYBRID C: third-STAGE CALIBRATION #################################
mm <- model.matrix(inst_3sls_HC, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))



# IS curve
eq_IS <- Y_GAP + 0.05 * r_GAP_cal ~
  Y_GAP_L1 + i5y_GAP + dNX + drer_L1 + bpy + bpr

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + IMcpi_GAP + ecpi_GAP + bpp

# Wage Phillips Curve
eq_WPC <- wage_GAP + 0.05 * u_GAP ~ 
  wage_GAP_L1 + dcpi_DEV_L1 + prod_GAP

# Taylor Rule
eq_TR <- i_UK ~ 
  i_UK_L1 + dcpi_DEV + Y_GAP

# Okun equation
eq_OKUN <- u_GAP ~ 
  u_GAP_L1 + Y_GAP + bpu1 + bpu2 + bpu3 + bpu4

# Productivity
eq_OLP <- prod_GAP ~ 
  prod_GAP_L1 + Y_GAP

# UIP
eq_UIP <- drer ~ 
  drer_L1 + i_DIFFL_GAP + dNX + IMcpi_GAP

system_eqs_HC <- list(
  IS   = eq_IS,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls_HC <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP_cal + i5y_GAP +
  drer_L1 +
  bpp + bpy +
  bpu1 + bpu2 + bpu3 + bpu4

fit_3sls_HC <- systemfit(
  system_eqs_HC,
  method = "3SLS",
  inst = inst_3sls_HC,
  data = MODEL_READY
)

summary(fit_3sls_HC)

# EXAMINE SOME IRFs TO SEE IF HYBRID C ACTUALLY REQUIRED #######################
coef_sys <- coef(fit_3sls_HC)
print(coef_sys)

state_names <- c(
  "Y_GAP",
  "dcpi_DEV",
  "wage_GAP",
  "u_GAP",
  "i_UK",
  "prod_GAP",
  "drer"
)

shock_names <- c("IS", "PC", "WPC", "TR", "OKUN", "OLP", "UIP")

r_star <- 0.5   # same units you used for r_GAP_cal

b <- function(name) {
  if (name %in% names(coef_sys))
    as.numeric(coef_sys[name])
  else
    0
}

alpha_r <- -0.05   # fixed IS real‑rate elasticity (Hybrid C)

step_system <- function(state, shock) {
  
  with(as.list(state), {
    
    ## 1. POLICY RULE FIRST (shock hits here)
    i_UK_new <-
      b("TR_i_UK_L1")  * i_UK +
      b("TR_dcpi_DEV") * dcpi_DEV +
      b("TR_Y_GAP")    * Y_GAP +
      shock["TR"]
    
    ## 2. REAL RATE GAP SEES THE SHOCK IMMEDIATELY
    r_GAP_cal <- i_UK_new - dcpi_DEV - r_star
    
    ## 3. IS CURVE (HYBRID C: FIXED ELASTICITY)
    Y_GAP_new <-
      b("IS_Y_GAP_L1") * Y_GAP +
      alpha_r           * r_GAP_cal +
      shock["IS"]
    
    ## 4. PHILLIPS CURVE
    dcpi_DEV_new <-
      b("PC_dcpi_DEV_L1") * dcpi_DEV +
      shock["PC"]
    
    ## 5. WAGE SETTING
    wage_GAP_new <-
      b("WPC_wage_GAP_L1") * wage_GAP +
      b("WPC_prod_GAP")    * prod_GAP +
      shock["WPC"]
    
    ## 6. OKUN
    u_GAP_new <-
      b("OKUN_u_GAP_L1") * u_GAP +
      b("OKUN_Y_GAP")    * Y_GAP +
      shock["OKUN"]
    
    ## 7. PRODUCTIVITY
    prod_GAP_new <-
      b("OLP_prod_GAP_L1") * prod_GAP +
      b("OLP_Y_GAP")       * Y_GAP +
      shock["OLP"]
    
    ## 8. EXCHANGE RATE
    drer_new <-
      b("UIP_drer_L1") * drer +
      shock["UIP"]
    
    c(
      Y_GAP    = Y_GAP_new,
      dcpi_DEV = dcpi_DEV_new,
      wage_GAP = wage_GAP_new,
      u_GAP    = u_GAP_new,
      i_UK     = i_UK_new,
      prod_GAP = prod_GAP_new,
      drer     = drer_new
    )
  })
}

simulate_irf <- function(shock_name, shock_size = 1, H = 20) {
  
  state0 <- setNames(rep(0, length(state_names)), state_names)
  
  responses <- matrix(0, H + 1, length(state_names))
  colnames(responses) <- state_names
  responses[1, ] <- state0
  
  shock_vec <- setNames(rep(0, length(shock_names)), shock_names)
  shock_vec[shock_name] <- shock_size
  
  zero_shock <- setNames(rep(0, length(shock_names)), shock_names)
  
  for (h in 1:H) {
    responses[h + 1, ] <-
      step_system(
        responses[h, ],
        shock = if (h == 1) shock_vec else zero_shock
      )
  }
  
  responses
}


grep("r_GAP", names(coef_sys), value = TRUE)
"IS_r_GAP_cal" %in% names(coef_sys)
# should be TRUE

exists("r_star")
# should be TRUE

irf_mp     <- simulate_irf("TR", 1.0, 16)   # Monetary policy shock (100bp)
irf_demand <- simulate_irf("IS", 1.0, 16)   # Demand shock
irf_supply <- simulate_irf("PC", 1.0, 16)   # Supply shock

head(irf_mp)
tail(irf_mp)

head(irf_demand)
head(irf_supply)



plot_irf <- function(irf, vars, title = "") {
  matplot(
    irf[, vars],
    type = "l",
    lty = 1,
    lwd = 2,
    col = seq_along(vars),
    main = title,
    xlab = "Horizon",
    ylab = "Deviation from baseline"
  )
  legend(
    "topright",
    legend = vars,
    col = seq_along(vars),
    lwd = 2,
    bty = "n"
  )
}

plot_irf(irf_mp,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Monetary Policy Shock – Hybrid C")



irf_to_tidy <- function(irf_mat, shock_name) {
  
  irf_mat %>%
    as_tibble() %>%
    mutate(horizon = row_number() - 1) %>%
    pivot_longer(
      cols = -horizon,
      names_to = "variable",
      values_to = "response"
    ) %>%
    mutate(shock = shock_name)
}

plot_irf_stacked <- function(
    irf_tidy,
    variables,
    title,
    horizon_max = 20
) {
  
  irf_tidy_filtered <- irf_tidy %>%
    filter(variable %in% variables,
           horizon <= horizon_max)
  
  ggplot(irf_tidy_filtered,
         aes(x = horizon, y = response)) +
    
    # IRF line
    geom_line(
      linewidth = 1.2,
      colour = "#000000"   # black: colour‑blind & print safe
    ) +
    
    # Zero reference line
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      colour = "grey40"
    ) +
    
    # One panel per variable (stacked)
    facet_wrap(
      ~ variable,
      ncol = 1,
      scales = "free_y",
      strip.position = "right"
    ) +
    
    labs(
      title = title,
      x = "Horizon (quarters)",
      y = "Deviation from baseline"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 11
      ),
      plot.title = element_text(
        face = "bold",
        size = 14,
        margin = margin(b = 10)
      ),
      axis.title = element_text(size = 12)
    )
}

irf_mp_tidy <- irf_to_tidy(irf_mp, "Monetary policy shock")

plot_irf_stacked(
  irf_mp_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a monetary policy shock (Hybrid C)"
)



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



# TESTING
