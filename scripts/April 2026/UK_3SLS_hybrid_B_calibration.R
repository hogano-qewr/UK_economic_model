# What fixing r∗r^*r∗ does in Hybrid B (recap): doing three very deliberate and coherent things:
# Separating policy stance from policy behaviour: r_STAR_cal = 0.5 pins down a structural neutral real rate.
#   Variation in r_GAP_cal now reflects policy stance, not movements in estimated trends.
# Removing an identification problem: In fully‑free systems, time‑varying or implicitly estimated r* can 
#   contaminate: IS slope estimates, Phillips‑curve dynamics, interpretation of monetary shocks. Fixing r* 
#   eliminates that ambiguity.
# Putting the model firmly into “conditional forecasting / scenario mode”: the essence of Hybrid B: policy
#   is conditioned, not estimated, the rest of the system responds structurally.
# This is exactly how OBR/BoE models are typically used when running: market‑consistent forecast conditioning,
#   policy‑path scenarios, or counterfactual monetary experiments.
# 



########## HYBRID B: second-STAGE CALIBRATION #################################
mm <- model.matrix(inst_3sls_HB, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))



# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP_cal + i5y_GAP + dNX + drer_L1

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + IMcpi_GAP + ecpi_GAP + bpp_BEG

# Wage Phillips Curve
eq_WPC <- wage_GAP + 0.05 * u_GAP ~ 
  wage_GAP_L1 + dcpi_DEV_L1 + prod_GAP

# Taylor Rule
eq_TR <- i_UK ~ 
  i_UK_L1 + dcpi_DEV + Y_GAP

# Okun equation
eq_OKUN <- u_GAP ~ 
  u_GAP_L1 + Y_GAP + bpu1_GFC + bpu2_POST

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
  bpp_BEG + 
  bpu1_GFC + bpu2_POST


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
    lwd = 1,
    bty = "n"
  )
}

plot_irf(irf_mp,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Monetary Policy Shock – Hybrid B")
plot_irf(irf_demand,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Demand Shock – Hybrid B")
plot_irf(irf_supply,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Supply Shock – Hybrid B")
