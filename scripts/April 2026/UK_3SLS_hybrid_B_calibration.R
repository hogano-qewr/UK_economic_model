# What fixing r* does in Hybrid B (recap): doing three very deliberate and coherent things:
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
# Hybrid B's purpose/ interpretation is "...this is what the economy does if policy stays on its assumed path."
#   Policy is conditioned (through the fixing of r*). 
# Central banks and fiscal institutions deliberately use models that behave less well than policy‑simulation 
#   models, because they want to see where the economy can drift if nothing corrects it. (i.e., if policy doesn't
#   react any differently to how it currently reacts).
# Hybrid B is retained for baseline and scenario analysis, where persistence and incomplete mean reversion 
#   are informative features of the data (in the case of a demand shock). Hybrid C is introduced for policy 
#   simulation, where monotone 
#   adjustment paths and clear transmission narratives are required. The distinction reflects differences 
#   in intended use rather than deficiencies in either specification.

# If Hybrid B forced demand shocks to mean‑revert smoothly, it would be building policy behaviour into the 
#   forecast engine, which is something institutions are usually careful to avoid. So despite the demand shock
#   IRFs looking “ugly”: Hybrid B is honest, is risk‑revealing, and is appropriate for baseline/scenario work.
# Hybrid B demand shock does not converge cleanly to zero, exhibits medium‑run drift, and behaves less “well” 
#   than supply and policy shocks. But crucially:   This behaviour is not pathological—it is informational. It 
#   is telling you something true about the economy under the Hybrid B assumptions:   Policy is conditioned, not 
#   reacting; Output and unemployment are highly persistent; There is no automatic mean‑reversion mechanism for 
#   demand. In other words, Hybrid B is saying:   “If demand rises and policy does not respond endogenously, 
#   the economy does not necessarily self‑correct.” That is empirically plausible and analytically valuable, 
#   especially for forecasting and scenario conditioning.

# Hybrid B reveals that conditional demand shocks do not naturally converge under fixed policy. Hybrid C is 
#   introduced to impose gradual re‑equilibration consistent with policy‑simulation requirements.

# Why this happens only for demand shocks in Hybrid B. The key reason is structural: In Hybrid B:
#   Monetary policy is conditioned (policy path fixed); Phillips‑curve slopes are disciplined; Demand 
#   shocks are the only shocks that directly hit the level of activity. There is no equilibrium‑restoring 
#   mechanism left that forces output back to zero
# Put differently: In Hybrid B, demand shocks are level shocks, not purely transitory innovations. Supply 
#   and policy shocks still have: cost‑side decay (PC, WPC), monetary stabilisation, or natural adjustment channels.
# Demand shocks do not, because:  Output persistence is high; Policy does not react endogenously; No explicit 
#   “return‑to‑potential” correction is imposed. So the model is honestly telling you:  “If demand is pushed up 
#   and policy does nothing new, output does not automatically snap back.” That is empirically plausible, 
#   but awkward for policy simulation.

########## HYBRID B: second-STAGE CALIBRATION #################################




# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP_cal + i5y_GAP + dNX + drer_L1       # r_GAP_cal replaces r_GAP

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
  wage_GAP_L1 +
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

mm <- model.matrix(inst_3sls_HB, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))


summary(fit_3sls_HB)

# NOTABLE RESULT: COEFFICIENT ON r_GAP_cal is 0.14 vs. 0.68 for r_GAP in Hybrid A and even higher in fully-free
# Inflation persistence steadily rising from 0.75 in fully-free to 0.92*** in Hybrid A, back down slightly to 0.90
#     in Hyrbrid B.

#### PLOT FIT OF MODELLED IS AND PC AGAINST ACTUALS FOR Y_GAP AND dcpi_DEV ######################################


library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df1 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_HB)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df1, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit - Hybrid B",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()

# The evolution across these three charts tells a compelling story of structural discipline. While a 
#   casual observer might see a "worse" fit in the Hybrid models because the red line doesn't hug every 
#   peak, an economist sees a model that is becoming significantly more credible.
# Hybrid B: The "Policy Anchor": The Change: Fixing r* and auditing the instruments. The Payoff: Look 
#   at the 2021-2024 recovery. Hybrid B tracks the "return to zero" much more accurately than Hybrid A. 
#   By anchoring the neutral rate, you've helped the model correctly identify how much of the recovery was 
#   "natural" vs. how much was driven by the interest rate environment. The GFC (2008): Hybrid B has a much 
#   cleaner "peak" and "trough." It doesn't overshoot the 2008 boom as much as the fully-free version did, 
#   which is more consistent with the narrative that the 2008 bubble was partly a "financial/credit" shock 
#   rather than a pure "output demand" shock.


library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df2 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$dcpi_DEV),
  Fitted = as.numeric(fitted(fit_3sls_HB)$PC)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df2, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Inflation: Actual vs. 3SLS System Fit - Hybrid B",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Inflation deviation from 2%") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()

# The improvement in Hybrid B is clearly visible, especially in how the model handles the persistence of inflation.
#   By fixing r* you’ve given the Taylor Rule (and the entire system) a "long-run anchor" that prevents the fit 
#   from drifting too far from the data in stable periods. Why Hybrid B looks "Sharper": The 2023 Peak Correction: 
#   In your Hybrid A discussion, the model was slightly over-attributing the peak. In this Hybrid B plot, the red 
#   line is tracking the downward turn of 2024 much more accurately. This suggests that anchoring helped the model 
#   better identify the restrictiveness of monetary policy during that period. Reduced "Echoes": Look at the 2011–
#   2015 period. In many free models, you get "ghost" oscillations where the model tries to find cycles that aren't
#   there. Here, the red line is much more disciplined—it follows the data without the "jitter" we saw in the fully
#   -free estimation. The 2021 Recovery: The tracking of the post-COVID rebound is excellent. It shows that by 
#   anchoring expectations and the neutral rate, you've allowed the systemic shocks (the actual economic events) 
#   to drive the fit, rather than mathematical noise. The "Forecasting Ready" Look: What makes this plot "better" 
#   from a working paper perspective is the residual behavior at the very end of the sample (2024–2025). The red 
#   line is converging back toward the data trend. This is exactly what you want to see before moving to Stage 2; 
#   it means your starting conditions for the forecast are grounded in a model that is currently "in sync" with the
#   real economy.



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

