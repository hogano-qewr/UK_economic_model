# Hybrid F is not: expectations, DSGE, a new policy rule, a regime switch.
# Hybrid F is simply:   Hybrid E + one explicit cost‑side supply channel in the Phillips curve.
# The minimal Hybrid F change. Add productivity directly to the Phillips curve with a negative sign
# Conceptually: Productivity ↑ → marginal cost ↓ → inflation ↓


# IS curve
eq_IS <- Y_GAP + 0.05 * r_GAP_cal ~
  Y_GAP_L1 + i5y_GAP + dNX + drer_L1

# explicitly endogenising import prices
eq_IM <- IMcpi_GAP - 0.5 * IMcpi_GAP_L1 ~ drer_L1

# Phillips Curve
eq_PC <- (dcpi_DEV - 0.05 * Y_GAP + 0.04 * prod_GAP) ~           # HYBRID F ADDS calibrated prod_GAP to PC
  dcpi_DEV_L1 + dcpi_DEV_L2 +
  IMcpi_GAP + ecpi_GAP +
  PLG_L1

# Wage Phillips Curve
eq_WPC <- wage_GAP + 0.05 * u_GAP ~ wage_GAP_L1 + dcpi_DEV_L1 + prod_GAP

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
eq_UIP <- drer - 0.1 * i_DIFFL_GAP ~ drer_L1 + dNX + IMcpi_GAP


system_eqs_HF <- list(
  IS   = eq_IS,
  IM   = eq_IM,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls_HF <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  PLG_L1 + wage_GAP_L1 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP_cal + i5y_GAP +
  IMcpi_GAP_L1 + drer_L1 + 
  bpp_BEG + 
  bpu1_GFC + bpu2_POST

fit_3sls_HF <- systemfit(
  system_eqs_HF,
  method = "3SLS",
  inst = inst_3sls_HF,
  data = MODEL_READY
)

mm <- model.matrix(inst_3sls_HF, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))

summary(fit_3sls_HF)

#  looks good and stable, and—importantly—it looks exactly like a mature semi‑structural Hybrid F should
#   look once the supply‑side channel is disciplined by calibration. 
# 1. High‑level system diagnostics. McElroy R² ≈ 0.90. detRCov ≈ 0.037. System SSR essentially unchanged 
#     relative to Hybrid E. Hybrid F has not degraded system coherence. That’s the first pass/fail test, 
#     and it passes cleanly.
# 2. Phillips Curve. Inflation persistence L1 1.11***. [this exploded above 1 in Hybrid E] L2 0.07 . Error‑
#     correction (PLG_L1) ≈ −0.032, borderline
#     significant. Exactly the magnitude you want for a slow nominal anchor. Import prices - Small, positive, 
#     insignificant. Correctly not dominating CPI. No estimated productivity term on RHS.
#     Interpretation: The Phillips curve is now well‑behaved, anchored, and no longer trying to “learn” 
#     marginal cost from noisy data. 
# 3. Why this PC result is better than before. Compare this to the earlier estimated‑prod version. Before:
#     Negative prod coefficient, unstable magnitude, weak identification. NOW, Prod effect imposed structurally.
#     PC fit essentially unchanged. EC term still doing the anchoring. The data accept the calibrated supply 
#     channel without fighting it. 
# 4. Wage Phillips Curve: strong and complementary. prod_GAP ≈ −0.38(***). This is excellent. It means:
#     Productivity → wages ↓ (strongly). Hybrid F then adds: productivity → prices ↓ (moderately). So you now 
#     have two reinforcing cost channels, not competing ones: Wage costs and Marginal cost / price‑setting. 
#     How supply shocks should work.
# 5. Policy rule: unchanged and credible. Smoothing ≈ 0.97. Inflation response ≈ 0.07 ***. Output response 
#     small, plausible. Crucially:  You did not have to “fix” policy to get stability. That is a sign the nominal
#     anchor + supply channel are doing their job.
# 6. Import prices and UIP: behaving as expected. IM equation: calibrated persistence + strong drer pass‑through.
#     UIP:  drer_L1 ≈ 0.20 *.   IMcpi enters with sensible sign. R² low (this is normal). Nothing here is 
#     pathological or surprising.
# 7. Residual correlations: healthy. Key observations: PC residual negatively correlated with IS and OLP (normal).
#     No explosion in cross‑equation covariance. UIP residual correlations unchanged. Hybrid F did not create hidden 
#     feedback problems.
# 8. What this implies for IRFs (very important). With this exact specification, you should now expect:
#     Productivity shock: Inflation: flat or falling initially, then converging. Policy: much less tightening, 
#     possibly easing. Exchange rate: appreciation but bounded. Output: persistent expansion. 
#     Demand shock: Essentially unchanged from Hybrid E. 
#     Monetary policy shock: Clean disinflation, slow unwind. If that’s what you see (and it almost certainly will 
#     be), then:  Hybrid F is complete.

## ✅ USE THE CORRECT MODEL
coef_sys <- coef(fit_3sls_HF)

## STATES
state_names <- c(
  "Y_GAP",
  "dcpi_DEV",
  "dcpi_DEV_L1",   # ✅ REQUIRED
  "wage_GAP",
  "u_GAP",
  "i_UK",
  "prod_GAP",
  "drer",
  "IMcpi_GAP",
  "PLG"
)

## SHOCKS
shock_names <- c(
  "IS",
  "PC",
  "WPC",
  "TR",
  "OKUN",
  "OLP",
  "UIP",
  "IM"
)

r_star <- 0.5   # same units you used for r_GAP_cal

b <- function(name) {
  if (name %in% names(coef_sys))
    as.numeric(coef_sys[name])
  else
    0
}

alpha_r <- -0.05   # fixed IS real‑rate elasticity (Hybrid C)

step_system_F <- function(state, shock) {
  with(as.list(state), {
    
    ## 1. POLICY RULE
    i_UK_new <-
      b("TR_i_UK_L1") * i_UK +
      b("TR_dcpi_DEV") * dcpi_DEV +
      b("TR_Y_GAP") * Y_GAP +
      unname(shock["TR"])
    
    ## 2. REAL RATE GAP
    r_GAP_cal_new <- i_UK_new - dcpi_DEV - r_star
    
    ## 3. IS CURVE (lagged exchange rate)
    Y_GAP_new <-
      b("IS_Y_GAP_L1") * Y_GAP +
      alpha_r * r_GAP_cal_new +
      b("IS_drer_L1") * drer +
      unname(shock["IS"])
    
    ## 4. OKUN
    u_GAP_new <-
      b("OKUN_u_GAP_L1") * u_GAP +
      b("OKUN_Y_GAP") * Y_GAP_new +
      unname(shock["OKUN"])
    
    ## 5. PRODUCTIVITY
    prod_GAP_new <-
      b("OLP_prod_GAP_L1") * prod_GAP +
      b("OLP_Y_GAP") * Y_GAP_new +
      unname(shock["OLP"])
    
    ## 6. WAGES
    wage_GAP_new <-
      b("WPC_wage_GAP_L1") * wage_GAP +
      b("WPC_prod_GAP") * prod_GAP_new -
      0.05 * u_GAP_new +
      unname(shock["WPC"])
    
    ## 7. EXCHANGE RATE (UIP)
    i_DIFFL_GAP <- i_UK_new
    drer_new <-
      b("UIP_drer_L1") * drer +
      0.1 * i_DIFFL_GAP +
      unname(shock["UIP"])
    
    ## ✅ IMPORT PRICE PASS-THROUGH (Hybrid E, calibrated)
    IMcpi_GAP_new <-
      0.5 * IMcpi_GAP +
      b("IM_drer_L1") * drer +
      unname(shock["IM"])
    
    ## ✅ REVISED PHILLIPS CURVE
    dcpi_DEV_new <-
      b("PC_dcpi_DEV_L1") * dcpi_DEV +
      b("PC_dcpi_DEV_L2") * dcpi_DEV_L1 +
      0.05 * Y_GAP_new -
      0.04 * prod_GAP +                     # <-- lagged
      b("PC_IMcpi_GAP") * IMcpi_GAP_new +
      b("PC_PLG_L1") * PLG +
      unname(shock["PC"])
    
    dcpi_DEV_L1_new <- dcpi_DEV
    
    PLG_new <- PLG + dcpi_DEV_new
    
    out <- c(
      Y_GAP = Y_GAP_new,
      dcpi_DEV = dcpi_DEV_new,
      dcpi_DEV_L1 = dcpi_DEV_L1_new,
      wage_GAP = wage_GAP_new,
      u_GAP = u_GAP_new,
      i_UK = i_UK_new,
      prod_GAP = prod_GAP_new,
      drer = drer_new,
      IMcpi_GAP = IMcpi_GAP_new,
      PLG = PLG_new
    )
    
    # enforce correct order AND keep names
    out[state_names]
  })
}






## INITIAL STATE
state_0 <- c(
  Y_GAP = 0,
  dcpi_DEV = 0,
  dcpi_DEV_L1 = 0,
  wage_GAP = 0,
  u_GAP = 0,
  i_UK = 0,
  prod_GAP = 0,
  drer = 0,
  IMcpi_GAP = 0,
  PLG = 0
)
names(state_0)
length(state_names)   # should be 10
length(state_0)       # should be 10
names(step_system_F(state_0, shock_0))
names(step_system_F(state_0, shock_0)) == state_names
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


names(coef_sys)[grepl("PC_PLG_L1", names(coef_sys))]
names(coef_sys)[grepl("PC_dcpi_DEV_L2", names(coef_sys))]
names(coef_sys)[grepl("IM_drer_L1", names(coef_sys))]





## SHOCK VECTORS
shock_0 <- setNames(rep(0, length(shock_names)), shock_names)

simulate_irf <- function(shock_name, shock_size = 1, H = 20) {
  
  responses <- matrix(0, H + 1, length(state_names))
  colnames(responses) <- state_names
  responses[1, ] <- state_0
  
  shock_vec <- shock_0
  shock_vec[shock_name] <- shock_size
  
  for (h in 1:H) {
    responses[h + 1, ] <-
      step_system_F(
        responses[h, ],
        shock = if (h == 1) shock_vec else shock_0
      )
  }
  
  responses
}


length(state_names)            # 10
length(state_0)                # 10
names(state_0) == state_names  # TRUE
length(shock_names)            # 8
names(step_system_F(state_0, shock_0)) == state_names  # TRUE




exists("r_star")
# should be TRUE

irf_mp     <- simulate_irf("TR", 1.0, 16)   # Monetary policy shock (100bp)
irf_demand <- simulate_irf("IS", 1.0, 16)   # Demand shock
irf_productivity <- simulate_irf("OLP", 1.0, 20) # Productivity shock

head(irf_mp)
tail(irf_mp)

head(irf_demand)
head(irf_productivity)


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
         title = "Monetary Policy Shock – Hybrid E")
plot_irf(irf_demand,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Demand Shock – Hybrid E")
plot_irf(irf_productivity,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Productivity Shock – Hybrid E")

#################################################################################
#################################################################################

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
    horizon_max = 25
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
  title = "Impulse responses to a monetary policy shock (Hybrid F)"
)
plot_irf_stacked(
  irf_mp_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a monetary policy shock (Hybrid F)"
)
# Monetary policy shock (works). Looks good because anchor supplied exogenously: policy shock is transitory,
#   real activity falls, inflation falls, Taylor rule unwinds, UIP stabilizes. No problem here because the 
#   shock itself is mean‑zero. [MEANING?]



irf_demand_tidy <- irf_to_tidy(irf_demand, "Demand shock")

plot_irf_stacked(
  irf_demand_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a demand shock (Hybrid F)"
)
plot_irf_stacked(
  irf_demand_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a demand shock (Hybrid F)"
)

# Demand shock (inflation downward spiral): Demand shock raises output, Inflation rises, Policy tightens
#   Output slows, Inflation falls, but keeps falling because: The price‑level EC term (PLG) keeps pushing 
#   inflation down, Demand has died out, There is no lower nominal bound mechanism, Policy reacts only to 
#   current inflation, not misses. So the system says:b  “Inflation undershot? Fine, but nothing requires 
#   re‑inflation.” This is price‑level targeting without makeup. This is internally consistent but not 
#   realistic for modern monetary regimes


irf_productivity_tidy <- irf_to_tidy(irf_productivity, "Productivity (boost) shock")
plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a productivity boost (Hybrid F)"
)

plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a productivity boost (Hybrid F)"
)

# Productivity shock (the real problem). This is the core issue as with Hybrid E. What happens mechanically
#   Productivity increases potential output; Actual output rises persistently; Output gap remains positive; 
#   Policy sees persistent demand pressure; Policy tightens forever; UIP converts permanent rate differential
#   into ever‑stronger drer. Even with the Hybrid F correction: inflation may be damped in the short run, but 
#   policy keeps tightening in the long run. The result:  i_UK never returns; drer never returns
#     Not a numerical instability. It is a missing equilibrium condition.

# DIAGNOSIS. Model currently satisfies: Short‑run stabilization; Nominal determinacy; Correct sign responses. 
#   But it violates: Long‑run real–nominal neutrality. Specifically: There is no mechanism that equates long‑run 
#   real interest rate to long‑run growth / productivity. So policy keeps leaning against a permanently higher
#   level of activity as if it were excess demand.

# Hybrid F ONLY adds marginal‑cost realism. It does not add: a long‑run neutral rate condition; potential output 
#   closure; balanced‑growth equilibrium. Meaning the model thinks “Every positive output gap is inflationary 
#   forever.”

# Rather than endogenising r* or cranking down policy reaction / smoothing, Hybrid G will make the output gap 
#   relative to potential growth. We will tell the model “A productivity‑driven rise in activity is not demand 
#   pressure.” Formally, we can either: redefine Y_GAP as deviation from time‑varying potential, or subtract a 
#   productivity term from the output gap used in policy and PC, or allow productivity to raise r_star. This 
#   closes the system without expectations or DSGE. Realistic. Minimal. Consistent with overall hybrid (semi-
#   structural) framework