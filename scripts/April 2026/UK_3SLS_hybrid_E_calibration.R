# Hybrid E closes the open‑economy nominal loop by allowing the exchange rate to affect inflation through 
#   import‑price pass‑through, while preserving Hybrid C monotonicity and Hybrid D openness.
# Hybrid E introduces an explicit import‑price pass‑through channel to close the open‑economy nominal 
#   transmission mechanism. Energy price inflation is treated separately to avoid conflating exchange‑
#   rate‑driven and exogenous commodity‑price effects.
# Hybrid E + error correction added subsequently due to instability...PLG (price level gap)
#   Hybrid E + EC has achieved what it was meant to achieve: Nominal drift is gone. Policy no longer 
#     ratchets indefinitely. Exchange rate is bounded. Demand and monetary‑policy shocks look good.
# The remaining issue is specific and narrow: Favourable supply (productivity) shocks still look inflationary 
#     and induce prolonged tightening. That is not a coding problem, and it is not a failure of the nominal anchor.
#     It is a missing economic channel.
# Why Hybrid E cannot fully fix supply shocks by construction. In Hybrid E, inflation is driven by: output gap 
#     (demand pressure), import prices, inertia, price‑level error correction.
# What is missing is a channel that says: “Higher productivity lowers marginal cost.” So when productivity rises:
#   output rises, demand pressure dominates, inflation rises, policy tightens.
# That is exactly what your IRFs are showing. Hybrid E answers the question: “Is the nominal system stable?”
#   It does not answer: “Do supply shocks look nominally realistic?” That second question requires Hybrid F.



# IS curve
eq_IS <- Y_GAP + 0.05 * r_GAP_cal ~
  Y_GAP_L1 + i5y_GAP + dNX + drer_L1

# explicitly endogenising import prices
eq_IM <- IMcpi_GAP - 0.5 * IMcpi_GAP_L1 ~ drer_L1       # Hybrid E started with just this change

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~
  dcpi_DEV_L1 + dcpi_DEV_L2 +
  IMcpi_GAP + ecpi_GAP + bpp_BEG +
  PLG_L1                                                # we then added PLG for error correction 

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


system_eqs_HE <- list(
  IS   = eq_IS,
  IM   = eq_IM,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls_HE <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  PLG_L1 + wage_GAP_L1 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP_cal + i5y_GAP +
  IMcpi_GAP_L1 + drer_L1 + 
  bpp_BEG + 
  bpu1_GFC + bpu2_POST

fit_3sls_HE <- systemfit(
  system_eqs_HE,
  method = "3SLS",
  inst = inst_3sls_HE,
  data = MODEL_READY
)

mm <- model.matrix(inst_3sls_HE, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))

summary(fit_3sls_HE)


names(step_system_E(state_0, shock_0))

## ✅ USE THE CORRECT MODEL
coef_sys <- coef(fit_3sls_HE)

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

step_system_E <- function(state, shock) {
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
      0.05 * Y_GAP_new +
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
names(step_system_E(state_0, shock_0))
names(step_system_E(state_0, shock_0)) == state_names
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
      step_system_E(
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
names(step_system_E(state_0, shock_0)) == state_names  # TRUE




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
  title = "Impulse responses to a monetary policy shock (Hybrid E)"
)
plot_irf_stacked(
  irf_mp_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a monetary policy shock (Hybrid E)"
)


irf_demand_tidy <- irf_to_tidy(irf_demand, "Demand shock")

plot_irf_stacked(
  irf_demand_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a demand shock (Hybrid E)"
)
plot_irf_stacked(
  irf_demand_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a demand shock (Hybrid E)"
)


irf_productivity_tidy <- irf_to_tidy(irf_productivity, "Productivity (boost) shock")
plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a productivity boost (Hybrid E)"
)

plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a productivity boost (Hybrid E)"
)

