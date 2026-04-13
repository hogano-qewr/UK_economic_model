


########## HYBRID D: fourth-STAGE CALIBRATION #################################



# IS curve
eq_IS <- Y_GAP + 0.05 * r_GAP_cal ~
  Y_GAP_L1 + i5y_GAP + dNX + drer_L1

# Phillips Curve
eq_PC <- dcpi_DEV - 0.05 * Y_GAP ~ 
  dcpi_DEV_L1 + dcpi_DEV_L2 + IMcpi_GAP + ecpi_GAP + bpp_BEG

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
eq_UIP <- drer - 0.1 * i_DIFFL_GAP ~ drer_L1 + dNX + IMcpi_GAP    # the HYBRID D change


system_eqs_HD <- list(
  IS   = eq_IS,
  PC   = eq_PC,
  WPC  = eq_WPC,
  TR   = eq_TR,
  OKUN = eq_OKUN,
  OLP  = eq_OLP,
  UIP  = eq_UIP
)

inst_3sls_HD <- ~
  Y_GAP_L2 + Y_GAP_L3 +
  wage_GAP_L1 +
  u_GAP_L2 + u_GAP_L3 +
  prod_GAP_L1 +
  r_GAP_cal + i5y_GAP +
  drer_L1 +
  bpp_BEG + 
  bpu1_GFC + bpu2_POST

fit_3sls_HD <- systemfit(
  system_eqs_HD,
  method = "3SLS",
  inst = inst_3sls_HD,
  data = MODEL_READY
)

mm <- model.matrix(inst_3sls_HD, data = MODEL_READY)
c(rank = qr(mm)$rank, ncol = ncol(mm))

summary(fit_3sls_HD)


#### PLOT FIT OF MODELLED IS AND PC AGAINST ACTUALS FOR Y_GAP AND dcpi_DEV ######################################


library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df1 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_HD)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df1, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit - Hybrid D",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()
# "The Hybrid D specification represents the final structural evolution of the system. By incorporating the 
#     active UIP channel, the model achieves its highest level of systemic sensitivity. While this introduces 
#     more high-frequency movement in the system fit, it significantly improves the model’s ability to track 
#     the volatile post-pandemic recovery. The tight alignment at the 2024/25 boundary provides a robust, 
#     zero-residual jumping-off point for the preliminary forecasting in Stage 2."


library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df2 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$dcpi_DEV),
  Fitted = as.numeric(fitted(fit_3sls_HD)$PC)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df2, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Inflation: Actual vs. 3SLS System Fit - Hybrid D",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Inflation deviation from 2%") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()
# Hybrid D represents the structural culmination of the model. As shown in the inflation fit, the 
#   inclusion of an active exchange rate channel and calibrated structural slopes allows the model 
#   to capture the 2022-23 inflationary episode with high fidelity. Unlike the unconstrained estimation,
#   Hybrid D avoids overshooting the peaks, providing a stable and theoretically consistent foundation for 
#   the forecasting





# EXAMINE SOME IRFs #######################
coef_sys <- coef(fit_3sls_HD)
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
    
    ## 1. POLICY RULE
    i_UK_new <-
      b("TR_i_UK_L1") * i_UK +
      b("TR_dcpi_DEV") * dcpi_DEV +
      b("TR_Y_GAP") * Y_GAP +
      shock["TR"]
    
    ## 2. REAL RATE GAP
    r_GAP_cal_new <- i_UK_new - dcpi_DEV - r_star
    
    ## 3. IS CURVE (lagged exchange rate)
    Y_GAP_new <-
      b("IS_Y_GAP_L1")*Y_GAP +
      alpha_r*r_GAP_cal_new +
      b("IS_drer_L1")*drer +
      shock["IS"]
    
    ## 4. OKUN
    u_GAP_new <-
      b("OKUN_u_GAP_L1")*u_GAP +
      b("OKUN_Y_GAP")*Y_GAP_new +
      shock["OKUN"]
    
    ## 5. PHILLIPS CURVE
    dcpi_DEV_new <-
      b("PC_dcpi_DEV_L1")*dcpi_DEV +
      0.05*Y_GAP_new +
      shock["PC"]
    
    ## 6. PRODUCTIVITY
    prod_GAP_new <-
      b("OLP_prod_GAP_L1")*prod_GAP +
      b("OLP_Y_GAP")*Y_GAP_new +
      shock["OLP"]
    
    ## 7. WAGES
    wage_GAP_new <-
      b("WPC_wage_GAP_L1")*wage_GAP +
      b("WPC_prod_GAP")*prod_GAP_new -
      0.05*u_GAP_new +
      shock["WPC"]
    
    ## 8. UIP (Hybrid D)
    i_DIFFL_GAP <- i_UK_new
    drer_new <-
      b("UIP_drer_L1")*drer +
      0.1*i_DIFFL_GAP +
      shock["UIP"]
    
    c(Y_GAP=Y_GAP_new, dcpi_DEV=dcpi_DEV_new, wage_GAP=wage_GAP_new, 
      u_GAP=u_GAP_new, i_UK=i_UK_new, prod_GAP=prod_GAP_new, drer=drer_new)
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
         title = "Monetary Policy Shock – Hybrid D")
plot_irf(irf_demand,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Demand Shock – Hybrid D")
plot_irf(irf_productivity,
         vars = c("Y_GAP", "dcpi_DEV", "u_GAP"),
         title = "Productivity Shock – Hybrid D")


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
  title = "Impulse responses to a monetary policy shock (Hybrid D)"
)
plot_irf_stacked(
  irf_mp_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a monetary policy shock (Hybrid D)"
)
# 1. Start with what changed (UIP activation): Exchange rate (drer)...In the second figure (top panel):
#     drer jumps immediately upward following the policy shock. It then decays smoothly. as intended by 
#     imposed UIP equation implies:   policy tightening → higher interest differential → appreciation 
#     pressure. UIP is now doing work. The response is monotone, not oscillatory. No Dornbusch-style 
#     overshooting (because UIP feeds into IS only with a lag). This confirms Hybrid D is functioning 
#     as an open‑economy extension, not a rewrite.
# 2. Does policy transmission remain intact? Policy rate (i_UK). Immediate jump on impact. Smooth decay.
#     Identical qualitative behaviour to Hybrid C. UIP did not contaminate the policy rule; Policy 
#     remains the anchor of the system. A critical pass.
# 3. Core real-side transmission: still textbook? 
#     Output (Y_GAP): Immediate contraction; Trough around 5–7 quarters. Gradual recovery. The shape and 
#       timing are virtually unchanged relative to Hybrid C, except: the contraction is slightly deeper 
#       and more persistent; as expected when an appreciation channel is added: tighter policy → stronger 
#       currency → weaker net exports → slightly larger output hit. Not distortion but realism? The sign 
#       ordering and monotonicity are preserved?
#     Unemployment (u_GAP): Gradual rise. Peak after output trough. Smooth convergence. Again, almost 
#       identical to Hybrid C but marginally amplified. Okun’s law remains intact. No labour-market 
#       feedback into policy or UIP.
# 4. Inflation (dcpi_DEV): still anchored. Inflation shows: Gradual disinflation. No overshoot. No oscillation. 
#     Two important points here: Inflation responds more slowly than activity, as should be implied by our 
#     Phillips Curve structure. The exchange-rate channel does not cause abrupt disinflation — because 
#     you have not hard-wired import price pass-through into inflation. Design choice for policy simulation - 
#     TO BE EXPLORED/ RELAXED LATER. Inflation remains well behaved. UIP does not “blow up” prices.
# 5. Secondary channels: consistent. 
#       Productivity (prod_GAP): Small negative dip. Smooth recovery. This is consistent with:  weaker demand → 
#         lower utilisation → productivity dip. UIP nudges this channel mildly, but does not destabilise it.
#       Wages (wage_GAP): Slight rise. Then slow decline. Consistent with:  delayed labour-market adjustment; 
#         weaker demand putting gradual pressure on wages. IMPORTANTLY, crucially: Wage dynamics do not amplify 
#         the UIP channel; fix of wage persistence holds.
# 6. The key question: did UIP break Hybrid C discipline? Arguably not: Correct signs✅Monotonicity✅No policy ↔ UIP 
#     feedback loop✅Output still demand-led✅Labour market downstream✅Inflation anchored✅  
#     Hybrid D has added one extra transmission channel (exchange rate), and nothing else.
# 7. How to interpret Hybrid D vs Hybrid C. Hybrid C: closed-economy, core policy transmission; 
#     Hybrid D: open-economy policy transmission with UIP. Hybrid D IRFs are: slightly stronger, slightly 
#     longer-lasting, but structurally identical in shape. Healthy “small open economy” behaviour.
# TENTATIVE CONCLUSION: Activating the exchange-rate channel through an imposed interest-parity condition 
#     strengthens and prolongs the contractionary effects of a monetary tightening, while preserving monotone 
#     adjustment and the qualitative transmission mechanisms of the closed-economy benchmark.

irf_demand_tidy <- irf_to_tidy(irf_demand, "Demand shock")

plot_irf_stacked(
  irf_demand_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a demand shock (Hybrid D)"
)
plot_irf_stacked(
  irf_demand_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a demand shock (Hybrid D)"
)
# 1. Anchor first: the demand shock itself (Y_GAP). Y_GAP jumps up sharply on impact. Then decays 
#     monotonically toward baseline. No overshoot above steady state. No medium‑run undershoot. Exogenous 
#     demand impulse → immediate expansion → gradual dissipation. The core demand mechanism is intact.
# 2. Monetary policy reaction (i_UK): endogenous and stabilising. i_UK responds gradually, not on impact.
#     It rises steadily as inflation and activity increase. No reversal or oscillation. Likely meaning, 
#     the Taylor‑rule block behaving exactly as in Hybrid C; UIP activation has not fed back into policy
#     inappropriately. Policy reacts to domestic conditions, not directly to the exchange rate. This is 
#     exactly the separation intended in Hybrid D.
# 3. Inflation (dcpi_DEV): demand‑driven and anchored. Inflation rises smoothly. Peaks with a lag. Slowly 
#     declines (but does not overshoot downward). Consistent with calibrated Phillips curve, strong 
#     inflation persistence, demand pressure as the dominant channel.  Crucially: Inflation is not 
#     destabilised by UIP; There is no sharp disinflation via appreciation; That tells you: the exchange‑
#     rate channel is operating primarily through quantities, not prices — by design. Feature of policy‑
#     simulation model.
# 5. Exchange rate (drer): now active, but disciplined. drer appreciates gradually. The response is monotone.
#     No overshooting, no oscillation. Hybrid D UIP implementation implies: interest‑rate differential drives 
#     the exchange rate, but only one‑way, with no contemporaneous feedback into output or inflation. The 
#     appreciation is delayed relative to the initial demand shock. That delay is intentional, because IS uses
#     lagged drer. So the narrative is: demand expands → policy tightens → exchange rate appreciates → dampens 
#     demand over time. A clean small‑open‑economy channel?
# 6. Productivity (prod_GAP): secondary but sensible. Productivity: Initial rise 
#     (utilisation effect). Gradual decay, which is standard: stronger demand → higher utilisation → 
#     temporary productivity gain.
#    Wages (wage_GAP): secondary but sensible. Real wages fall (or rise less than productivity). Then recover 
#     slowly. Consistent with: labour slack tightening, delayed wage bargaining, no wage‑price spiral. IMPORTANTLY,
#     Wage dynamics do not amplify the UIP channel; fix to wage persistence continues to hold.
# 7. Crucial comparison: Hybrid C vs Hybrid D (demand shock). What changed when you activated UIP? Output response: 
#     slightly damped and more persistent; Policy response: very similar. Inflation response: nearly unchanged. 
#     Exchange rate: now clearly active. As expected when adding open‑economy channel:  same core domestic dynamics, 
#     plus an external stabiliser. 

irf_productivity_tidy <- irf_to_tidy(irf_productivity, "Productivity (boost) shock")
plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("Y_GAP", "u_GAP", "dcpi_DEV", "i_UK"),
  title = "Impulse responses to a productivity boost (Hybrid D)"
)

plot_irf_stacked(
  irf_productivity_tidy,
  variables = c("wage_GAP", "prod_GAP", "drer"),
  title = "Impulse responses to a productivity boost (Hybrid D)"
)

# Nominals are not “unstable” in a pathological sense — but they are doing something that is inconsistent 
#   with a “pure favourable technology shock” once UIP is active - interaction‑driven nominal drift, not 
# 1. The real side is doing exactly the right thing: Real economy responses; Productivity (prod_GAP): sharp 
#     positive jump, quick decay. Output (Y_GAP): rises and settles above baseline. Unemployment (u_GAP): falls
#     monotonically. Wages (wage_GAP): fall sharply then recover. This is textbook favourable productivity/
#     efficiency behaviour. No concern on the real side. So if something looks odd, it must be coming from 
#     the nominal–external interaction, not from mis‑specified supply dynamics.
# 2. What exactly looks concerning in the nominals? There are two features that trigger the instinctive “hm” 
#     reaction: Inflation (dcpi_DEV) drifts up persistently; Policy (i_UK) keeps tightening instead of stabilising
#     Exchange rate (drer) appreciates monotonically without turning. None of these explode, but together they 
#     fail to close the nominal loop. This is an important diagnostic.
# 3. Why this is not instability. This is NOT instability because no variable diverges explosively; No oscillation 
#     or alternating sign behaviour; All paths are smooth and bounded; Persistence parameters are < 1. 
# 4. What is happening: nominal drift caused by one‑way UIP coupling. The correct diagnosis is:
#     Hybrid D has introduced a one‑way open‑economy nominal channel that is not closed by any countervailing
#     nominal mechanism. 
#     Step A: Productivity shock → output rises. This raises Y_GAP. Working fine. 
#     Step B: Output enters the Phillips curve positively (by construction). So: productivity → output → inflation 
#       pressure. Even though marginal costs fall, the only explicit inflation driver is demand. This already biases 
#       inflation upwards. 
#     Step C: Taylor rule sees inflation and output and tightens. So rising inflation/output → rising interest rates.
#       This is policy responding as designed.
#     Step D: UIP forces appreciation every time policy tightens. There is nothing in the system that makes 
#       importance of the interest differential go away because: foreign rate is fixed at zero, risk premia are 
#       absent, expectations are not modelled, UIP coefficient is imposed, not mean‑reverting. So: as long as i_UK 
#       is above baseline, drer keeps drifting. Not a bug — what an unconditional UIP identity does.
#     Step E: Appreciation does not close the inflation loop. This is crucial. In our model: drer feeds into IS with 
#       a lag but does not feed into inflation and only weakly into import prices (if at all); inflation is driven 
#       overwhelmingly by output and inertia. So: exchange‑rate appreciation does not reduce inflation enough to stop
#       policy tightening. That is why: dcpi_DEV drifts upward; i_UK keeps tightening, drer keeps appreciating. 
#       Not unstable — but unanchored.
# 5. Why Hybrid C did not have this problem? In Hybrid C: Exchange rate was effectively dormant; Policy → output → 
#       inflation loop was self‑contained. Inflation persistence + output decay eventually stopped tightening. 
#       Hybrid D breaks that closure on purpose, by letting UIP in. But it does so without adding a nominal 
#       closure mechanism.
# 6. This is exactly the boundary where Hybrid D should stop. This is the key conceptual point: Hybrid D was meant 
#       to “wake up” the exchange rate for policy transmission — not to become a full open‑economy nominal model.
#       IRFs are telling you: “If you wake up UIP without also waking up nominal offsetting channels, the price
#       system won’t close.” That’s not a mistake — it’s a diagnostic signal.
# 7. What would be required to “fix” this (but should NOT go into Hybrid D). To make inflation fall or stabilise 
#       after a productivity shock, you would need at least one of: Import‑price pass‑through from drer into inflation;
#       A risk‑premium term in UIP that mean‑revert; Forward‑looking expectations in inflation or policy; A 
#       productivity / marginal‑cost term in the Phillips curve.
#     All of those are Hybrid E territory, not Hybrid D. Trying to bolt them onto Hybrid D would: destroy its clean 
#       purpose, reintroduce oscillations, blur interpretation.
# With an active UIP channel and no nominal offsetting mechanisms, favourable productivity improvements can generate
#   persistent appreciation and policy tightening driven by demand‑led inflation pressure. not wrong — it is a partial
#   ‑equilibrium open‑economy result.
# Hybrid D is: excellent for policy shocks; excellent for demand shocks; not sufficient for full supply‑side nominal 
#   analysis. You already anticipated this — correctly — when you said: “I will set up Hybrid E later.”
# When a favourable productivity shock is combined with an imposed UIP condition, inflation and interest rates 
#   exhibit persistent adjustment reflecting the absence of explicit nominal offsetting channels. This behaviour 
#   motivates a further extension to incorporate import‑price pass‑through and expectations mechanisms.