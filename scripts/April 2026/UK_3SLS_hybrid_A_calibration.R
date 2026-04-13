# “Earlier work on the hybrid specifications calibrated selected Phillips‑curve slopes to guard against 
#   simultaneity bias and weak identification. Fully‑free 3SLS estimation on the revised dataset produces
#   slope estimates consistent with these calibrations, indicating that the calibrated values anticipated
#   the equilibrium restrictions imposed endogenously by the full system.”
# This frames Hybrid‑A as: a diagnostic scaffold, not a shortcut; an informed prior, not a dogma.


########## HYBRID A: FIRST-STAGE CALIBRATION (PC and WPC slopes) #################################
# IS curve
eq_IS <- Y_GAP ~ 
  Y_GAP_L1 + r_GAP + i5y_GAP + dNX + drer_L1

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
  wage_GAP_L1 + prod_GAP_L1 +
  r_GAP + i5y_GAP +
  drer_L1 +
  bpp_BEG + 
  bpu1_GFC + bpu2_POST

fit_3sls_HA <- systemfit(
  system_eqs_HA,
  method = "3SLS",
  inst = inst_3sls,
  data = MODEL_READY
)

summary(fit_3sls_HA)

# “”


# 1. System‑level results: very strong. Overall fit: McElroy R² ≈ 0.91 → slightly higher than fully-free
#     It tells you joint dynamics are well captured. detRCov ≈ 0.008 a good bit lower than fully-free → 
#     Low determinant = strong cross‑ equation correlation being exploited even more efficiently. 
#   Importantly:  The system clearly gains efficiency relative to fully free 3SLS estimation.

# 2. Equation‑by‑equation interpretation
#      (1) IS curve: Key results: Lagged output softens: 0.62*** Real rate gap softened: +0.68***, Long‑rate spread: 
#           now insignificant. The dominance of the lagged output term (0.62) suggests that UK 
#           demand is characterized more by momentum and 'stickiness' than by high sensitivity to immediate market 
#           rate spreads. But this is a brave conclusion from such a simplistic model. Net exports and exchange rate
#           insignificant.  The positive real‑rate coefficient once again reflects policy reaction endogeneity, not
#           causal stimulus. 
#      (2) Price Phillips Curve (PC). Fully‑free system estimation reveals that contemporaneous Phillips‑curve 
#           slopes are not well identified once policy feedback is internalised. We therefore adopt a hybrid 
#           approach in which these slopes are calibrated to preserve economically meaningful transmission, 
#           while allowing the remainder of the system to be freely estimated. The resulting Hybrid‑A specification 
#           improves interpretability without compromising fit or stability.Key results: 
#           Inflation persistence has increased (dcpi_DEV_L1 ≈ 0.92***); Import prices insignificant; Energy prices 
#           significant. Inflation regime dummy not significant. Interpretation: persistence rose. The regime 
#           dummy again plays an organisational role, not a shock‑absorbing one. 
#      (3) Wage Phillips Curve (WPC). Key results:
#           Wage persistence rises slightly to ~0.85***. This is stable and means wage_GAP is now naturally mean-reverting.
#           implies that wage shocks have a half-life of about 4-5 quarters "in the absence of further shocks," as the
#           feedback loop from the Taylor Rule and Okun's Law will influence the actual recovery time in a full 
#           simulation. This is highly realistic for UK collective bargaining and salary review cycles. 
#           Inflation pass‑through remains insignificant; The prod_GAP 
#           coefficient is -0.21* and significant. Why this is actually an "Efficiency Squeeze"  This negative sign 
#           is common in UK data and represents Labor Hoarding or Real Wage Rigidity: The Drag: When productivity 
#           drops, firms are less efficient, but wages don't adjust downward instantly (they are "sticky").
#           The "Gap": Because wages stay high while productivity is low, the Wage Gap increases.
#           The Risk: From a policy perspective, this is a stagflationary signal. It means firms are facing higher 
#           unit labor costs (paying the same for less output), which eventually forces them to raise prices, 
#           feeding into your inflation equation. So, instead of a "self-correcting" mechanism, this negative sign
#           actually represents a structural friction in the UK economy where wages fail to "give" when productivity
#           slows down.
# A BIT MORE ON THIS: If we lived in a textbook world of perfect competition, wages would fall one-for-one with 
#           productivity. But in the real UK economy, you have Real Wage Rigidity. When productivity slumps 
#           (as it has notoriously done in the UK since 2008), workers and unions try to maintain their "real" 
#           take-home pay. Why this is a "Credibility Win" for your paper: The Unit Labour Cost (ULC) Channel: 
#           Because your productivity coefficient is negative, a drop in productivity widens the wage gap. This 
#           effectively models an increase in Unit Labour Costs. For a DSGE model, this is the "holy grail" of 
#           supply-side inflation stories. Explaining the "UK Productivity Puzzle": Your model suggests that when
#           the UK has a productivity "lost decade," it doesn't just lower growth—it creates structural inflationary
#           pressure because wages don't "flex" downward to match the lower output per worker. Policy Realism: This
#           makes your Taylor Rule response even more interesting. The Bank of England has to be more aggressive
#           because the labor market isn't doing any of the "cooling" for them when productivity shocks hit.
#           The "Golden Thread" of your 4 Hybrids: By the time you get to Hybrid D, you are telling a sophisticated
#           story: Demand is well-behaved (Hybrid B/C). Exchange Rates help the BoE (Hybrid D). BUT, the Labor 
#           Market is a source of friction (Audited WPC), where productivity slumps actually make the inflation 
#           fight harder because of wage stickiness.
#           Interesting narrative reflecting structural reality of the UK’s low-growth, high-cost trap. 
#             By the time you present Hybrid D, you’ve built a model
#             that effectively says: Monetary policy works through the exchange rate and demand, but it has to fight 
#             against a rigid labor market. When productivity fails, the economy doesn't just "reset"—it gets more 
#             expensive because wages are sticky. This makes your Stage 2: Preliminary Forecasting much more than 
#             just a statistical exercise; it becomes a test of how the UK economy recovers when the "supply side"
#             (productivity) is the thing holding it back. [NEED COMMENTARY ON LATER HYBRIDS...]

#      (4) Taylor Rule (TR). Key results: Smoothing ~0.97***; Inflation response positive and significant; Output 
#           gap marginal but positive. Interpretation: This is extremely clean and stable. The policy rule absorbs 
#           system feedback exactly as expected. Strong confirmation that no policy‑rule breaks are needed.
#      (5) Okun’s Law Key results:  Persistence down a little ~0.62***; Output gap strongly negative, GFC and post‑2013 dummies 
#           both significant with opposite signs. Interpretation: This block remains one of the strongest in the 
#           system. Structural labour‑market regimes are clearly identified. Breaks survive full system estimation 
#           — a very strong validation. This fully justifies your unemployment break strategy.
#      (6) Productivity (OLP). Key results: Mild persistence down a bit 0.16*; Output gap positive and significant; Low R². 
#           Interpretation: Productivity behaves as expected: noisy, utilisation‑driven. No structural changes 
#           emerge. Exactly right.
#      (7) UIP / exchange rate. Key results:  Interest differential large and significant but a bit lower (−2.75**). 
#           The Empirical Trap: the data is trying to tell the model that higher interest rates lead to a weaker 
#           currency. This is the exchange-rate version of the "price puzzle." The Logic: Without the Hybrid D 
#           restriction, the 3SLS is simply picking up the fact that during periods of sterling weakness (like the 
#           post-Brexit vote or the 2022 energy crisis), the Bank of England was forced to raise rates to defend the
#           pound or fight imported inflation. The estimator sees "Rates Up + Currency Down" and assumes a negative
#           causal link. The Hybrid D Solution: By moving it to the LHS as drer - 0.1 * i_DIFFL_GAP, you effectively 
#           told the model: "I don't care what the messy historical correlation says; structurally, a rate hike must
#           support the currency." 
#         "In the freely estimated components of Hybrid A, the interest rate differential exhibits a significant 
#             negative coefficient (-2.75). Rather than a causal link, this reflects the historical 'defense of 
#             the currency' and the BoE's reaction to imported inflationary shocks. To ensure the model is suitable 
#             for forward-looking policy simulation, we transition to Hybrid D, which imposes a theoretically 
#             consistent positive linkage (appreciation) between the policy rate and the real exchange rate."
#           Everything else weak or insignificant. Very low R² but a bit better than fully-free. Interpretation: UIP 
#           becomes “sharper” under 3SLS because policy, inflation, and output are jointly determined. 
# 3. Residual covariance and correlation structure:  Notable residual 
#     correlations: IS ↔ Okun: 0.71 (stronger than fully-free); IS ↔ WPC: 0.41 (stronger); WPC ↔ OLP: 0.48 (stronger); 
#     TR ↔ UIP: 0.42 (same; PC now negatively correlated with all real‑side blocks. This is good news and as it 
#     should be? Yes, in a sound model, an unexplained "upward" shock to inflation (PC residual) should typically 
#     correlate with a "downward" move in the output gap (IS residual) if it’s a supply-side shock. It shows the 
#     model is correctly separating demand-pull from cost-push dynamics.



#### PLOT FIT OF MODELLED IS AND PC AGAINST ACTUALS FOR Y_GAP AND dcpi_DEV ######################################


library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df1 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$Y_GAP),
  Fitted = as.numeric(fitted(fit_3sls_HA)$IS)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df1, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Output Gap: Actual vs. 3SLS System Fit - Hybrid A",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Output Gap") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()

# Hybrid A: The "Stabilizer": The Change: Once you imposed the Phillips Curve and Wage constraints, the output 
#   gap fit "smoothed out." Observation: Notice the 2011–2018 period. The red line is much more "level." It’s no 
#   longer trying to explain every tiny wiggle in the output gap using inflation data that doesn't actually 
#   correlate. Credibility: This is a more honest representation of the UK's "Potential Output." It acknowledges 
#   that much of the output gap volatility in that decade was likely driven by shocks the model isn't designed to 
#   "guess" (like Brexit uncertainty or fiscal austerity).



library(zoo)
library(ggplot2)

# 1. Create the data frame and convert the 'Quarterly' string to a Date object
plot_df2 <- data.frame(
  # as.yearqtr converts "2020 Q1" to a numeric year/quarter
  # as.Date then turns it into the first day of that quarter
  Date = as.Date(as.yearqtr(MODEL_READY$date)), 
  Actual = as.numeric(MODEL_READY$dcpi_DEV),
  Fitted = as.numeric(fitted(fit_3sls_HA)$PC)
)

# 2. Plot using the Date-aware x-axis
ggplot(plot_df2, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Fitted), color = "red", size = 1) +
  labs(title = "UK Inflation: Actual vs. 3SLS System Fit - Hybrid A",
       subtitle = "Black = Actual data | Red (Dashed) = Model Fit",
       x = "Year",
       y = "Inflation deviation from 2%") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal()

# "As illustrated in the Hybrid A System Fit, the model successfully captures the broad inflationary 
#     regimes of the last 25 years. While a fully-free estimation might offer marginal gains in in-sample fit, 
#     Hybrid A preserves the 'structural integrity' of the price-discovery process. By anchoring the Phillips
#     Curve slope, we prevent the model from over-attributing the 2022-23 energy-led inflation to domestic 
#     demand, resulting in a more robust foundation for policy simulation." [NOTE: EVOLUTION TO HYBRID G AND H]