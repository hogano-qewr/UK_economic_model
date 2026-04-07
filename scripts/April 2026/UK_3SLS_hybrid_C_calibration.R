
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

