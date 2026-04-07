
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


