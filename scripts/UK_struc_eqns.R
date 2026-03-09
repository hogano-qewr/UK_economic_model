



# Following Scarth (1988), we need the cyclical component (Output Gap)
uk_model_df <- gdp_raw %>%
  mutate(log_gdp = log(value)) %>%
  mutate(gdp_gap = mFilter::hpfilter(log_gdp, freq = 1600)$cycle)

# Define the structural equations
eq_IS      <- y_gap ~ lag(y_gap, 1) + real_rate + real_exchange_rate
eq_Phillips <- inflation ~ lag(inflation, 1) + y_gap + import_price_growth
eq_Taylor   <- bank_rate ~ lag(bank_rate, 1) + inflation + y_gap

system <- list(IS = eq_IS, Phillips = eq_Phillips, Taylor = eq_Taylor)

# Estimate using 3SLS (identifying via instrumental variables)
fit_structural <- systemfit(system, method = "3SLS", data = uk_data)
summary(fit_structural)
