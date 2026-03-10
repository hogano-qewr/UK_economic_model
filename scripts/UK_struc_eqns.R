



model_ready_df

# Estimate the IS Curve (Aggregate Demand)
# We include the lagged Output Gap to capture inertia/persistence
is_curve <- lm(y_gap ~ lag(y_gap, 1) + lag(r_rate, 1) + lag(log_er, 1) + covid_dummy, 
               data = model_ready_df)

summary(is_curve)

# Estimate the Phillips Curve (Aggregate Supply)
# We include lagged inflation to represent 'Expectations' (Adaptive/Inertial)
phillips_curve <- lm(pi ~ lag(pi, 1) + y_gap + lag(log_er, 1) + covid_dummy, 
                     data = model_ready_df)

summary(phillips_curve)

library(ggplot2)

library(ggplot2)

# Ensure the interest rate is strictly numeric
model_ready_df <- model_ready_df |>
  mutate(r_rate = as.numeric(r_rate))

# 1. Plotting with Crisis Points Highlighted
ggplot(model_ready_df, aes(x = lag(r_rate, 1), y = y_gap)) +
  # Points colored by whether they are 'Normal' or 'COVID/Crisis'
  geom_point(aes(color = as.factor(covid_dummy)), alpha = 0.7, size = 2) +
  # Add a regression line ONLY for the non-crisis data
  geom_smooth(data = filter(model_ready_df, covid_dummy == 0), 
              method = "lm", color = "red", se = TRUE) +
  scale_color_manual(values = c("steelblue", "orange"), 
                     labels = c("Normal UK Economy", "COVID Shock"),
                     name = "Period") +
  labs(title = "UK IS Curve: The 'Clean' Structural Relationship",
       subtitle = "Red line shows the negative slope expected by Scarth (1988)",
       x = "Real Interest Rate (%)",
       y = "Output Gap") +
  theme_minimal()



# Pull 5-year Gilt Yield (Market-based interest rate)
gilt_5y <- tidyquant::tq_get("IRLTLT01GBM156N", get = "economic.data", from = "1997-01-01") |>
  mutate(Date = zoo::as.yearqtr(date)) |>
  group_by(Date) |>
  summarise(market_rate = mean(price)) |>
  mutate(Date = format(Date, "%Y Q%q"))

# Join to your df
model_ready_df <- model_ready_df |>
  inner_join(gilt_5y, by = "Date") |>
  mutate(r_market = market_rate - pi)

# Re-run the IS Curve with the Market Rate
is_curve_market <- lm(y_gap ~ lag(y_gap, 1) + lag(r_market, 1) + lag(log_er, 1) + covid_dummy, 
                      data = model_ready_df)
summary(is_curve_market)


# DURBAN-WATSON TEST ON PHILLIPS CURVE

dwtest(phillips_curve)

# Serial Correlation (Higher Order): Use the Breusch-Godfrey test which is more robust than 
#   Durbin-Watson for models with lagged dependent variables.

bgtest(phillips_curve)

# Heteroscedasticity: Use bptest() to check for non-constant variance. If found, use Robust Standard Errors 
#  (e.g., the sandwich package in R).

bptest(phillips_curve)

# New model with two lags of inflation
phillips_curve_v2 <- lm(pi ~ lag(pi, 1) + lag(pi, 2) + y_gap + lag(log_er, 1) + covid_dummy, 
                        data = model_ready_df)

summary(phillips_curve_v2)

# Re-run the BG test to see if that p-value (0.065) improves
bgtest(phillips_curve_v2)

library(tseries)

# Test the level of inflation
adf.test(model_ready_df$pi, alternative = "stationary")

# p-value = 0.01 < 0.05

library(sandwich)
library(lmtest)

# Get robust p-values that account for serial correlation (HAC)
coeftest(phillips_curve_v2, vcov = vcovHAC)

# New model with squared output gap term
# Note: I(y_gap^2) tells R to treat it as a math operation, not an interaction
phillips_nonlinear <- lm(pi ~ lag(pi, 1) + lag(pi, 2) + y_gap + I(y_gap^2) + 
                           lag(log_er, 1) + covid_dummy, data = model_ready_df)

# View results with robust standard errors
library(sandwich)
library(lmtest)
coeftest(phillips_nonlinear, vcov = vcovHAC)


### adding Imports of goods and services deflator:SA

# Process the Imports deflator (ONS series YBFZ)
IM_defl <- series_100326  |> 
  dplyr::select(Date = 1, IM_defl = `Imports of goods and services deflator:SA`)    # Select 1st col as Date and ABMI

view(IM_defl)
