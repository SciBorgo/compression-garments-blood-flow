

# Do Sports Compression Garments Alter Measures of Peripheral Blood Flow? A Systematic Review with Meta-Analysis
# Calculate prediction intervals for figures 1 to 6

# Packages
library(dplyr)

# Load data
d <- read.csv('data.csv')

names(d)

# Prediction interval calculation
alpha <- 0.05
z_95 <- qnorm(p = 1 - alpha/2) # Z-score for calculation of standard error
t_dist_fun <- function(k,alpha){qt(p = 1 - {{alpha}}/2, df={{k}}-k_correction)} # t distribution needed for calculation of pi
k_correction <- 2

d %>% mutate(
  se = ((upper_ci-lower_ci)/(2*z_95)),
  se_square = se^2,
  k = as.integer(k),
  pooled_effect = as.numeric(pooled_effect),
  pi_lower = round(pooled_effect-t_dist_fun(k = k, alpha = alpha)*sqrt(se_square+tau_square),2),
  pi_upper = round(pooled_effect+t_dist_fun(k = k, alpha = alpha)*sqrt(se_square+tau_square),2)
)
