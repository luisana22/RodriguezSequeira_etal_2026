## Monte carlo simulation of data points to explore uncertainty

library(tidyverse)
library(knitr)

setwd("/Users/pooirodriguez/Manuscript_Codes/RScripts and files")

trials <- 1000000
norm.sims <- rnorm(trials, mean = 1, sd = 10)
sum(norm.sims >= 3 & norm.sims <= 6)/trials


# my data ####
#log10(y) = 1.27 + 0.04 *  x
#with avgtotal: #0.5548 #log10(avgtotal) 0.6198 #log10(turb) 0.4235 #log-log 0.5696
y <- log10(winterKent$AVGTOTAL)
x <- (winterKent$Turbidity_SW)
modelData <- data.frame(x,y)
plot(modelData)
model1 <- lm((y) ~ (x), data = modelData)
summary(model1)

### Example equation — replace this with your real one
equation <- function(X) {
  10^(1.25 +0.04* X)
}


# Sample uncertain inputs
# Monte Carlo simulation for MP prediction from turbidity
set.seed(123)

# number of simulations
N <- 50000  

# --- Define uncertain parameters ---

# Example assumptions (you can adjust these):
# Turbidity follows a normal distribution with mean 30 NTU and sd 5
# Regression coefficients have some uncertainty (from model fit)
turbidity_mean <- 8.651333
turbidity_sd   <- 7.369931

intercept_mean <- 1.248757
intercept_sd   <- 0.070096   # adjust if you know regression SE
slope_mean     <- 0.035326
slope_sd       <- 0.006256   # adjust if you know regression SE

# --- Sample from distributions ---
Turbidity <- rnorm(N, turbidity_mean, turbidity_sd)
Intercept <- rnorm(N, intercept_mean, intercept_sd)
Slope     <- rnorm(N, slope_mean, slope_sd)

# --- Calculate predicted MP/L ---
log_MP <- Intercept + Slope * Turbidity
MP <- 10^log_MP  # convert from log10 scale

# --- Summaries ---
summary_stats <- c(
  mean = mean(MP),
  median = median(MP),
  sd = sd(MP),
  `2.5%` = quantile(MP, 0.025),
  `97.5%` = quantile(MP, 0.975)
)
print(summary_stats)

# --- Plot distribution ---
hist(MP, breaks = 80, col = "skyblue",
     main = "Predicted Microplastic Concentration (MP/L)",
     xlab = "MP/L", freq = FALSE)
lines(density(MP), col = "darkblue", lwd = 2)

# --- Convergence check ---
running_mean <- cumsum(MP) / seq_along(MP)
plot(running_mean, type = "l", col = "darkgreen",
     main = "Convergence of Running Mean",
     xlab = "Iteration", ylab = "Running Mean of MP/L")

# --- Sensitivity (Spearman correlation) ---
sens <- data.frame(
  variable = c("Turbidity", "Intercept", "Slope"),
  rho = c(
    cor(Turbidity, MP, method = "spearman"),
    cor(Intercept, MP, method = "spearman"),
    cor(Slope, MP, method = "spearman")
  )
)
print(sens[order(abs(sens$rho), decreasing = TRUE), ])

sens[order(abs(sens$rho), decreasing = TRUE), ]

#to check boundaries:
#data <- data.frame(Turbidity, MP)
#cropped_data <- subset(data, Turbidity >= 0 & Turbidity <= 30)

plot(Turbidity, MP, xlim = c(0, 30), ylim = c(0, 500), pch = 19, cex = 0.3, col = rgb(0,0,1,0.3),
     main = "Turbidity vs MP/L", xlab = "Turbidity", ylab = "MP/L")

#"For the observed turbidity range, predicted MP concentrations have a 
# median value of 39.84 MP/L and a 95% uncertainty interval of 8.41 to 234.62 MP/L."
#"Turbidity was the primary driver of uncertainty (Spearman ρ = 0.94), 
# followed by uncertainty in the intercept estimate (ρ = 0.23).”
#"At a given turbidity value, predictions show substantial dispersion 
# (heteroscedasticity) due to uncertainty in the regression parameters."


# packages
library(MASS)    # for mvrnorm
library(ggplot2)

# grid of turbidity values where we want bands
turb_grid <- seq(0, 30, by = 0.2)

# number of MC draws for bands
nsim <- 5000

# draw coefficients jointly (intercept, slope) using fitted model covariance
coefs_mean <- coef(model1)                # c("(Intercept)", "x")
coefs_vcov <- vcov(model1)                # 2x2 covariance matrix

set.seed(123)
coefs_draws <- mvrnorm(n = nsim, mu = coefs_mean, Sigma = coefs_vcov)
# coefs_draws is nsim x 2: column1 = intercept, column2 = slope

# compute predictions on the log10 scale for each draw and each turbidity in grid
# result: matrix nsim x length(turb_grid)
logpred_mat <- sapply(turb_grid, function(t) coefs_draws[,1] + coefs_draws[,2] * t)

# convert to MP/L (real units)
pred_mat <- 10^logpred_mat

# get median and 95% interval at each turbidity
median_pred <- apply(pred_mat, 2, median)
lower_pred  <- apply(pred_mat, 2, quantile, probs = 0.025)
upper_pred  <- apply(pred_mat, 2, quantile, probs = 0.975)

# make a dataframe for plotting
df_band <- data.frame(
  turb = turb_grid,
  median = median_pred,
  lower  = lower_pred,
  upper  = upper_pred
)

# observed points (convert back to MP/L)
obs <- data.frame(turb = modelData$x, MP = 10^modelData$y)

# ggplot with ribbon (95% band) and median line
ggplot() +
  geom_point(data = obs, aes(x = turb, y = MP), alpha = 0.4, size = 2) +
  geom_ribbon(data = df_band, aes(x = turb, ymin = lower, ymax = upper), alpha = 0.25) +
  geom_line(data = df_band, aes(x = turb, y = median)) +
  coord_cartesian(xlim = c(0,30)) +
  labs(x = "Turbidity", y = "Predicted MP/L",
       title = "Prediction (Monte Carlo) band: 95% uncertainty",
       subtitle = "Median (line) and 95% interval (ribbon)") 

turb_grid <- seq(0, 30, by = 0.1)

# For each turbidity on the grid, simulate predictions
predict_MC <- replicate(5000, {
  alpha <- rnorm(1, intercept_mean, intercept_sd)
  beta  <- rnorm(1, slope_mean, slope_sd)
  10^(alpha + beta * turb_grid)
})

lower <- apply(predict_MC, 1, quantile, 0.025)
upper <- apply(predict_MC, 1, quantile, 0.975)
median_pred <- apply(predict_MC, 1, median)

plot(turb_grid, median_pred, type="l", lwd=2)
lines(turb_grid, lower, col="red")
lines(turb_grid, upper, col="red")





##take 2####
model_turb_mp   <- lm(log10(AVGTOTAL) ~ Turbidity_SW, data = winterKent)  # Eq. 1
model_ndti_turb <- lm(Turbidity_SW ~ NDTI, data = compareBD)            # Eq. 2

library(MASS)
library(ggplot2)

set.seed(123)
nsim <- 50000

# ---- Example NDTI values to predict for (could be a vector of satellite pixels)
ndti_vals <- seq(min(compareBD$NDTI, na.rm = TRUE),
                 max(compareBD$NDTI, na.rm = TRUE),
                 length.out = 50)

# ---- Extract coefficients & covariance from both models
coef_eq2 <- coef(model_ndti_turb)     # turb = c + d*NDTI
vcov_eq2 <- vcov(model_ndti_turb)
sigma_eq2 <- summary(model_ndti_turb)$sigma  # residual SD

coef_eq1 <- coef(model_turb_mp)       # log10(MP) = a + b*turb
vcov_eq1 <- vcov(model_turb_mp)
sigma_eq1 <- summary(model_turb_mp)$sigma    # residual SD

# ---- Draw coefficients jointly
draws_eq2 <- mvrnorm(nsim, mu = coef_eq2, Sigma = vcov_eq2)
draws_eq1 <- mvrnorm(nsim, mu = coef_eq1, Sigma = vcov_eq1)

# ---- Monte Carlo propagation
pred_mat <- sapply(ndti_vals, function(ndti) {
  
  # Eq.2: predict turbidity from NDTI + residual error
  turb_mc <- draws_eq2[,1] + draws_eq2[,2] * ndti +
    rnorm(nsim, 0, sigma_eq2)
  
  # Eq.1: predict log10(MP) from turbidity + residual error
  logmp_mc <- draws_eq1[,1] + draws_eq1[,2] * turb_mc +
    rnorm(nsim, 0, sigma_eq1)
  
  # back-transform
  10^logmp_mc
})

# ---- Summarize uncertainty bands
median_mp <- apply(pred_mat, 2, median)
lower_mp  <- apply(pred_mat, 2, quantile, 0.025)
upper_mp  <- apply(pred_mat, 2, quantile, 0.975)

df_band <- data.frame(
  NDTI = ndti_vals,
  median = median_mp,
  lower = lower_mp,
  upper = upper_mp
)


ggplot(df_band, aes(x = NDTI)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(y = median), linewidth = 1) +
  labs(title = "Satellite-derived MP proxy with uncertainty",
       subtitle = "95% Monte Carlo uncertainty propagated from NDTI → Turbidity → MP", #ropagation of Eq.1 (NDTI→Turb) and Eq.2 (Turb→MP)",
       x = "Satellite-derived turbidity (NDTI)",
       y = "Proxy MP/L") 
#+theme_minimal()


ggplot(df_band, aes(x = NDTI)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(y = median), linewidth = 1) +
  scale_y_log10() +
  labs(title = "Satellite-derived MP proxy with uncertainty",
       subtitle = "95% Monte Carlo uncertainty propagated from NDTI → Turbidity → MP",
       x = "NDTI",
       y = "Predicted MP/L (log10 scale)") 

