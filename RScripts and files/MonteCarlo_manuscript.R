## Monte carlo simulation of data points to explore uncertainty

library(tidyverse)
library(knitr)

setwd("/Users/pooirodriguez/Manuscript_Codes/RScripts and files")

ndti_turb  <- read_xlsx("ndti_turb.xlsx")
winterKent <- read_xlsx("winterKent.xlsx")


trials <- 1000000
norm.sims <- rnorm(trials, mean = 1, sd = 10)
sum(norm.sims >= 3 & norm.sims <= 6)/trials


# my data ####
#log10(y) = 1.25 + 0.04 *  x
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
N <- 40000 

# --- Define uncertain parameters ---

# Example assumptions (you can adjust these):
# Turbidity follows a normal distribution with mean 30 NTU and sd 5
# Regression coefficients have some uncertainty (from model fit)
turbidity_mean <- 8.651333
turbidity_sd   <- 7.369931
ndti_mean <- mean(ndti_turb$NDTI, na.rm = TRUE)
ndti_sd <- sd(ndti_turb$NDTI, na.rm = TRUE)

intercept_mean <- 1.248757
intercept_sd   <- 0.070096   # adjust if you know regression SE
slope_mean     <- 0.035326
slope_sd       <- 0.006256   # adjust if you know regression SE

# --- Sample from distributions ---
Turbidity <- rnorm(N, turbidity_mean, turbidity_sd)
Intercept <- rnorm(N, intercept_mean, intercept_sd)
Slope     <- rnorm(N, slope_mean, slope_sd)
NDTI      <- rnorm(N, ndti_mean, ndti_sd)
  
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
data <- data.frame(Turbidity, MP)
cropped_data <- subset(data, Turbidity >= 0)

plot(MP ~ Turbidity, data = cropped_data, xlim = c(0, 30), ylim = c(0, 500), pch = 19, cex = 0.5, col = rgb(0,0,1,0.3),
     main = "Turbidity vs MP/L", xlab = "Turbidity", ylab = "MP/L")



#######Claude
# --- Enhanced Convergence Plot ---
checkpoints <- seq(100, N, by = 100)

running_mean   <- numeric(length(checkpoints))
running_median <- numeric(length(checkpoints))
running_q025   <- numeric(length(checkpoints))
running_q975   <- numeric(length(checkpoints))

for (i in seq_along(checkpoints)) {
  mp_sub <- MP[1:checkpoints[i]]
  running_mean[i]   <- mean(mp_sub)
  running_median[i] <- median(mp_sub)
  running_q025[i]   <- quantile(mp_sub, 0.025)
  running_q975[i]   <- quantile(mp_sub, 0.975)
}

# Plot
plot(checkpoints, running_mean, type = "l", col = "darkblue", lwd = 2,
     ylim = range(c(running_q025, running_q975)),
     main = "Monte Carlo Convergence",
     xlab = "Number of Iterations", ylab = "MP/L")
lines(checkpoints, running_median, col = "darkgreen", lwd = 2)
lines(checkpoints, running_q025,  col = "firebrick",  lwd = 1.5, lty = 2)
lines(checkpoints, running_q975,  col = "firebrick",  lwd = 1.5, lty = 2)
legend("topright",
       legend = c("Mean", "Median", "2.5th percentile", "97.5th percentile"),
       col    = c("darkblue", "darkgreen", "firebrick", "firebrick"),
       lwd    = c(2, 2, 1.5, 1.5),
       lty    = c(1, 1, 2, 2))





par(mfrow = c(3, 1))

# -------------------------------------------------------
# PLOT 1: Field Input - Turbidity only
# -------------------------------------------------------
cum_turb <- cumsum(Turbidity)[checkpoints] / checkpoints

normalise_centred <- function(cum_vec, full_vec) {
  centre <- mean(full_vec)
  spread <- max(abs(cum_vec - centre))
  0.5 + (cum_vec - centre) / (2 * spread)
}

cum_turb_n <- normalise_centred(cum_turb, Turbidity)

col_turb <- "darkorange"

par(mar = c(4, 10, 4, 2))

plot(checkpoints, cum_turb_n,
     type = "l", col = col_turb, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Field Input Cumulative Mean",
     xlab = "Number of Iterations", ylab = "")
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

centre_turb <- mean(Turbidity)
spread_turb <- max(abs(cum_turb - centre_turb))
axis(2, at = c(0, 0.5, 1),
     labels = round(c(centre_turb - spread_turb,
                      centre_turb,
                      centre_turb + spread_turb), 4),
     col = col_turb, col.axis = col_turb,
     las = 1, tick = TRUE, cex.axis = 0.75)
mtext("Turbidity (NTU)", side = 2, line = 3.5,
      col = col_turb, cex = 0.65)

# -------------------------------------------------------
# PLOT 2: Model Parameters - Intercept and Slope
# -------------------------------------------------------
cum_intercept <- cumsum(Intercept)[checkpoints] / checkpoints
cum_slope     <- cumsum(Slope)[checkpoints]     / checkpoints

cum_intercept_n <- normalise_centred(cum_intercept, Intercept)
cum_slope_n     <- normalise_centred(cum_slope,     Slope)

col_int <- "purple"
col_slp <- "darkgreen"

par(mar = c(4, 16, 4, 2))

plot(checkpoints, cum_intercept_n,
     type = "l", col = col_int, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Model Parameter Cumulative Means",
     xlab = "Number of Iterations", ylab = "")
lines(checkpoints, cum_slope_n, col = col_slp, lwd = 2)
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

draw_axis_segment <- function(raw_cum_vec, full_vec, colour, line_offset, label) {
  centre <- mean(full_vec)
  spread <- max(abs(raw_cum_vec - centre))
  lo  <- centre - spread
  hi  <- centre + spread
  axis(2, at = c(0, 0.5, 1),
       labels = round(c(lo, centre, hi), 4),
       col = colour, col.axis = colour,
       las = 1, line = line_offset, tick = TRUE,
       cex.axis = 0.75)
  mtext(label, side = 2, line = line_offset + 3.5,
        col = colour, cex = 0.65, las = 0)
}

draw_axis_segment(cum_intercept, Intercept, col_int, line_offset = 0, label = "Intercept")
draw_axis_segment(cum_slope,     Slope,     col_slp, line_offset = 5, label = "Slope")

legend("topright",
       legend = c("Intercept", "Slope"),
       col = c(col_int, col_slp),
       lwd = 2, cex = 0.75, bty = "n")

# -------------------------------------------------------
# PLOT 3: Output - MP/L
# -------------------------------------------------------
cum_MP <- cumsum(MP)[checkpoints] / checkpoints

centre_mp <- mean(MP)
spread_mp <- max(abs(cum_MP - centre_mp))
cum_MP_n  <- 0.5 + (cum_MP - centre_mp) / (2 * spread_mp)

col_mp <- "darkblue"

par(mar = c(4, 10, 4, 2))

plot(checkpoints, cum_MP_n,
     type = "l", col = col_mp, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Output Cumulative Mean (MP/L)",
     xlab = "Number of Iterations", ylab = "")
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

axis(2, at = c(0, 0.5, 1),
     labels = round(c(centre_mp - spread_mp,
                      centre_mp,
                      centre_mp + spread_mp), 2),
     col = col_mp, col.axis = col_mp,
     las = 1, tick = TRUE, cex.axis = 0.75)
mtext("MP/L", side = 2, line = 3.5, col = col_mp, cex = 0.65)

# -------------------------------------------------------
# Reset
# -------------------------------------------------------
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2))



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
model_ndti_turb <- lm(Turbidity_SW ~ NDTI, data = ndti_turb)            # Eq. 2

library(MASS)
library(ggplot2)

set.seed(123)
nsim <- 50000

# ---- Example NDTI values to predict for (could be a vector of satellite pixels)
ndti_vals <- seq(min(ndti_turb$NDTI, na.rm = TRUE),
                 max(ndti_turb$NDTI, na.rm = TRUE),
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






# ---- Summary table across NDTI range ----
# pred_mat rows = simulations, columns = NDTI values

summary_table <- data.frame(
  NDTI     = round(ndti_vals, 4),
  median   = round(apply(pred_mat, 2, median), 4),
  lower_95 = round(apply(pred_mat, 2, quantile, 0.025), 4),
  upper_95 = round(apply(pred_mat, 2, quantile, 0.975), 4),
  min      = round(apply(pred_mat, 2, min), 4),
  max      = round(apply(pred_mat, 2, max), 4)
)

# ---- Report the key values your advisor asked for ----
cat("=== Proxy MP ~ NDTI: Median, Min, Max with 95% uncertainty intervals ===\n\n")

cat("Median MP/L across NDTI range:\n")
cat("  Overall median:", round(median(summary_table$median), 4), "\n")
cat("  Range:         ", round(min(summary_table$median), 4), 
    "to", round(max(summary_table$median), 4), "\n\n")

cat("Minimum predicted MP/L:\n")
cat("  Value:         ", round(min(summary_table$median), 4), "\n")
cat("  95% UI:        [", round(summary_table$lower_95[which.min(summary_table$median)], 4),
    "-", round(summary_table$upper_95[which.min(summary_table$median)], 4), "]\n")
cat("  At NDTI =      ", round(ndti_vals[which.min(summary_table$median)], 4), "\n\n")

cat("Maximum predicted MP/L:\n")
cat("  Value:         ", round(max(summary_table$median), 4), "\n")
cat("  95% UI:        [", round(summary_table$lower_95[which.max(summary_table$median)], 4),
    "-", round(summary_table$upper_95[which.max(summary_table$median)], 4), "]\n")
cat("  At NDTI =      ", round(ndti_vals[which.max(summary_table$median)], 4), "\n\n")

# ---- Print full table ----
print(summary_table)




# ---- Summary statistics for reporting ----
cat("=== MP/L Prediction Summary ===\n")
cat("Median:", round(median(MP), 4),
    " [", round(quantile(MP, 0.025), 4), "-", round(quantile(MP, 0.975), 4), "]\n")
cat("Mean:  ", round(mean(MP), 4),
    " [", round(quantile(MP, 0.025), 4), "-", round(quantile(MP, 0.975), 4), "]\n")
cat("Min:   ", round(min(MP), 4),
    " [", round(quantile(MP, 0.025), 4), "-", round(quantile(MP, 0.975), 4), "]\n")
cat("Max:   ", round(max(MP), 4),
    " [", round(quantile(MP, 0.025), 4), "-", round(quantile(MP, 0.975), 4), "]\n")



# ---- Key values for annotation ----
idx_min    <- which.min(summary_table$median)
idx_max    <- which.max(summary_table$median)
idx_median <- which.min(abs(summary_table$median - median(summary_table$median)))

par(mar = c(5, 6, 4, 2))

plot(summary_table$NDTI, summary_table$median,
     type = "l", lwd = 2, col = "darkblue",
     ylim = c(0, max(summary_table$upper_95) * 1.1),
     xlab = "NDTI (satellite input)",
     ylab = "Predicted MP/L",
     main = "Satellite-derived MP Proxy with 95% Monte Carlo Uncertainty",
     bty = "n")

# 95% uncertainty ribbon
polygon(c(summary_table$NDTI, rev(summary_table$NDTI)),
        c(summary_table$lower_95, rev(summary_table$upper_95)),
        col = rgb(0, 0, 1, 0.15), border = NA)

# Upper and lower 95% boundary lines
lines(summary_table$NDTI, summary_table$upper_95,
      col = "steelblue", lwd = 1, lty = 2)
lines(summary_table$NDTI, summary_table$lower_95,
      col = "steelblue", lwd = 1, lty = 2)

# ---- Annotate min ----
points(summary_table$NDTI[idx_min], summary_table$median[idx_min],
       pch = 19, col = "firebrick", cex = 1.4)
arrows(x0 = summary_table$NDTI[idx_min],
       y0 = summary_table$lower_95[idx_min],
       y1 = summary_table$upper_95[idx_min],
       code = 3, angle = 90, length = 0.05, col = "firebrick", lwd = 1.5)
text(summary_table$NDTI[idx_min], summary_table$upper_95[idx_min] * 1.05,
     labels = paste0("Min\n", round(summary_table$median[idx_min], 2),
                     "\n[", round(summary_table$lower_95[idx_min], 2),
                     "-", round(summary_table$upper_95[idx_min], 2), "]"),
     col = "firebrick", cex = 0.7, pos = 4)

# ---- Annotate median ----
points(summary_table$NDTI[idx_median], summary_table$median[idx_median],
       pch = 19, col = "darkgreen", cex = 1.4)
arrows(x0 = summary_table$NDTI[idx_median],
       y0 = summary_table$lower_95[idx_median],
       y1 = summary_table$upper_95[idx_median],
       code = 3, angle = 90, length = 0.05, col = "darkgreen", lwd = 1.5)
text(summary_table$NDTI[idx_median], summary_table$upper_95[idx_median] * 1.05,
     labels = paste0("Median\n", round(summary_table$median[idx_median], 2),
                     "\n[", round(summary_table$lower_95[idx_median], 2),
                     "-", round(summary_table$upper_95[idx_median], 2), "]"),
     col = "darkgreen", cex = 0.7, pos = 4)

# ---- Annotate max ----
points(summary_table$NDTI[idx_max], summary_table$median[idx_max],
       pch = 19, col = "darkorange", cex = 1.4)
arrows(x0 = summary_table$NDTI[idx_max],
       y0 = summary_table$lower_95[idx_max],
       y1 = summary_table$upper_95[idx_max],
       code = 3, angle = 90, length = 0.05, col = "darkorange", lwd = 1.5)
text(summary_table$NDTI[idx_max], summary_table$upper_95[idx_max] * 1.05,
     labels = paste0("Max\n", round(summary_table$median[idx_max], 2),
                     "\n[", round(summary_table$lower_95[idx_max], 2),
                     "-", round(summary_table$upper_95[idx_max], 2), "]"),
     col = "darkorange", cex = 0.7, pos = 2)

# ---- Legend ----
legend("topleft",
       legend = c("Median prediction", "95% uncertainty band",
                  "Min", "Median point", "Max"),
       col = c("darkblue", rgb(0,0,1,0.4), "firebrick", "darkgreen", "darkorange"),
       lwd = c(2, 8, NA, NA, NA),
       pch = c(NA, NA, 19, 19, 19),
       cex = 0.8, bty = "n")
