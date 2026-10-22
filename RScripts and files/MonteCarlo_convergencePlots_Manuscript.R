# ================================================================
# MONTE CARLO UNCERTAINTY ANALYSIS
# NDTI -> Turbidity -> MP/L
# ================================================================

# ---- Libraries ----
library(MASS)
library(ggplot2)

ndti_turb <- read_xlsx("ndti_turb.xlsx")
winterKent <- read_xlsx("winterKent.xlsx")

# ---- Fit regression models ----
model_ndti_turb <- lm(Turbidity_SW ~ NDTI,            data = ndti_turb)   # Eq.1
model_turb_mp   <- lm(log10(AVGTOTAL) ~ Turbidity_SW, data = winterKent)  # Eq.2

# ---- Simulation settings ----
set.seed(123)
n <- 20000

# ---- Eq.1 parameters: NDTI -> Turbidity ----
c_mean <- coef(model_ndti_turb)[1]        # intercept
c_sd   <- summary(model_ndti_turb)$coefficients[1, 2]
d_mean <- coef(model_ndti_turb)[2]        # slope
d_sd   <- summary(model_ndti_turb)$coefficients[2, 2]
sigma_eq1 <- summary(model_ndti_turb)$sigma

# ---- Eq.2 parameters: Turbidity -> MP ----
a_mean <- coef(model_turb_mp)[1]          # intercept
a_sd   <- summary(model_turb_mp)$coefficients[1, 2]
b_mean <- coef(model_turb_mp)[2]          # slope
b_sd   <- summary(model_turb_mp)$coefficients[2, 2]
sigma_eq2 <- summary(model_turb_mp)$sigma

# ---- Field/satellite input parameters ----
ndti_mean <- mean(ndti_turb$NDTI,         na.rm = TRUE)
ndti_sd   <- sd(ndti_turb$NDTI,           na.rm = TRUE)
turb_mean <- mean(ndti_turb$Turbidity_SW, na.rm = TRUE)
turb_sd   <- sd(ndti_turb$Turbidity_SW,   na.rm = TRUE)

# ---- Print all parameters for inspection ----
cat("=== Eq.1: NDTI -> Turbidity ===\n")
cat("Intercept (c): mean =", round(c_mean, 4), " sd =", round(c_sd, 4), "\n")
cat("Slope     (d): mean =", round(d_mean, 4), " sd =", round(d_sd, 4), "\n")
cat("Residual sigma:", round(sigma_eq1, 4), "\n\n")

cat("=== Eq.2: Turbidity -> MP ===\n")
cat("Intercept (a): mean =", round(a_mean, 4), " sd =", round(a_sd, 4), "\n")
cat("Slope     (b): mean =", round(b_mean, 4), " sd =", round(b_sd, 4), "\n")
cat("Residual sigma:", round(sigma_eq2, 4), "\n\n")

cat("=== Inputs ===\n")
cat("NDTI:      mean =", round(ndti_mean, 4), " sd =", round(ndti_sd, 4), "\n")
cat("Turbidity: mean =", round(turb_mean, 4), " sd =", round(turb_sd, 4), "\n")

# ---- Sample inputs ----
NDTI      <- rnorm(n, ndti_mean, ndti_sd)
Turbidity <- rnorm(n, turb_mean, turb_sd)

# ---- Sample model coefficients jointly from covariance matrix ----
vcov_eq1 <- vcov(model_ndti_turb)
vcov_eq2 <- vcov(model_turb_mp)

draws_eq1 <- mvrnorm(n, mu = coef(model_ndti_turb), Sigma = vcov_eq1)
draws_eq2 <- mvrnorm(n, mu = coef(model_turb_mp),   Sigma = vcov_eq2)

# ---- Propagate uncertainty ----
# Eq.1: NDTI -> Turbidity
turb_pred <- draws_eq1[,1] + draws_eq1[,2] * NDTI +
  rnorm(n, 0, sigma_eq1)

# Eq.2: Turbidity -> log10(MP) -> MP
logmp_pred <- draws_eq2[,1] + draws_eq2[,2] * turb_pred +
  rnorm(n, 0, sigma_eq2)

MP <- 10^logmp_pred

# ---- Summary statistics ----
cat("\n=== Output: Predicted MP/L ===\n")
print(round(c(
  mean   = mean(MP),
  median = median(MP),
  sd     = sd(MP),
  `2.5%` = quantile(MP, 0.025),
  `97.5%`= quantile(MP, 0.975)
), 4))

# ================================================================
# CONVERGENCE PLOTS
# ================================================================

checkpoints <- seq(100, n, by = 100)

# ---- Cumulative means ----
cum_ndti <- cumsum(NDTI)[checkpoints]       / checkpoints
cum_turb <- cumsum(Turbidity)[checkpoints]  / checkpoints
cum_c    <- cumsum(draws_eq1[,1])[checkpoints] / checkpoints
cum_d    <- cumsum(draws_eq1[,2])[checkpoints] / checkpoints
cum_a    <- cumsum(draws_eq2[,1])[checkpoints] / checkpoints
cum_b    <- cumsum(draws_eq2[,2])[checkpoints] / checkpoints
cum_MP   <- cumsum(MP)[checkpoints]         / checkpoints

# ---- Normalise function ----
normalise_centred <- function(cum_vec, full_vec) {
  centre <- mean(full_vec)
  spread <- max(abs(cum_vec - centre))
  0.5 + (cum_vec - centre) / (2 * spread)
}

# ---- Normalise all ----
cum_ndti_n <- normalise_centred(cum_ndti, NDTI)
cum_turb_n <- normalise_centred(cum_turb, Turbidity)
cum_c_n    <- normalise_centred(cum_c,    draws_eq1[,1])
cum_d_n    <- normalise_centred(cum_d,    draws_eq1[,2])
cum_a_n    <- normalise_centred(cum_a,    draws_eq2[,1])
cum_b_n    <- normalise_centred(cum_b,    draws_eq2[,2])
cum_MP_n   <- normalise_centred(cum_MP,   MP)

# ---- Colours ----
col_ndti <- "firebrick"
col_turb <- "darkorange"
col_c    <- "blue"
col_d    <- "lightblue"
col_a    <- "purple"
col_b    <- "darkgreen"
col_mp   <- "darkblue"

# ---- Helper axis function ----
draw_axis_segment <- function(raw_cum_vec, full_vec, colour, line_offset, label) {
  centre <- mean(full_vec)
  spread <- max(abs(raw_cum_vec - centre))
  lo  <- centre - spread
  hi  <- centre + spread
  axis(2, at = c(0, 0.5, 1),
       labels = round(c(lo, centre, hi), 4),
       col = colour, col.axis = colour,
       las = 1, line = line_offset, tick = TRUE,
       cex.axis = 0.7)
  mtext(label, side = 2, line = line_offset + 3.5,
        col = colour, cex = 0.6, las = 0)
}

# ================================================================
par(mfrow = c(4, 1))

# ---- PLOT 1: Satellite input (NDTI) and field input (Turbidity) ----
par(mar = c(4, 16, 4, 2))

plot(checkpoints, cum_ndti_n,
     type = "l", col = col_ndti, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Input Cumulative Means",
     xlab = "Number of Iterations", ylab = "")
lines(checkpoints, cum_turb_n, col = col_turb, lwd = 2)
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

draw_axis_segment(cum_ndti, NDTI,      col_ndti, line_offset = 0, label = "NDTI (satellite)")
draw_axis_segment(cum_turb, Turbidity, col_turb, line_offset = 5, label = "Turbidity (field)")

legend("topright",
       legend = c("NDTI (satellite input)", "Turbidity (field input)"),
       col = c(col_ndti, col_turb),
       lwd = 2, cex = 0.75, bty = "n")

# ---- PLOT 2: Eq.1 parameters (NDTI -> Turbidity) ----
par(mar = c(4, 16, 4, 2))

plot(checkpoints, cum_c_n,
     type = "l", col = col_c, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Eq.1 Parameter Cumulative Means (NDTI → Turbidity)",
     xlab = "Number of Iterations", ylab = "")
lines(checkpoints, cum_d_n, col = col_d, lwd = 2)
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

draw_axis_segment(cum_c, draws_eq1[,1], col_c, line_offset = 0, label = "Intercept (c)")
draw_axis_segment(cum_d, draws_eq1[,2], col_d, line_offset = 5, label = "Slope (d)")

legend("topright", legend = c("Eq.1 Intercept", "Eq.1 Slope"),
       col = c(col_c, col_d), lwd = 2, cex = 0.75, bty = "n")

# ---- PLOT 3: Eq.2 parameters (Turbidity -> MP) ----
par(mar = c(4, 16, 4, 2))

plot(checkpoints, cum_a_n,
     type = "l", col = col_a, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Eq.2 Parameter Cumulative Means (Turbidity → MP)",
     xlab = "Number of Iterations", ylab = "")
lines(checkpoints, cum_b_n, col = col_b, lwd = 2)
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

draw_axis_segment(cum_a, draws_eq2[,1], col_a, line_offset = 0, label = "Intercept (a)")
draw_axis_segment(cum_b, draws_eq2[,2], col_b, line_offset = 5, label = "Slope (b)")

legend("topright", legend = c("Eq.2 Intercept", "Eq.2 Slope"),
       col = c(col_a, col_b), lwd = 2, cex = 0.75, bty = "n")

# ---- PLOT 4: Output MP/L ----
par(mar = c(4, 16, 4, 2))

plot(checkpoints, cum_MP_n,
     type = "l", col = col_mp, lwd = 2,
     ylim = c(0, 1), axes = FALSE,
     main = "Convergence of Output Cumulative Mean (MP/L)",
     xlab = "Number of Iterations", ylab = "")
abline(h = 0.5, lty = 2, col = "grey40")
axis(1)

centre_mp <- mean(MP)
spread_mp <- max(abs(cum_MP - centre_mp))
axis(2, at = c(0, 0.5, 1),
     labels = round(c(centre_mp - spread_mp, centre_mp, centre_mp + spread_mp), 2),
     col = col_mp, col.axis = col_mp,
     las = 1, tick = TRUE, cex.axis = 0.7)
mtext("MP/L", side = 2, line = 3.5, col = col_mp, cex = 0.65)

# ---- Reset ----
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2))
