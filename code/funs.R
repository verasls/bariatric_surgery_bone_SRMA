# Data tranformation functions --------------------------------------------

# Compute mean
#
# Compute the mean value based on the median, 1st and 3rd quartiles.
#
# Args:
#   median: The median value.
#   q1, q3: The 1st and 3rd quartiles.
compute_mean <- function(median, q1, q3) {
  (q1 + median + q3) / 3
}

# Compute standard deviation
#
# Compute the standard deviation from either the confidence interval or the
# interquartile range.
#
# Args:
#   from: A character string indicating from which statistic to compute the
# standard deviation from. Can be either "ci" or "iqr".
#   n: The sample size.
#   x_lower, x_upper: The lower and upper levels of the summary statistic
# used to compute the standard deviation.
compute_sd <- function(from, n, mean, x_lower = NA, x_upper = NA) {
  if (from == "ci") {
    sd_from_ci(n, mean, x_lower, x_upper)
  } else if (from == "iqr") {
    sd_from_iqr(n, x_lower, x_upper)
  }
}

# Compute the standard deviation from the mean change
#
# Compute the standard deviation from the raw change score based on the
# standard deviations from the pre and post mean values and the correlation
# coefficient between these values.
#
# Args:
#   sd_pre, sd_post: The standard deviation values.
#   corr: The estimated correlation coefficient.
compute_sd_mean_change <- function(sd_pre, sd_post, corr) {
  sqrt(sd_pre^2 + sd_post^2 - (2 * corr * sd_pre * sd_post))
}


# Other aux functions below
sd_from_ci <- function(n, mean, ci_lower, ci_upper) {
  sqrt(n) * (ci_upper - ci_lower) / (2 * qt(0.975, df = n - 1))
}

sd_from_iqr <- function(n, q1, q3) {
  (q3 - q1) / (2 * qnorm((0.75 * n - 0.125) / (n + 0.25)))
}

# Figure functions --------------------------------------------------------

# Build the text that forms the follow-up column in the forest plot
#
# Args:
#   data: A data.frame or escalc object.
#   x: The x coordinate to plot the text.
followup_text <- function(data, x) {
  k <- nrow(data) + 1
  for (i in seq(nrow(data), 1, -1)) {
    text(
      x, i, font = 1, pos = 2, cex = 1,
      data$time_after_surgery[k - i]
    )
  }
}

# Build the text that forms the sample size column in the forest plot
#
# Args:
#   data: A data.frame or escalc object.
#   x: The x coordinate to plot the text.
n_text <- function(data, x) {
  k <- nrow(data) + 1
  for (i in seq(nrow(data), 1, -1)) {
    text(
      x, i, font = 1, pos = 2, cex = 1,
      data$n[k - i]
    )
  }
}

# Build the text that forms the weights column in the forest plot
#
# Args:
#   model: An rma object.
#   x: The x coordinate to plot the text.
weights_text <- function(model, x) {
  w <- unname(weights(model))
  k <- length(w) + 1
  for (i in seq(length(w), 1, -1)) {
    text(
      x, i, font = 1, pos = 2, cex = 1,
      paste0(broman::myround(w[k - i], 1), "%")
    )
  }
}

# Build the text showing the regression equation of the meta-regression model
#
# Args:
#   model: The meta-regression model.
#   mod: A character string indicating the moderator (predictor) variable name.
#   x, y: The coordinates to plot the text.
#
# Obs: only works with simple meta-regression (one moderator/predictor).
reg_equation <- function(mr, mod, x, y, pos = 2) {
  b0 <- mr$beta[1]
  b1 <- mr$beta[2]

  equation <- paste0(
    "Y = ",
    ifelse(b0 < 0, "- ", ""),
    broman::myround(abs(b0), 3),
    ifelse(b1 < 0, " - ", " + "),
    broman::myround(abs(b1), 3),
    " * ", mod
  )

  text(x, y, font = 1, pos = pos, cex = 0.9, equation)
}

# Other functions ---------------------------------------------------------

# Variance components
#
# Calculate I2 and variance distribution for multilevel meta-analysis models
# and present the values in a data.frame.
#
# Args:
#   model: An object of class `rma.mv`.
#   site: A character string indicating the skeletal site analysed in
# the model.
variance_components <- function(model, site) {
  var_comp <- dmetar::var.comp(model)[[1]]
  var_comp <- tibble::tibble(
    Site = rep(site, 3),
    Component = c(
      "Sampling error", "Studies", "Time"
    ),
    "% of total variance" = c(var_comp[1, 1], var_comp[3, 1], var_comp[2, 1]),
    I2 = c(NA, var_comp[3, 2], var_comp[2, 2])
  )
  dplyr::mutate(
    var_comp,
    I2 = as.numeric(I2),
    dplyr::across(where(is.numeric), round, 1)
  )
}

# Compute the I^2
#
# Args:
#   data: A data.frame or escalc object.
#   model: An rma object.
i2 <- function(data, model) {
  W <- diag(1 / data$vi)
  X <- model.matrix(model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(model$sigma2) /
    (sum(model$sigma2) + (model$k - model$p) / sum(diag(P)))
}

# Compute the (pseudo) R^2
#
# See:
# López-López, J. A., Marín-Martínez, F., Sánchez-Meca, J.,
# Van den Noortgate, W., & Viechtbauer, W. (2014). Estimation of the predictive
# power of the model in mixed-effects meta-regression: A simulation study.
# British Journal of Mathematical and Statistical Psychology, 67(1), 30–48.
#
# Args:
#   re: A random-effects meta-analysis model.
#   mr: A mixed-effects meta-regression model.
r2 <- function(re, mr) {
  (sum(re$sigma2) - sum(mr$sigma2)) / sum(re$sigma2)
}

# Compute the weighted mean and 95% confidence interval of the follow-up time
#
# Args:
#   data: A data.frame.
followup_weighted_mean <- function(data, model) {
  w <- weights(model)
  x <- data[["time_after_surgery"]]
  xm <- round(weighted.mean(x, w), 1)
  se <- sqrt(Hmisc::wtd.var(x, w)) / sqrt(nrow(data))
  ci_lower <- round(xm - (se * qt(0.975, df = nrow(data) - 1)), 1)
  ci_upper <- round(xm + (se * qt(0.975, df = nrow(data) - 1)), 1)
  data.frame(
    mean = xm, lower_ci = ci_lower, upper_ci = ci_upper
  )
}

# Get meta-analysis model pooled effect size estimate and confidence intervals
#
# Args:
#   model: An rma object.
#   site: A character string indicating the skeletal site.
get_model_estimate <- function(model, site) {
  m <- as.numeric(model$b)
  ci_lower <- model$ci.lb
  ci_upper <- model$ci.ub
  tibble::tibble(
    site = site,
    mean = m,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# Get meta-analysis model pooled effect size estimate and confidence intervals
# to build a table
#
# Args:
#   model: An rma object.
get_model_estimate_tb <- function(model) {
  m <- as.numeric(model$b)
  ci_lower <- model$ci.lb
  ci_upper <- model$ci.ub
  tb <- tibble::tibble(
    mean = broman::myround(m, 1),
    ci_lower = broman::myround(ci_lower, 1),
    ci_upper = broman::myround(ci_upper, 1)
  )
  paste0(tb$mean, " (", tb$ci_lower, ", ", tb$ci_upper, ")")
}
