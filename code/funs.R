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
