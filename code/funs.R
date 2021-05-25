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
