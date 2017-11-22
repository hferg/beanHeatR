#' calculateTargets
#'
#' This is a function that is designed to calculate the targets that a roast
#' ought to hit. It is a bit rough I think... It is based on the idea that once
#' the turning point is passed, and the temperature is increasing the increase
#' should be approximately logarithmic. The inputs, then, are starting
#' temperature, what time you want the roast to end at, the temperature you want
#' the roast to end at, and when you want to try and hit the turning point. The
#' output will be a table of times in ten seecond intervals, with targets.
#' @param start
#' @param turnaround
#' @param end_time
#' @param end_temp

calculateTargets <- function(start, turnaround, end_time, end_temp) {
  x <- seq.int(from = 0, to = (15 * 60), by = 10)

  # This gives the basic log curve - then it needs to be adjusted to be
  # somewhat flattened at end_time, and somewhat extended so that end_temp
  # corresponds to end_time. It also needs to be adjusted toward the turning
  # point, allowing for the fact the beans don't reach 0 degrees.
  y <- log(x)
  plot(y ~ x)
}
