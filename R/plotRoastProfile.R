#' plotRoastProfile
#'
#' Plot a single roast profile - takes the filename of a filled out profile
#' template (found in /data). It takes the total_seconds and temp column to
#' plot the profile, the target column to plot the targets, and uses the notes
#' column to plot points in the roast where any note was taken. Uses the note
#' as a label, unless the notes are standard - e.g. fcs becomes a label
#' start of first crack. Rate of rise max will be calculated.
#' Also plots the delta bean temp, which is, I think, the first differential
#' of the curve?
#' @param filename The filename of the roast profile
#' @param labels Logical - plot labels or not
#' @param delta Logical - plot delta bean temp or not.

plotRoastProfile <- function(filename, labels = TRUE, delta = TRUE,
                             degree = 8) {
  # TODO I want time in minutes.
  # TODO I am not sure the derivitive is correct - plus I want the unit for it
  # to be F/min - at the moment it seems to be pretty crazy units. 877 is the
  # max - that can't be F/min!

  x <- read.csv(filename)
  # Remove the pre-roast information.
  env <- x[1 , ]
  x <- x[-1 , ]
  x <- x[1:(min(which(is.na(x$temp))) - 1), ]
  # Put total_seconds into minutes.
  x$time <- x$total_seconds / 60

  # Get the temp per second for each interval.
  x$diff[2:nrow(x)] <- (x$temp[2:nrow(x)] - x$temp[1:(nrow(x) - 1)]) /
    (x$time[2:nrow(x)] - x$time[1:(nrow(x) - 1)])

  # make a dataframe for the plot.
  xx <- data.frame(time = x$time, temp = x$temp, diff = x$diff)
  xx <- reshape2::melt(xx, id.vars = "time")

  # Use the autoplot methods for ts objects to plot the two plots, and make
  # use of gridExtra to arrange the change plot below the time plot so they
  # can both be seen.





  p <- ggplot2::ggplot(data = xx, ggplot2::aes(x = time, y = value, colour = variable)) +
    geom_line()
  p




  # Fit a high order polynomial to calculate the derivative for delta
  # BT.
  mod <- lm(temp ~ poly(total_seconds/60, degree), data = x)
  x$fitted <- fitted(mod)

  # Try something more simple?
  x$diff <- sapply(1:nrow(x), function(k) x$temp[k + 1] - x$temp[k]) / (10 / 60)

  p <- ggplot2::ggplot(x, ggplot2::aes(x = total_seconds/60)) +
    ggthemes::theme_tufte(base_family = "Helvetica") +
    ggplot2::geom_line(aes(y = temp), lwd = 0.8, alpha = 0.3) +
    ggplot2::geom_line(aes(y = diff), lwd = 0.8, alpha = 0.3)
  p

  # Try with second axis.
  p + scale_y_continuous(sec.axis = ggplot2::sec_axis(~ ., name = "Delta BT"))

  }


