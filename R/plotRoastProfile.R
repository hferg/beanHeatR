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
#' @param combined Logical - plot temp and delta on the same plot, or not.
#' @param targets Logical - show temperature targets?
#' @param degree The degree of the polynomial used for smoothing time.
#' @name plotRoastProfile
#' @export

plotRoastProfile <- function(filename, labels = TRUE, combined = FALSE,
                             targets = TRUE, degree = 8) {
  # TODO Add in the targets
  # TODO Add in the milestones.

  x <- read.csv(filename)
  # Remove the pre-roast information.
  x <- x[-1 , ]
  x <- x[1:(min(which(is.na(x$temp))) - 1), ]
  # Put total_seconds into minutes.
  x$time <- x$total_seconds / 60

  # Get the temp per second for each interval.
  x$diff[2:nrow(x)] <- (x$temp[2:nrow(x)] - x$temp[1:(nrow(x) - 1)]) /
    (x$time[2:nrow(x)] - x$time[1:(nrow(x) - 1)])
  x$diff[1] <- 0
  # Plot the data, and a sliding mean-smoothed curve on one plot,
  # and below it plot the diff plot, with axes that stop at zero (cooling is
  # a) not important and b) shouldn't be happening...)
  mod_temp <- lm(temp ~ poly(time, degree), data = x)
  x$fitted_temp <- fitted(mod_temp)

  # p1 - all on same plot.
  if (combined) {
    p <- ggplot2::ggplot(x, ggplot2::aes(x = total_seconds/60)) +
      ggplot2::geom_point(aes(y = temp),
                         lwd = 0.8,
                         colour = viridis::viridis(5)[1],
                         alpha = 0.8) +
      ggplot2::geom_line(aes(y = fitted_temp),
                         lwd = 0.8,
                         colour = viridis::viridis(5)[2]) +
      ggplot2::geom_smooth(aes(y = diff * 3),
                         colour = viridis::viridis(5)[3],
                         se = FALSE, method = "loess") +
      ggplot2::geom_line(aes(y = diff * 3),
                           colour = viridis::viridis(5)[4]) +
      xlab("Time (minutes)") +
      ylab("Temperature (F)") +
      scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / 3, name = "F/min")) +
      coord_cartesian(ylim = c(0, 450)) +
      ggthemes::theme_tufte(base_family = "Helvetica") +
      theme(
        panel.grid.major.y = element_line(colour = "white"),
        panel.background = element_rect(fill = "lightgrey")
      ) +
      scale_x_continuous(breaks = seq(0, max(x$time), 1))
    if (targets) {
      p <- p +
          geom_point(aes(y = target),
                       shape = 18,
                       size = 3,
                       colour = "hotpink",
                       alpha = 0.6)
    }
  } else {
    p2a <- ggplot2::ggplot(x, ggplot2::aes(x = total_seconds/60)) +
      ggplot2::geom_point(aes(y = temp),
                          lwd = 0.8,
                          colour = viridis::viridis(5)[1],
                          alpha = 0.8) +
      ggplot2::geom_line(aes(y = fitted_temp),
                         lwd = 0.8,
                         colour = viridis::viridis(5)[2]) +
      coord_cartesian(ylim = c(0, 450)) +
      ggthemes::theme_tufte(base_family = "Helvetica") +
      theme(
        panel.grid.major.y = element_line(colour = "white"),
        panel.background = element_rect(fill = "lightgrey"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      ) +
      ylab("Temperature (F)") +
      scale_x_continuous(breaks = seq(0, max(x$time), 1))

    if (targets) {
      p2a <- p2a +
        geom_point(aes(y = target),
                   shape = 18,
                   size = 3,
                   colour = "hotpink",
                   alpha = 0.6)
    }

    p2b <- ggplot2::ggplot(x, ggplot2::aes(x = total_seconds/60)) +
      ggplot2::geom_smooth(aes(y = diff),
                           colour = viridis::viridis(5)[3],
                           se = FALSE,
                           method = "loess") +
      ggplot2::geom_line(aes(y = diff),
                         colour = viridis::viridis(5)[4]) +
      ggthemes::theme_tufte(base_family = "Helvetica") +
      theme(
        panel.grid.major.y = element_line(colour = "white"),
        panel.background = element_rect(fill = "lightgrey")
      )+
      coord_cartesian(ylim = c(0, 100)) +
      xlab("Time (minutes)") +
      ylab("F/min") +
      scale_x_continuous(breaks = seq(0, max(x$time), 1))
  }

  if (combined) {
    p
  } else {
    grid::grid.newpage()
    grid::grid.draw(rbind(ggplot2::ggplotGrob(p2a),
                          ggplot2::ggplotGrob(p2b),
                          size = "last"))
  }
}


