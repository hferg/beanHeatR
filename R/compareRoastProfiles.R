#' compareRoastProfiles
#'
#' A function that will compare roast profiles for a number of CSV files.
#' @param file_list A vector of roast files.
#' @param degree The degree of the polynomial used for smoothing temp vs. time
#' @param names The names of the roasts (for the legend).
#' @name compareRoastProfiles
#' @export

compareRoastProfiles <- function(file_list, degree = 8, names = NULL) {

  loadAndDiff <- function(filename, degree) {
    x <- read.csv(filename)
    x <- x[-1 , ]
    x <- x[1:(min(which(is.na(x$temp))) - 1), ]
    x$time <- x$total_seconds / 60
    x$diff[2:nrow(x)] <- (x$temp[2:nrow(x)] - x$temp[1:(nrow(x) - 1)]) /
      (x$time[2:nrow(x)] - x$time[1:(nrow(x) - 1)])
    x$diff[1] <- 0
    mod_temp <- lm(temp ~ poly(time, degree), data = x)
    x$fitted_temp <- fitted(mod_temp)
    x$roast <- filename
    x
  }

  if (is.null(names)) {
    names <- file_list
  } else {
    if (length(names) != length(file_list)) {
      stop("The length of names must equal the length of file_list.")
    }
  }

  dat <- lapply(file_list, function(x) {
    loadAndDiff(x, degree = degree)
  })

  names(dat) <- names

  x <- reshape2::melt(dat, id.vars = "time", measure.vars = c("temp", "fitted_temp", "diff"))

  x$type <- NA
  x$type[x$variable == "temp" | x$variable == "fitted_temp"] <- "temp"
  x$type[x$variable == "diff"] <- "rate"
  temps <- subset(x, type == "temp")
  diffs <- subset(x, type == "rate")

  p1 <- ggplot2::ggplot(temps, ggplot2::aes(x = time, colour = L1)) +
    ggplot2::geom_line(data = subset(temps, variable == "fitted_temp"),
                       ggplot2::aes(y = value), lwd = 1.2) +
    ggplot2::geom_point(data = subset(temps, variable == "temp"),
                        ggplot2::aes(y = value), alpha = 0.5) +
    viridis::scale_colour_viridis(discrete = TRUE) +
    coord_cartesian(ylim = c(0, 450)) +
    ggthemes::theme_tufte(base_family = "Helvetica") +
    theme(
      panel.grid.major.y = element_line(colour = "white"),
      panel.background = element_rect(fill = "lightgrey"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    ylab("Temperature (F)")

  p2 <- ggplot2::ggplot(diffs, ggplot2::aes(x = time, colour = L1)) +
    ggplot2::geom_line(ggplot2::aes(y = value), alpha = 0.3) +
    ggplot2::geom_smooth(ggplot2::aes(y = value), method = "loess", se = FALSE) +
    coord_cartesian(ylim = c(0, 100)) +
    viridis::scale_colour_viridis(discrete = TRUE) +
    ggthemes::theme_tufte(base_family = "Helvetica") +
    theme(
      panel.grid.major.y = element_line(colour = "white"),
      panel.background = element_rect(fill = "lightgrey")
    ) +
    xlab("Time (minutes)") +
    ylab("F/min")

  grid::grid.newpage()
  grid::grid.draw(rbind(ggplot2::ggplotGrob(p1),
                        ggplot2::ggplotGrob(p2),
                        size = "last"))
}