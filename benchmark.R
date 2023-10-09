#!/usr/bin/env Rscript

#Using the system of equations from Robertson 1966, as given on the Wikipedia page on stiff equations.
#Robertson 1966. "The solution of a set of reaction rate equations". Numerical Analysis: An Introduction.
#https://en.wikipedia.org/wiki/Stiff_equation#Motivating_example

#Constants
reaction_rates <- c(0.04, 3e7, 1e4)
y0 <- c(1, 0, 0)
event_influx <- c(1, 0, 0)
t_max <- 1e11
output_interval <- 1e6
event_interval <- 1e8
method <- "lsoda"

stopifnot(length(reaction_rates) == length(y0), length(reaction_rates) == length(event_influx))
stopifnot(t_max %% event_interval == 0, event_interval %% output_interval == 0)

all_times <- seq(0, t_max, by = output_interval)
event_times <- seq(event_interval, t_max - event_interval, by = event_interval)
divided_times <- lapply(c(0, event_times), function(t) {
	seq(t, t + event_interval, by = output_interval)
})

#`rates` should be a vector of three reaction rates.
derivative <- function(t, y, rates) {
	rates <- rates * c(
		y[1],
		y[2]^2,
		y[2] * y[3]
	)
	y <- c(
		- rates[1] + rates[3],
		rates[1] - rates[2] - rates[3],
		rates[2]
	)
	return(list(y))
}
