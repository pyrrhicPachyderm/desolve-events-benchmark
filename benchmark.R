#!/usr/bin/env Rscript

#Using the system of equations from Robertson 1966, as given on the Wikipedia page on stiff equations.
#Robertson 1966. "The solution of a set of reaction rate equations". Numerical Analysis: An Introduction.
#https://en.wikipedia.org/wiki/Stiff_equation#Motivating_example
#However, this is too stiff, and causes LSODA to throw a bunch of warnings.
#So slow down the reactions.

#Constants
reaction_rates <- 1e-3 * c(0.04, 3e7, 1e4)
y0 <- c(1, 0, 0)
event_influx <- c(1, 0, 0)
t_max <- 1e11
output_interval <- 1e6
event_interval <- 1e8
method <- "lsoda"

stopifnot(length(reaction_rates) == length(y0), length(reaction_rates) == length(event_influx))
stopifnot(t_max %% event_interval == 0, event_interval %% output_interval == 0)

names(y0) <- LETTERS[1:length(y0)]

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

events_df <- function() {
	events_data <- data.frame(
		var = rep(1:length(y0), times = length(event_times)),
		time = rep(event_times, each = length(y0)),
		value = rep(event_influx, times = length(event_times)),
		method = "add"
	)
	deSolve::ode(y0, all_times, derivative, reaction_rates, method = method,
		events = list(data = events_data)
	)
}

events_func <- function() {
	event_f <- function(t, y, parms) {
		y + event_influx
	}
	deSolve::ode(y0, all_times, derivative, reaction_rates, method = method,
		events = list(func = event_f, time = event_times)
	)
}

manual <- function() {
	solve <- function(y0, times) {
		deSolve::ode(y0, times, derivative, reaction_rates, method = method)
	}
	result <- solve(y0, divided_times[[1]])
	
	for(times in divided_times[-1]) {
		result <- rbind(
			result,
			solve(result[nrow(result),-1] + event_influx, times)[-1,]
		)
	}
	return(result)
}
