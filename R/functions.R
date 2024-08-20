x <- 3

square <- function(x) {
	square_formula = x*x
	return(square_formula)
}

square(x)
square(53)
53^2

y <- c(1, 2, 3, 4, 5)

range <- function(y) {
	range_formula = max(y) - min(y)
	return(range_formula)
}

range(y)
5-4

x <- c(1, 2, 3, 4, 5)

new_mean <- function(x) {
	n <- length(x)
  mean_val <- sum(x) / n
  return(mean_val)
}

new_mean(x)

prop <- function(x, multiplier) {
	n <- length(x)
	proportion_val <- sum(x) / n
	multiplied_val <- multiplier * proportion_val
	return(multiplied_val)
}

prop(c(1, 1, 1, 0, 0), multiplier = 100)

raise <- function(base_number, power = 2) {
	answer <- base_number ^ power
	return(answer)
}

raise(base_number = 2, power = 5)
2^5
raise(2, 5)
raise(2) #uses default here

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		)
	)
}

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
												 data = nlsy, family = binomial(link = "log")
)

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	)
)

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
			income ~ "Income"
		)
	)
}

