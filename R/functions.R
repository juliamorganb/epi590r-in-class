## mean ##

x <- 3
new_mean <- function(x){
	n <- length(x)
	mean_val <- sum(x)/n
	return(mean_val)
}
x <- c(1,3,5,7,9)
n <- length(x)
mean_val <- sum(x) / n

new_mean(x)
new_mean(x = c(534, 4531, 12))

## square root ##

x <- 3
x^2
square <- function(x) {
sqrt <- x^2
return(sqrt)
}
square(x)
square(53)
53^2

## raising ##

x<-3
y <- 4
x^y

raise <- function(base_number, power) {
	answer <- base_number^power
	return(answer)
}
# test with
raise(base_number = 2, power = 4)
rase(2,5) #gives same answer
# should give you
2^4


##### 2.5 models ####
library(tidyverse)
library(gtsummary)
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
		)
	)
}

###adding tidyfun - not complete

#help file showed us default
new_table_function <- function(model, tidy_fun =broom.helpers::tidy_with_broom_or_parameters()
															 ) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
		)
	)

tidy_fun = partial(tidy_robust, vcov = "HC1")

### she is going to upload a quarto document that explains all of this
