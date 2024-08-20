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
