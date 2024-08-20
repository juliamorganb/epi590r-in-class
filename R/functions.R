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

## prop ???? ##

prop <- function(x,multiplier =1) {
	n<-length(n)
	proportion_val <-sum(x)/n
	multiplied_val <- multiplier*proportion_val
	return(multiplied_val)
}
prop(c(1,1,1,0,0), multiplier =100)

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
