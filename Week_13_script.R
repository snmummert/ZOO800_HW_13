#######################################################
################### Week 13 HW ######################
################# Sophia Mummert ######################
#######################################################

#### Libraries #####
library(readr)

#### Load Data #####
dragons = read_csv("C:/RStudio/ZOO800_HW_13/dragon_data.csv")

######## OBJ1: Use analytical solution to estimate parameters #########

# parameters
y <- dragons$acres_on_fire #predicted 
X <- cbind(1, dragons$size) #intercept and predictor

# analytical solution
beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
beta_hat

# estimated parameters of the linear model are beta0 (intercept) = -1.37551
# and beta1 (slope) = 1.345594





######## OBJ2: Use OLS to estimate parameters #########

# parameters 
x = dragons$size
y = dragons$acres_on_fire

## part a##

## brute force grid search try 1 ##
#beta0_range = seq(mean(y) - 5*sd(y), mean(y) + 5*sd(y), by = 0.1)
#beta1_low = -abs((max(y)-min(y)) / (max(x)-min(x))) * 3
#beta1_high =  abs((max(y)-min(y)) / (max(x)-min(x))) * 3
#beta1_range = seq(beta1_low, beta1_high, by = 0.1)
#I asked chatGPT for help with this part, as I did not understand the 
#what a "plausible" range would be without just using analytic soultion results. 
#The explanation for multiplying thr sd by 5 that it would be a safe coverage,
#and multiple of 3 for the max/min has the same reasonsing without creating too 
#large of a range.
#the first time I'm running this, my beta0 = -2.316538, (which is off from 
#the analytic solution). The beta1 however is 1.3747 which is close to the analytic.


# grid search try 2

slope_guess = sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
intercept_guess = mean(y) - slope_guess * mean(x)

beta0_range = seq(intercept_guess - 1, intercept_guess + 1, by = 0.1)
beta1_range = seq(slope_guess - 1, slope_guess + 1, by = 0.1)

#had to choose plausible range off of slope_guess

# nested for loop of plausible values and slope
minSSE_matrix = matrix(NA, nrow = length(beta0_range), ncol = length(beta1_range))
for (i in 1:length(beta0_range)) {
  for (j in 1:length(beta1_range)) {
    beta0 = beta0_range[i]
    beta1 = beta1_range[j]
    y_hat = beta0 + beta1 * x
    SSE = sum((y - y_hat)^2)
    minSSE_matrix[i, j] = SSE
  }
}

# find minimum SSE and corresponding parameters using which()
min_val <- which(minSSE_matrix == min(minSSE_matrix), arr.ind = TRUE)

est_beta0 = beta0_range[min_val[1]]
est_beta1 = beta1_range[min_val[2]]

est_beta0
est_beta1

# estimated parameters of the grid are beta0 (intercept) = -1.4 and 
# beta1 (slope) = 1.3 (exactly the same)

## part b ##

# objective function
minSSE = function(par, x, y) {
  beta0 = par[1]
  beta1 = par[2]
  y_hat = beta0 + beta1 * x
  sum((y-y_hat)^2)
}

start_par = c(0, 0)

optim_results = optim(start_par, minSSE, x = x, y = y)
optest_beta0 = optim_results$par[1]
optest_best1 = optim_results$par[2]

optest_beta0
optest_best1

# estimated parameters of optim() are beta0 (intercept) = -1.366789
# and beta1 (slope) = 1.346519

## part c ##
optim_results$convergence == 0

check = list(c(0,0), c(10,10), c(-10,-10))
opt_results = lapply(check, function(start) {
  optim(par = start, fn = minSSE, x = x, y = y)$par
})
opt_results

# checking for converged and if its sensitive to starting values. since
# values are all very similar, it is not sensitive to starting values.





######## OBJ3: estimate parameters from max-likelihood #########

## part a ##

#uses same beta0/beta1 ranges as in OBJ2
slope_guess = sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
intercept_guess = mean(y) - slope_guess * mean(x)

beta0_range = seq(intercept_guess - 1, intercept_guess + 1, by = 0.1)
beta1_range = seq(slope_guess - 1, slope_guess + 1, by = 0.1)


# Grid search for negative log-likelihood
negLL_matrix = matrix(NA, nrow = length(beta0_range), ncol = length(beta1_range))

for (i in 1:length(beta0_range)) {
  for (j in 1:length(beta1_range)) {
    beta0 = beta0_range[i]
    beta1 = beta1_range[j]
    y_hat = beta0 + beta1*x
    SSE = sum((y - y_hat)^2)
    negLL_matrix[i,j] = SSE / 2 
  }
}

# Find combination minimizing NLL
min_val = which(negLL_matrix == min(negLL_matrix), arr.ind = TRUE)
negLL_grid_beta0 = beta0_range[min_val[1]]
negLL_grid_beta1 = beta1_range[min_val[2]]

negLL_grid_beta0
negLL_grid_beta1

# estimated parameters of the neg LL grid are beta0 (intercept) = -1.37551
# and beta1 (slope) = 1.346694

## part b ##

#function
negLL<- function(par, x, y) {
  beta0 = par[1]
  beta1 = par[2]
  y_hat = beta0 + beta1*x
  SSE = sum((y - y_hat)^2)
  return(SSE / 2)
}

start_par = c(0, 0) 
opt_res = optim(par = start_par, fn = negLL, x = x, y = y)

optest_beta0_negLL = opt_res$par[1]
optest_beta1_negLL = opt_res$par[2]

optest_beta0_negLL
optest_beta1_negLL

# estimated parameters of optim() for neg LL are beta0 (intercept) = -1.366789
# and beta1 (slope) = 1.346519

## part c ##
opt_res$convergence == 0
check = list(c(0,0), c(10,10), c(-10,-10))
opt_results_negLL = lapply(check, function(start) {
  optim(par = start, fn = negLL, x = x, y = y)$par
})
opt_results_negLL
# checking for converged and if its sensitive to starting values. since
# values are all very similar, it is not sensitive to starting values.








######## OBJ4: compare results #########
# all methods produced very similar results for beta0 and beta1

# analytical solution: beta0 = -1.37551, beta1 = 1.345594

# OLS grid search: beta0 = -1.4, beta1 = 1.3
# OLS optim(): beta0 = -1.366789, beta1 = 1.346519

# neg LL grid search: beta0 = -1.4, beta1 = 1.3
# neg LL optim(): beta0 = -1.366789, beta1 = 1.346519

