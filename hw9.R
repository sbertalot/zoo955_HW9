
#Set metrics 
set.seed(42)
r <- 0.2
K <- 100
time = seq(from=1, to=50)
N = array(dim=c(1,length(time))); N[1]=0.9*K
for (i in time [2:length(time)]) {
  n = N[i-1]
  N[i] = n+(r*n)*(1-n/K)
}

#Q1:
#Plot the data
plot(N[1:50])

#Generate a model predicted time series
data <- data.frame(
  x = 1:50,
  y = c(N[1:50])
)


model = nls(y ~ SSlogis(x, a, b, c), data = data)

lines(data$x, predict(model))
coef(model)

#Q3 Create a function that returns the negative log likelihood of the 
#model parameters (r, K) given the data (Nt)
return_parameters <- function(x, y_pred){
  -sum(dnorm(x=x, mean = y_pred, log=TRUE))
}

obj_func <- function(par){
  r <- par[1]
  N <- par[2]
  K <- par[3]
  y_pred <-  N + (r*N)*((1-N)/K)
  nll <- return_parameters(N, y_pred)
}


# Estimate parameters using optim()
optfit <- optim(par=c(16.481497, 5.038222, 100.463116), fn=obj_func)
optcoefs <- optfit$par

#################### QUESTION 4

N.error = N + rnorm(N, mean = 0, sd = 5)

return_parameters4 <- function(x, y_pred, sigma){
  -sum(dnorm(x=data4, mean = y_pred, sd = sigma, log=TRUE))
}


obj_func4 <- function(par){
  r <- par[1]
  N <- par[2]
  K <- par[3]
  sigma <- par[4]
  y_pred <-  (N + (r*N)*((1-N)/K))
  nll <- return_parameters4(N, y_pred, sigma)
}


# Estimate parameters using optim()
optfit <- optim(par=c(0.2, 5, 100, 5), fn=obj_func4)
optcoefs.4 <- optfit$par


calc_nll_new <- function(r, K, sigma, N){
  y_pred <- (N + (r*N)*((1-N)/K))
  nll <- return_parameters4(y_obs, y_pred, sigma)
}

x <- seq(-15,15,1)
y <- seq(80,120, 1)
sigma_hold <- 5

# Setup grid search
nll_matrix <- matrix(data=NA, nrow=length(x), ncol=length(y), 
                     dimnames=list(x, y))
# Begin grid search: i <- 1; j <- 1
for(i in 1:length(x)){
  x_curr <- x[i]
  for(j in 1:length(y)){
    y_curr <- y[j]
    nll <- calc_nll_new(x_curr, y_curr, sigma_hold, N.error)
    nll_matrix[i,j] <- nll
  }
}

persp(x=x, y=y, z=nll_matrix, theta=90, phi=20)

#There is a ridge along the likelihood surface coorelating with the estimated K value, the estimate r value seems to be better represented



