
#Set metrics 
set.seed(42)
r <- 0.2
K <- 100
time = seq(from=1, to=50)
N = array(dim=c(1,length(time))); N[1]=0.05*K
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
set.seed(42)
r.q4 <- 0.2
K.q4 <- 100
time.q4 = seq(from=1, to=50)
N.q4 = array(dim=c(1,length(time.q4))); N.q4[1]=0.05*K.q4
for (i in time.q4 [2:length(time.q4)]) {
  n.q4 = N.q4[i-1]
  N.q4[i] = n.q4+(r.q4*n.q4)*(1-n.q4/K.q4)
}
data4 = N.q4 + rnorm(N.q4, mean = 0, sd = 0.5)

return_parameters4 <- function(x, y_pred, sigma){
  -sum(dnorm(x=x, mean = y_pred, sd = sigma, log=TRUE))
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
optfit <- optim(par=c(0.2, 5, 100, 0.5), fn=obj_func4)
waoptcoefs <- optfit$par

          