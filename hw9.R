
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
#making changes in order to push again
return_parameters <- function(time, y_pred, sd){
  -sum(dnorm(x=time, mean = y_pred, sd = sd, log=TRUE))
}


# Estimate parameters using optim()
optfit <- optim(par=c(0,1,2), fn=obj_func)
optcoefs <- optfit$par

calc_nll <- function(r, K, sd) {
  N1 = array(dim=c(1,length(time))); N1[1]=0.05*K
  for (i in time [2:length(time)]) {
    n = N1[i-1]
    y_pred = n+(r*n)*(1-n/K)
  }
  nll <- return_parameters(data$x, y_pred, sd = sd)
}

optim(par = c(0, 1, 2), fn = calc_nll, data = data)
