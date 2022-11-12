## evaluating horwitz-thompson estimator
## n --- number of observations in estimating phi
## xi --- inclusion probability for R

eval_horwitz_thompson <- function(n, Y, R, X, xi)
{
  n <- length(Y)
  if(n != length(R)) stop("Y and R don't match")
  ## compute Horwitz-Thompson estimator
  psi.ht <- mean (R * Y / xi[X])
  
  return(psi.ht = psi.ht)
}

## simulating hajek estimator
## n --- number of observations in estimating phi
## xi --- inclusion probability for R


eval_hajek <- function(n, Y, R, X, xi)
{
  n <- length(Y)
  if(n != length(R)) stop("Y and R don't match")
  ## compute Horwitz-Thompson estimator
  psi.hajek <- mean (R*Y/xi[X])/mean(R/xi[X])
  
  return(psi.hajek = psi.hajek)
}

## evaluating Bayes estimator
## n --- number of observations in estimating phi
## alpha --- parameter of Beta prior for 'phi'

eval_bayes <- function(n, Y, R, alpha)
{
  n <- length(Y)
  if(n != length(R)) stop("Y and R don't match")
  ## compute Bayes estimator
  psi.bayes <-  (sum (Y[R==1]) + alpha) / (sum (R == 1) + 2 * alpha)
  return(psi.bayes = psi.bayes)
}

## evaluating jkg's estimator 
## n --- number of observations in estimating phi

library(Hmisc)
library(dplyr)

midcut<-function(x,from,to,by){
  ## cut the data into bins...
  x=cut(x,seq(from,to,by),include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=seq(from+by/2,to-by/2,by)
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}

eval_jkg <- function(n, Y, R, X, delta, k=10)
{
  n <- length(Y)
  if(n != length(R)) stop("Y and R don't match")
  
  ## compute JKG's estimator
  
  dat = data.frame(Y = Y, R = R, X = X, xi_x = xi[X])
  dat2 <- dat %>% mutate(xi_gps = cut2(xi_x, g = k), 
                         xi_mids = midcut(xi_x, delta, 1-delta, by = (1-2*delta)/k)) %>%
                 group_by(xi_gps) %>% 
                 dplyr::summarize(mean_RY = mean(Y*R), mean_R = mean(R), n_j = n(), xi_mid = mean(xi_mids))
    
  psi.num  = sum((dat2$n_j/n)*dat2$mean_RY/dat2$xi_mid)
  psi.denom = sum((dat2$n_j/n)*dat2$mean_R/dat2$xi_mid)
  psi.jkg1 = psi.num
  psi.jkg2 = psi.num/psi.denom
  
  return(list(psi.jkg1 = psi.jkg1, psi.jkg2 = psi.jkg2))
}



