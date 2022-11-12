### Generate data for Wasserman's example 
### 
## X_i ~ Discrete Uniform(1, ..., B), B very large, i = 1, .., n
## p_j ~ grid values from delta to 1-delta (delta small), j = 1, .., B
## R_i ~ Bernoulli(p_X_i) 
## theta_j ~ U(lb, ub), j = 1,..., B. 
## Y_i ~ Bernoulli(theta_X_i) if R_i = 1 // 0 if R_i = 0. 

rm(list = ls())

setwd("~/R/IPW")


source("~/R/IPW/ipw_methods.r")
source("~/R/IPW/functions_utils.R")

library(ggplot2)
set.seed(12345)
B = 1000
n = 100
delta = 0.01
lb = 0.35; ub = 0.65

theta = runif(B, lb, ub)
(psi = mean(theta))
xi = seq(delta, 1-delta, length.out = B)

neval = 100
niter = 100

bayes_mse = numeric(niter); ht_mse = numeric(niter); 
hajek_mse = numeric(niter); jkg_mse = numeric(niter)

for(iter in 1:niter){
  bayes.fit = numeric(neval)
  ht.fit = numeric(neval)
  hajek.fit = numeric(neval)
  jkg.fit = numeric(neval)
  
  for(i in 1:neval){
    X <- sample(1:B, n, replace = T)
    ## draw Bernoulli random numbers with probabilities xi[X]
    R <- (runif(n) < xi[X]) * 1
    Y <- (runif(n) < theta[X]) * 1
    
    bayes.fit[i] <- eval_bayes(n, Y, R, alpha = 1)
    ht.fit[i] <- eval_horwitz_thompson(n, Y, R, X, xi)
    hajek.fit[i] <- eval_hajek(n,Y,R,X, xi)
    jkg.fit[i] <- eval_jkg(n, Y, R, X, delta, k = 10)$psi.jkg2
  }
  
  bayes_square_errors <- (bayes.fit- mean(theta))^2
  bayes_mse[iter] <- mean(bayes_square_errors)
  # bayes_sd[iter] <- sd(bayes_square_errors)/sqrt(neval)
  
  ht_square_errors <- (ht.fit- mean(theta))^2
  ht_mse[iter] <- mean(ht_square_errors)
  # ht_sd[iter] <- sd(ht_square_errors)/sqrt(neval)
  
  hajek_square_errors <- (hajek.fit- mean(theta))^2
  hajek_mse[iter] <- mean(hajek_square_errors)
  # ht_sd[iter] <- sd(ht_square_errors)/sqrt(neval)
  
  jkg_square_errors <- (jkg.fit- mean(theta))^2
  jkg_mse[iter] <- mean(jkg_square_errors)
  
}


# plot(bayes_mse)
# plot(ht_mse)

library(ggplot2)
library(viridis)
library(latex2exp)

fit.data = rbind(data.frame(MSE = bayes.fit, method = "Bayes"),
                 data.frame(MSE = ht.fit, method = "HT"),
                 data.frame(MSE = hajek.fit, method = "Hajek"),
                 data.frame(MSE = jkg.fit, method = "Binning-smoothing"))

(plt <- ggplot(fit.data, aes(MSE, fill = method)) + 
    geom_histogram(alpha=0.75,binwidth = 0.05, position="identity",aes(y = ..density..))+
    geom_density(alpha=0.5,adjust = 1/2, stat="density",position="identity",aes(y = ..density..))+
    geom_vline(xintercept=psi)+coord_flip()+xlab(expression(psi))+
    facet_grid(~method)+ 
    # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    # geom_hline(yintercept = psi)+
    # geom_jitter(color="black", size=0.4, alpha=0.9) +
    # ggtitle(paste0("MSE comparison for HT, Bayes, Hajek, and JKG \n for B =", B, ", n=", n, ", delta =", delta))+
    plotTheme()
  )

ggsave(paste0("~/R/IPW/art/","rrw_histogram_lb",10*lb,"ub",10*ub,".pdf"), plt, width = 7, height = 5, device = cairo_pdf)


mse.data = rbind(data.frame(MSE = bayes_mse, method = "Bayes"),
                 data.frame(MSE = ht_mse, method = "HT"),
                 data.frame(MSE = hajek_mse, method = "Hajek"),
                 data.frame(MSE = jkg_mse, method = "Binning-smoothing"))

(plt <- ggplot(mse.data, aes(y = MSE, x = method, fill = method)) + geom_boxplot(alpha = 0.6)+
    # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    # ggtitle(paste0("MSE comparison for HT, Bayes, Hajek, and JKG \n for B =", B, ", n=", n, ", delta =", delta))+
    plotTheme())

ggsave(paste0("~/R/IPW/art/","rrw_compare_lb",10*lb,"ub",10*ub,".pdf"), plt, width = 7, height = 5, device = cairo_pdf)

library(dplyr)

mse.table <- mse.data %>% 
  group_by(method) %>% 
  summarize(mean = mean(MSE), sd = sd(MSE))

mse.table %>% as.data.frame()
