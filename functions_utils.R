## All small functions here 

# library(pacman)
# p_load(clue, mcclust)

logit <- function(p){log(p/(1-p))}
softmax <- function(x){exp(x)/(sum(exp(x)))}
logit.inv <- function(x)(1/(1+exp(-x)))

rcategorical <- function(n, pr){ sapply(1:n, function(x)
{
  sample(1:length(pr),size = 1, prob = pr)
})}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

## for network & admixtures

pMatrix.min <- function(A, B) { 
  n <- nrow(A) 
  D <- matrix(NA, n, n) 
  for (i in 1:n) { 
    for (j in 1:n) { 
      D[j, i] <- (sum((B[j, ] - A[i, ])^2)) 
    } } 
  vec <- c(solve_LSAP(D)) 
  list(A=A[vec,], pvec=vec) 
} 

# commerror<-function(commtrue,commest){
#   n = length(commtrue)
#   K = max(commtrue)
#   if (length(commest) != n) stop("lengths of community vectors are different")
#   if (length(unique(commest)) != K) stop("number of communities are different")
#   tab = table(commtrue,commest)
#   mat = diag(table(commtrue))
#   X <- pMatrix.min(tab,mat) 
#   foo = X$A
#   return((sum(foo) - sum(diag(foo)))/n)
# }

## Plot themes 

mapTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    panel.border = element_blank()
  )
}

plotTheme <- function() {
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 12, family = "sans", face = "italic", hjust = 0), 
    axis.title.x = element_text(size = rel(1.1), family = "sans", face = "plain", hjust = 1, vjust = -0.5),
    axis.title.y = element_text(size = rel(1.1), family = "sans", face = "plain", hjust = 1, vjust = 1),
    axis.text = element_text(size = rel(1.1), family = "sans", face = "plain"),
    panel.background = element_blank(),
    panel.grid.minor = element_line(colour = "gray"),
    panel.grid.major = element_line(colour = "gray"),
    axis.ticks = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans"),
    axis.line = element_blank()
  )
}

## AAL-BAAL

# for(i in 1:N){
#  lp_ci= rep(0,K)
#  for(h in 1:K){
#    lik = dmultinom(Y[i,], prob = pi.init[h,], log = T)  + log(lambda.init[h])
#    lp_ci[h] = lik
#  }
#  new_c <- rcategorical(1,exp(lp_ci))
# }
# (dir_lik = dirichlet::ddirichlet(pi.init[h,], alpha = Y[i,] + prior.pars$beta.vec))



