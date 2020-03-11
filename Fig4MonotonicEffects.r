
library(rethinking)
m <- extract.samples(m4)
#Monotonic effects

#Sigma
Delta_sigmas <- c()

for (i in 1:19) {
  Delta_sigmas[i] <- mean(m$delta_sigma[,i])
}
Delta_sigmas <- c(0, Delta_sigmas)

m_Mat_sigma <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_sigma[j,i] <- inv_logit(mean(m$logit_sigma_first) + mean(m$v_ID[,j,1])) -
                       (inv_logit(mean(m$logit_sigma_first) + mean(m$v_ID[,j,1]))- inv_logit(mean(m$logit_sigma_last) + mean(m$v_ID[,j,2])))*
                        sum(Delta_sigmas[1:i])
  }
}
Mean_M_mat_sigma <- apply(m_Mat_sigma,2,mean)


#f
Delta_f <- c()

for (i in 1:19) {
  Delta_f[i] <- mean(m$delta_f[,i])
}
Delta_f <- c(0, Delta_f)

m_Mat_f <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_f[j,i] <- exp(mean(m$log_f_first) + mean(m$v_ID[,j,5])) -
                   (exp(mean(m$log_f_first) + mean(m$v_ID[,j,5])) - exp(mean(m$log_f_last) + mean(m$v_ID[,j,6]))) *
                    sum(Delta_f[1:i])
  }
}
Mean_m_Mat_f <- apply(m_Mat_f,2,mean)



#beta
Delta_beta <- c()

for (i in 1:19) {
  Delta_beta[i] <- mean(m$delta_beta[,i])
}
Delta_beta <- c(0, Delta_beta)

m_Mat_beta <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_beta[j,i] <- (mean(m$Gauss_beta_first) + mean(m$v_ID[,j,3])) -
                      ((mean(m$Gauss_beta_first) + mean(m$v_ID[,j,3])) - (mean(m$Gauss_beta_last) + mean(m$v_ID[,j,4]))) *
                       sum(Delta_beta[1:i])
  }
}
Mean_m_Mat_beta <- apply(m_Mat_beta,2,mean)


#kappa
Delta_kappa <- c()

for (i in 1:19) {
  Delta_kappa[i] <- mean(m$delta_kappa[,i])
}
Delta_kappa <- c(0, Delta_kappa)

m_Mat_kappa <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_kappa[j,i] <- inv_logit(mean(m$logit_kappa_first) + mean(m$v_ID[,j,7])) -
                       (inv_logit(mean(m$logit_kappa_first) + mean(m$v_ID[,j,7])) - inv_logit(mean(m$logit_kappa_last) + mean(m$v_ID[,j,8]))) *
                        sum(Delta_kappa[1:i])
  }
}
Mean_m_Mat_kappa <- apply(m_Mat_kappa,2,mean)




#phi
Delta_phi <- c()

for (i in 1:19) {
  Delta_phi[i] <- mean(m$delta_phi[,i])
}
Delta_phi <- c(0, Delta_phi)

m_Mat_phi <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_phi[j,i] <- inv_logit(mean(m$logit_phi_first) + mean(m$v_ID[,j,11])) -
      (inv_logit(mean(m$logit_phi_first) + mean(m$v_ID[,j,11])) - inv_logit(mean(m$logit_phi_last) + mean(m$v_ID[,j,12]))) *
      sum(Delta_phi[1:i])
  }
}
Mean_m_Mat_phi <- apply(m_Mat_phi,2,mean)


#L
Delta_L <- c()

for (i in 1:19) {
  Delta_L[i] <- mean(m$delta_L[,i])
}
Delta_L <- c(0, Delta_L)

m_Mat_L <- matrix(0, nrow = 200, ncol = 20)
for (j in 1:200) {
  for (i in 1:20) {
    m_Mat_L[j,i] <- exp(mean(m$log_L_first) + mean(m$v_ID[,j,9])) -
      (exp(mean(m$log_L_first) + mean(m$v_ID[,j,9])) - exp(mean(m$log_L_last) + mean(m$v_ID[,j,10]))) *
      sum(Delta_L[1:i])
  }
}
Mean_m_Mat_L <- apply(m_Mat_L,2,mean)









#color stuff
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values



graphics.off()

png("Fig4MonotonicEffectsNEW.png", res = 1200, height = 15, width = 22, units = "cm")


par(mfrow= c(2,3), 
    mar=c(1,4,2,0.4), 
    oma=c(3,0,0,0))

plot(Mean_M_mat_sigma, ylim=c(0,1), ylab="",type="n", xlab="", lwd=3, col="black")
for (j in 1:200) {
  lines(m_Mat_sigma[j,], type="l", ylim=c(0,1), ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_M_mat_sigma, type="l", ylim=c(0,1), ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Weight of social learning  ", italic(sigma))), cex = 0.9)
legend("topleft", "A", cex=1.1, bty="n")


plot(Mean_m_Mat_f, ylab="",type="n", xlab="", lwd=3, col="black", ylim=c(0,5))
for (j in 1:200) {
  lines(m_Mat_f[j,], type="l",  ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_m_Mat_f, type="l", ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Conformity exponent  ", italic(f))), cex = 0.9)
legend("topleft", "B", cex=1.1, bty="n")
abline(h=1, lty=2)

plot(Mean_m_Mat_beta, ylim=c(-0.25,1), ylab="",type="n", xlab="", lwd=3, col="black")
for (j in 1:200) {
  lines(m_Mat_beta[j,], type="l", ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_m_Mat_beta, type="l", ylim=c(-0.25,1), ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Experience bias  ", italic(beta))), cex = 0.9)
legend("topleft", "C", cex=1.1, bty="n")
abline(h=0, lty=2)


plot(Mean_m_Mat_kappa, ylim=c(0,1), ylab="",type="n", xlab="", lwd=3, col="black")
for (j in 1:200) {
  lines(m_Mat_kappa[j,], type="l", ylim=c(0,1), ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_m_Mat_kappa, type="l", ylim=c(0,1), ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Weight of experience bias  ", italic(kappa))), cex = 0.9)
legend("topleft", "D", cex=1.1, bty="n")


plot(Mean_m_Mat_phi, ylim=c(0,1), ylab="",type="n", xlab="", lwd=3, col="black")
for (j in 1:200) {
  lines(m_Mat_phi[j,], type="l", ylim=c(0,1), ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_m_Mat_phi, type="l", ylim=c(0,1), ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Learning rate  ", italic(phi))), cex = 0.9)
legend("topleft", "E", cex=1.1, bty="n")


plot(Mean_m_Mat_L, ylim=c(0,0.3), ylab="",type="n", xlab="", lwd=3, col="black")
for (j in 1:200) {
  lines(m_Mat_L[j,], type="l", ylim=c(0,1), ylab="", xlab="", lwd=1,col=alpha(col.pal[2],alpha = 0.2))
}
lines(Mean_m_Mat_L, type="l", ylim=c(0,1), ylab="", xlab="", lwd=3, col="black")
mtext(side = 2, line = 2.2 , expression(paste("Exploration rate  ", italic(lambda))), cex = 0.9)
legend("topleft", "F", cex=1.1, bty="n")


mtext(side = 1, line = 1.4 , "Round after migration", outer = TRUE, cex = 0.9)

dev.off()
