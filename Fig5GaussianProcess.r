
setwd("C:/Users/dominik_deffner/Documents/GitHub/Experience-Social-Learning")

m <- extract.samples(m8)

Upper_M_mat_sigma <- c()
Mean_M_mat_sigma <- c()
Lower_M_mat_sigma <- c()

Upper_M_mat_sigma2 <- c()
Lower_M_mat_sigma2 <- c()


for (i in 1:20) {
  Mean_M_mat_sigma[i]  <- inv_logit(mean(m$mean_sigma)    + mean(m$dev_sigma[,i]))
  Lower_M_mat_sigma[i] <- inv_logit(HPDI(m$mean_sigma)[1] + mean(m$dev_sigma[,i]))
  Upper_M_mat_sigma[i] <- inv_logit(HPDI(m$mean_sigma)[2] + mean(m$dev_sigma[,i]))
  
  Lower_M_mat_sigma2[i] <- inv_logit(HPDI(m$mean_sigma)[1] + HPDI(m$dev_sigma[,i])[1])
  Upper_M_mat_sigma2[i] <- inv_logit(HPDI(m$mean_sigma)[2] + HPDI(m$dev_sigma[,i])[2])
}


#GP curves for f


Upper_m_Mat_f <- c()
Mean_m_Mat_f <- c()
Lower_m_Mat_f <- c()

Upper_m_Mat_f2 <- c()
Lower_m_Mat_f2 <- c()

for (i in 1:20) {
  Mean_m_Mat_f[i] <- exp(mean(m$mean_f)  + mean(m$dev_f[,i]))
  Lower_m_Mat_f[i] <- exp(HPDI(m$mean_f)[1]  +  mean(m$dev_f[,i]))
  Upper_m_Mat_f[i] <- exp(HPDI(m$mean_f)[2]  +  mean(m$dev_f[,i]))
  
  Lower_m_Mat_f2[i] <- exp(HPDI(m$mean_f)[1]  + HPDI(m$dev_f[,i])[1])
  Upper_m_Mat_f2[i] <- exp(HPDI(m$mean_f)[2]  + HPDI(m$dev_f[,i])[2])
}



#GP curves for beta
Upper_m_Mat_beta <- c()
Mean_m_Mat_beta <- c()
Lower_m_Mat_beta <- c()

Upper_m_Mat_beta2 <- c()
Lower_m_Mat_beta2 <- c()

for (i in 1:20) {
  Mean_m_Mat_beta[i] <- mean(m$mean_beta)  + mean(m$dev_beta[,i])
  Lower_m_Mat_beta[i] <- HPDI(m$mean_beta)[1]  +  mean(m$dev_beta[,i])
  Upper_m_Mat_beta[i] <- HPDI(m$mean_beta)[2]  +  mean(m$dev_beta[,i])
  
  Lower_m_Mat_beta2[i] <- HPDI(m$mean_beta)[1]  + HPDI(m$dev_beta[,i])[1]
  Upper_m_Mat_beta2[i] <- HPDI(m$mean_beta)[2]  + HPDI(m$dev_beta[,i])[2]
}

#GP curves for kappa
Upper_M_mat_kappa <- c()
Mean_M_mat_kappa <- c()
Lower_M_mat_kappa <- c()

Upper_M_mat_kappa2 <- c()
Lower_M_mat_kappa2 <- c()

for (i in 1:20) {
  Mean_M_mat_kappa[i]  <- inv_logit(mean(m$mean_kappa) + mean(m$dev_kappa[,i]))
  Lower_M_mat_kappa[i] <- inv_logit(HPDI(m$mean_kappa)[1] +  mean(m$dev_kappa[,i]))
  Upper_M_mat_kappa[i] <- inv_logit(HPDI(m$mean_kappa)[2] +  mean(m$dev_kappa[,i]))
  
  Lower_M_mat_kappa2[i] <- inv_logit(HPDI(m$mean_kappa)[1] + HPDI(m$dev_kappa[,i])[1])
  Upper_M_mat_kappa2[i] <- inv_logit(HPDI(m$mean_kappa)[2] + HPDI(m$dev_kappa[,i])[2])
}


#GP curves for phi
Upper_M_mat_phi <- c()
Mean_M_mat_phi <- c()
Lower_M_mat_phi <- c()

Upper_M_mat_phi2 <- c()
Lower_M_mat_phi2 <- c()

for (i in 1:20) {
  Mean_M_mat_phi[i]  <- inv_logit(mean(m$mean_phi) + mean(m$dev_phi[,i]))
  Lower_M_mat_phi[i] <- inv_logit(HPDI(m$mean_phi)[1] + mean(m$dev_phi[,i]))
  Upper_M_mat_phi[i] <- inv_logit(HPDI(m$mean_phi)[2] + mean(m$dev_phi[,i]))
  
  Lower_M_mat_phi2[i] <- inv_logit(HPDI(m$mean_phi)[1] + HPDI(m$dev_phi[,i])[1])
  Upper_M_mat_phi2[i] <- inv_logit(HPDI(m$mean_phi)[2] + HPDI(m$dev_phi[,i])[2])
}


#GP curves for L


Upper_m_Mat_L <- c()
Mean_m_Mat_L <- c()
Lower_m_Mat_L <- c()

Upper_m_Mat_L2 <- c()
Lower_m_Mat_L2 <- c()

for (i in 1:20) {
  Mean_m_Mat_L[i] <- exp(mean(m$mean_lambda)  + mean(m$dev_L[,i]))
  Lower_m_Mat_L[i] <- exp(HPDI(m$mean_lambda)[1]  +  mean(m$dev_L[,i]))
  Upper_m_Mat_L[i] <- exp(HPDI(m$mean_lambda)[2]  +  mean(m$dev_L[,i]))
  
  Lower_m_Mat_L2[i] <- exp(HPDI(m$mean_lambda)[1]  + HPDI(m$dev_L[,i])[1])
  Upper_m_Mat_L2[i] <- exp(HPDI(m$mean_lambda)[2]  + HPDI(m$dev_L[,i])[2])
}

#color stuff
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values



graphics.off()
png("Fig5GP.png", res = 1600, height = 15, width = 22, units = "cm")
par(mfrow = c(2,3), 
    mar=c(1,4,2,0.4), 
    oma=c(3,0,0,0))

plot(Lower_M_mat_sigma, type = "n", xlab = "", ylab = "", ylim = c(0,1),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_M_mat_sigma, rev(Lower_M_mat_sigma)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(0,1))
polygon(c(1:20,20:1), c(Upper_M_mat_sigma2, rev(Lower_M_mat_sigma2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
par(new=TRUE)
plot(Mean_M_mat_sigma ,type="l",col="black",xlab = "", ylab = "",  ylim = c(0,1), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Weight of social learning  ", italic(sigma))), cex = 0.9)
legend("topleft", "A", cex=1.1, bty="n")


plot(Lower_m_Mat_f, type = "n", xlab = "", ylab = "", ylim = c(0,5),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_m_Mat_f, rev(Lower_m_Mat_f)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(0,5))
polygon(c(1:20,20:1), c(Upper_m_Mat_f2, rev(Lower_m_Mat_f2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,5))
par(new=TRUE)
plot(Mean_m_Mat_f ,type="l",col="black",xlab = "", ylab = "",  ylim = c(0,5), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Conformity exponent  ", italic(f))), cex = 0.9)
legend("topleft", "B", cex=1.1, bty="n")
abline(h=1, lty =2)


plot(Lower_m_Mat_beta, type = "n", xlab = "", ylab = "", ylim = c(-0.25,1),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_m_Mat_beta, rev(Lower_m_Mat_beta)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(-1,1))
polygon(c(1:20,20:1), c(Upper_m_Mat_beta2, rev(Lower_m_Mat_beta2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(-1,1))
par(new=TRUE)
plot(Mean_m_Mat_beta ,type="l",col="black",xlab = "", ylab = "",  ylim = c(-0.25,1), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Experience bias  ", italic(beta))), cex = 0.9)
legend("topleft", "C", cex=1.1, bty="n")
abline(h=0, lty =2)


plot(Lower_M_mat_kappa, type = "n", xlab = "", ylab = "", ylim = c(0,1),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_M_mat_kappa, rev(Lower_M_mat_kappa)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(0,1))
polygon(c(1:20,20:1), c(Upper_M_mat_kappa2, rev(Lower_M_mat_kappa2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))
par(new=TRUE)
plot(Mean_M_mat_kappa ,type="l",col="black",xlab = "", ylab = "",  ylim = c(0,1), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Weight of experience bias  ", italic(kappa))), cex = 0.9)
legend("topleft", "D", cex=1.1, bty="n")


plot(Lower_M_mat_phi, type = "n", xlab = "", ylab = "", ylim = c(0,1),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_M_mat_phi, rev(Lower_M_mat_phi)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(0,1))
polygon(c(1:20,20:1), c(Upper_M_mat_phi2, rev(Lower_M_mat_phi2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,1))

par(new=TRUE)
plot(Mean_M_mat_phi ,type="l",col="black",xlab = "", ylab = "",  ylim = c(0,1), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Learning rate  ", italic(phi))), cex = 0.9)
legend("topleft", "E", cex=1.1, bty="n")

plot(Lower_m_Mat_L, type = "n", xlab = "", ylab = "", ylim = c(0,0.3),col=alpha(col.pal[1], alpha=0.2))
polygon(c(1:20,20:1), c(Upper_m_Mat_L, rev(Lower_m_Mat_L)), col=alpha(col.pal[1],alpha = 0.5), border = NA, ylim=c(0,0.3))
polygon(c(1:20,20:1), c(Upper_m_Mat_L2, rev(Lower_m_Mat_L2)), col=alpha(col.pal[1],alpha = 0.2), border = NA, ylim=c(0,0.3))

par(new=TRUE)
plot(Mean_m_Mat_L ,type="l",col="black",xlab = "", ylab = "",  ylim = c(0,0.3), lwd=3)
mtext(side = 2, line = 2.2 , expression(paste("Exploration rate  ", italic(lambda))), cex = 0.9)
legend("topleft", "F", cex=1.1, bty="n")






mtext(side = 1, line = 1.4 , "Round after migration", outer = TRUE, cex = 0.9)


dev.off()
