
# Script for Fig. 3 - Contrast between temporal and spatial changes


mCont <- extract.samples(m7)
sigmaTemp <- inv_logit( mCont$b0_sigma + mCont$btemp_sigma )
sigmaSpat <- inv_logit( mCont$b0_sigma + mCont$bspat_sigma )
sigmaCont <- sigmaSpat-sigmaTemp


kappaTemp <- inv_logit( mCont$b0_kappa + mCont$btemp_kappa )
kappaSpat <- inv_logit( mCont$b0_kappa + mCont$bspat_kappa )
kappaCont <- kappaSpat-kappaTemp

betaTemp <- mCont$b0_beta + mCont$btemp_beta 
betaSpat <- mCont$b0_beta + mCont$bspat_beta
betaCont <- betaSpat- betaTemp

fTemp <- exp( mCont$b0_f + mCont$btemp_f )
fSpat <- exp( mCont$b0_f + mCont$bspat_f )
fCont <- fSpat- fTemp

library(scales)   #for the transparent colors

#color stuff
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values


graphics.off()
png("Fig3Simple.png", res = 1500, height = 16, width = 16, units = "cm")

par(mfrow = c(2,2), 
    mar= c(2.5,1,2,2.5), 
    oma =c(1,2.7,1,0))

#SIGMA

dens <- density(sigmaSpat)
x1 <- min(which(dens$x >= quantile(sigmaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(sigmaSpat, 1)))
plot(dens, xlim = c(0,1), ylim = c(0,20), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(sigmaTemp)
x1 <- min(which(dens$x >= quantile(sigmaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(sigmaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,20), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext(expression(paste("Weight of social learning  ", italic(sigma))), cex = 1, side = 3, line = 0)
legend("topleft","A",bty = "n",cex = 1.1)


#f
dens <- density(fSpat)
x1 <- min(which(dens$x >= quantile(fSpat, 0)))  
x2 <- max(which(dens$x <  quantile(fSpat, 1)))
plot(dens, xlim = c(0,5), ylim = c(0,1), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(fTemp)
x1 <- min(which(dens$x >= quantile(fTemp, 0)))  
x2 <- max(which(dens$x <  quantile(fTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,5), ylim = c(0,1), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext(expression(paste("Conformity exponent  ", italic(f))), cex = 1, side = 3, line = 0)
legend("topleft","B",bty = "n",cex = 1.1)


#beta
dens <- density(betaSpat)
x1 <- min(which(dens$x >= quantile(betaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(betaSpat, 1)))
plot(dens, xlim = c(-1,2), ylim = c(0,2), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(betaTemp)
x1 <- min(which(dens$x >= quantile(betaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(betaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(-1,2), ylim = c(0,2), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext(expression(paste("Experience bias  ", italic(beta))),  cex = 1, side = 3, line = 0)
legend("topleft","C",bty = "n",cex = 1.1)

#kappa
dens <- density(kappaSpat)
x1 <- min(which(dens$x >= quantile(kappaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(kappaSpat, 1)))
plot(dens, xlim = c(0,1), ylim = c(0,9), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(kappaTemp)
x1 <- min(which(dens$x >= quantile(kappaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(kappaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,9), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext("Density", side = 2, line = 1.5, outer = TRUE, cex = 1.2)
mtext(expression(paste("Weight of experience bias  ", italic(kappa))),  cex = 1, side = 3, line = 0)
legend("right", title = "Environmental Change", c("Spatial (Migration)", "Temporal"), col = c(alpha(col.pal[4],alpha = 0.6),alpha(col.pal[5],alpha = 0.6)), lwd = 10, bty="n")
legend("topleft","D",bty = "n",cex = 1.1)

dev.off()


graphics.off()
png("Fig3Contrasts.png", res = 1500, height = 16, width = 16, units = "cm")

par(mfrow = c(2,2), 
    mar= c(2.5,1,2.5,2), 
    oma =c(1,2.7,1,0))

#SIGMA
dens <- density(sigmaCont)
x1 <- min(which(dens$x >= quantile(sigmaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(sigmaCont, 0.945 )))
plot(dens, xlim = c(-0.2,0.4), ylim = c(0,20), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
abline(v=0, lty=2)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border =NA))
mtext(expression(paste("Weight of social learning  ", italic(sigma))), cex = 1, side = 3, line = 0)
legend("topleft","A",bty = "n",cex = 1.1)

#f
dens <- density(fCont)
x1 <- min(which(dens$x >= quantile(fCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(fCont, 0.945 )))
plot(dens, xlim = c(-1,3), ylim = c(0,1), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
mtext(expression(paste("Conformity exponent  ", italic(f))), cex = 1, side = 3, line = 0)
legend("topleft","B",bty = "n",cex = 1.1)


#beta
dens <- density(betaCont)
x1 <- min(which(dens$x >= quantile(betaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(betaCont, 0.945 )))
plot(dens, xlim = c(-1,1.5), ylim = c(0,2), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0),  col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
mtext(expression(paste("Experience bias  ", italic(beta))),  cex = 1, side = 3, line = 0)
legend("topleft","C",bty = "n",cex = 1.1)

#kappa
dens <- density(kappaCont)
x1 <- min(which(dens$x >= quantile(kappaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(kappaCont, 0.945 )))
plot(dens, xlim = c(-0.3,0.3), ylim = c(0,9), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
mtext("Density", side = 2, line = 1.5, outer = TRUE, cex = 1.2)
mtext(expression(paste("Weight of experience bias  ", italic(kappa))),  cex = 1, side = 3, line = 0)
legend("topleft","D",bty = "n",cex = 1.1)

dev.off()



graphics.off()
png("Fig3Panel.png", res = 1200, height = 16, width = 24, units = "cm")

par(mfrow = c(2,4), 
    mar= c(4.5,0.5,1,2.5), 
    oma =c(0,2.9,1.4,0))

#SIGMA
dens <- density(sigmaSpat)
x1 <- min(which(dens$x >= quantile(sigmaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(sigmaSpat, 1)))
plot(dens, xlim = c(0,1), ylim = c(0,20), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))
legend("topright", title = "Type of Change", c("Spatial", "Temporal"), col = c(alpha(col.pal[4],alpha = 0.6),alpha(col.pal[5],alpha = 0.6)), lwd = 10, bty="n")

dens <- density(sigmaTemp)
x1 <- min(which(dens$x >= quantile(sigmaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(sigmaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,20), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
legend("topleft","A",bty = "n",cex = 1.1)
mtext(expression(paste("Weight of social learning  ", italic(sigma))),  cex = 1.2, side = 3, line = 0.5, at=1.2)

par(mar= c(4,1,1,2.5))


dens <- density(sigmaCont)
x1 <- min(which(dens$x >= quantile(sigmaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(sigmaCont, 0.945 )))
plot(dens, xlim = c(-0.4,0.4), ylim = c(0,20), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
abline(v=0, lty=2)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border =NA))
legend("topleft","B",bty = "n",cex = 1.1)
legend("topright", "Contrast", bty="n", cex=1.1) 

par(mar=c(4,2.5,1,1.5))

#f
dens <- density(fSpat)
x1 <- min(which(dens$x >= quantile(fSpat, 0)))  
x2 <- max(which(dens$x <  quantile(fSpat, 1)))
plot(dens, xlim = c(0,5), ylim = c(0,1), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(fTemp)
x1 <- min(which(dens$x >= quantile(fTemp, 0)))  
x2 <- max(which(dens$x <  quantile(fTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,5), ylim = c(0,1), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
legend("topleft","C",bty = "n",cex = 1.1)
mtext(expression(paste("Conformity exponent  ", italic(f))), cex = 1.2, side = 3, line = 0.5, at=6)

par(mar=c(4,1,1,2))

dens <- density(fCont)
x1 <- min(which(dens$x >= quantile(fCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(fCont, 0.945 )))
plot(dens, xlim = c(-3,3), ylim = c(0,1), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
legend("topright", "Contrast", bty="n", cex=1.1) 
legend("topleft","D",bty = "n",cex = 1.1)


#beta
dens <- density(betaSpat)
x1 <- min(which(dens$x >= quantile(betaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(betaSpat, 1)))
plot(dens, xlim = c(-1,2), ylim = c(0,2), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(betaTemp)
x1 <- min(which(dens$x >= quantile(betaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(betaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(-1,2), ylim = c(0,2), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext(expression(paste("Experience bias  ", italic(beta))),  cex = 1.2, side = 3, line = 0.5, at=2.5)
legend("topleft","E",bty = "n",cex = 1.1)

dens <- density(betaCont)
x1 <- min(which(dens$x >= quantile(betaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(betaCont, 0.945 )))
plot(dens, xlim = c(-1.5,1.5), ylim = c(0,2), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0),  col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
legend("topleft","F",bty = "n",cex = 1.1)
legend("topright", "Contrast", bty="n", cex=1.1) 


#kappa
dens <- density(kappaSpat)
x1 <- min(which(dens$x >= quantile(kappaSpat, 0)))  
x2 <- max(which(dens$x <  quantile(kappaSpat, 1)))
plot(dens, xlim = c(0,1), ylim = c(0,9), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[4],alpha = 0.6), border = NA))

dens <- density(kappaTemp)
x1 <- min(which(dens$x >= quantile(kappaTemp, 0)))  
x2 <- max(which(dens$x <  quantile(kappaTemp, 1)))
par(new = TRUE)
plot(dens, xlim = c(0,1), ylim = c(0,9), type="n", ann = FALSE)
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha(col.pal[5],alpha = 0.6), border = NA))
mtext("Density", side = 2, line = 1.5, outer = TRUE, cex = 1.2)
mtext(expression(paste("Weight of experience bias  ", italic(kappa))),  cex = 1.2, side = 3, line = 0.5, at=1.2)
legend("topleft","G",bty = "n",cex = 1.1)

dens <- density(kappaCont)
x1 <- min(which(dens$x >= quantile(kappaCont, 0.055)))  
x2 <- max(which(dens$x <  quantile(kappaCont, 0.945 )))
plot(dens, xlim = c(-0.3,0.3), ylim = c(0,9), type="l", ann = FALSE,col=alpha("black",alpha = 0.6))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=alpha("black",alpha = 0.6), border = NA))
abline(v=0, lty=2)
legend("topright", "Contrast", bty="n", cex=1.1) 
mtext("Density", side = 2, line = 1.5, outer = TRUE, cex = 1.2)
legend("topleft","H",bty = "n",cex = 1.1)

dev.off()


