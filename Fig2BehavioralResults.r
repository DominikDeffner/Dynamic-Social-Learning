

#Proportion Correct
# Spatial
CorrectperExpHard <- matrix(0, nrow = 200, ncol = 20)
CorrectperExpEasy <- matrix(0, nrow = 200, ncol = 20)

for (i in 1:20) {
  for (j in unique(d$id)) {
    CorrectperExpHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$ExperienceSelf==i & d$Round != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$ExperienceSelf==i& d$Round != 1 & d$Hard==1)])
    CorrectperExpEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$ExperienceSelf==i & d$Round != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$ExperienceSelf==i& d$Round != 1 & d$Hard==0)])
     }
}

se <- function(x) sd(x)/sqrt(length(x))
meanCorrectperExpHard <- apply(CorrectperExpHard,2, mean)
SECorrectperExpHard <- apply(CorrectperExpHard,2, se)
meanCorrectperExpEasy <- apply(CorrectperExpEasy,2, mean)
SECorrectperExpEasy <- apply(CorrectperExpHard,2, se)


# Temporal
CorrectperTimeHard <- matrix(0, nrow = 200, ncol = 25)
CorrectperTimeEasy <- matrix(0, nrow = 200, ncol = 25)

for (i in 1:25) {
  for (j in unique(d$id)) {
    CorrectperTimeHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$Round != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$Round != 1 & d$Hard==1)])
    CorrectperTimeEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$Round != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$Round != 1 & d$Hard==0)])
  }
}

meanCorrectperTimeHard <- apply(CorrectperTimeHard,2, mean)
SECorrectperTimeHard <- apply(CorrectperTimeHard,2, se)
meanCorrectperTimeEasy <- apply(CorrectperTimeEasy,2, mean)
SECorrectperTimeEasy <- apply(CorrectperTimeHard,2, se)

#Proportion Vied
# Spatial
ChoiceperExp <- matrix(0, nrow = 200, ncol = 20)
ExpperExp <- matrix(0, nrow = 200, ncol = 20)

for (i in 1:20) {
  for (j in unique(d$id)) {
    ChoiceperExp[which(unique(d$id)==j), i] <- sum(d$Prop_Choice[which(d$id == j & d$ExperienceSelf==i & d$Round != 1 & d$Hard==1)])/length(d$Prop_Choice[which(d$id == j &d$ExperienceSelf==i& d$Round != 1 & d$Hard==1)])
    ExpperExp[which(unique(d$id)==j), i]    <- sum(d$Prop_Exp[which(d$id == j & d$ExperienceSelf==i & d$Round != 1 & d$Hard==0)])/length(d$Prop_Exp[which(d$id == j &d$ExperienceSelf==i& d$Round != 1 & d$Hard==0)])
  }
}

meanChoiceperExp <- apply(ChoiceperExp,2, mean)
SEChoiceperExp <- apply(ChoiceperExp,2, se)
meanExpperExp <- apply(ExpperExp,2, mean)
SEExpperExp <- apply(ExpperExp,2, se)


# Temporal
ChoiceperTime <- matrix(0, nrow = 200, ncol = 25)
ExpperTime <- matrix(0, nrow = 200, ncol = 25)

for (i in 1:25) {
  for (j in unique(d$id)) {
    ChoiceperTime[which(unique(d$id)==j), i] <- sum(d$Prop_Choice[which(d$id == j & d$TimeSinceChange==i & d$Round != 1 & d$Hard==1)])/length(d$Prop_Choice[which(d$id == j &d$TimeSinceChange==i& d$Round != 1 & d$Hard==1)])
    ExpperTime[which(unique(d$id)==j), i]    <- sum(d$Prop_Exp[which(d$id == j & d$TimeSinceChange==i & d$Round != 1 & d$Hard==0)])/length(d$Prop_Exp[which(d$id == j &d$TimeSinceChange==i& d$Round != 1 & d$Hard==0)])
  }
}

meanChoiceperTime<- apply(ChoiceperTime,2, mean)
SEChoiceperTime <- apply(ChoiceperTime,2, se)
meanExpperTime <- apply(ExpperTime,2, mean)
SEExpperTime <- apply(ExpperTime,2, se)

#color stuff
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values



graphics.off()
png("Fig2BehavioralResults.png", res = 1600, height = 16, width = 16, units = "cm")
par(mfrow = c(2,2), 
    mar= c(1,1,2,2), 
    oma =c(3,3,1,0))

plot(meanCorrectperExpHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="Proportion Correct",xlab="", col = col.pal[1])
arrows(1:20,meanCorrectperExpHard-SECorrectperExpHard,1:20,meanCorrectperExpHard+SECorrectperExpHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectperExpEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ", main="Spatial Change (Migration)", xlab="", col = col.pal[2])
arrows(1:20,meanCorrectperExpEasy-SECorrectperExpEasy,1:20,meanCorrectperExpEasy+SECorrectperExpEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "A", cex=1.1, bty="n")
text(10, 0.27, "Chance level", col="black", cex = 0.8)


plot(meanCorrectperTimeHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="",xlab="", col = col.pal[1])
arrows(1:25,meanCorrectperTimeHard-SECorrectperTimeHard,1:25,meanCorrectperTimeHard+SECorrectperTimeHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectperTimeEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ", main="Temporal Change", xlab="", col = col.pal[2])
arrows(1:25,meanCorrectperTimeEasy-SECorrectperTimeEasy,1:25,meanCorrectperTimeEasy+SECorrectperTimeEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "B", cex=1.1, bty="n")
text(12.5, 0.27, "Chance level", col="black", cex = 0.8)

legend("bottomright", c("Easy Phases", "Hard Phases"), col = c(col.pal[2],col.pal[1]), lty=c(1,2), lwd=2, bty ="n", cex=0.9)

plot(meanChoiceperExp, type="l", lwd=2, lty=2, ylim=c(0.3,1), ylab="Proportion ",xlab="", col = col.pal[3])
arrows(1:20,meanChoiceperExp-SEChoiceperExp,1:20,meanChoiceperExp+SEChoiceperExp, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[3])
par(new=TRUE)

plot(meanExpperExp, type="l",lwd=2,lty=1,  ylim=c(0.3,1), ylab=" ", xlab="", col = col.pal[6])
arrows(1:20,meanExpperExp-SEExpperExp,1:20,meanExpperExp+SEExpperExp, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[6])
legend("topleft", "C", cex=1.1, bty="n")



plot(meanChoiceperTime, type="l", lwd=2, lty=2, ylim=c(0.3,1), ylab="",xlab="", col = col.pal[3])
arrows(1:25,meanChoiceperTime-SEChoiceperTime,1:25,meanChoiceperTime+SEChoiceperTime, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[3])
par(new=TRUE)

plot(meanExpperTime, type="l",lwd=2,lty=1,  ylim=c(0.3,1), ylab=" ", xlab="", col = col.pal[6])
arrows(1:25,meanExpperTime-SEExpperTime,1:25,meanExpperTime+SEExpperTime, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[6])
legend("topleft", "D", cex=1.1, bty="n")


legend("bottomright", c("Choice Boxes", "Experience Boxes"), col = c(col.pal[3],col.pal[6]),lty = c(2,1), lwd=2, bty ="n", cex=0.9)
mtext("Round after change", side = 1, outer = TRUE, line = 2, cex = 1)

mtext("Proportion boxes viewed                   Proportion optimal choices", side = 2, outer = TRUE, line = 2, cex = 1)

dev.off()
