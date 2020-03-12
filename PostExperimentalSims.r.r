
# Code for posterior simulations. We sample new participants from estimated distribution of varying effects


`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
library(mvtnorm)
library(rethinking)
library(truncnorm)

N_sim = 10 #Number of simulations


#1. Baseline model without time-varying parameters

#Sample strategies from posteriors

#Extract samples from baseline model
samp <- extract.samples(m1)


#Compute correlation matrix from Cholesky factors
Correlations <- array(numeric(),c(10000,6,6))
for (i in 1:10000) {
  Correlations[i,,] <- samp$Rho_ID[i,,] %*% t(samp$Rho_ID[i,,])
}

sigma_id <- apply(samp$sigma_ID, 2, mean)
Corr_matrix <- apply( Correlations , c(2,3), mean)

# Compute variance - covariance matrix from variances and correlation matrix
S <- diag(sigma_id) %*%  Corr_matrix  %*% diag(sigma_id)


L <- mean(samp$log_L)
Beta <- mean(samp$gauss_beta)
f <- mean(samp$log_f)
Sigma <- mean(samp$logit_sigma)
Kappa <- mean(samp$logit_kappa)
Phi <- mean(samp$logit_phi)


#Simulation function
Sim_fct <- function(Nsim = N_sim, 
                    Tmax = 100,
                    Group_size = 4,
                    N_group= 2 ,
                    N_sessions = 25,
                    Payoff_Better = 19, 
                    Payoff_Worse = 10,
                    Hard_SD = 3,
                    Easy_SD = 1.5){

  Result <- list()
  
  for (sim in 1:Nsim) {
    
  #Create big output matrix
  d_Overall <- c()
  
  N_part= Group_size*N_group
  
  #Loop over different experimental sessions (8 participants each)
  
  for (session in 1:N_sessions) {
    
    
    #Create simulated participants divided into two groups who have attraction scores A_i
    Homunculi <- data.frame(id=1:8, L=NA, phi=NA, sigma=NA, f=NA, b=NA, kappa= NA, Group=c(rep(1,4), rep(2,4)), A1=NA, A2=NA, A3= NA, A4=NA)
    
    
    Id_s <- sample(1:200, 8)
    
    # Sample participants from multivariant normal population distribution
    
    Sim_parameters <- rmvnorm(200, c(L, f, Beta, Sigma, Kappa, Phi), S)
    
    Sim_parameters[,1] <- exp(Sim_parameters[,1])
    Sim_parameters[,2] <- exp(Sim_parameters[,2])
    Sim_parameters[,4] <- inv_logit(Sim_parameters[,4])
    Sim_parameters[,5] <- inv_logit(Sim_parameters[,5])
    Sim_parameters[,6] <- inv_logit(Sim_parameters[,6])   
    
    
    Homunculi$L <-     Sim_parameters[,1][((session-1)*8+1) : (session*8)]
    Homunculi$f <-    Sim_parameters[,2][((session-1)*8+1) : (session*8)]
    Homunculi$b <-    Sim_parameters[,3][((session-1)*8+1) : (session*8)]
    Homunculi$sigma <- Sim_parameters[,4][((session-1)*8+1) : (session*8)]
    Homunculi$kappa <-  Sim_parameters[,5][((session-1)*8+1) : (session*8)]
    Homunculi$phi <-   Sim_parameters[,6][((session-1)*8+1) : (session*8)]
    
    
    
    #Create output matrix to record choices and payoffs participants received as well as observed choices and levels of experience (equivalent to real experimental data)
    
    d <- data.frame(Session_ID=session,id=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),
                                                 rep(5,100),rep(6,100),rep(7,100),rep(8,100)),
                         trial= rep(1:Tmax, N_part), group.id=NA, Choice=NA, Correct=NA, Payoff=NA, SD_Payoff=NA, Experience = NA)
    
    #Define payoff schedule, change every 25 rounds, there are 2 hard and 2 easy phases
    Hard_Phases <- sample(1:4, 2)
    
    Times_Env_Change <- c(25,50,75)
    
    if (1 %in% Hard_Phases){
      d$SD_Payoff[which(d$trial %in% 1:25)] <- Hard_SD
    } else {
      d$SD_Payoff[which(d$trial %in% 1:25)] <- Easy_SD
    }
    
    if (2 %in% Hard_Phases){
      d$SD_Payoff[which(d$trial %in% 26:50)] <- Hard_SD
    } else {
      d$SD_Payoff[which(d$trial %in% 26:50)] <- Easy_SD
    }
    
    if (3 %in% Hard_Phases){
      d$SD_Payoff[which(d$trial %in% 51:75)] <- Hard_SD
    } else {
      d$SD_Payoff[which(d$trial %in% 51:75)] <- Easy_SD
    }
    
    if (4 %in% Hard_Phases){
      d$SD_Payoff[which(d$trial %in% 76:100)] <- Hard_SD
    } else {
      d$SD_Payoff[which(d$trial %in% 76:100)] <- Easy_SD
    }
    
    
    #4 different options
    Crops <- sample(1:4)
    
    #Define Optimal choice for all 100 rounds
    Better_choices <- matrix(nrow = Tmax, ncol = N_group)
    
    Better_choices[,1] <- c(rep(Crops[1],25), rep(Crops[3],25),rep(Crops[2],25), rep(Crops[4],25))
    Better_choices[,2] <- c(rep(Crops[2],25), rep(Crops[4],25),rep(Crops[1],25), rep(Crops[3],25))
    
    
    #Define migration shedule, migration every five rounds. Define when individuals from 1st group migrate, always switch with same "partner"
    Migration <- rep(c(0,0,0,0,1,0,0,0,0,2,0,0,0,0,3,0,0,0,0,4),5)
    

    #Initialize attractions, we assume there are no preexisting differences
    Homunculi$A1 <- 0
    Homunculi$A2 <- 0
    Homunculi$A3 <- 0
    Homunculi$A4 <- 0
    
    print(session)
    
    # Start simulation loop
    for (i in 1:Tmax) {
      
      #Loop over all individuals
      
      for (x in Homunculi$id){
        
        #Assign new vars for individual-specific parameter values, just to make code more readable
        sigma_Ind <- Homunculi$sigma[which(Homunculi$id==x)]
        phi_Ind <-   Homunculi$phi[which(Homunculi$id==x)]
        f_Ind <-     Homunculi$f[which(Homunculi$id==x)]
        b_Ind <-     Homunculi$b[which(Homunculi$id==x)]
        L_Ind <-     Homunculi$L[which(Homunculi$id==x)]
        kappa_Ind <- Homunculi$kappa[which(Homunculi$id==x)]
        
        Attractions <- c(Homunculi$A1[which(Homunculi$id==x)],Homunculi$A2[which(Homunculi$id==x)],Homunculi$A3[which(Homunculi$id==x)],Homunculi$A4[which(Homunculi$id==x)] )
        
        
        
        #Record group_id in output matrix
        d$group.id[which(d$id==x & d$trial==i)] <- Homunculi$Group[which(Homunculi$id==x)]
        
        #Record experience in present region
        #Individual has not migrated yet
        if (length(unique(na.omit(d$group.id[which(d$id==x)])))==1){
          d$Experience[which(d$id==x & d$trial==i)] <- i
          
          #Individual has migrated already
        } else {
          d$Experience[which(d$id==x & d$trial==i)] <- i- max(na.omit(d$trial[which(d$id==x & d$group.id != d$group.id[which(d$id==x & d$trial==i)])]))
        }
        #Get other current group members
        Group_members <- Homunculi$id[which(Homunculi$Group == Homunculi$Group[which(Homunculi$id == x)] & Homunculi$id != x)]
        
        #Indicates whether there is a newly arrived member, which cannot be copied
        New_Member <- d$id[which(d$id %in% Group_members & d$trial==i & d$Experience==1)]
        
        #Define vector of choices and ages of 3 social partners
        
        Choice_vec <- d$Choice[which(d$trial==i-1  & d$id %in% Group_members & d$id %not in% New_Member)]
        Age_vec    <- d$Experience[which(d$trial==i-1  & d$id %in% Group_members & d$id %not in% New_Member)]
        
        

        
        #Frequency of options
        n <- c()
        #Calculate age scores for all 4 options
        Ages <- rep(0,4)
        
        for (xx in 1:4) {
          n[xx] <- length(which(Choice_vec == xx))
          
          for (z in Age_vec[Choice_vec== xx]) {
            Ages[xx] <-  Ages[1] + exp(b_Ind*z)
          }
        }
        
        #Compute probability fo reach option depending on learning strategy
        
        pConf <- c()
        pAge  <- c()
        
        for (xx in 1:4) {
          pConf[xx] <- n[xx]^f_Ind / sum(n^f_Ind)
          pAge[xx]  <- Ages[xx] / sum(Ages) 
        }
      
        
        #Generate choice based on previous attraction score and choice of other group members

        #1st round
        #There are no choices to copy, so basic reinforcement equation
        
        Prob <- c()
        
        if(i==1){
          
          for (xx in 1:4) {
            Prob[xx] <- exp(L_Ind*Attractions[xx]) / sum(exp(L_Ind*Attractions))
          }
          
          #From 2nd round, combination of private and social information
        } else {
          
          for (xx in 1:4) {
            Prob[xx] <- (1-sigma_Ind) * exp(L_Ind*Attractions[xx]) / sum(exp(L_Ind*Attractions)) + sigma_Ind * ((1-kappa_Ind) * pConf[xx] + kappa_Ind * pAge[xx])
          }    
        
        }
        
        #Make choice proportional to attraction scores and social information
        d$Choice[which(d$id==x & d$trial==i)] <- sample(c(1:4), size = 1, prob = Prob)
        
        
        #Generate a payoff based on choice
        #Individual chose better option
        if (d$Choice[which(d$id==x & d$trial==i)] == Better_choices[i,Homunculi$Group[which(Homunculi$id == x)]]){
          d$Payoff[which(d$id==x & d$trial==i)] <- round(rtruncnorm(1, a=0, b=+Inf, mean=Payoff_Better, sd=d$SD_Payoff[which(d$id==x & d$trial==i)]))
          d$Correct[which(d$id==x & d$trial==i)] <- 1
          
          #Individual chose worse option
        } else {
          d$Payoff[which(d$id==x & d$trial==i)] <- round(rtruncnorm(1, a=0, b=+Inf, mean=Payoff_Worse, sd=d$SD_Payoff[which(d$id==x & d$trial==i)]))
          d$Correct[which(d$id==x & d$trial==i)] <- 0
        }
        
        #Update attraction scores based on payoff
        for (xx in 1:4) {
          if ( d$Choice[which(d$id==x & d$trial==i)] == xx) {
            Attractions[xx] <- (1-phi_Ind) * Attractions[xx] + phi_Ind * d$Payoff[which(d$id==x & d$trial==i)]
          } else {
            Attractions[xx] <- (1-phi_Ind) * Attractions[xx] 
          }
        }
        
          Homunculi$A1[which(Homunculi$id==x)] <- Attractions[1]
          Homunculi$A2[which(Homunculi$id==x)] <- Attractions[2]
          Homunculi$A3[which(Homunculi$id==x)] <- Attractions[3]
          Homunculi$A4[which(Homunculi$id==x)] <- Attractions[4]
        
      }#individual x
      
      #Migration
      #Always the same migration partner
      
      if (Migration[i] == 1){
        Old1 <- Homunculi$Group[which(Homunculi$id==1)]
        Old5 <- Homunculi$Group[which(Homunculi$id==5)]
        Homunculi$Group[which(Homunculi$id==1)] <- Old5
        Homunculi$Group[which(Homunculi$id==5)] <- Old1
      }
      if (Migration[i] == 2){
        Old2 <- Homunculi$Group[which(Homunculi$id==2)]
        Old6 <- Homunculi$Group[which(Homunculi$id==6)]
        Homunculi$Group[which(Homunculi$id==2)] <- Old6
        Homunculi$Group[which(Homunculi$id==6)] <- Old2
      }
      if (Migration[i] == 3){
        Old3 <- Homunculi$Group[which(Homunculi$id==3)]
        Old7 <- Homunculi$Group[which(Homunculi$id==7)]
        Homunculi$Group[which(Homunculi$id==3)] <- Old7
        Homunculi$Group[which(Homunculi$id==7)] <- Old3
      }
      if (Migration[i] == 4){
        Old4 <- Homunculi$Group[which(Homunculi$id==4)]
        Old8 <- Homunculi$Group[which(Homunculi$id==8)]
        Homunculi$Group[which(Homunculi$id==4)] <- Old8
        Homunculi$Group[which(Homunculi$id==8)] <- Old4
      }
      
    }#time i
    d_Overall <- rbind(d_Overall, d)
    
  }#session
  
  Result[[sim]] <- d_Overall
  }
  
  return(Result)
  
}#sim_funct


d <- Sim_fct() 

All_d <- d

meanCorrectperExpHard <- list()
meanCorrectperExpEasy <- list()
meanCorrectperTimeHard <- list()
meanCorrectperTimeEasy <- list()

for (x in 1:N_sim) {
  
  print(x)
  
  d <- All_d[[x]]
  
  
  d$id <- (d$Session_ID - 1)*8 + d$id
  d$Hard[d$SD_Payoff==3] <- 1
  d$Hard[d$SD_Payoff==1.5] <- 0
  d$TimeSinceChange <- NA
  d$ExpStable <- NA
  Times_Env_Change <- c(25,50,75)
  
  for (i in 1:nrow(d)){
    if (d$trial[i] <= 25){
      d$TimeSinceChange[i] <- d$trial[i]
    } else { 
      d$TimeSinceChange[i] <- d$trial[i] - max(Times_Env_Change[which(Times_Env_Change<d$trial[i])])
    }
    if (d$TimeSinceChange[i] >= d$Experience[i]){
      d$ExpStable[i] <- d$Experience[i]
    } else {
      d$ExpStable[i] <- d$TimeSinceChange[i]
    }
  }
  
  #Proportion Correct
  # Spatial
  CorrectperExpHard <- matrix(0, nrow = 200, ncol = 20)
  CorrectperExpEasy <- matrix(0, nrow = 200, ncol = 20)
  
  for (i in 1:20) {
    for (j in unique(d$id)) {
      CorrectperExpHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$Experience==i & d$trial != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$Experience==i& d$trial != 1 & d$Hard==1)])
      CorrectperExpEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$Experience==i & d$trial != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$Experience==i& d$trial != 1 & d$Hard==0)])
    }
  }
  
  se <- function(x) sd(x)/sqrt(length(x))
  
  meanCorrectperExpHard[[x]] <- apply(CorrectperExpHard,2, mean)
  meanCorrectperExpEasy[[x]] <- apply(CorrectperExpEasy,2, mean)
  
  
  # Temporal
  CorrectperTimeHard <- matrix(0, nrow = 200, ncol = 25)
  CorrectperTimeEasy <- matrix(0, nrow = 200, ncol = 25)
  
  for (i in 1:25) {
    for (j in unique(d$id)) {
      CorrectperTimeHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$trial != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$trial != 1 & d$Hard==1)])
      CorrectperTimeEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$trial != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$trial != 1 & d$Hard==0)])
    }
  }
  
  meanCorrectperTimeHard[[x]] <- apply(CorrectperTimeHard,2, mean)
  meanCorrectperTimeEasy[[x]] <- apply(CorrectperTimeEasy,2, mean)
  
}

meanCorrectExpHard <- c()
meanCorrectExpEasy <- c()
SDCorrectExpHard <- c()
SDCorrectExpEasy <- c()

for (x in 1:20) {
  p <- c()
  q <- c()
  for (y in 1:N_sim) {
    p <- c(p, meanCorrectperExpHard[[y]][x])
    q <- c(q, meanCorrectperExpEasy[[y]][x])
  }
  meanCorrectExpHard[x] <- mean(p)
  meanCorrectExpEasy[x] <- mean(q)
  
  SDCorrectExpHard[x] <- sd(p)
  SDCorrectExpEasy[x] <- sd(q)
}


meanCorrectTimeHard <- c()
meanCorrectTimeEasy <- c()
SDCorrectTimeHard <- c()
SDCorrectTimeEasy <- c()
for (x in 1:25) {
  p <- c()
  q <- c()
  for (y in 1:N_sim) {
    p <- c(p, meanCorrectperTimeHard[[y]][x])
    q <- c(q, meanCorrectperTimeEasy[[y]][x])
  }
  meanCorrectTimeHard[x] <- mean(p)
  meanCorrectTimeEasy[x] <- mean(q)
  
  SDCorrectTimeHard[x] <- sd(p)
  SDCorrectTimeEasy[x] <- sd(q)
}



#


#color stuff
require(RColorBrewer)#load package
x <- seq(from=0, to=1, by=0.2) # fake data
col.pal <- brewer.pal(length(x), "Dark2") #create a pallette which you loop over for corresponding values

graphics.off()
png("FigPostsims2.png", res = 1600, height = 16, width = 16, units = "cm")
par(mfrow = c(2,2), 
    mar= c(1,1,2,2), 
    oma =c(3,3,1,0))

plot(meanCorrectExpHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="Proportion Correct",xlab="", col = col.pal[1])
#arrows(1:20,meanCorrectperExpHard-SECorrectperExpHard,1:20,meanCorrectperExpHard+SECorrectperExpHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectExpEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ", main="Spatial Change (Migration)", xlab="", col = col.pal[2])
#arrows(1:20,meanCorrectperExpEasy-SECorrectperExpEasy,1:20,meanCorrectperExpEasy+SECorrectperExpEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "A", cex=1.1, bty="n")
text(10, 0.27, "Chance level", col="black", cex = 0.8)


plot(meanCorrectTimeHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="",xlab="", col = col.pal[1])
#arrows(1:25,meanCorrectperTimeHard-SECorrectperTimeHard,1:25,meanCorrectperTimeHard+SECorrectperTimeHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectTimeEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ", main="Temporal Change", xlab="", col = col.pal[2])
#arrows(1:25,meanCorrectperTimeEasy-SECorrectperTimeEasy,1:25,meanCorrectperTimeEasy+SECorrectperTimeEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "B", cex=1.1, bty="n")
text(12.5, 0.27, "Chance level", col="black", cex = 0.8)







# 2. Monotonic-effects model with time-varying parameters



samp <- extract.samples(m3)

#Sample strategies from posteriors
#Compute correlation matrix from Cholesky factors

Correlations <- array(numeric(),c(10000,12,12))

for (i in 1:10000) {
  Correlations[i,,] <- samp$Rho_ID[i,,] %*% t(samp$Rho_ID[i,,])
}


sigma_id <- apply(samp$sigma_ID, 2, mean)
Corr_matrix <- apply( Correlations , c(2,3), mean)


S <- diag(sigma_id) %*%  Corr_matrix  %*% diag(sigma_id)

sigma0 <- mean(samp$logit_sigma_first)
sigma1 <- mean(samp$logit_sigma_last)

beta0 <- mean(samp$Gauss_beta_first)
beta1 <- mean(samp$Gauss_beta_last)

f0 <- mean(samp$log_f_first)
f1 <- mean(samp$log_f_last)

kappa0 <- mean(samp$logit_kappa_first)
kappa1 <- mean(samp$logit_kappa_last)

L0 <- mean(samp$log_L_first)
L1 <- mean(samp$log_L_last)

phi0 <- mean(samp$logit_phi_first)
phi1 <- mean(samp$logit_phi_last)


# for utilities

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))


#Simulation function
Sim_fct <- function(Nsim =5, 
                    Tmax = 100,
                    Group_size = 4,
                    N_group= 2 ,
                    N_sessions = 25, 
                    Payoff_Better = 19, 
                    Payoff_Worse = 10,
                    Hard_SD = 3,
                    Easy_SD = 1.5){
  
  Result <- list()
  
  for (sim in 1:Nsim) {
    
    #Create big output matrix
    d_Overall <- c()
    
    N_part= Group_size*N_group
    
    #Loop over different experimental sessions (8 participants each)
    
    for (session in 1:N_sessions) {
      
      
      #Create simulated participants divided into two groups who have attraction scores A_i
      Homunculi <- data.frame(id=1:8, L=NA, phi=NA, sigma=NA, f=NA, b=NA, kappa= NA, Group=c(rep(1,4), rep(2,4)), A1=NA, A2=NA, A3= NA, A4=NA)
      
      
      # Sample participants from multivariant normal population distribution
      
      Sim_parameters <- rmvnorm(8, c(sigma0,sigma1,beta0 ,beta1 ,f0 ,f1 ,kappa0 ,kappa1 ,L0 ,L1 ,phi0 ,phi1), S)
      
      Sim_parameters[,1] <- inv_logit(Sim_parameters[,1])
      Sim_parameters[,2] <- inv_logit(Sim_parameters[,2])
      
      Sim_parameters[,5] <- exp(Sim_parameters[,5])
      Sim_parameters[,6] <- exp(Sim_parameters[,6])   
      
      Sim_parameters[,7] <- inv_logit(Sim_parameters[,7])
      Sim_parameters[,8] <- inv_logit(Sim_parameters[,8])
      
      Sim_parameters[,9] <- exp(Sim_parameters[,9])
      Sim_parameters[,10] <- exp(Sim_parameters[,10])   
      
      Sim_parameters[,11] <- inv_logit(Sim_parameters[,11])
      Sim_parameters[,12] <- inv_logit(Sim_parameters[,12])
      
      
      
      
      Sigma_matrix <- matrix(0, 8, 20)
      Delta_sigmas <- c()
      for (i in 1:19) {
        Delta_sigmas[i] <- mean(samp$delta_sigma[,i])
      }
      Delta_sigmas <- c(0, Delta_sigmas)
      for (ind in 1:8) {
        for (time in 1:20) {
          Sigma_matrix[ind,time] <-  Sim_parameters[ind,1] - (Sim_parameters[ind,1] - Sim_parameters[ind,2]) * sum(Delta_sigmas[1:time])
        }
      }
      
      
      Beta_matrix <- matrix(0, 8, 20)
      Delta_Beta <- c()
      for (i in 1:19) {
        Delta_Beta[i] <- mean(samp$delta_beta[,i])
      }
      Delta_Beta <- c(0, Delta_Beta)
      for (ind in 1:8) {
        for (time in 1:20) {
          Beta_matrix[ind,time] <-  Sim_parameters[ind,3] - (Sim_parameters[ind,3] - Sim_parameters[ind,4]) * sum(Delta_Beta[1:time])
        }
      }
      
      
      f_matrix <- matrix(0, 8, 20)
      Delta_f <- c()
      for (i in 1:19) {
        Delta_f[i] <- mean(samp$delta_f[,i])
      }
      Delta_f <- c(0, Delta_f)
      for (ind in 1:8) {
        for (time in 1:20) {
          f_matrix[ind,time] <-  Sim_parameters[ind,5] - (Sim_parameters[ind,5] - Sim_parameters[ind,6]) * sum(Delta_f[1:time])
        }
      }
      
      
      kappa_matrix <- matrix(0, 8, 20)
      Delta_kappa <- c()
      for (i in 1:19) {
        Delta_kappa[i] <- mean(samp$delta_kappa[,i])
      }
      Delta_kappa <- c(0, Delta_kappa)
      
      for (ind in 1:8) {
        for (time in 1:20) {
          kappa_matrix[ind,time] <-  Sim_parameters[ind,7] - (Sim_parameters[ind,7] - Sim_parameters[ind,8]) * sum(Delta_kappa[1:time])
        }
      }
      
      
      L_matrix <- matrix(0, 8, 20)
      Delta_L <- c()
      for (i in 1:19) {
        Delta_L[i] <- mean(samp$delta_L[,i])
      }
      Delta_L <- c(0, Delta_L)
      
      for (ind in 1:8) {
        for (time in 1:20) {
          L_matrix[ind,time] <-  Sim_parameters[ind,9] - (Sim_parameters[ind,9] - Sim_parameters[ind,10]) * sum(Delta_L[1:time])
        }
      }
      
      
      phi_matrix <- matrix(0, 8, 20)
      Delta_phi <- c()
      for (i in 1:19) {
        Delta_phi[i] <- mean(samp$delta_phi[,i])
      }
      Delta_phi <- c(0, Delta_phi)
      
      for (ind in 1:8) {
        for (time in 1:20) {
          phi_matrix[ind,time] <-  Sim_parameters[ind,11] - (Sim_parameters[ind,11] - Sim_parameters[ind,12]) * sum(Delta_phi[1:time])
        }
      }
      
      
      #Create output matrix to record choices and payoffs participants received as well as observed choices and levels of experience (equivalent to real experimental data)
      
      d <- data.frame(Session_ID=session,id=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),
                                              rep(5,100),rep(6,100),rep(7,100),rep(8,100)),
                      trial= rep(1:Tmax, N_part), group.id=NA, Choice=NA, Correct=NA, Payoff=NA, SD_Payoff=NA, Experience=NA, TimeSinceChange=NA, Exp_Stable=NA)
      
      #Define payoff schedule, change every 25 rounds, there are 2 hard and 2 easy phases
      Hard_Phases <- sample(1:4, 2)
      
      Times_Env_Change <- c(25,50,75)
      
      if (1 %in% Hard_Phases){
        d$SD_Payoff[which(d$trial %in% 1:25)] <- Hard_SD
      } else {
        d$SD_Payoff[which(d$trial %in% 1:25)] <- Easy_SD
      }
      
      if (2 %in% Hard_Phases){
        d$SD_Payoff[which(d$trial %in% 26:50)] <- Hard_SD
      } else {
        d$SD_Payoff[which(d$trial %in% 26:50)] <- Easy_SD
      }
      
      if (3 %in% Hard_Phases){
        d$SD_Payoff[which(d$trial %in% 51:75)] <- Hard_SD
      } else {
        d$SD_Payoff[which(d$trial %in% 51:75)] <- Easy_SD
      }
      
      if (4 %in% Hard_Phases){
        d$SD_Payoff[which(d$trial %in% 76:100)] <- Hard_SD
      } else {
        d$SD_Payoff[which(d$trial %in% 76:100)] <- Easy_SD
      }
      
      
      #4 different options
      Crops <- sample(1:4)
      
      #Define Optimal choice for all 100 rounds
      Better_choices <- matrix(nrow = Tmax, ncol = N_group)
      
      Better_choices[,1] <- c(rep(Crops[1],25), rep(Crops[3],25),rep(Crops[2],25), rep(Crops[4],25))
      Better_choices[,2] <- c(rep(Crops[2],25), rep(Crops[4],25),rep(Crops[1],25), rep(Crops[3],25))
      
      
      #Define migration shedule, migration every five rounds. Define when individuals from 1st group migrate, always switch with same "partner"
      Migration <- rep(c(0,0,0,0,1,0,0,0,0,2,0,0,0,0,3,0,0,0,0,4),5)
      
      
      #Initialize attractions, we assume there are no preexisting differences
      Homunculi$A1 <- 0
      Homunculi$A2 <- 0
      Homunculi$A3 <- 0
      Homunculi$A4 <- 0
      
      print(session)
      
      # Start simulation loop
      for (i in 1:Tmax) {
        
        #Loop over all individuals
        
        for (x in Homunculi$id){
          
          
          Attractions <- c(Homunculi$A1[which(Homunculi$id==x)],Homunculi$A2[which(Homunculi$id==x)],Homunculi$A3[which(Homunculi$id==x)],Homunculi$A4[which(Homunculi$id==x)] )
          
          
          
          #Record group_id in output matrix
          d$group.id[which(d$id==x & d$trial==i)] <- Homunculi$Group[which(Homunculi$id==x)]
          
          #Record experience in present region
          #Individual has not migrated yet
          if (length(unique(na.omit(d$group.id[which(d$id==x)])))==1){
            d$Experience[which(d$id==x & d$trial==i)] <- i
            
            #Individual has migrated already
          } else {
            d$Experience[which(d$id==x & d$trial==i)] <- i- max(na.omit(  d$trial[which(d$id==x & d$group.id != d$group.id[which(d$id==x & d$trial==i)])]  )    )
          }
          
          #Record experience in present environment so actual experience with one payoff structure
          #Environment has not changed yet
          if (i <= min(Times_Env_Change)){
            d$TimeSinceChange[which(d$id==x & d$trial==i)] <- i
            
            #Environment has changed
          } else {
            d$TimeSinceChange[which(d$id==x & d$trial==i)] <- i- max(Times_Env_Change[which(Times_Env_Change<i)])
          }
          
          if (d$TimeSinceChange[which(d$id==x & d$trial==i)] >= d$Experience[which(d$id==x & d$trial==i)]) {
            d$Exp_Stable[which(d$id==x & d$trial==i)] <- d$Experience[which(d$id==x & d$trial==i)]
          } else {
            d$Exp_Stable[which(d$id==x & d$trial==i)] <- d$TimeSinceChange[which(d$id==x & d$trial==i)]
          }
          
          
          #Get other current group members
          Group_members <- Homunculi$id[which(Homunculi$Group == Homunculi$Group[which(Homunculi$id == x)] & Homunculi$id != x)]
          
          #Indicates whether there is a newly arrived member, which cannot be copied
          New_Member <- d$id[which(d$id %in% Group_members & d$trial==i & d$Experience==1)]
          
          #Define vector of choices and ages of 3 social partners
          
          Choice_vec <- d$Choice[which(d$trial==i-1  & d$id %in% Group_members & d$id %not in% New_Member)]
          Age_vec    <- d$Experience[which(d$trial==i-1  & d$id %in% Group_members & d$id %not in% New_Member)]
          
          
          
          sigma_Ind <- Sigma_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          phi_Ind <-   phi_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          f_Ind <-     f_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          b_Ind <-     Beta_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          L_Ind <-     L_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          kappa_Ind <- kappa_matrix[which(Homunculi$id==x), d$Exp_Stable[which(d$id==x & d$trial==i)]]
          
          
          
          
          #Frequency of options
          n <- c()
          #Calculate age scores for all 4 options
          Ages <- rep(0,4)
          
          for (xx in 1:4) {
            n[xx] <- length(which(Choice_vec == xx))
            
            for (z in Age_vec[Choice_vec== xx]) {
              Ages[xx] <-  Ages[1] + exp(b_Ind*z)
            }
          }
          
          #Compute probability fo reach option depending on learning strategy
          
          pConf <- c()
          pAge  <- c()
          
          for (xx in 1:4) {
            pConf[xx] <- n[xx]^f_Ind / sum(n^f_Ind)
            pAge[xx]  <- Ages[xx] / sum(Ages) 
          }
          
          
          #Generate choice based on previous attraction score and choice of other group members
          
          #1st round
          #There are no choices to copy, so basic reinforcement equation
          
          Prob <- c()
          
          if(i==1){
            
            for (xx in 1:4) {
              Prob[xx] <- exp(L_Ind*Attractions[xx]) / sum(exp(L_Ind*Attractions))
            }
            
            #From 2nd round, combination of private and social information
          } else {
            
            for (xx in 1:4) {
              Prob[xx] <- (1-sigma_Ind) * exp(L_Ind*Attractions[xx]) / sum(exp(L_Ind*Attractions)) + sigma_Ind * ((1-kappa_Ind) * pConf[xx] + kappa_Ind * pAge[xx])
            }    
            
          }
          
          #Make choice proportional to attraction scores and social information
          d$Choice[which(d$id==x & d$trial==i)] <- sample(c(1:4), size = 1, prob = Prob)
          
          
          #Generate a payoff based on choice
          #Individual chose better option
          if (d$Choice[which(d$id==x & d$trial==i)] == Better_choices[i,Homunculi$Group[which(Homunculi$id == x)]]){
            d$Payoff[which(d$id==x & d$trial==i)] <- round(rtruncnorm(1, a=0, b=+Inf, mean=Payoff_Better, sd=d$SD_Payoff[which(d$id==x & d$trial==i)]))
            d$Correct[which(d$id==x & d$trial==i)] <- 1
            
            #Individual chose worse option
          } else {
            d$Payoff[which(d$id==x & d$trial==i)] <- round(rtruncnorm(1, a=0, b=+Inf, mean=Payoff_Worse, sd=d$SD_Payoff[which(d$id==x & d$trial==i)]))
            d$Correct[which(d$id==x & d$trial==i)] <- 0
          }
          
          #Update attraction scores based on payoff
          for (xx in 1:4) {
            if ( d$Choice[which(d$id==x & d$trial==i)] == xx) {
              Attractions[xx] <- (1-phi_Ind) * Attractions[xx] + phi_Ind * d$Payoff[which(d$id==x & d$trial==i)]
            } else {
              Attractions[xx] <- (1-phi_Ind) * Attractions[xx] 
            }
          }
          
          Homunculi$A1[which(Homunculi$id==x)] <- Attractions[1]
          Homunculi$A2[which(Homunculi$id==x)] <- Attractions[2]
          Homunculi$A3[which(Homunculi$id==x)] <- Attractions[3]
          Homunculi$A4[which(Homunculi$id==x)] <- Attractions[4]
          
        }#individual x
        
        #Migration
        #Always the same migration partner
        
        if (Migration[i] == 1){
          Old1 <- Homunculi$Group[which(Homunculi$id==1)]
          Old5 <- Homunculi$Group[which(Homunculi$id==5)]
          Homunculi$Group[which(Homunculi$id==1)] <- Old5
          Homunculi$Group[which(Homunculi$id==5)] <- Old1
        }
        if (Migration[i] == 2){
          Old2 <- Homunculi$Group[which(Homunculi$id==2)]
          Old6 <- Homunculi$Group[which(Homunculi$id==6)]
          Homunculi$Group[which(Homunculi$id==2)] <- Old6
          Homunculi$Group[which(Homunculi$id==6)] <- Old2
        }
        if (Migration[i] == 3){
          Old3 <- Homunculi$Group[which(Homunculi$id==3)]
          Old7 <- Homunculi$Group[which(Homunculi$id==7)]
          Homunculi$Group[which(Homunculi$id==3)] <- Old7
          Homunculi$Group[which(Homunculi$id==7)] <- Old3
        }
        if (Migration[i] == 4){
          Old4 <- Homunculi$Group[which(Homunculi$id==4)]
          Old8 <- Homunculi$Group[which(Homunculi$id==8)]
          Homunculi$Group[which(Homunculi$id==4)] <- Old8
          Homunculi$Group[which(Homunculi$id==8)] <- Old4
        }
        
      }#time i
      d_Overall <- rbind(d_Overall, d)
      
    }#session
    
    Result[[sim]] <- d_Overall
  }
  
  return(Result)
  
}#sim_funct




d <- Sim_fct() 


All_d <- d



meanCorrectperExpHard <- list()
meanCorrectperExpEasy <- list()

meanCorrectperTimeHard <- list()
meanCorrectperTimeEasy <- list()

for (x in 1:N_sim) {
  
  print(x)
  
  d <- All_d[[x]]
  
  
  d$id <- (d$Session_ID - 1)*8 + d$id
  
  d$Hard[d$SD_Payoff==3] <- 1
  d$Hard[d$SD_Payoff==1.5] <- 0
  
  d$TimeSinceChange <- NA
  d$ExpStable <- NA
  Times_Env_Change <- c(25,50,75)
  
  for (i in 1:nrow(d)){
    if (d$trial[i] <= 25){
      d$TimeSinceChange[i] <- d$trial[i]
    } else { 
      d$TimeSinceChange[i] <- d$trial[i] - max(Times_Env_Change[which(Times_Env_Change<d$trial[i])])
    }
    if (d$TimeSinceChange[i] >= d$Experience[i]){
      d$ExpStable[i] <- d$Experience[i]
    } else {
      d$ExpStable[i] <- d$TimeSinceChange[i]
    }
  }
  
  #Proportion Correct
  # Spatial
  CorrectperExpHard <- matrix(0, nrow = 200, ncol = 20)
  CorrectperExpEasy <- matrix(0, nrow = 200, ncol = 20)
  
  for (i in 1:20) {
    for (j in unique(d$id)) {
      CorrectperExpHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$Experience==i & d$trial != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$Experience==i& d$trial != 1 & d$Hard==1)])
      CorrectperExpEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$Experience==i & d$trial != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$Experience==i& d$trial != 1 & d$Hard==0)])
    }
  }
  
  se <- function(x) sd(x)/sqrt(length(x))
  
  meanCorrectperExpHard[[x]] <- apply(CorrectperExpHard,2, mean)
  meanCorrectperExpEasy[[x]] <- apply(CorrectperExpEasy,2, mean)
  
  
  # Temporal
  CorrectperTimeHard <- matrix(0, nrow = 200, ncol = 25)
  CorrectperTimeEasy <- matrix(0, nrow = 200, ncol = 25)
  
  for (i in 1:25) {
    for (j in unique(d$id)) {
      CorrectperTimeHard[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$trial != 1 & d$Hard==1)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$trial != 1 & d$Hard==1)])
      CorrectperTimeEasy[which(unique(d$id)==j), i] <- sum(d$Correct[which(d$id == j & d$TimeSinceChange==i & d$trial != 1 & d$Hard==0)])/length(d$Correct[which(d$id == j &d$TimeSinceChange==i& d$trial != 1 & d$Hard==0)])
    }
  }
  
  meanCorrectperTimeHard[[x]] <- apply(CorrectperTimeHard,2, mean)
  meanCorrectperTimeEasy[[x]] <- apply(CorrectperTimeEasy,2, mean)
  
}



meanCorrectExpHard <- c()
meanCorrectExpEasy <- c()

SDCorrectExpHard <- c()
SDCorrectExpEasy <- c()

for (x in 1:20) {
  p <- c()
  q <- c()
  for (y in 1:N_sim) {
    p <- c(p, meanCorrectperExpHard[[y]][x])
    q <- c(q, meanCorrectperExpEasy[[y]][x])
  }
  meanCorrectExpHard[x] <- mean(p)
  meanCorrectExpEasy[x] <- mean(q)
  
  SDCorrectExpHard[x] <- sd(p)
  SDCorrectExpEasy[x] <- sd(q)
}



meanCorrectTimeHard <- c()
meanCorrectTimeEasy <- c()
SDCorrectTimeHard <- c()
SDCorrectTimeEasy <- c()
for (x in 1:25) {
  p <- c()
  q <- c()
  for (y in 1:N_sim) {
    p <- c(p, meanCorrectperTimeHard[[y]][x])
    q <- c(q, meanCorrectperTimeEasy[[y]][x])
  }
  meanCorrectTimeHard[x] <- mean(p)
  meanCorrectTimeEasy[x] <- mean(q)
  
  SDCorrectTimeHard[x] <- sd(p)
  SDCorrectTimeEasy[x] <- sd(q)
}



plot(meanCorrectExpHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="Proportion Correct",xlab="", col = col.pal[1])
#arrows(1:20,meanCorrectperExpHard-SECorrectperExpHard,1:20,meanCorrectperExpHard+SECorrectperExpHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectExpEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ",  xlab="", col = col.pal[2])
#arrows(1:20,meanCorrectperExpEasy-SECorrectperExpEasy,1:20,meanCorrectperExpEasy+SECorrectperExpEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "C", cex=1.1, bty="n")
text(10, 0.27, "Chance level", col="black", cex = 0.8)


plot(meanCorrectTimeHard, type="l", lwd=2, lty=2, ylim=c(0.1,0.75), ylab="",xlab="", col = col.pal[1])
#arrows(1:25,meanCorrectperTimeHard-SECorrectperTimeHard,1:25,meanCorrectperTimeHard+SECorrectperTimeHard, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[1])
par(new=TRUE)
plot(meanCorrectTimeEasy, type="l",lwd=2,lty=1,  ylim=c(0.1,0.75), ylab=" ", xlab="", col = col.pal[2])
#arrows(1:25,meanCorrectperTimeEasy-SECorrectperTimeEasy,1:25,meanCorrectperTimeEasy+SECorrectperTimeEasy, code=3, lwd=1, length=0.02, angle = 90, col = col.pal[2])
abline(h = 0.25, lty=2)
legend("topleft", "D", cex=1.1, bty="n")
text(12.5, 0.27, "Chance level", col="black", cex = 0.8)

legend("bottomright", c("Easy Phases", "Hard Phases"), col = c(col.pal[2],col.pal[1]), lty=c(1,2), lwd=2, bty ="n", cex=0.9)
mtext("Round after change", side = 1, outer = TRUE, line = 2, cex = 1)

mtext("Proportion optimal choices", side = 2, outer = TRUE, line = 2, cex = 1)
dev.off()

