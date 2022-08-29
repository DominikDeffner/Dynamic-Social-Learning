
###Dynamic Social Learning in Temporally and Spatially Variable Environments ###
### Data preparation script ###

library(readxl)
library(tidyverse)
library(rethinking)

#Load data
d <- read.csv("data.csv")

#Variable that indicates whether choice in round was "correct"
d$Correct <- ifelse(d$ChoiceSelf == d$Optimal, 1, 0)

#Number of models that chose option 1 to 4

d$n1 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==1))
d$n2 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==2))
d$n3 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==3))
d$n4 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==4))

# NAs are coded as zero, so they don't enter into learning model
d$n1[is.na(d$n1)] <- 0
d$n2[is.na(d$n2)] <- 0
d$n3[is.na(d$n3)] <- 0
d$n4[is.na(d$n4)] <- 0

#Process Mouse Tracking data
{

  #Define functions for processing of mouselab tracking data
  #Takes a string of process trace data and makes it into a data frame where each row is an event; columns are event, name, value, time
  parse.proctrace <- function( x ) {
    xx <- gsub( "  " , "\n" , x , fixed=TRUE ) #Substitute space with \n indicating line breaks
    z <- textConnection(xx)
    d <- read.csv( z )
    close(z)
    d
  }
  
  # Take a parsed trace and collapse mouseover and mouseout, making time into duration in box
  condense.proctrace <- function( input ) {
    #out <- input[1,] # copy load event
    r <- 2
    f.out.made <- FALSE
    while( r < length(input[,1]) ) {
      row1 <- input[r,]
      if( row1$event == "mouseover" ) {
        row2 <- input[r+1,]
        t1 <- row1$time
        t2 <- row2$time
        dur <- t2 - t1
        row1$time <- dur
        
        if ( f.out.made ) {
          out <- rbind( out , row1 )
        } else {
          out <- row1
          f.out.made <- TRUE
        }
        r <- r + 2
      }
    }
    out
  }
  
  # Wrap two functions above
  clean.proctrace <- function( x ) {
    d <- parse.proctrace( x )
    condense.proctrace( d )
  }
  
  # Count total time in ms spent in box with name "name"
  dviewed.proctrace <- function( x , name ) {
    d <- 0
    for( r in 1:length(x[,1]) ) {
      if( x[r,2] == name ) d <- d + x[r,4]
    }
    d
  }
  
  # Create list with cleaned protrace data
  
  Clean.protrace.list <- list()
  for (i in 1:nrow(d)) {
    #Check if it's first round or individuals didn't open any box. In that case insert 0
    if (is.na(d$MouseTracking[i]) == TRUE | nrow(parse.proctrace(d$MouseTracking[i])) < 2 ){
      Clean.protrace.list[[i]] <- 0
    } else {
      Clean.protrace.list[[i]] <- clean.proctrace(d$MouseTracking[i])
    }
  }
  
  #Compute for each box time spent in it in ms
  #Initialize variables
  d$ChoiceSelf_ms <- 0
  d$Payoff_ms <- 0
  d$ExperienceSelf_ms <- 0
  d$Choice1_ms <- 0
  d$Choice2_ms <- 0
  d$Choice3_ms <- 0
  d$Experience1_ms <- 0
  d$Experience2_ms <- 0
  d$Experience3_ms <- 0
  
  for (i in 1:nrow(d)) {
    if (mode(Clean.protrace.list[[i]]) != "numeric"){
      d$ChoiceSelf_ms[i]     <- dviewed.proctrace(Clean.protrace.list[[i]], "CropSelf")
      d$Payoff_ms[i]         <- dviewed.proctrace(Clean.protrace.list[[i]], "PayoffSelf")
      d$ExperienceSelf_ms[i] <- dviewed.proctrace(Clean.protrace.list[[i]], "ExpSelf")
      d$Choice1_ms[i]        <- dviewed.proctrace(Clean.protrace.list[[i]], "Crop1")
      d$Choice2_ms[i]        <- dviewed.proctrace(Clean.protrace.list[[i]], "Crop2")
      d$Choice3_ms[i]        <- dviewed.proctrace(Clean.protrace.list[[i]], "Crop3")
      d$Experience1_ms[i]    <- dviewed.proctrace(Clean.protrace.list[[i]], "Exp1")
      d$Experience2_ms[i]    <- dviewed.proctrace(Clean.protrace.list[[i]], "Exp2")
      d$Experience3_ms[i]    <- dviewed.proctrace(Clean.protrace.list[[i]], "Exp3")
    }
  }
  
  #Compute proportion of neighbors inspected and duration per trial for choice boxes and experience boxes 
  d$Prop_Choice <- sapply(1:nrow(d),function (i) sum(c(d$Choice1_ms[i], d$Choice2_ms[i],d$Choice3_ms[i])>0) / 3)
  d$Dur_Choice  <- sapply(1:nrow(d),function (i) sum(c(d$Choice1_ms[i], d$Choice2_ms[i],d$Choice3_ms[i])))
  d$Prop_Exp    <- sapply(1:nrow(d),function (i) sum(c(d$Experience1_ms[i], d$Experience2_ms[i],d$Experience3_ms[i])>0) / 3)
  d$Dur_Exp     <- sapply(1:nrow(d),function (i) sum(c(d$Experience1_ms[i], d$Experience2_ms[i],d$Experience3_ms[i])))
  
}


#Include mouse tracking data to condition learning models on information actually accessed by individuals

Check_fct <- function (i, x) {
  Obs1 <- ifelse(d$Choice1_ms[i] > 0, d$Choice1[i],0)
  Obs2 <- ifelse(d$Choice2_ms[i] > 0, d$Choice2[i],0)
  Obs3 <- ifelse(d$Choice3_ms[i] > 0, d$Choice3[i],0)
  return(sum(c(Obs1, Obs2, Obs3)==x))
}

# Create variables equivalent to above but incorporating mouse tracking info

#Number of models that chose option 1 to 4
d$n1_obs <- sapply(1:nrow(d), Check_fct, x=1)
d$n2_obs <- sapply(1:nrow(d), Check_fct, x=2)
d$n3_obs <- sapply(1:nrow(d), Check_fct, x=3)
d$n4_obs <- sapply(1:nrow(d), Check_fct, x=4)

d$n1_obs[is.na(d$n1_obs)] <- 0
d$n2_obs[is.na(d$n2_obs)] <- 0
d$n3_obs[is.na(d$n3_obs)] <- 0
d$n4_obs[is.na(d$n4_obs)] <- 0

#Social information from group members: 0 if not viewed by participant
d$Choice1_obs <- ifelse(d$Choice1_ms > 0 , d$Choice1, 0 )
d$Choice2_obs <- ifelse(d$Choice2_ms > 0 , d$Choice2, 0 )
d$Choice3_obs <- ifelse(d$Choice3_ms > 0 , d$Choice3, 0 )

d$Experience1_obs <- ifelse(d$Experience1_ms > 0 , d$Experience1, 0 )
d$Experience2_obs <- ifelse(d$Experience2_ms > 0 , d$Experience2, 0 )
d$Experience3_obs <- ifelse(d$Experience3_ms > 0 , d$Experience3, 0 )


#Computer phase (1 to 4), times of temporal changes and time since last temporal change
d$Phase <- c()
d$Phase <- as.integer( ceiling( d$Round / 25 ) )
d$TempChange <- sapply( 1:nrow(d) , function(i) {
  if ( d$Round[i]>1 ) {
    return( ifelse(d$Phase[i]!=d$Phase[i-1],1,0) )
  } else {
    return(0)
  }
 }
)

d$TimeSinceChange <- NA
Times_Env_Change <- c(25,50,75)

for (i in 1:nrow(d)){
  if (d$Round[i] <= 25){
    d$TimeSinceChange[i] <- d$Round[i]
  } else { 
    d$TimeSinceChange[i] <- d$Round[i] - max(Times_Env_Change[which(Times_Env_Change<d$Round[i])])
  }
}


# Prep data list for Stan models

 dat <- as.list(select(d, Session_ID, ID_In_Session, Round, ChoiceSelf,Payoff, ExperienceSelf, Phase, TempChange,TimeSinceChange))
 

 #Make numeric IDs
 dat$Session_ID <- as.numeric(factor(dat$Session_ID, ordered = TRUE))
 dat$id <- (dat$Session_ID - 1)*8 + dat$ID_In_Session
 dat$sid <- dat$ID_In_Session
 dat$N_id <- max(dat$id)
 dat$N <- nrow(d)
 
 
 #prep matrices with choices and experience levels ("ages"). One version with all infos, one with only info viewed by participant
 nmat     <- cbind( d$n1 , d$n2 , d$n3 , d$n4 )
 nmat_obs <- cbind( d$n1_obs , d$n2_obs , d$n3_obs , d$n4_obs )
 
 choice_models     <- cbind(d$Choice1, d$Choice2, d$Choice3)
 choice_models_obs <- cbind(d$Choice1_obs, d$Choice2_obs, d$Choice3_obs)
 
 age_models_obs    <- cbind(d$Experience1_obs, d$Experience2_obs, d$Experience3_obs)
 age_models        <- cbind(d$Experience1, d$Experience2, d$Experience3)
 
 
 nmat[is.na(nmat)] <- 0
 choice_models[is.na(choice_models)] <- 0
 age_models[is.na(age_models)] <- 0
 
 nmat_obs[is.na(nmat_obs)] <- 0
 choice_models_obs[is.na(choice_models_obs)] <- 0
 age_models_obs[is.na(age_models_obs)] <- 0
 
 dat$nmat <- nmat
 dat$choice_models <- choice_models
 dat$age_models <- age_models
 
 dat$nmat_obs <- nmat_obs
 dat$choice_models_obs <- choice_models_obs
 dat$age_models_obs <- age_models_obs
 
 #Experience distance matrix for Gaussian processes
 dat$expmat <- matrix(nrow = 20, ncol = 20)
 for (i in 1:20) {
   for (j in 1:20) {
     dat$expmat[i,j] <- abs(i-j)
   }
 }
 

 #Dummy variables for contrasts between temporal and spatial changes
 dat$spat <- ifelse(dat$Experience <=5 & dat$Round > 5, 1, 0)
 dat$temp <- ifelse(dat$TimeSinceChange <=5 & dat$Round > 5, 1, 0)
 
 
 # Run different stan models

 #Baseline Multilevel Experience-weighted attraction model
 m1 <- stan( file="EWA_baseline_multilevel.stan" , data=dat , chains=5, cores=5, iter = 5000, control = list(adapt_delta=0.95, max_treedepth = 13))       
 
 # Spatial vs. Temporal Changes: Experience-weighted attraction model with dummy variables to compute contrasts
 m2 <- stan( file="EWA_contrasts.stan" ,data=dat , chains=5, cores=5, iter = 5000, control = list(adapt_delta=0.95, max_treedepth = 13))    
 
 # Time-varying Multilevel Experience-weighted attraction model with monotonic effects
 m3 <- stan( file="EWA_MonotonicEffects.stan" ,data=dat , chains=5, cores=5, iter = 5000, control = list(adapt_delta=0.95, max_treedepth = 13)) 
 
 # Time-varying Experience-weighted attraction model with Gaussian processes
 m4 <- stan( file="EWA_GaussianProcess.stan" , data=dat , chains=5, cores=5, iter = 5000, control = list(adapt_delta=0.95, max_treedepth = 13))  
 

