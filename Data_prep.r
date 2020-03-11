
###Dynamic Social Learning in Temporally and Spatially Variable Environments ###
### Data preparation script ###
# We combine files from single sessions into one big dataframe and prepare it for statistical analysis

library(readxl)
library(tidyverse)
library(rethinking)

setwd("Y:/eco_lab/ESL1/Data cleaned/ESL1")
file.list <- list.files(pattern='*.xlsx')

#Load data and combine them into one object

d_raw <- c()
for (i in file.list) {
d_raw <- rbind(d_raw, read_excel(i))
}


setwd("C:/Users/dominik_deffner/Documents/GitHub/Experience-Social-Learning")


# Create dataset with only relevant experimental data

d <- select(d_raw, session.code, participant.code, participant.label, subsession.round_number, participant.payoff, 
         player.crop_choice, player.experience, 
         player.Choice1, player.Choice2, player.Choice3, 
         player.Exp1, player.Exp2, player.Exp3, player.payoff, 
         group.id_in_subsession, group.better_crop, 
         subsession.Payoff_Better, 
         subsession.SD_payoff)

colnames(d) <- c("Session_ID", "id", "ID_In_Session", "Round", "SumPayoff", "ChoiceSelf", "ExperienceSelf", "Choice1", "Choice2", "Choice3",
                         "Experience1", "Experience2", "Experience3", "Payoff", "group_id", "Optimal", "PayoffBetter", "Hard" )


#Order according to session and then individual
d <- d[with(d, order(d$Session_ID, d$ID_In_Session)), ]

d$Hard[d$Hard==3] <- 1
d$Hard[d$Hard==1.5] <- 0

#Recode choices

d[d=="New_Member"] <- 0
d[d=="Kartoffeln"] <- 1
d[d=="Weizen"]     <- 2
d[d=="Reis"]       <- 3
d[d=="Mais"]       <- 4

#Make variables integer vectors
d$Choice1       <- as.integer(d$Choice1)
d$Choice2       <- as.integer(d$Choice2)
d$Choice3       <- as.integer(d$Choice3)
d$ChoiceSelf    <- as.integer(d$ChoiceSelf)
d$ID_In_Session <- as.integer(d$ID_In_Session)
d$Optimal       <- as.integer(d$Optimal)


#Variable that indicates whether choice in round was "correct"
d$Correct <- ifelse(d$ChoiceSelf == d$Optimal, 1, 0)

#Number of models that chose each option

d$n1 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==1))
d$n2 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==2))
d$n3 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==3))
d$n4 <- sapply(1:nrow(d),function (i) sum(c(d$Choice1[i], d$Choice2[i],d$Choice3[i])==4))

d$n1[is.na(d$n1)] <- 0
d$n2[is.na(d$n2)] <- 0
d$n3[is.na(d$n3)] <- 0
d$n4[is.na(d$n4)] <- 0

#Mouse Tracking data
{

#Create dataframe with only mousetracking data
d_track <- select(d_raw, session.code, participant.code,  subsession.round_number, player.procdata)
colnames(d_track) <- c("Session_ID", "ID", "Round", "Tracking")
d_track <- d_track[with(d_track, order(d_track$Session_ID, d_track$ID)), ]


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
  for (i in 1:nrow(d_track)) {
    #Check if it's first round or individuals didn't open any box. In that case insert 0
    if (is.na(d_track$Tracking[i]) == TRUE | nrow(parse.proctrace(d_track$Tracking[i])) < 2 ){
      Clean.protrace.list[[i]] <- 0
    } else {
      Clean.protrace.list[[i]] <- clean.proctrace(d_track$Tracking[i])
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
    if (Clean.protrace.list[[i]] != 0){
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
  
  #Compute proportion of neighbors inspected per trial
  d$Prop_Choice <- sapply(1:nrow(d),function (i) sum(c(d$Choice1_ms[i], d$Choice2_ms[i],d$Choice3_ms[i])>0) / 3)
  d$Dur_Choice  <- sapply(1:nrow(d),function (i) sum(c(d$Choice1_ms[i], d$Choice2_ms[i],d$Choice3_ms[i])))
  d$Prop_Exp    <- sapply(1:nrow(d),function (i) sum(c(d$Experience1_ms[i], d$Experience2_ms[i],d$Experience3_ms[i])>0) / 3)
  d$Dur_Exp     <- sapply(1:nrow(d),function (i) sum(c(d$Experience1_ms[i], d$Experience2_ms[i],d$Experience3_ms[i])))
  
}


#Include mouse tracking data to make sure participants actually viewed a given piece of information

Check_fct <- function (i, x) {
  Obs1 <- ifelse(d$Choice1_ms[i] > 0, d$Choice1[i],0)
  Obs2 <- ifelse(d$Choice2_ms[i] > 0, d$Choice2[i],0)
  Obs3 <- ifelse(d$Choice3_ms[i] > 0, d$Choice3[i],0)
  return(sum(c(Obs1, Obs2, Obs3)==x))
}

d$n1_obs <- sapply(1:nrow(d), Check_fct, x=1)
d$n2_obs <- sapply(1:nrow(d), Check_fct, x=2)
d$n3_obs <- sapply(1:nrow(d), Check_fct, x=3)
d$n4_obs <- sapply(1:nrow(d), Check_fct, x=4)


d$n1_obs[is.na(d$n1_obs)] <- 0
d$n2_obs[is.na(d$n2_obs)] <- 0
d$n3_obs[is.na(d$n3_obs)] <- 0
d$n4_obs[is.na(d$n4_obs)] <- 0

d$Choice1_obs <- ifelse(d$Choice1_ms > 0 , d$Choice1, 0 )
d$Choice2_obs <- ifelse(d$Choice2_ms > 0 , d$Choice2, 0 )
d$Choice3_obs <- ifelse(d$Choice3_ms > 0 , d$Choice3, 0 )

d$Experience1_obs <- ifelse(d$Experience1_ms > 0 , d$Experience1, 0 )
d$Experience2_obs <- ifelse(d$Experience2_ms > 0 , d$Experience2, 0 )
d$Experience3_obs <- ifelse(d$Experience3_ms > 0 , d$Experience3, 0 )


#Computer phase (1 to 4)
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


#Calculate Time Since Temporal Change and "real" experience in stable environments without any change

d$TimeSinceChange <- NA
Times_Env_Change <- c(25,50,75)

for (i in 1:nrow(d)){
  if (d$Round[i] <= 25){
    d$TimeSinceChange[i] <- d$Round[i]
  } else { 
    d$TimeSinceChange[i] <- d$Round[i] - max(Times_Env_Change[which(Times_Env_Change<d$Round[i])])
  }
}


#Create dataframe with demographic information
{
  d_demo <- select(d_raw, session.code, participant.code, participant.label,  subsession.round_number, player.age, player.gender, player.lang, player.edu)
  colnames(d_demo) <- c("Session_ID", "ID","ID_In_Session", "Round", "Age", "Gender", "Language", "Education")
  d_demo <- d_demo[with(d_demo, order(d_demo$Session_ID, d_demo$ID_In_Session)), ]
  
  d_demo <- d_demo[d_demo$Round==1,]
  
  d_demo$Gender[which(d_demo$Gender=="Frau"|d_demo$Gender=="w"|d_demo$Gender=="weiblich"| d_demo$Gender=="Weiblich")] <- "f"
  d_demo$Gender[which(d_demo$Gender=="m"|d_demo$Gender=="M"|d_demo$Gender=="männlich"| d_demo$Gender=="Männlich" | d_demo$Gender=="Männlivh")] <- "m"
  
  d_demo$Native <- ifelse(d_demo$Language=="Deutsch"|d_demo$Language=="deutsch" | d_demo$Language=="d" | d_demo$Language=="Deutsch und Vietnamesisch", 1, 0)
}



 
 
 # Prep data list for Stan models
 
 dat <- as.list(select(d, Session_ID, ID_In_Session, Round, ChoiceSelf,Payoff, ExperienceSelf, Phase, TempChange,TimeSinceChange))
 

 #Make numeric IDs
 dat$Session_ID <- as.numeric(factor(dat$Session_ID, ordered = TRUE))
 dat$id <- (dat$Session_ID - 1)*8 + dat$ID_In_Session
 dat$sid <- dat$ID_In_Session
 dat$N_id <- max(dat$id)
 dat$N <- nrow(d)
 
 
 #prep matrices with choices and ages
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
 

 
 #Experience matric for Gaussian processes
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
 
 m1 <- stan( file="ewa_model1.stan" , data=dat_Ind ,iter = 500,  chains=1 )  #Individual learning only
 
 m2 <- stan( file="ewa_model2.stan" , data=dat , iter = 500, chains=1 )  #Simplest model with individual and (unbiased) social learning
 
 m3 <- stan( file="ewa_model3.stan" , data=dat ,iter = 500, chains=1 )  #Social Learning with frequency and experience bias
 m3_obs <- stan( file="ewa_model3_obs.stan" , data=dat ,iter = 500, chains=1 )  #Social Learning with frequency and experience bias
 
 m4 <- stan( file="ewa_model4.stan" , data=dat , chains=1 )  #Full Gaussian process regression
 
 m5 <- stan( file="ewa_model5.stan" , data=dat , chains=1, iter = 300 ) # Varying effects w/o time-varying parameters
 
 mspat_obs <- stan( file="monotonic_ewa_temp_obs.stan" , data=dat , chains=1, iter = 500, control = list(adapt_delta=0.8))  # All Monotonic effects
 
 m6Temp <- stan( file="monotonic_all.stan" , data=dat , chains=4, cores = 4, iter = 1000, control = list(adapt_delta=0.9))  # All Monotonic effects
 
 m7 <- stan( file="ewa_model_simpleContrasts.stan" , data=dat , chains=1, iter = 500 ) # Contrasts
 
 
 m7 <- stan( file="ewa_model_multilevel_contrasts_obs.stan" , data=dat ,chains=4, cores=4, iter = 2000, control = list(adapt_delta=0.95, max_treedepth = 12))  # Contrasts

 
 m8 <- stan( file="ewa_model_GP_Sigma_obs.stan" , data=dat ,chains=4, cores=4, iter = 3000, control = list(adapt_delta=0.99, max_treedepth = 12))  # Contrasts
 
 
 m9 <- stan( file="Full_GP_simple.stan" , data=dat ,chains=4, cores=4, iter = 2000, control = list(adapt_delta=0.9, max_treedepth = 12))  # Contrasts
 
 
 
 
 

#Individual Learning Data
{
setwd("Y:/eco_lab/ESL1/Data cleaned/ESL1_Ind")
file.list <- list.files(pattern='*.xlsx')

#Load data and combine them into one object

d_Ind <- c()
for (i in file.list) {
  d_Ind <- rbind(d_Ind, read_excel(i))
}

d_demo <- select(d_Ind, session.code, participant.code, participant.label,  subsession.round_number, player.age, player.gender, player.lang, player.edu)
colnames(d_demo) <- c("Session_ID", "ID","ID_In_Session", "Round", "Age", "Gender", "Language", "Education")
d_demo <- d_demo[with(d_demo, order(d_demo$Session_ID, d_demo$ID_In_Session)), ]
d_demo <- d_demo[d_demo$Round==1,]


d_Ind <- select(d_Ind, session.code, participant.code, participant.label, subsession.round_number, participant.payoff, 
                player.crop_choice,player.experience, player.payoff, subsession.better_crop, 
                subsession.Payoff_Better, subsession.SD_payoff)

colnames(d_Ind) <- c("Session_ID", "id", "ID_In_Session", "Round", "SumPayoff", "ChoiceSelf","ExperienceSelf" ,"Payoff", "Optimal", "PayoffBetter", "Hard" )

d_Ind <- d_Ind[- which(is.na(d_Ind$ID_In_Session)), ]
#Order according to session and then individual
d_Ind <- d_Ind[with(d_Ind, order(d_Ind$Session_ID, d_Ind$ID_In_Session)), ]

d_Ind$Hard[d_Ind$Hard==3] <- 1
d_Ind$Hard[d_Ind$Hard==1.5] <- 0

#Recode choices

d_Ind[d_Ind=="Kartoffeln"] <- 1
d_Ind[d_Ind=="Weizen"]     <- 2
d_Ind[d_Ind=="Reis"]       <- 3
d_Ind[d_Ind=="Mais"]       <- 4

#Make variables integer vectors
d_Ind$ChoiceSelf    <- as.integer(d_Ind$ChoiceSelf)
d_Ind$ID_In_Session <- as.integer(d_Ind$ID_In_Session)
d_Ind$Optimal       <- as.integer(d_Ind$Optimal)


#Variable that indicates whether choice in round was "correct"
d_Ind$Correct <- ifelse(d_Ind$ChoiceSelf == d_Ind$Optimal, 1, 0)


#Computer phase (1 to 4)
d_Ind$Phase <- c()
d_Ind$Phase <- as.integer( ceiling( d_Ind$Round / 25 ) )
d_Ind$TempChange <- sapply( 1:nrow(d_Ind) , function(i) {
  if ( d_Ind$Round[i]>1 ) {
    return( ifelse(d_Ind$Phase[i]!=d_Ind$Phase[i-1],1,0) )
  } else {
    return(0)
  }
}
)

d_Ind$TimeSinceChange <- NA
Times_Env_Change <- c(25,50,75)

for (i in 1:nrow(d_Ind)){
  if (d_Ind$Round[i] <= 25){
    d_Ind$TimeSinceChange[i] <- d_Ind$Round[i]
  } else { 
    d_Ind$TimeSinceChange[i] <- d_Ind$Round[i] - max(Times_Env_Change[which(Times_Env_Change<d_Ind$Round[i])])
  }
}

dat_Ind <- as.list(select(d_Ind, Session_ID, ID_In_Session, Round, ChoiceSelf,Payoff, ExperienceSelf, Correct, TempChange,TimeSinceChange))


#Make numeric IDs
dat_Ind$Session_ID <- as.numeric(factor(dat_Ind$Session_ID, ordered = TRUE))
dat_Ind$id <- sapply(1:nrow(d_Ind), function (i) which(unique(d_Ind$id) == d_Ind$id[i]) )

dat_Ind$sid <- dat_Ind$ID_In_Session
dat_Ind$N_id <- max(dat_Ind$id)
dat_Ind$N <- nrow(d_Ind)

setwd("C:/Users/dominik_deffner/Documents/GitHub/Experience-Social-Learning")

m1 <- stan( file="ewa_model1.stan" , data=dat_Ind ,iter = 500,  chains=1 )  #Individual learning only

}

