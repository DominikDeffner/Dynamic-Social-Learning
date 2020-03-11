# Dynamic-Social-Learning

This repository contains the scripts to reproduce all results and plots in 

***Deffner, D., Kleinow, V. & McElreath, R. (submitted to Nature Human Behaviour): Dynamic Social Learning in Temporally and Spatially Variable Environments***

**Data file and data preparation**

- "data.csv" contains full anonymized dataset from the experiment
- "data_Ind.csv" contains data from individual learning control condition
- "Data_prep.r" loads the data and prepares them for the stan models

 **Stan models**
 - "EWA_baseline_multilevel.stan": Baseline Multilevel Experience-weighted attraction model
 - "EWA_contrasts.stan": Spatial vs. Temporal Changes: Experience-weighted attraction model with dummy variables to compute contrasts   
 - EWA_MonotonicEfects.stan": Time-varying Multilevel Experience-weighted attraction model with monotonic effects
 - "EWA_GaussianProcess.stan": Time-varying Experience-weighted attraction model with Gaussian processes
 
  **Plotting code for Figs.2-5**
  - "Fig2BehavioralResults.r"
  - "Fig3Contrasts.r
  - "Fig4MonotonicEffects.r"
  - "Fig5GaussianProcess.r"
  
  
  **Simulation code**

  "PostExperimentalSims.r" contains simulation code for post-hoc simulations from model outputs
