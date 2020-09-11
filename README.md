# Dynamic Social Learning

This repository contains anonymized experimental data and all scripts to reproduce results and plots in 

***Deffner, D., Kleinow, V. & McElreath, R. (submitted to Royal Society Open Science): Dynamic Social Learning in Temporally and Spatially Variable Environments***

**Experimental software**
The folder "oTree code" contains full experimental software needed to run the experiment including code for mouse tracking. 


**Data files and preparation script**

- "data.csv" in the "Data" folder contains full anonymized dataset from the social learning experiment:
   "ChoiceSelf" is the option an individual chose in a given round, "ExperienceSelf" gives their own level of experience and       "Payoff" gives the number of points they collected. "Choice1","Choice2", "Choice3" and "Experience1", "Experience2", "Experience3" indicate the social information available from first, second and third (from left to right) group member, respectively. "group_id" says which region an individual is currently in, "Optimal" givs the currently optimal option, "PayoffBetter" the expected payoff of the optimal crop (other 3 points less) and "Hard" says whether phase was relatively difficult (SD = 3) or easy (SD = 1.5). "MouseTracking", finally, records all occasions when individuals entered and left a given box. 
- "data_Ind.csv" in the "Data" folder contains data from individual learning control condition. Variable names are equivalent. 
- "Data_prep.r" loads the data and prepares them for the stan models. The final product is a list with all relevant variables as required by stan. Processing the mouse tracking data might take a couple of minutes.

 **Stan models**
 - "EWA_baseline_multilevel.stan": Baseline Multilevel Experience-weighted attraction model. This model code is heavily commented with detailed description of all steps. If you want to delve deeper into the modeling, you should probably start here.
 - "EWA_contrasts.stan": Spatial vs. Temporal Changes: Experience-weighted attraction model with dummy variables to compute contrasts.   
 - EWA_MonotonicEffects.stan": Time-varying Multilevel Experience-weighted attraction model with monotonic effects.
 - "EWA_GaussianProcess.stan": Time-varying Experience-weighted attraction model with Gaussian processes.
 
  **Plotting code for Figs.2-5**
  - "Fig2BehavioralResults.r"
  - "Fig3Contrasts.r
  - "Fig4MonotonicEffects.r"
  - "Fig5GaussianProcess.r"
  
  
  **Simulation code**

  "PostExperimentalSims.r" contains simulation code for post-hoc simulations from model outputs.
  
  
   **Preregistration**

  Simulation and modeling code accopanying preregistration can be found here: https://github.com/DominikDeffner/Social-Learning-Experiments-in-Experience-Structured-Groups
