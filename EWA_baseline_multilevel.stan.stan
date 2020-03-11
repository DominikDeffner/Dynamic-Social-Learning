
// This model includes varying (aka random) effects on all parameters (no time-varying parameters)

// Stan models need a data, parameter and model block.
// Here we define name and type of the variables clustered

data{
int N;
int N_id;
int id[N];
int Round[N];
int ChoiceSelf[N];
real Payoff[N];
int ExperienceSelf[N];
matrix[N,4] nmat_obs;
matrix[N,3] age_models_obs;
matrix[N,3] choice_models_obs;
}

parameters{

// Define parameters: Sigma, kappa and phi must lie between 0 and 1, so we define them
// on the logit scale. lambda and f must be positive, so we define them on the log scale

real logit_sigma;
real logit_kappa;
real gauss_beta;
real log_f;
real logit_phi;
real log_L;

// Varying effects clustered on individual, defined as deviations from grand mean
// We do the non-centered version with the Cholesky factorization

matrix[6,N_id] z_ID;               //Matrix of uncorrelated z - values
vector<lower=0>[6] sigma_ID;       //SD of parameters among individuals
cholesky_factor_corr[6] Rho_ID;    // This is the Cholesky factor: if you multiply this matrix and it's transpose you get correlation matrix

}

transformed parameters{
matrix[N_id,6] v_ID; // Matrix of varying effects for each individual
v_ID = ( diag_pre_multiply( sigma_ID , Rho_ID ) * z_ID )';
}

model{

matrix[N_id,4] A; // Attraction matrix

// Here we define the priors
logit_phi ~  normal(0,1);
log_L ~  normal(-1,1);
logit_sigma ~ normal(0,1);
logit_kappa ~ normal(0,1);
gauss_beta ~ normal(0,1);
log_f ~ normal(0,1);

// Varying effects
to_vector(z_ID) ~ normal(0,1);
sigma_ID ~ exponential(1);
Rho_ID ~ lkj_corr_cholesky(4);

// initialize attraction scores, we assume no preexisting differences
for ( i in 1:N_id ) A[i,1:4] = rep_vector(0,4)';

// loop over all choices (20k rows in dataset)

for ( i in 1:N ) {

  vector[4] pay;
  vector[4] pA;
  vector[4] pC;
  vector[4] pS;
  vector[4] p;

  real sigma;
  real beta;
  real kappa;
  real f;
  real L;
  real phi;

  // First, what is log-prob of observed choice
  // Asocial choice probability
  L =  exp(log_L + v_ID[id[i],1]);
  pA = softmax( L*A[id[i],1:4]' );

  //Social choice probability
  if ( sum(nmat_obs[i,:])==0 ) {
    p = pA;
  } else {

    // Conformity
    f = exp(log_f + v_ID[id[i],2]);
    for ( j in 1:4 ) pC[j] = nmat_obs[i,j]^f;
    pC = pC / sum(pC);

    //Experience bias
    beta = gauss_beta + v_ID[id[i],3]   ;
    for ( an_option in 1:4 ){
      pS[an_option] = 0;
      for ( a_model in 1:3 ) {
        if ( choice_models_obs[i,a_model]==an_option )
        pS[an_option] = pS[an_option] + exp(beta*age_models_obs[i,a_model]);
      }
    }
    pS = pS / sum(pS);

    sigma = inv_logit( logit_sigma + v_ID[id[i],4]);
    kappa = inv_logit( logit_kappa + v_ID[id[i],5]);

    // Combine everything to get choice probabilities for each optio
    p = (1-sigma)*pA + sigma*( (1-kappa)*pC + kappa*pS );
  }

   //Multinomial Likelihood of observed choice
   ChoiceSelf[i] ~ categorical( p );

  // Second, update attractions conditional on observed choice
  pay[1:4] = rep_vector(0,4);
  pay[ ChoiceSelf[i] ] = Payoff[i];
  phi =  inv_logit(logit_phi + v_ID[id[i],6]);
  A[ id[i] , 1:4 ] = ( (1-phi)*to_vector(A[ id[i] , 1:4 ]) + phi*pay)';
}
//i
}

