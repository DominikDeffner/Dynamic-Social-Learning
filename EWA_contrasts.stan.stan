// Varying effects on everything (no time-varying parameters)


data{
int N;
int N_id;           // number of unique individuals
int sid[N];          // id within session
int Session_ID[N];
int id[N];         // unique id across all sessions
int Round[N];
int ChoiceSelf[N];
real Payoff[N];
int ExperienceSelf[N];
int Phase[N];
int TempChange[N];
int TimeSinceChange[N];
int spat[N];
int temp[N];

matrix[N,4] nmat_obs;
matrix[N,3] age_models_obs;
matrix[N,3] choice_models_obs;
}

parameters{

real b0_sigma;
real btemp_sigma;
real bspat_sigma;

real b0_kappa;
real btemp_kappa;
real bspat_kappa;

real b0_beta;
real btemp_beta;
real bspat_beta;

real b0_f;
real btemp_f;
real bspat_f;

real logit_phi;
real log_L;


// Varying effects clustered on individual
matrix[14,N_id] z_ID;
vector<lower=0>[14] sigma_ID;       //SD of parameters among individuals
cholesky_factor_corr[14] Rho_ID;

}

transformed parameters{
matrix[N_id,14] v_ID; // varying effects on stuff
v_ID = ( diag_pre_multiply( sigma_ID , Rho_ID ) * z_ID )';
}


model{

matrix[N_id,4] A; // attraction matrix

logit_phi ~  normal(0,1);
log_L ~  normal(0,1);

b0_sigma ~ normal(0,1);
btemp_sigma ~ normal(0,1);
bspat_sigma ~ normal(0,1);

b0_kappa ~ normal(0,1);
btemp_kappa ~ normal(0,1);
bspat_kappa ~ normal(0,1);

b0_beta ~ normal(0,0.3);
btemp_beta ~ normal(0,0.3);
bspat_beta ~ normal(0,0.3);

b0_f ~ normal(0,1);
btemp_f ~ normal(0,1);
bspat_f ~ normal(0,1);

// varying effects
to_vector(z_ID) ~ normal(0,1);
sigma_ID ~ exponential(1);
Rho_ID ~ lkj_corr_cholesky(4);

// initialize attraction scores

for ( i in 1:N_id ) A[i,1:4] = rep_vector(0,4)';

// loop over choices

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

  //if ( TempChange[i]==1 ) A[id[i],1:4] = rep_vector(0,4)';
  //if ( ExperienceSelf[i]==1 ) A[id[i],1:4] = rep_vector(0,4)';

  // first, what is log-prob of observed choice
  L =  exp(log_L + v_ID[id[i],1]);

  pA = softmax( L*A[id[i],1:4]' );

  // second compute conformity thing

  if ( sum(nmat_obs[i,:])==0 ) {
    p = pA;

  } else {

    // conformity

    f = exp( (b0_f + v_ID[id[i],2]) + (btemp_f + v_ID[id[i],3]) * temp[i] + (bspat_f + v_ID[id[i],4]) * spat[i] );

    for ( j in 1:4 ) pC[j] = nmat_obs[i,j]^f;

    pC = pC / sum(pC);

    //age bias

    beta = (b0_beta + v_ID[id[i],5]) + (btemp_beta + v_ID[id[i],6]) * temp[i] + (bspat_beta + v_ID[id[i],7]) * spat[i]   ;

    for ( an_option in 1:4 ){

      pS[an_option] = 0;

      for ( a_model in 1:3 ) {

        if ( choice_models_obs[i,a_model]==an_option )

        pS[an_option] = pS[an_option] + exp(beta*age_models_obs[i,a_model]);

      }
    }

    pS = pS / sum(pS);

    // combine everything
    sigma = inv_logit( (b0_sigma + v_ID[id[i],8]) + (btemp_sigma + v_ID[id[i],9]) * temp[i] + (bspat_sigma + v_ID[id[i],10]) * spat[i]);
    kappa = inv_logit( (b0_kappa + v_ID[id[i],11]) + (btemp_kappa + v_ID[id[i],12]) * temp[i] + (bspat_kappa + v_ID[id[i],13]) * spat[i]);


    p = (1-sigma)*pA + sigma*( (1-kappa)*pC + kappa*pS );

  }

  ChoiceSelf[i] ~ categorical( p );

  // second, update attractions conditional on observed choice

  pay[1:4] = rep_vector(0,4);
  pay[ ChoiceSelf[i] ] = Payoff[i];

  phi =  inv_logit(logit_phi + v_ID[id[i],14]);

  A[ id[i] , 1:4 ] = ( (1-phi)*to_vector(A[ id[i] , 1:4 ]) + phi*pay)';


}
//i
}
