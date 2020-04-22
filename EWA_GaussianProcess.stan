

// Time-varying Learning Parameters: Gaussian process model


//Function for Gaussian process kernel
functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real sq_sigma) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + sq_sigma;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + sq_sigma;
        return K;
    }
}

data{
int N;
int N_id;          
int id[N];
int Round[N];
int ChoiceSelf[N];
real Payoff[N];
int ExperienceSelf[N];


matrix[20,20] expmat;
matrix[N,4] nmat_obs;
matrix[N,3] age_models_obs;
matrix[N,3] choice_models_obs;
}

parameters{

    real mean_lambda;
    real mean_phi;
    real mean_sigma;
    real mean_beta;
    real mean_f;
    real mean_kappa;

    real log_etasq_sigma;
    real log_rhosq_sigma;
    real log_sigmasq_sigma;

    real log_etasq_beta;
    real log_rhosq_beta;
     real log_sigmasq_beta;

    real log_etasq_f;
    real log_rhosq_f;
    real log_sigmasq_f;

    real log_etasq_kappa;
    real log_rhosq_kappa;
    real log_sigmasq_kappa;

    real log_rhosq_phi;
    real log_etasq_phi;
    real log_sigmasq_phi;

    real log_rhosq_L;
    real log_etasq_L;
    real log_sigmasq_L;

    vector[20] dev_sigma;
    vector[20] dev_beta;
    vector[20] dev_f;
    vector[20] dev_kappa;
    vector[20] dev_L;
    vector[20] dev_phi;
}

model{

    matrix[N_id,4] A; // attraction matrix

    matrix[20, 20] Kmat_sigma;
    matrix[20, 20] Kmat_beta;
    matrix[20, 20] Kmat_f;
    matrix[20, 20] Kmat_kappa;
    matrix[20, 20] Kmat_L;
    matrix[20, 20] Kmat_phi;

    mean_sigma ~ normal(0,1);
    mean_beta ~ normal(0,1);
    mean_f ~ normal(0,1);
    mean_kappa ~ normal(0,1);
    mean_phi ~ normal(0,1);
    mean_lambda ~ normal(0,1);

    log_rhosq_sigma ~ normal(-2,1);
    log_etasq_sigma ~ normal(-2,1);
    log_sigmasq_sigma ~ normal(-2,1);

    log_rhosq_beta ~ normal(-2,1);
    log_etasq_beta ~ normal(-2,1);
    log_sigmasq_beta ~ normal(-2,1);

    log_rhosq_f ~ normal(-2,1);
    log_etasq_f ~ normal(-2,1);
    log_sigmasq_f ~ normal(-2,1);

    log_rhosq_kappa ~ normal(-2,1);
    log_etasq_kappa ~ normal(-2,1);
    log_sigmasq_kappa ~ normal(-2,1);

    log_rhosq_phi ~ normal(-2,1);
    log_etasq_phi ~ normal(-2,1);
    log_sigmasq_phi ~ normal(-2,1);

    log_rhosq_L ~ normal(-2,1);
    log_etasq_L ~ normal(-2,1);
    log_sigmasq_L ~ normal(-2,1);


    // Gaussian process fun

       Kmat_sigma = cov_GPL2(expmat, exp(log_etasq_sigma) , exp(log_rhosq_sigma), exp(log_sigmasq_sigma));
       dev_sigma ~ multi_normal(rep_vector(0,20) , Kmat_sigma);

       Kmat_beta = cov_GPL2(expmat, exp(log_etasq_beta) , exp(log_rhosq_beta), exp(log_sigmasq_beta));
       dev_beta ~ multi_normal(rep_vector(0,20) , Kmat_beta);

       Kmat_f= cov_GPL2(expmat, exp(log_etasq_f) , exp(log_rhosq_f ), exp(log_sigmasq_f));
       dev_f ~ multi_normal(rep_vector(0,20) , Kmat_f);

       Kmat_kappa = cov_GPL2(expmat, exp(log_etasq_kappa) , exp(log_rhosq_kappa), exp(log_sigmasq_kappa));
       dev_kappa ~ multi_normal(rep_vector(0,20) , Kmat_kappa);

       Kmat_phi = cov_GPL2(expmat, exp(log_etasq_phi) , exp(log_rhosq_phi), exp(log_sigmasq_phi));
       dev_phi ~ multi_normal(rep_vector(0,20) , Kmat_phi);

       Kmat_L= cov_GPL2(expmat, exp(log_etasq_L) , exp(log_rhosq_L ), exp(log_sigmasq_L));
       dev_L ~ multi_normal(rep_vector(0,20) , Kmat_L);

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
    real f;
    real kappa;
    real phi;
    real lambda;

    // first, what is log-prob of observed choice
    lambda = exp(mean_lambda + dev_L[ExperienceSelf[i]]);

    pA = softmax( lambda*A[id[i],1:4]' );

    // second compute conformity everything

    if ( sum(nmat_obs[i,:])==0 ) {
        p = pA;
    } else {

    // conformity
        f = exp(mean_f + dev_f[ExperienceSelf[i]]);
        for ( j in 1:4 ) pC[j] = nmat_obs[i,j]^f;
        pC = pC / sum(pC);

    //age bias
        beta = mean_beta  + dev_beta[ExperienceSelf[i]];
        for ( an_option in 1:4 ){
            pS[an_option] = 0;
        for ( a_model in 1:3 ) {
            if ( choice_models_obs[i,a_model]==an_option )
                pS[an_option] = pS[an_option] + exp(beta*age_models_obs[i,a_model]);
            }
        }

        pS = pS / sum(pS);

        // combine everything
        sigma = inv_logit(mean_sigma  + dev_sigma[ExperienceSelf[i]]);
        kappa = inv_logit(mean_kappa  + dev_kappa[ExperienceSelf[i]]);
        p = (1-sigma)*pA + sigma*( (1-kappa)*pC + kappa*pS );
    }

    ChoiceSelf[i] ~ categorical( p );

    // second, update attractions conditional on observed choice
    phi = inv_logit(mean_phi + dev_phi[ExperienceSelf[i]]);
    pay[1:4] = rep_vector(0,4);
    pay[ ChoiceSelf[i] ] = Payoff[i];
    A[ id[i] , 1:4 ] = ( (1-phi)*to_vector(A[ id[i] , 1:4 ]) + phi*pay)';
    }
}
