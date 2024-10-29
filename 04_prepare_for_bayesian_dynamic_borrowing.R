# Create 2 model types
    # 1. Weibull
    # 2. Exponential

# Execute on three populations
    # A. MORPHEUS only 
    # B. Flatiron atezo 1:1 PS matched to MORPHEUS atezo
    # C. Flatiron atezo meeting inclusion criteria


# 1 - A -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_1_A <- rjags::jags.model('./analysis/jags/prep1',
                                    data = list(
                                          n=nrow(ch_morpheus),
                                          is.censored = ch_morpheus$is_censored,
                                          t = ch_morpheus$t_noncenc,
                                          t.cen = ch_morpheus$t_cen,
                                          arm = as.integer(as.factor(ch_morpheus$arm)) - 1
                                      )
)
update(bdb_prep_model_1_A,1E4)
bdb_prep_model_1_A_cs <- rjags::coda.samples(bdb_prep_model_1_A,
                                                     variable.names = c('exp_rate',
                                                                        'exp_beta.arm'),
                                           n.iter = 1E6)
summary(bdb_prep_model_1_A_cs)

# 2 - A -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_2_A <- rjags::jags.model('./analysis/jags/prep2',
                                        data = list(
                                            n=nrow(ch_morpheus),
                                            is.censored = ch_morpheus$is_censored,
                                            t = ch_morpheus$t_noncenc,
                                            t.cen = ch_morpheus$t_cen,
                                            arm = as.integer(as.factor(ch_morpheus$arm)) - 1
                                        )
)
update(bdb_prep_model_2_A,1E4)
bdb_prep_model_2_A_cs <- rjags::coda.samples(bdb_prep_model_2_A,
                                             variable.names = c('exp_rate',
                                                                'exp_beta.arm'),
                                             n.iter = 1E6)
summary(bdb_prep_model_2_A_cs)

# 1 - B -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_1_B <- rjags::jags.model('./analysis/jags/prep1',
                                        data = list(
                                            n=nrow(combined_ch_ps),
                                            is.censored = combined_ch_ps$is_censored,
                                            t = combined_ch_ps$t_noncenc,
                                            t.cen = combined_ch_ps$t_cen,
                                            arm = as.integer(as.factor(combined_ch_ps$arm)) - 1
                                        )
)
update(bdb_prep_model_1_B,1E4)
bdb_prep_model_1_B_cs <- rjags::coda.samples(bdb_prep_model_1_B,
                                             variable.names = c('exp_rate',
                                                                'exp_beta.arm'),
                                             n.iter = 1E6)
summary(bdb_prep_model_1_B_cs)

# 2 - B -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_2_B <- rjags::jags.model('./analysis/jags/prep2',
                                        data = list(
                                            n=nrow(combined_ch_ps),
                                            is.censored = combined_ch_ps$is_censored,
                                            t = combined_ch_ps$t_noncenc,
                                            t.cen = combined_ch_ps$t_cen,
                                            arm = as.integer(as.factor(combined_ch_ps$arm)) - 1
                                        )
)
update(bdb_prep_model_2_B,1E4)
bdb_prep_model_2_B_cs <- rjags::coda.samples(bdb_prep_model_2_B,
                                             variable.names = c('exp_rate',
                                                                'exp_beta.arm'),
                                             n.iter = 1E6)
summary(bdb_prep_model_2_B_cs)

# 1 - C -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_1_C <- rjags::jags.model('./analysis/jags/prep1',
                                        data = list(
                                            n=nrow(combined_ch_full),
                                            is.censored = combined_ch_full$is_censored,
                                            t = combined_ch_full$t_noncenc,
                                            t.cen = combined_ch_full$t_cen,
                                            arm = as.integer(as.factor(combined_ch_full$arm)) - 1
                                        )
)
update(bdb_prep_model_1_C,1E4)
bdb_prep_model_1_C_cs <- rjags::coda.samples(bdb_prep_model_1_C,
                                             variable.names = c('exp_rate',
                                                                'exp_beta.arm'),
                                             n.iter = 1E6)
summary(bdb_prep_model_1_C_cs)

# 2 - B -------------------------------------------------------------------
set.seed(1094)
bdb_prep_model_2_C <- rjags::jags.model('./analysis/jags/prep2',
                                        data = list(
                                            n=nrow(combined_ch_full),
                                            is.censored = combined_ch_full$is_censored,
                                            t = combined_ch_full$t_noncenc,
                                            t.cen = combined_ch_full$t_cen,
                                            arm = as.integer(as.factor(combined_ch_full$arm)) - 1
                                        )
)
update(bdb_prep_model_2_C,1E4)
bdb_prep_model_2_C_cs <- rjags::coda.samples(bdb_prep_model_2_C,
                                             variable.names = c('exp_rate',
                                                                'exp_beta.arm'),
                                             n.iter = 1E6)
summary(bdb_prep_model_2_C_cs)

