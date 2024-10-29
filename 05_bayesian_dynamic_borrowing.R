# Create 4 model types
    # 1. Weibull,  tau ~ dgamma(0.001, 0.001)
    # 2. Weibull,  tau ~ dgamma(1, 0.001)
    # 3. Exponential,  tau ~ dgamma(0.001, 0.001)
    # 4. Exponential,  tau ~ dgamma(1, 0.001)

# Execute on two populations
    # A. Flatiron atezo 1:1 PS matched to MORPHEUS atezo
    # B. Flatiron atezo meeting inclusion criteria

# Additionally explore different parametrizations of model type 1
    # I. 2 alphas and 1 beta
    # II. 3 betas
    # III. zeros method
    # IV. Iterating separately

bdb_summary_models <- tribble(
    ~`model type`, ~distribution,~tau,
    '1', 'Weibull','dgamma(0.001,0.001)',
    '2', 'Weibull','dgamma(1,0.001)',
    '3','Exponential','dgamma(0.001,0.001)',
    '4','Exponential','dgamma(1,0.001)'
)

bdb_summary_populations <- tribble(
    ~population, ~description,
    'A','Flatiron atezo 1:1 PS matched to MORPHEUS atezo',
    'B','Flatiron atezo meeting all inclusion/exclusion criteria'
)

bdb_model1_validation <- tribble(
    ~parametrization, ~description,
    'I','2 alphas and 1 beta',
    'II','3 betas',
    'III','zeros method',
    'IV', 'iterating separately'
)

# Prep --------------------------------------------------------------------
# Confirm factor levels
combined_ch_ps %>% 
    mutate(fct_source = as.integer(source)) %>% 
    distinct(source,fct_source)

combined_ch_ps %>% 
    mutate(fct_arm_minus_one = as.integer(arm) - 1) %>% 
    distinct(arm,fct_arm_minus_one)

combined_ch_ps %>% 
    mutate(fct_group = as.integer(group)) %>% 
    distinct(group,fct_group)

# 1 - A - I ---------------------------------------------------------------
set.seed(1094)
hybrid_control_model_1_A_I <- rjags::jags.model('./analysis/jags/model1_I',
                                                     data = list(
                                                         n=nrow(combined_ch_ps),
                                                         is.censored = combined_ch_ps$is_censored,
                                                         t = combined_ch_ps$t_noncenc,
                                                         t.cen = combined_ch_ps$t_cen,
                                                         arm = as.integer(as.factor(combined_ch_ps$arm)) - 1,
                                                         source = as.integer(as.factor(combined_ch_ps$source))
                                                     )
)
update(hybrid_control_model_1_A_I,1E4)
hybrid_control_model_1_A_I_cs <- rjags::coda.samples(hybrid_control_model_1_A_I,
                                                 variable.names = c('HR_atezo_tira_vs_atezo',
                                                                    'HR_atezo_flatiron_vs_morpheus',
                                                                    'alpha',
                                                                    'beta',
                                                                    'tau',
                                                                    'v'),
                                                 n.iter = 1E6)
summary(hybrid_control_model_1_A_I_cs)

# 1 - A - II --------------------------------------------------------------
set.seed(1094)
hybrid_control_model_1_A_II <- rjags::jags.model('./analysis/jags/model1_II',
                                            data = list(
                                                n=nrow(combined_ch_ps),
                                                is.censored = combined_ch_ps$is_censored,
                                                t = combined_ch_ps$t_noncenc,
                                                t.cen = combined_ch_ps$t_cen,
                                                group = as.integer(as.factor(combined_ch_ps$group))
                                            )
)
update(hybrid_control_model_1_A_II,1E4)
hybrid_control_model_1_A_II_cs <- rjags::coda.samples(hybrid_control_model_1_A_II,
                                                 variable.names = c('HR_atezo_tira_vs_atezo',
                                                                    'HR_atezo_flatiron_vs_morpheus',
                                                                    'beta',
                                                                    'tau',
                                                                    'v'),
                                                 n.iter = 1E6)
summary(hybrid_control_model_1_A_II_cs)


# 1 - A - III -------------------------------------------------------------
set.seed(1094)
hybrid_control_model_1_A_III <- rjags::jags.model('./analysis/jags/model1_III',
                                            data = list(
                                                n=nrow(combined_ch_ps),
                                                arm = as.integer(as.factor(combined_ch_ps$arm)) - 1,
                                                source = as.integer(as.factor(combined_ch_ps$source)),
                                                os_months = combined_ch_ps$os_months,
                                                os_event = combined_ch_ps$os_event,
                                                zeros = rep(0, NROW(combined_ch_ps)),
                                                C = 10000
                                            )
)
update(hybrid_control_model_1_A_III,1E4)
hybrid_control_model_1_A_III_cs <- rjags::coda.samples(hybrid_control_model_1_A_III,
                                                 variable.names = c('HR_atezo_tira_vs_atezo',
                                                                    'HR_atezo_flatiron_vs_morpheus',
                                                                    'alpha',
                                                                    'beta',
                                                                    'tau',
                                                                    'v'),
                                                 n.iter = 1E6)
summary(hybrid_control_model_1_A_III_cs)


# 1 - A - IV --------------------------------------------------------------
set.seed(1094)
g1_ind <- which(combined_ch_ps$group=='Flatiron Atezo')
g2_ind <- which(combined_ch_ps$group=='MORPHEUS Atezo')
g3_ind <- which(combined_ch_ps$group=='MORPHEUS Atezo+Tira')
hybrid_control_model_1_A_IV <- rjags::jags.model('./analysis/jags/model1_IV',
                                            data = list(
                                                is.censored = combined_ch_ps$is_censored,
                                                t = combined_ch_ps$t_noncenc,
                                                t.cen = combined_ch_ps$t_cen,
                                                g1_ind = g1_ind,
                                                g2_ind = g2_ind,
                                                g3_ind = g3_ind
                                            )
)
update(hybrid_control_model_1_A_IV,1E4)
hybrid_control_model_1_A_IV_cs <- rjags::coda.samples(hybrid_control_model_1_A_IV,
                                                 variable.names = c('HR_atezo_tira_vs_atezo',
                                                                    'HR_atezo_flatiron_vs_morpheus',
                                                                    'beta',
                                                                    'tau',
                                                                    'v'),
                                                 n.iter = 1E6)
summary(hybrid_control_model_1_A_IV_cs)

# 2 - A -------------------------------------------------------------------
set.seed(1094)
hybrid_control_model_2_A <- rjags::jags.model('./analysis/jags/model2',
                                             data = list(
                                                 n=nrow(combined_ch_ps),
                                                 is.censored = combined_ch_ps$is_censored,
                                                 t = combined_ch_ps$t_noncenc,
                                                 t.cen = combined_ch_ps$t_cen,
                                                 arm = as.integer(as.factor(combined_ch_ps$arm)) - 1,
                                                 source = as.integer(as.factor(combined_ch_ps$source))
                                             )
)
update(hybrid_control_model_2_A,1E4)
hybrid_control_model_2_A_cs <- rjags::coda.samples(hybrid_control_model_2_A,
                                                  variable.names = c('HR_atezo_tira_vs_atezo',
                                                                     'HR_atezo_flatiron_vs_morpheus',
                                                                     'alpha',
                                                                     'beta',
                                                                     'tau',
                                                                     'v'),
                                                  n.iter = 1E6)
summary(hybrid_control_model_2_A_cs)

# 3 - A -------------------------------------------------------------------
set.seed(1094)
hybrid_control_model_3_A <- rjags::jags.model('./analysis/jags/model3',
                                             data = list(
                                                 n=nrow(combined_ch_ps),
                                                 is.censored = combined_ch_ps$is_censored,
                                                 t = combined_ch_ps$t_noncenc,
                                                 t.cen = combined_ch_ps$t_cen,
                                                 arm = as.integer(as.factor(combined_ch_ps$arm)) - 1,
                                                 source = as.integer(as.factor(combined_ch_ps$source))
                                             )
)
update(hybrid_control_model_3_A,1E4)
hybrid_control_model_3_A_cs <- rjags::coda.samples(hybrid_control_model_3_A,
                                                  variable.names = c('HR_atezo_tira_vs_atezo',
                                                                     'HR_atezo_flatiron_vs_morpheus',
                                                                     'alpha',
                                                                     'beta',
                                                                     'tau'),
                                                  n.iter = 1E6)
summary(hybrid_control_model_3_A_cs)

# 4 - A -------------------------------------------------------------------

set.seed(1094)
hybrid_control_model_4_A <- rjags::jags.model('./analysis/jags/model4',
                                            data = list(
                                                n=nrow(combined_ch_ps),
                                                is.censored = combined_ch_ps$is_censored,
                                                t = combined_ch_ps$t_noncenc,
                                                t.cen = combined_ch_ps$t_cen,
                                                arm = as.integer(as.factor(combined_ch_ps$arm)) - 1,
                                                source = as.integer(as.factor(combined_ch_ps$source))
                                            )
)
update(hybrid_control_model_4_A,1E4)
hybrid_control_model_4_A_cs <- rjags::coda.samples(hybrid_control_model_4_A,
                                                 variable.names = c('HR_atezo_tira_vs_atezo',
                                                                    'HR_atezo_flatiron_vs_morpheus',
                                                                    'alpha',
                                                                    'beta',
                                                                    'tau'),
                                                 n.iter = 1E6)
summary(hybrid_control_model_4_A_cs)


# 1 - B -------------------------------------------------------------------
set.seed(1094)
hybrid_control_model_1_B <- rjags::jags.model('./analysis/jags/model1_I',
                                             data = list(
                                                 n=nrow(combined_ch_full),
                                                 is.censored = combined_ch_full$is_censored,
                                                 t = combined_ch_full$t_noncenc,
                                                 t.cen = combined_ch_full$t_cen,
                                                 arm = as.integer(as.factor(combined_ch_full$arm)) - 1,
                                                 source = as.integer(as.factor(combined_ch_full$source))
                                             )
)
update(hybrid_control_model_1_B,1E4)
hybrid_control_model_1_B_cs <- rjags::coda.samples(hybrid_control_model_1_B,
                                                  variable.names = c('HR_atezo_tira_vs_atezo',
                                                                     'HR_atezo_flatiron_vs_morpheus',
                                                                     'alpha',
                                                                     'beta',
                                                                     'tau',
                                                                     'v'),
                                                  n.iter = 1E6)
summary(hybrid_control_model_1_B_cs)


# 2 - B -------------------------------------------------------------------

set.seed(1094)
hybrid_control_model_2_B <- rjags::jags.model('./analysis/jags/model2',
                                              data = list(
                                                  n=nrow(combined_ch_full),
                                                  is.censored = combined_ch_full$is_censored,
                                                  t = combined_ch_full$t_noncenc,
                                                  t.cen = combined_ch_full$t_cen,
                                                  arm = as.integer(as.factor(combined_ch_full$arm)) - 1,
                                                  source = as.integer(as.factor(combined_ch_full$source))
                                              )
)
update(hybrid_control_model_2_B,1E4)
hybrid_control_model_2_B_cs <- rjags::coda.samples(hybrid_control_model_2_B,
                                                   variable.names = c('HR_atezo_tira_vs_atezo',
                                                                      'HR_atezo_flatiron_vs_morpheus',
                                                                      'alpha',
                                                                      'beta',
                                                                      'tau',
                                                                      'v'),
                                                   n.iter = 1E6)
summary(hybrid_control_model_2_B_cs)

# 3 - B -------------------------------------------------------------------

set.seed(1094)
hybrid_control_model_3_B <- rjags::jags.model('./analysis/jags/model3',
                                              data = list(
                                                  n=nrow(combined_ch_full),
                                                  is.censored = combined_ch_full$is_censored,
                                                  t = combined_ch_full$t_noncenc,
                                                  t.cen = combined_ch_full$t_cen,
                                                  arm = as.integer(as.factor(combined_ch_full$arm)) - 1,
                                                  source = as.integer(as.factor(combined_ch_full$source))
                                              )
)
update(hybrid_control_model_3_B,1E4)
hybrid_control_model_3_B_cs <- rjags::coda.samples(hybrid_control_model_3_B,
                                                   variable.names = c('HR_atezo_tira_vs_atezo',
                                                                      'HR_atezo_flatiron_vs_morpheus',
                                                                      'alpha',
                                                                      'beta',
                                                                      'tau'),
                                                   n.iter = 1E6)
summary(hybrid_control_model_3_B_cs)


# 4 - B -------------------------------------------------------------------

set.seed(1094)
hybrid_control_model_4_B <- rjags::jags.model('./analysis/jags/model4',
                                              data = list(
                                                  n=nrow(combined_ch_full),
                                                  is.censored = combined_ch_full$is_censored,
                                                  t = combined_ch_full$t_noncenc,
                                                  t.cen = combined_ch_full$t_cen,
                                                  arm = as.integer(as.factor(combined_ch_full$arm)) - 1,
                                                  source = as.integer(as.factor(combined_ch_full$source))
                                              )
)
update(hybrid_control_model_4_B,1E4)
hybrid_control_model_4_B_cs <- rjags::coda.samples(hybrid_control_model_4_B,
                                                   variable.names = c('HR_atezo_tira_vs_atezo',
                                                                      'HR_atezo_flatiron_vs_morpheus',
                                                                      'alpha',
                                                                      'beta',
                                                                      'tau'),
                                                   n.iter = 1E6)
summary(hybrid_control_model_4_B_cs)
