
# First, explore MORPHEUS results -----------------------------------------
## Cox proportional hazards model - MORPHEUS only
morpheus_only_cox <- coxph(Surv(os_months, os_event) ~ arm, 
                           data = ch_morpheus)

morpheus_only_km <- simple_km_out(
    Surv(os_months, os_event) ~ arm,
    df = ch_morpheus,
    conf.int=T
)

## Cox proportional hazards model - full population only
combined_full_cox <- coxph(Surv(os_months, os_event) ~ arm, 
                           data = combined_ch_full)

combined_full_km <- simple_km_out(
    Surv(os_months, os_event) ~ arm,
    df = combined_ch_full,
    conf.int=T
)

# Evaluate matching combinations ------------------------------------------
## Define combinations
match_combinations <- list(
    c('ecog','lymphofl','viscfl','age_cat'),
    c('sex','ecog','bmi','age','tobhx','surgery'),
    c('sex','ecog','bmi','age','tobhx','surgery','lymphofl'),
    c('sex','ecog','bmi','age','tobhx','surgery','viscfl'),
    c('sex','ecog','bmi','age','tobhx','surgery','lymphofl','viscfl')
)

match_combinations_cat <- list(
    c('ecog','lymphofl','viscfl','age_cat'),
    c('sex','ecog','bmi_cat','age_cat','tobhx','surgery'),
    c('sex','ecog','bmi_cat','age_cat','tobhx','surgery','lymphofl'),
    c('sex','ecog','bmi_cat','age_cat','tobhx','surgery','viscfl'),
    c('sex','ecog','bmi_cat','age_cat','tobhx','surgery','lymphofl','viscfl')
)

## Execute comparisons
full_matches <- map(
    match_combinations,
    find_the_match,
    'full'
)

greedy_matches <-map(
    match_combinations,
    find_the_match,
    'nearest'
) 

exact_matches <- map(
    match_combinations_cat,
    find_the_match,
    'exact'
) 

# Frequentist parametric models -------------------------------------------
## Parametric survival models
### Exponential
### Weibull
### Gompertz

## First create custom Weibull PH
custom_weibullPH <- list(name = "weibullPH",
                         pars = c("shape", "scale"), location = "scale",
                         transforms = c(log, log),
                         inv.transforms = c(exp, exp),
                         inits = function(t){
                             c(1,1)
                         })

## Execute comparisons
dist_explore <- list('exp',custom_weibullPH,'gompertz')
optim_method <- c('Nelder-Mead','SANN','Nelder-Mead')

## MORPHEUS only
just_trial_parametric <- map2(dist_explore,optim_method,function(.x,.y) parametric_freq_survival_morpheus(.x,ch_morpheus,.y))
names(just_trial_parametric) <- c('Exponential','Weibull','Gompertz')

## Including all RWD patients
full_pop_parametric <- map2(dist_explore,optim_method,function(.x,.y) parametric_freq_survival_morpheus(.x,combined_ch_full,.y))
names(full_pop_parametric) <- c('Exponential','Weibull','Gompertz')
