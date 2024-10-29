# Conduct frequentist HC analysis -----------------------------------------
## 1:1 greedy nearest neighbor matching with sex, ecog, bmi, age, tobhx, surgery, viscfl was best combination

## Match
ps_match <- MatchIt::matchit(
    source ~ sex + ecog + bmi + age + tobhx + surgery + viscfl,
    combined_ch_atezo_cc,
    'nearest',
    'glm',
    estimand = 'ATC', # level 1 = Morpheus, so this is average treatment effect on Morpheus patients
    tol = 1e-7,
)

ps_match_df <- match.data(ps_match) %>%
    mutate_at(
        c('pdl1','alkph'),
        function(z) ifelse(is.na(z), 'Unknown',z)
    )

## Compare atezo arms by RWD status
ps_match_km <- simple_km_out(
    Surv(os_months,os_event)~source,
    ps_match_df,
    conf.int=T
)

combined_ch_ps <- ps_match_df %>%
    union_all(filter(combined_ch_full, arm=='Atezo+Tira')) %>% 
    mutate(
        group = factor(case_when(
            arm == 'Atezo+Tira' ~ 'MORPHEUS Atezo+Tira',
            source == 'Flatiron' ~ 'Flatiron Atezo',
            T ~ 'MORPHEUS Atezo'
        ), levels = c('Flatiron Atezo','MORPHEUS Atezo','MORPHEUS Atezo+Tira')
    ))

# Cox model
combined_ps_match_cox <- coxph(Surv(os_months, os_event) ~ arm,
                               data = combined_ch_ps)

combined_ps_match_km <-  simple_km_out(
    Surv(os_months,os_event)~arm,
    combined_ch_ps,
    conf.int=T
)

# Parametric models
ps_match_pop_parametric <- map2(dist_explore,optim_method,function(.x,.y) parametric_freq_survival_morpheus(.x,combined_ch_ps,.y))
names(ps_match_pop_parametric) <- c('Exponential','Weibull','Gompertz')