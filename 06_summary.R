
# Summarise ---------------------------------------------------------------
# MORPHEUS-only models
morpheus_only_results <- extract_cox_effect(morpheus_only_cox,'armAtezo+Tira', dsc = 'Cox PH model') %>% 
    union_all(
        extract_flexsurvreg_effect(just_trial_parametric$Weibull[[1]], nm = 'armAtezo+Tira',dsc = 'Weibull PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_1_A_cs, nm = 'exp_beta.arm',dsc = 'Weibull PH model in JAGS (non-informative priors)')
    ) %>% 
    union_all(
        extract_flexsurvreg_effect(just_trial_parametric$Exponential[[1]], nm = 'armAtezo+Tira',dsc = 'Exponential PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_2_A_cs, nm = 'exp_beta.arm',dsc = 'Exponential PH model in JAGS (non-informative priors)')
    )

morpheus_only_results_out <- summary_table_and_plot(morpheus_only_results)

# Full population models
full_population_results <- extract_cox_effect(combined_full_cox,'armAtezo+Tira', dsc = 'Cox PH model') %>% 
    union_all(
        extract_flexsurvreg_effect(full_pop_parametric$Weibull[[1]], nm = 'armAtezo+Tira',dsc = 'Weibull PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_1_C_cs, nm = 'exp_beta.arm',dsc = 'Weibull PH model in JAGS (non-informative priors)')
    ) %>% 
    union_all(
        extract_flexsurvreg_effect(full_pop_parametric$Exponential[[1]], nm = 'armAtezo+Tira',dsc = 'Exponential PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_2_C_cs, nm = 'exp_beta.arm',dsc = 'Exponential PH model in JAGS (non-informative priors)')
    )

full_population_results_out <- summary_table_and_plot(full_population_results)

# 1:1 PS matched populations
matched_1_1_non_dynamic_results <- extract_cox_effect(combined_ps_match_cox,'armAtezo+Tira', dsc = 'Cox PH model') %>% 
    union_all(
        extract_flexsurvreg_effect(ps_match_pop_parametric$Weibull[[1]], nm = 'armAtezo+Tira',dsc = 'Weibull PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_1_B_cs, nm = 'exp_beta.arm',dsc = 'Weibull PH model in JAGS (non-informative priors)')
    ) %>% 
    union_all(
        extract_flexsurvreg_effect(ps_match_pop_parametric$Exponential[[1]], nm = 'armAtezo+Tira',dsc = 'Exponential PH model')
    ) %>% 
    union_all(
        extract_jags_effect(bdb_prep_model_2_B_cs, nm = 'exp_beta.arm',dsc = 'Exponential PH model in JAGS (non-informative priors)')
    )

matched_1_1_non_dynamic_results_out <- summary_table_and_plot(matched_1_1_non_dynamic_results)

# Bayesian dynamic borrowing - matched population
bdb_summary_matched_population <- extract_jags_effect(hybrid_control_model_1_A_I_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '1.A.I. Weibull dist, less informative tau, (2 alphas 1 beta)') %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_1_A_II_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '1.A.II. Weibull dist, less informative tau, (3 betas)')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_1_A_III_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '1.A.III. Weibull dist, less informative tau, (zeros method)')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_1_A_IV_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '1.A.IV. Weibull dist, less informative tau, (iterating separately)')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_2_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '2.A. Weibull dist, more informative tau')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_3_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '3.A. Exponential dist, less informative tau')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_4_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '4.A. Exponential dist, more informative tau')
    )

bdb_summary_matched_population_out <- summary_table_and_plot(bdb_summary_matched_population)

# Bayesian dynamic borrowing - full population
bdb_summary_full_population <- extract_jags_effect(hybrid_control_model_1_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '2.B. Weibull dist, less informative tau') %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_2_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '2.B. Weibull dist, more informative tau')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_3_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '3.B. Exponential dist, less informative tau')
    )%>% 
    union_all(
        extract_jags_effect(hybrid_control_model_4_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = '4.B. Exponential dist, more informative tau')
    )

bdb_summary_full_population_out <- summary_table_and_plot(bdb_summary_full_population)

# Succinct summary
succinct_summary <- extract_cox_effect(morpheus_only_cox,'armAtezo+Tira', dsc = 'Frequentist__MORPHEUS__Cox PH__') %>% 
    union_all(
        extract_cox_effect(combined_ps_match_cox,'armAtezo+Tira', dsc = 'Frequentist__1:1 PS matched population__Cox PH__')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_1_A_I_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__1:1 PS matched population__Weibull PH__less informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_2_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__1:1 PS matched population__Weibull PH__more informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_3_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__1:1 PS matched population__Exponential PH__less informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_4_A_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__1:1 PS matched population__Exponential PH__more informative tau')
    ) %>% 
    
    union_all(
        extract_jags_effect(hybrid_control_model_1_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__Full population__Weibull PH__less informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_2_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__Full population__Weibull PH__more informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_3_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__Full population__Exponential PH__less informative tau')
    ) %>% 
    union_all(
        extract_jags_effect(hybrid_control_model_4_B_cs, nm = 'HR_atezo_tira_vs_atezo',dsc = 'Bayesian dynamic borrowing__Full population__Exponential PH__more informative tau')
    ) %>% 
    
    separate('desc',into=c('Frequentist or Bayesian','Population','Distribution','Additional'),sep='__') %>% 
    mutate_at(1:4,fix_length) %>% 
    mutate(id = row_number(),
           id = factor(id, labels = paste0(`Frequentist or Bayesian`,' ',
                                           Population,' ',
                                           Distribution,' ',
                                           Additional)))



succinct_summary_table <- succinct_summary %>% 
    select(-id) %>% 
    gt() %>% 
    fmt_number(columns = 5:7,decimals = 3)

succinct_summary_plot <- succinct_summary %>% 
    ggplot() + 
    geom_point(aes(x = `0.500`, y = fct_rev(id)),color = 'blue')+
    geom_errorbarh(aes(xmin=`0.025`,xmax=`0.975`,y=rev(id)),color = 'blue',height=.2)+
    scale_x_continuous(trans='log10',
                       breaks=c(.1, .2,.5,1,2,5,10),
                       limits=c(.1,11),
                       minor_breaks = c(seq(.1,1,.1),seq(1,10,1)))+
    geom_vline(aes(xintercept=1),linetype=2)+
    labs(
        x='Hazard ratio',
        y = NULL
    )+
    theme_bw(base_family = 'mono', base_size = 12)+
    theme(axis.text.y = element_text(colour = 'black'))
    