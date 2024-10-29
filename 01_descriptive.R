# Table 1
nm_ref <- c(
    'Age at index' = 'age',
    'BMI' = 'bmi',
    'Race' = 'race',
    'Sex' = 'sex',
    'ECOG' = 'ecog',
    'Alkaline phosphatase' = 'alkph',
    'PDL1 status (<5% negative for SP263 clone, else <1% negative)' = 'pdl1',
    'Smoking status' = 'tobhx',
    'Surgery' = 'surgery',
    'Any liver mets' = 'liverfl',
    'Any visceral mets' = 'viscfl',
    'Only Lymph mets' = 'lymphofl',
    'Arm' = 'source_arm'
)
    
tbl1 <- combined_ch_full %>% 
    select(nm_ref) %>% 
    tbl_summary(by='Arm') %>% 
    add_overall()

# KM curves
crude_km_by_arm <- simple_km_out(
    Surv(os_months,os_event)~source_arm,
    combined_ch_full
)
