Tabulated descriptive data
================
Peter Kamerman and Tory Madden

Description
-----------

Descriptive statistics of core demographic, depression, quality of life, pain, and self-efficacy data at baseline (time = 0 weeks) for the whole cohort, and stratified by sex.

**Note: Participant consent did not provide for the publication of their data, and hence neither the original nor cleaned data have been made available. However, we do not wish to bar access to the data unnecessarily and we will judge requests to access the data on a case-by-case basis. Examples of potential use cases include independent assessments of our analyses, and secondary data analyses. Please contact Prof Romy Parker (<romy.parker@uct.ac.za>), Dr Antonia Wadley (<antonia.wadley@wits.ac.za>), or open an [*issue*](https://github.com/kamermanpr/HIP-supplement/issues) on this repo.**

Setup
-----

``` r
# Load packages
library(tidyverse)
library(magrittr)
library(skimr)
library(tableone)

# Set knitr options
knitr::opts_chunk$set(warning = FALSE,
                      message = FALSE)
```

Import, clean, and process data
-------------------------------

#### Demographic data

``` r
# Get data
demo <- read_rds('../data/demographics.rds') %>%
    # Transfer CD4 data to CD4_recent if missing CD4_recent data 
    # (i.e. most updated CD4 count available)
    mutate(CD4_uptodate =
               ifelse(is.na(CD4_recent),
                      yes = CD4,
                      no = CD4_recent)) %>%
    # Categorise years of schooling into 7 years or less, 8-12 years, 
    # and more than 12 years of education
    mutate(Education_category = case_when(
        Years_education <= 7 ~ '0-7 years',
        Years_education > 7 & Years_education <= 12 ~ '8-12 years',
        Years_education > 12 ~ 'More than 12 years')) %>%
    # Re-level HIV treatment column
    mutate(HIV_mx = case_when(
        HIV_mx == 'first-line' ~ 'first-line ART',
        HIV_mx == 'second-line' ~ 'second-line ART',
        HIV_mx == 'monitoring' ~ 'no ART')) %>% 
    mutate(HIV_mx = factor(HIV_mx)) %>% 
    # Select required columns
    select(ID, Site, Sex, Age,
           Years_on_ART, CD4_uptodate, HIV_mx,
           Education_category, SOS_mnemonic) %>% 
    # Final clean-up
    mutate(Years_on_ART = round(Years_on_ART, 1),
           SOS_mnemonic = ifelse(SOS_mnemonic == 'lhl',
                                 yes = 'Limited health literacy',
                                 no = 'Health literacy')) %>% 
    rename(Age_in_years = Age,
           Years_on_ART = Years_on_ART,
           Most_recent_CD4 = CD4_uptodate,
           HIV_treatment = HIV_mx,
           Years_of_education = Education_category,
           Study_site = Site)
```

#### Depression

Beck's Depression Inventory

``` r
# Get data
bdi <- read_rds('../data/bdi.rds') %>%
    # Select required columns
    select(ID, ends_with('BL'))  %>%
    # Make a total score column
    mutate_at(.vars = 2:22, as.integer) %>%
    mutate(bdi_TOTAL = rowSums(.[2:22])) %>%
    # Select required columns
    select(ID, bdi_TOTAL) %>% 
    # Final clean-up
    rename(BDI_score = bdi_TOTAL)
```

#### Pain data

Brief Pain Inventory

``` r
# Get data
bpi <- read_rds('../data/bpi.rds')

# Rural KZN site did not include average pain question, 
# so use split-apply-combine approach.

# Split-apply
bpi_M <- bpi %>% 
    # Extract site M participants
    filter(str_detect(ID, pattern = 'M')) %>% 
    # Select required columns
    select(ID, ends_with('BL')) %>%
    select(ID, 3:4, 6, 9:15) %>%
    # Calculate Pain severity score (PSS) at baseline (averaged over 4 items)
    mutate(PSS = round(rowMeans(.[2:4]), 1)) %>%
    # Calculate Pain interference score (PIS) at baseline
    mutate(PIS = round(rowMeans(.[5:11]), 1)) %>%
    #remove unwanted columns
    select(ID, PSS, PIS)

bpi_OTHER <- bpi %>% 
    # Extract site M participants
    filter(!str_detect(ID, pattern = 'M')) %>% 
    # Select required columns
    select(ID, ends_with('BL')) %>%
    select(ID, 3:6, 9:15) %>%
    # Calculate Pain severity score (PSS) at baseline (averaged over 5 items)
    mutate(PSS = round(rowMeans(.[2:5]), 1)) %>%
    # Calculate Pain interference score (PIS) at baseline
    mutate(PIS = round(rowMeans(.[6:12]), 1)) %>%
    # Select required columns
    select(ID, PSS, PIS) 

# Combine
bpi <- bind_rows(bpi_OTHER, bpi_M) %>% 
    # Final clean-up
    rename(Pain_severity_score = PSS,
           Pain_interference_score = PIS)

# Clean-up
rm(bpi_M, bpi_OTHER)
```

#### Quality of life

Euroqol(EQ)-5D (3L)

``` r
eq5d <- read_rds('../data/eq5d.rds') %>%
    # Select required columns
    select(ID, contains('BL'))

# Calculate eq5d index score
# Create basic term = 1 for all cases in new column
eq5d$index_core <- 1

# Sum all rows for total BL index score and allocate to a new column in eq5d data frame 
eq5d %<>% mutate(index_sum = rowSums(.[2:6]))

# Create constant term to subtract if any domain scores > 1 (i.e. index_sum > 5)
eq5d %<>% mutate(EQ5D_constant = ifelse(index_sum > 5, 
                                        yes = 0.081, 
                                        no = 0))

# Assign index scores to each domain
eq5d %<>% mutate(mobility_index = ifelse(Mobility.BL == 2, 
                                         yes = 0.069,
                                         no = ifelse(Mobility.BL == 3, 
                                                     yes = 0.314,
                                                     no = 0)))

eq5d %<>% mutate(self_care_index = ifelse(Self_care.BL == 2,
                                          yes = 0.104,
                                          no = ifelse(Self_care.BL == 3, 
                                                      yes = 0.214,
                                                      no = 0)))

eq5d %<>% mutate(usual_act_index = ifelse(Usual_activities.BL == 2, 
                                          yes = 0.036,
                                          no = ifelse(Usual_activities.BL == 3, 
                                                      yes = 0.094,
                                                      no = 0)))

eq5d %<>% mutate(pain_index = ifelse(Pain.BL == 2,
                                     yes = 0.123,
                                     no = ifelse(Pain.BL == 3, 
                                                 yes = 0.386,
                                                 no = 0)))

eq5d %<>% mutate(anx_depr_index = ifelse(Anxiety_and_depression.BL == 2,
                                         yes = 0.071,
                                         no = ifelse(Anxiety_and_depression.BL == 3, 
                                                     yes = 0.236,
                                                     no = 0)))

# Calculate index score
eq5d %<>% mutate(eq5d_index = 
                     index_core - 
                     EQ5D_constant - 
                     mobility_index - 
                     self_care_index - 
                     usual_act_index - 
                     pain_index - 
                     anx_depr_index)

# Select required columns
eq5d %<>% select(ID,
                 State_of_health.BL,
                 eq5d_index) %>% 
    # Final clean-up
    rename(EQ5D_state_of_health_VAS = State_of_health.BL,
           EQ5D_index_score = eq5d_index)
```

#### Self-efficacy

Self-efficacy for Managing Chronic Disease 6-item Scale (SE6)

``` r
se6 <- read_rds('../data/se6.rds') %>%
    # Select required columns
    select(ID, ends_with('BL')) %>%
    # Calculate SE6 at baseline
    mutate(se6_score = round(rowMeans(.[2:7]), 1)) %>%
    # Select required columns
    select(ID, se6_score) %>% 
    # Final clean-up
    rename(SE6_score = se6_score)
```

#### Combine datasets

``` r
data <- demo %>% 
    left_join(bdi) %>%
    left_join(bpi) %>%
    left_join(eq5d) %>%
    left_join(se6)
```

Quick look
----------

``` r
# Check data
glimpse(data)
```

    ## Observations: 160
    ## Variables: 15
    ## $ ID                       <chr> "J1", "J3", "J4", "J5", "J6", "J7", "...
    ## $ Study_site               <chr> "U1", "U1", "U1", "U1", "U1", "U1", "...
    ## $ Sex                      <chr> "female", "female", "female", "female...
    ## $ Age_in_years             <dbl> 37, 36, 36, 58, 33, 32, 37, 46, 31, 3...
    ## $ Years_on_ART             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ Most_recent_CD4          <dbl> 354, 728, 172, 189, NA, 86, 667, 205,...
    ## $ HIV_treatment            <fct> first-line ART, first-line ART, first...
    ## $ Years_of_education       <chr> "8-12 years", NA, "8-12 years", "0-7 ...
    ## $ SOS_mnemonic             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ BDI_score                <dbl> 46, 38, 8, 32, 28, 20, 23, 13, 38, 8,...
    ## $ Pain_severity_score      <dbl> 5.5, 4.5, 2.2, 6.8, 3.8, 6.5, 4.8, 5....
    ## $ Pain_interference_score  <dbl> 4.6, 9.4, 1.0, 7.3, 3.3, 2.7, 3.4, 3....
    ## $ EQ5D_state_of_health_VAS <int> 60, 0, 80, 60, 80, 80, 20, 60, 30, 90...
    ## $ EQ5D_index_score         <dbl> NA, 0.533, 0.727, 0.533, 0.812, 0.497...
    ## $ SE6_score                <dbl> 4.3, NA, 9.5, 9.3, 4.5, 10.0, 8.3, 9....

Summary
-------

#### Missingness

``` r
# Check completeness
data %>% 
    skim_to_wide() %>% 
    select(variable, missing, complete, n) %>%
    mutate_at(.vars = c(2:4),
              .funs = as.integer) %>% 
    arrange(complete)
```

    ## # A tibble: 15 x 4
    ##    variable                 missing complete     n
    ##    <chr>                      <int>    <int> <int>
    ##  1 Years_on_ART                  78       82   160
    ##  2 SOS_mnemonic                  47      113   160
    ##  3 Pain_interference_score       21      139   160
    ##  4 SE6_score                     20      140   160
    ##  5 BDI_score                     19      141   160
    ##  6 EQ5D_index_score              18      142   160
    ##  7 EQ5D_state_of_health_VAS      16      144   160
    ##  8 Pain_severity_score           16      144   160
    ##  9 Most_recent_CD4                8      152   160
    ## 10 HIV_treatment                  4      156   160
    ## 11 Years_of_education             2      158   160
    ## 12 ID                             0      160   160
    ## 13 Sex                            0      160   160
    ## 14 Study_site                     0      160   160
    ## 15 Age_in_years                   0      160   160

#### Tabulated for the whole cohort

``` r
# Define table parameters
# Create a list of the variables we want to tabulate
vars_list <- names(data[ , 2:15])

# Define categorical variables
cat_vars <- vars_list[c(1:2, 6:8)] 

# Define non-normal data
non_vars <- vars_list[c(4:5, 8:14)]
```

``` r
# Create overall table
tab_all <- CreateTableOne(vars = vars_list, 
                          data = data, 
                          factorVars = cat_vars)

# Print table
print(tab_all, 
      nonnormal = non_vars,
      showAllLevels = TRUE)
```

    ##                                          
    ##                                           level                  
    ##   n                                                              
    ##   Study_site (%)                          R1                     
    ##                                           R2                     
    ##                                           U1                     
    ##                                           U2                     
    ##   Sex (%)                                 female                 
    ##                                           male                   
    ##   Age_in_years (mean (sd))                                       
    ##   Years_on_ART (median [IQR])                                    
    ##   Most_recent_CD4 (median [IQR])                                 
    ##   HIV_treatment (%)                       first-line ART         
    ##                                           no ART                 
    ##                                           second-line ART        
    ##   Years_of_education (%)                  0-7 years              
    ##                                           8-12 years             
    ##                                           More than 12 years     
    ##   SOS_mnemonic (%)                        Health literacy        
    ##                                           Limited health literacy
    ##   BDI_score (median [IQR])                                       
    ##   Pain_severity_score (median [IQR])                             
    ##   Pain_interference_score (median [IQR])                         
    ##   EQ5D_state_of_health_VAS (median [IQR])                        
    ##   EQ5D_index_score (median [IQR])                                
    ##   SE6_score (median [IQR])                                       
    ##                                          
    ##                                           Overall                
    ##   n                                          160                 
    ##   Study_site (%)                              47 (29.4)          
    ##                                               49 (30.6)          
    ##                                               47 (29.4)          
    ##                                               17 (10.6)          
    ##   Sex (%)                                     97 (60.6)          
    ##                                               63 (39.4)          
    ##   Age_in_years (mean (sd))                 35.23 (5.65)          
    ##   Years_on_ART (median [IQR])               3.00 [1.00, 5.07]    
    ##   Most_recent_CD4 (median [IQR])          376.00 [224.75, 547.00]
    ##   HIV_treatment (%)                          115 (73.7)          
    ##                                                5 ( 3.2)          
    ##                                               36 (23.1)          
    ##   Years_of_education (%)                      44 (27.8)          
    ##                                              112 (70.9)          
    ##                                                2 ( 1.3)          
    ##   SOS_mnemonic (%)                            35 (31.0)          
    ##                                               78 (69.0)          
    ##   BDI_score (median [IQR])                 21.00 [13.00, 31.00]  
    ##   Pain_severity_score (median [IQR])        5.00 [3.80, 6.00]    
    ##   Pain_interference_score (median [IQR])    5.30 [3.30, 7.30]    
    ##   EQ5D_state_of_health_VAS (median [IQR])  60.00 [50.00, 76.25]  
    ##   EQ5D_index_score (median [IQR])           0.69 [0.49, 0.76]    
    ##   SE6_score (median [IQR])                  7.40 [5.30, 8.70]

#### Tabulated by sex

``` r
# Create table by sex
tab_sex <- CreateTableOne(vars = vars_list, 
                          data = data, 
                          factorVars = cat_vars, 
                          strata = c('Sex'))

# Print table
print(tab_sex, 
      nonnormal = non_vars,
      showAllLevels = TRUE,
      test = FALSE)
```

    ##                                          Stratified by Sex
    ##                                           level                  
    ##   n                                                              
    ##   Study_site (%)                          R1                     
    ##                                           R2                     
    ##                                           U1                     
    ##                                           U2                     
    ##   Sex (%)                                 female                 
    ##                                           male                   
    ##   Age_in_years (mean (sd))                                       
    ##   Years_on_ART (median [IQR])                                    
    ##   Most_recent_CD4 (median [IQR])                                 
    ##   HIV_treatment (%)                       first-line ART         
    ##                                           no ART                 
    ##                                           second-line ART        
    ##   Years_of_education (%)                  0-7 years              
    ##                                           8-12 years             
    ##                                           More than 12 years     
    ##   SOS_mnemonic (%)                        Health literacy        
    ##                                           Limited health literacy
    ##   BDI_score (median [IQR])                                       
    ##   Pain_severity_score (median [IQR])                             
    ##   Pain_interference_score (median [IQR])                         
    ##   EQ5D_state_of_health_VAS (median [IQR])                        
    ##   EQ5D_index_score (median [IQR])                                
    ##   SE6_score (median [IQR])                                       
    ##                                          Stratified by Sex
    ##                                           female                 
    ##   n                                           97                 
    ##   Study_site (%)                               0 (  0.0)         
    ##                                               49 ( 50.5)         
    ##                                               31 ( 32.0)         
    ##                                               17 ( 17.5)         
    ##   Sex (%)                                     97 (100.0)         
    ##                                                0 (  0.0)         
    ##   Age_in_years (mean (sd))                 34.23 (5.97)          
    ##   Years_on_ART (median [IQR])               3.00 [0.95, 5.00]    
    ##   Most_recent_CD4 (median [IQR])          410.50 [244.75, 570.50]
    ##   HIV_treatment (%)                           65 ( 69.1)         
    ##                                                5 (  5.3)         
    ##                                               24 ( 25.5)         
    ##   Years_of_education (%)                      26 ( 27.1)         
    ##                                               68 ( 70.8)         
    ##                                                2 (  2.1)         
    ##   SOS_mnemonic (%)                             7 ( 10.6)         
    ##                                               59 ( 89.4)         
    ##   BDI_score (median [IQR])                 26.00 [17.00, 33.00]  
    ##   Pain_severity_score (median [IQR])        5.00 [3.80, 6.00]    
    ##   Pain_interference_score (median [IQR])    5.40 [3.78, 7.30]    
    ##   EQ5D_state_of_health_VAS (median [IQR])  60.00 [50.00, 75.00]  
    ##   EQ5D_index_score (median [IQR])           0.66 [0.47, 0.76]    
    ##   SE6_score (median [IQR])                  7.50 [5.30, 8.70]    
    ##                                          Stratified by Sex
    ##                                           male                   
    ##   n                                           63                 
    ##   Study_site (%)                              47 ( 74.6)         
    ##                                                0 (  0.0)         
    ##                                               16 ( 25.4)         
    ##                                                0 (  0.0)         
    ##   Sex (%)                                      0 (  0.0)         
    ##                                               63 (100.0)         
    ##   Age_in_years (mean (sd))                 36.76 (4.76)          
    ##   Years_on_ART (median [IQR])               4.15 [2.28, 5.47]    
    ##   Most_recent_CD4 (median [IQR])          335.00 [210.50, 491.75]
    ##   HIV_treatment (%)                           50 ( 80.6)         
    ##                                                0 (  0.0)         
    ##                                               12 ( 19.4)         
    ##   Years_of_education (%)                      18 ( 29.0)         
    ##                                               44 ( 71.0)         
    ##                                                0 (  0.0)         
    ##   SOS_mnemonic (%)                            28 ( 59.6)         
    ##                                               19 ( 40.4)         
    ##   BDI_score (median [IQR])                 15.50 [10.00, 25.00]  
    ##   Pain_severity_score (median [IQR])        5.00 [3.70, 6.10]    
    ##   Pain_interference_score (median [IQR])    4.70 [2.40, 7.10]    
    ##   EQ5D_state_of_health_VAS (median [IQR])  70.00 [50.00, 80.00]  
    ##   EQ5D_index_score (median [IQR])           0.73 [0.51, 0.76]    
    ##   SE6_score (median [IQR])                  7.25 [5.55, 8.68]

Session information
-------------------

    ## R version 3.4.4 (2018-03-15)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.4
    ## 
    ## Matrix products: default
    ## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] tableone_0.9.2     bindrcpp_0.2.2     skimr_1.0.2       
    ##  [4] magrittr_1.5       forcats_0.3.0      stringr_1.3.0     
    ##  [7] dplyr_0.7.4        purrr_0.2.4        readr_1.1.1       
    ## [10] tidyr_0.8.0        tibble_1.4.2       ggplot2_2.2.1.9000
    ## [13] tidyverse_1.2.1    readxl_1.0.0       usethis_1.3.0     
    ## [16] devtools_1.13.5   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.16        lubridate_1.7.4     lattice_0.20-35    
    ##  [4] zoo_1.8-1           class_7.3-14        utf8_1.1.3         
    ##  [7] assertthat_0.2.0    rprojroot_1.3-2     digest_0.6.15      
    ## [10] psych_1.8.3.3       R6_2.2.2            cellranger_1.1.0   
    ## [13] plyr_1.8.4          backports_1.1.2     labelled_1.0.1     
    ## [16] e1071_1.6-8         survey_3.33-2       evaluate_0.10.1    
    ## [19] httr_1.3.1          pillar_1.2.1        rlang_0.2.0        
    ## [22] lazyeval_0.2.1      curl_3.2            rstudioapi_0.7     
    ## [25] data.table_1.10.4-3 car_3.0-0           Matrix_1.2-14      
    ## [28] rmarkdown_1.9       splines_3.4.4       foreign_0.8-69     
    ## [31] pander_0.6.1        munsell_0.4.3       broom_0.4.4        
    ## [34] compiler_3.4.4      modelr_0.1.1        pkgconfig_2.0.1    
    ## [37] mnormt_1.5-5        littleboxes_0.1.0   htmltools_0.3.6    
    ## [40] tidyselect_0.2.4    rio_0.5.10          crayon_1.3.4       
    ## [43] withr_2.1.2         MASS_7.3-49         grid_3.4.4         
    ## [46] nlme_3.1-137        jsonlite_1.5        gtable_0.2.0       
    ## [49] scales_0.5.0.9000   cli_1.0.0           stringi_1.1.7      
    ## [52] carData_3.0-1       reshape2_1.4.3      xml2_1.2.0         
    ## [55] openxlsx_4.0.17     tools_3.4.4         glue_1.2.0         
    ## [58] hms_0.4.2           survival_2.41-3     rsconnect_0.8.8    
    ## [61] abind_1.4-5         parallel_3.4.4      yaml_2.1.18        
    ## [64] colorspace_1.3-2    rvest_0.3.2         memoise_1.1.0      
    ## [67] knitr_1.20          bindr_0.1.1         haven_1.1.1
