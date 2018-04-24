############################################################
#                                                          #
#                 Extract data from Excel                  #
#                                                          #
############################################################

############################################################
#                                                          #
#           Note: The original demographic data            #
#           that this script cleans is available           #
#              on request only. The data have              #
#        not been made publically available because        #
#              of the potential for personal               #
#        identification. However, we have made this        #
#        cleaning script available, which shows all        #
#       steps executed during the cleaning process.        #
#                                                          #
############################################################

# Load packages
library(readxl)
library(tidyverse)
library(magrittr)
library(skimr)

############################################################
#                                                          #
#                       Demographics                       #
#                                                          #
############################################################
# Extract demographic information
demographics <- read_excel(path = 'original-data/demographics.xlsx', 
                           sheet = "demographics") %>%
    # Remove empty rows 
    filter(!is.na(Nr)) %>%
    # Convert '9999' missing to <NA>
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .))) %>%
    # Remove X__1 and DOD columns (provide inconsistent day/month info)
    select(-X__1, -DOD) %>%
    # Fix column names
    rename(ID = Nr,
           Site = `Clinical Site`,
           DODx = X__2,
           Years_on_ART = `Years since ARV start`,
           CD4_recent = `Recent CD4`,
           HIV_stage = `HIV Stage`,
           HIV_mx = `HIV Mx`,
           CM_dx = `CM Dx`,
           SOS_mnemonic = `SOS Mnemonic`) %>%
    # Fix column classes
    mutate(# DOB 
           DOB = gsub(pattern = '/',
                      replacement = '-',
                      x = DOB),
           DOB = lubridate::dmy(DOB)) %>%
    # Fix column formating
    mutate(# DODx
           DODx = as.numeric(DODx),
           # CD4
           CD4 = round(CD4),
           # Recent CD4
           CD4_recent = round(CD4_recent)) %>%
    # Convert all text to lowercase
    mutate_if(.predicate = is.character, 
              .funs = str_to_lower) %>% 
    # Convert ID, Group, Site back to uppercase
    mutate_at(.vars = c('ID', 'Group', 'Site'), 
              .funs = str_to_upper)

# Have a look
glimpse(demographics)
skim(demographics)

# Check levels of character classes
demographics %>%
    select_if(is.character) %>%
    as.list(.) %>%
    purrr::map(unique)

# Further cleaning after inspecting data
demographics %<>%
    # Remove unneeded column
    select(-Duration,
           -CM_dx,
           -Infections,
           -Language) %>%
    # Simplify analgesics column
    mutate(Adjuvant = case_when(
        str_detect(.$Analgesic, 'adjuvant') ~ 'yes',
        TRUE ~ 'no'
    ),
    NSAID = case_when(
        str_detect(.$Analgesic, 'ibuprofen') |
            str_detect(.$Analgesic, 'nsaid') ~ 'yes',
        TRUE ~ 'no'
    ),
    Paracetamol = case_when(
        str_detect(.$Analgesic, 'paracetamol') ~ 'yes',
        TRUE ~ 'no'
    ),
    Mild_opioid = case_when(
        str_detect(.$Analgesic, 'opioid') ~ 'yes',
        TRUE ~ 'no'
    )) %>%
    # Specify WHO analgesic ladder
    mutate(WHO_level = ifelse(Mild_opioid == 'yes',
                              yes = 2,
                              no = ifelse(Paracetamol == 'yes' |
                                              NSAID == 'yes',
                                          yes = 1,
                                          no = 0))) %>%
    # Remove Analgesic column
    select(-Analgesic) %>%
    # Fix education
    ## Relevel
    mutate(Years_education  = case_when(
        stringr::str_detect(.$HLOE, 'grade 12') ~ '12',
            stringr::str_detect(.$HLOE, 'receptionist training') |
            stringr::str_detect(.$HLOE, 'ecd level 4') |
            stringr::str_detect(.$HLOE, 'anciliary healthcare') |
            stringr::str_detect(.$HLOE, 'tertiary degree') |
            stringr::str_detect(.$HLOE, 'computer course') ~ '12+',
        stringr::str_detect(.$HLOE, 'grade 11') ~ '11',
        stringr::str_detect(.$HLOE, 'grade 10') ~ '10',
        stringr::str_detect(.$HLOE, 'grade 9') ~ '9',
        stringr::str_detect(.$HLOE, 'grade 8') ~ '8',
        stringr::str_detect(.$HLOE, 'grade 7') ~ '7',
        stringr::str_detect(.$HLOE, 'grade 6') ~ '6',
        stringr::str_detect(.$HLOE, 'grade 5') ~ '5',
        stringr::str_detect(.$HLOE, 'grade 4') ~ '4',
        stringr::str_detect(.$HLOE, 'grade 3') ~ '3',
        stringr::str_detect(.$HLOE, 'grade 2') ~ '2',
        stringr::str_detect(.$HLOE, 'grade 1') ~ '1',
        stringr::str_detect(.$HLOE, 'grade r') |
            stringr::str_detect(.$HLOE, 'nil') |
            stringr::str_detect(.$HLOE, 'no formal schooling') ~ '0'
        )) %>%
    # Order education levels
    mutate(Years_education = factor(Years_education,
                                    levels = rev(c('12+', '12', '11', '10', '9',
                                                   '8', '7', '6', '5', '4', '3', 
                                                   '2', '1', '0')),
                                    ordered = TRUE)) %>%
    # Remove HLOE column
    select(-HLOE) %>%
    # Fix Occupation column
    mutate(Occupation = ifelse(Occupation == 'temporary employment' |
                                   Occupation == 'employed',
                               yes = 'employed',
                               no = ifelse(Occupation == 'student' |
                                               Occupation == 'volunteering',
                                           yes = 'student/volunteer',
                                           no = Occupation))) %>%
    # Remove Group E participants
    filter(Group != 'E')

# Save outputs
write_rds(x = demographics, 
          path = './data/demographics.rds')
write_csv(x = demographics,
          path = './data/demographics.csv')

############################################################
#                                                          #
#                           BPI                            #
#                                                          #
############################################################
# Group E (for removal later)
group_e <- c('J2', 'J8', 'J13', 'J14', 'J15', 'J16', 'J20', 'J21', 'J25', 
             'J26', 'J27', 'J28', 'J39', 'J47', 'J48', 'J50', 'J58', 
             'J63', 'J64', 'J65', 'J66', 'J69', 'J70')

# Extract BPI information
BPI_scores <- read_excel(path = 'original-data/amalgamated_data.xlsx', 
                         sheet = "BPI")

# Remove empty row at top of sheet
BPI_clean <- BPI_scores[-1,]
        
# Fix column names by renaming appropriately. 
# Use setnames command with concatenation as assign to BPI_clean data frame:
BPI_clean <-
    setNames(BPI_clean, c(
        'ID', 
        'Pain_present.BL',
        'Pain_present.Wk4',
        'Pain_present.Wk8',
        'Pain_present.Wk12',
        'Pain_present.Wk24',
        'Pain_present.Wk48',
        'Worst_pain.BL',
        'Worst_pain.Wk4',
        'Worst_pain.Wk8',
        'Worst_pain.Wk12',
        'Worst_pain.Wk24',
        'Worst_pain.Wk48',
        'Least_pain.BL', 
        'Least_pain.Wk4',
        'Least_pain.Wk8',
        'Least_pain.Wk12',
        'Least_pain.Wk24',
        'Least_pain.Wk48',
        'Average_pain.BL',
        'Average_pain.Wk4',
        'Average_pain.Wk8',
        'Average_pain.Wk12',
        'Average_pain.Wk24',
        'Average_pain.Wk48',
        'Pain_now.BL',
        'Pain_now.Wk4',
        'Pain_now.Wk8',
        'Pain_now.Wk12',
        'Pain_now.Wk24',
        'Pain_now.Wk48',
        'Receiving_rx.BL',
        'Receiving_rx.Wk4',
        'Receiving_rx.Wk8',
        'Receiving_rx.Wk12',
        'Receiving_rx.Wk24',
        'Receiving_rx.Wk48',
        'Relief_rx.BL',
        'Relief_rx.Wk4',
        'Relief_rx.Wk8',
        'Relief_rx.Wk12',
        'Relief_rx.Wk24',
        'Relief_rx.Wk48',
        'Activities_of_daily_living.BL',
        'Activities_of_daily_living.Wk4',
        'Activities_of_daily_living.Wk8',
        'Activities_of_daily_living.Wk12',
        'Activities_of_daily_living.Wk24',
        'Activities_of_daily_living.Wk48',
        'Mood.BL',
        'Mood.Wk4',
        'Mood.Wk8',
        'Mood.Wk12',
        'Mood.Wk24',
        'Mood.Wk48',
        'Walking.BL',
        'Walking.Wk4',
        'Walking.Wk8',
        'Walking.Wk12',
        'Walking.Wk24',
        'Walking.Wk48',
        'Work.BL',
        'Work.Wk4',
        'Work.Wk8',
        'Work.Wk12',
        'Work.Wk24',
        'Work.Wk48',
        'Relationship_with_others.BL',
        'Relationship_with_others.Wk4',
        'Relationship_with_others.Wk8',
        'Relationship_with_others.Wk12',
        'Relationship_with_others.Wk24',
        'Relationship_with_others.Wk48',
        'Sleep.BL',
        'Sleep.Wk4',
        'Sleep.Wk8',
        'Sleep.Wk12',
        'Sleep.Wk24',
        'Sleep.Wk48',
        'Enjoyment_of_life.BL',
        'Enjoyment_of_life.Wk4',
        'Enjoyment_of_life.Wk8',
        'Enjoyment_of_life.Wk12',
        'Enjoyment_of_life.Wk24',
        'Enjoyment_of_life.Wk48'))
    
head(BPI_clean)

# Convert '9999' missing to <NA>
BPI_clean %<>%    
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .)))

# Fix column classes
BPI_clean %<>%
    mutate_at(.vars = c(1:7, 32:37), 
              .funs = as.character) %>% 
    mutate_at(.vars = c(8:31, 38:85), 
              .funs = as.integer)

# Remove Group E participants
BPI_clean <- filter(BPI_clean, 
                    !ID %in% group_e)

# Have a look
glimpse(BPI_clean)
skim(BPI_clean)

# Save outputs
write_rds(x = BPI_clean, 
          path = './data/bpi.rds')
write_csv(x = BPI_clean,
          path = './data/bpi.csv')

# Select Baseline columns only for factor analysis
BPI_baseline <- select(BPI_clean, contains('BL')) %>% 
    # Remove unrequired columns
    select(-Pain_present.BL,
           -Receiving_rx.BL, 
           -Relief_rx.BL) %>% 
    # Retain complete cases only 
    # (will remove R1 data - missing 'Avg.BL' data)
    filter(complete.cases(.))

# Save output
write_rds(x = BPI_baseline, 
          path = './data/bpi_factor_analysis.rds')
write_csv(x = BPI_baseline, 
          path = './data/bpi_factor_analysis.csv')

############################################################
#                                                          #
#                           BDI                            #
#                                                          #
############################################################
# Group E (for removal later)
group_e <- c('J2', 'J8', 'J13', 'J14', 'J15', 'J16', 'J20', 'J21', 'J25', 
             'J26', 'J27', 'J28', 'J39', 'J47', 'J48', 'J50', 'J58', 
             'J63', 'J64', 'J65', 'J66', 'J69', 'J70')

# Extract BDI information
BDI_scores <- read_excel(path = 'original-data/amalgamated_data.xlsx', 
                         sheet = "BDI")

# Remove empty row at top of sheet
BDI_clean <- BDI_scores[-1,]

# Check
glimpse(BDI_clean)

# Fix column names by renaming appropriately. 
# Use setnames command with concatenation as assign to BDI_clean data frame:
BDI_clean <-
    setNames(BDI_clean, c(
        'ID', 
        'Sadness.BL',
        'Sadness.Wk4',
        'Sadness.Wk8',
        'Sadness.Wk12',
        'Sadness.Wk24',
        'Sadness.Wk48',
        'Pessimism.BL',
        'Pessimism.Wk4',
        'Pessimism.Wk8',
        'Pessimism.Wk12',
        'Pessimism.Wk24',
        'Pessimism.Wk48',
        'Past_failures.BL', 
        'Past_failures.Wk4',
        'Past_failures.Wk8',
        'Past_failures.Wk12',
        'Past_failures.Wk24',
        'Past_failures.Wk48',
        'Loss_of_pleasure.BL',
        'Loss_of_pleasure.Wk4',
        'Loss_of_pleasure.Wk8',
        'Loss_of_pleasure.Wk12',
        'Loss_of_pleasure.Wk24',
        'Loss_of_pleasure.Wk48',
        'Guilty_feelings.BL',
        'Guilty_feelings.Wk4',
        'Guilty_feelings.Wk8',
        'Guilty_feelings.Wk12',
        'Guilty_feelings.Wk24',
        'Guilty_feelings.Wk48',
        'Punishment_feelings.BL',
        'Punishment_feelings.Wk4',
        'Punishment_feelings.Wk8',
        'Punishment_feelings.Wk12',
        'Punishment_feelings.Wk24',
        'Punishment_feelings.Wk48',
        'Self_dislike.BL',
        'Self_dislike.Wk4',
        'Self_dislike.Wk8',
        'Self_dislike.Wk12',
        'Self_dislike.Wk24',
        'Self_dislike.Wk48',
        'Self_critical.BL',
        'Self_critical.Wk4',
        'Self_critical.Wk8',
        'Self_critical.Wk12',
        'Self_critical.Wk24',
        'Self_critical.Wk48',
        'Suicidal.BL',
        'Suicidal.Wk4',
        'Suicidal.Wk8',
        'Suicidal.Wk12',
        'Suicidal.Wk24',
        'Suicidal.Wk48',
        'Crying.BL',
        'Crying.Wk4',
        'Crying.Wk8',
        'Crying.Wk12',
        'Crying.Wk24',
        'Crying.Wk48',
        'Agitation.BL',
        'Agitation.Wk4',
        'Agitation.Wk8',
        'Agitation.Wk12',
        'Agitation.Wk24',
        'Agitation.Wk48',
        'Loss_of_interest.BL',
        'Loss_of_interest.Wk4',
        'Loss_of_interest.Wk8',
        'Loss_of_interest.Wk12',
        'Loss_of_interest.Wk24',
        'Loss_of_interest.Wk48',
        'Indecisiveness.BL',
        'Indecisivenes.Wk4',
        'Indecisivenes.Wk8',
        'Indecisivenes.Wk12',
        'Indecisivenes.Wk24',
        'Indecisivenes.Wk48',
        'Worthlessness.BL',
        'Worthlessness.Wk4',
        'Worthlessness.Wk8',
        'Worthlessness.Wk12',
        'Worthlessness.Wk24',
        'Worthlessness.Wk48',
        'Loss_of_energy.BL',
        'Loss_of_energy.Wk4',
        'Loss_of_energy.Wk8',
        'Loss_of_energy.Wk12',
        'Loss_of_energy.Wk24',
        'Loss_of_energy.Wk48',
        'Sleep_a.BL',
        'Sleep_a.Wk4',
        'Sleep_a.Wk8',
        'Sleep_a.Wk12',
        'Sleep_a.Wk24',
        'Sleep_a.Wk48',
        'Irritability.BL',
        'Irritability.Wk4',
        'Irritability.Wk8',
        'Irritability.Wk12',
        'Irritability.Wk24',
        'Irritability.Wk48',
        'Appetite_a.BL',
        'Appetite_a.Wk4',
        'Appetite_a.Wk8',
        'Appetite_a.Wk12',
        'Appetite_a.Wk24',
        'Appetite_a.Wk48',
        'Concentration_difficulty.BL',
        'Concentration_difficulty.Wk4',
        'Concentration_difficulty.Wk8',
        'Concentration_difficulty.Wk12',
        'Concentration_difficulty.Wk24',
        'Concentration_difficulty.Wk48',
        'Fatigue.BL',
        'Fatigue.Wk4',
        'Fatigue.Wk8',
        'Fatigue.Wk12',
        'Fatigue.Wk24',
        'Fatigue.Wk48',
        'Loss_of_interest_in_sex.BL',
        'Loss_of_interest_in_sex.Wk4',
        'Loss_of_interest_in_sex.Wk8',
        'Loss_of_interest_in_sex.Wk12',
        'Loss_of_interest_in_sex.Wk24',
        'Loss_of_interest_in_sex.Wk48'))

# Check
glimpse(BDI_clean)

# Convert '9999' missing to <NA>
BDI_clean %<>%    
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .)))

# Remove Group E participants
BDI_clean %<>%
    filter(!ID %in% group_e)

# Fix column classes with 
BDI_clean %<>%
    separate(col = Appetite_a.BL, 
             into = c('Appetite.BL', 'extra'), 
             sep = 1) %>%
    separate(col = Appetite_a.Wk4, 
             into = c('Appetite.Wk4', 'extra'), 
             sep = 1) %>%
    separate(col = Appetite_a.Wk8, 
             into = c('Appetite.Wk8', 'extra'), 
             sep = 1) %>%
    separate(col = Appetite_a.Wk12, 
             into = c('Appetite.Wk12', 'extra'), 
             sep = 1) %>%
    separate(col = Appetite_a.Wk24, 
             into = c('Appetite.Wk24', 'extra'), 
             sep = 1) %>%
    separate(col = Appetite_a.Wk48, 
             into = c('Appetite.Wk48', 'extra'), 
             sep = 1) %>% 
    separate(col = Sleep_a.BL, 
             into = c('Sleep.BL', 'extra'), 
             sep = 1) %>%
    separate(col = Sleep_a.Wk4, 
             into = c('Sleep.Wk4', 'extra'), 
             sep = 1) %>%
    separate(col = Sleep_a.Wk8, 
             into = c('Sleep.Wk8', 'extra'), 
             sep = 1) %>%
    separate(col = Sleep_a.Wk12, 
             into = c('Sleep.Wk12', 'extra'), 
             sep = 1) %>%
    separate(col = Sleep_a.Wk24, 
             into = c('Sleep.Wk24', 'extra'), 
             sep = 1) %>%
    separate(col = Sleep_a.Wk48, 
             into = c('Sleep.Wk48', 'extra'), 
             sep = 1) %>% 
    select(-extra)

# Convert columns to numeric
BDI_clean %>%
    mutate_at(.vars = 2:ncol(BPI_clean),
              .funs = as.integer)

# Have a look
glimpse(BDI_clean)
skim(BDI_clean)

# Save outputs
write_rds(x = BDI_clean, 
          path = './data/bdi.rds')
write_csv(x = BDI_clean,
          path = './data/bdi.csv')

############################################################
#                                                          #
#                           EQ-5D                          #
#                                                          #
############################################################
# Group E (for removal later)
group_e <- c('J2', 'J8', 'J13', 'J14', 'J15', 'J16', 'J20', 'J21', 'J25', 
             'J26', 'J27', 'J28', 'J39', 'J47', 'J48', 'J50', 'J58', 
             'J63', 'J64', 'J65', 'J66', 'J69', 'J70')

# Extract EQ-5D information
EQ5D <- read_excel(path = 'original-data/amalgamated_data.xlsx', 
                   sheet = "EQ5D")

# Remove empty row at top of sheet
EQ5D_clean <- EQ5D[-1,]

# Fix column names by renaming appropriately. 
# Use setnames command with concatenation as assign to BPI_clean data frame:
EQ5D_clean <-
    setNames(EQ5D_clean, c(
        'ID', 
        'Mobility.BL',
        'Mobility.Wk4',
        'Mobility.Wk8',
        'Mobility.Wk12',
        'Mobility.Wk24',
        'Mobility.Wk48',
        'Self_care.BL',
        'Self_care.Wk4',
        'Self_care.Wk8',
        'Self_care.Wk12',
        'Self_care.Wk24',
        'Self_care.Wk48',
        'Usual_activities.BL', 
        'Usual_activities.Wk4',
        'Usual_activities.Wk8',
        'Usual_activities.Wk12',
        'Usual_activities.Wk24',
        'Usual_activities.Wk48',
        'Pain.BL',
        'Pain.Wk4',
        'Pain.Wk8',
        'Pain.Wk12',
        'Pain.Wk24',
        'Pain.Wk48',
        'Anxiety_and_depression.BL',
        'Anxiety_and_depression.Wk4',
        'Anxiety_and_depression.Wk8',
        'Anxiety_and_depression.Wk12',
        'Anxiety_and_depression.Wk24',
        'Anxiety_and_depression.Wk48',
        'State_of_health.BL',
        'State_of_health.Wk4',
        'State_of_health.Wk8',
        'State_of_health.Wk12',
        'State_of_health.Wk24',
        'State_of_health.Wk48'))

# Quick check
glimpse(EQ5D_clean)

# Convert '9999' missing to <NA>
EQ5D_clean %<>%    
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .)))

# Remove Group E participants
EQ5D_clean %<>% 
    filter(!ID %in% group_e)

# Fix column classes
EQ5D_clean %<>%
    mutate_at(.vars = 2:37, 
              .funs = as.integer)

# Have a look
glimpse(EQ5D_clean)
skim(EQ5D_clean)

# Save outputs
write_rds(x = EQ5D_clean, 
          path = './data/eq5d.rds')
write_csv(x = EQ5D_clean,
          path = './data/eq5d.csv')

############################################################
#                                                          #
#                           SE-6                           #
#                                                          #
############################################################
# Group E (for removal later)
group_e <- c('J2', 'J8', 'J13', 'J14', 'J15', 'J16', 'J20', 'J21', 'J25', 
             'J26', 'J27', 'J28', 'J39', 'J47', 'J48', 'J50', 'J58', 
             'J63', 'J64', 'J65', 'J66', 'J69', 'J70')

# Extract SE-6 information
SE6_scores <- read_excel(path = 'original-data/amalgamated_data.xlsx', 
                         sheet = "SE6")

# Remove empty row at top of sheet
SE6_clean <- SE6_scores[-1,]

# Fix column names by renaming appropriately. 
# Use setnames command with concatenation and assign to SE6_clean data frame:
SE6_clean <-
    setNames(SE6_clean, c(
        'ID', 
        'Fatigue.BL',
        'Fatigue.Wk4',
        'Fatigue.Wk8',
        'Fatigue.Wk12',
        'Fatigue.Wk24',
        'Fatigue.Wk48',
        'Physical_discomfort.BL',
        'Physical_discomfort.Wk4',
        'Physical_discomfort.Wk8',
        'Physical_discomfort.Wk12',
        'Physical_discomfort.Wk24',
        'Physical_discomfort.Wk48',
        'Emotional_distress.BL', 
        'Emotional_distress.Wk4',
        'Emotional_distress.Wk8',
        'Emotional_distress.Wk12',
        'Emotional_distress.Wk24',
        'Emotional_distress.Wk48',
        'Other_symptoms.BL',
        'Other_symptoms.Wk4',
        'Other_symptoms.Wk8',
        'Other_symptoms.Wk12',
        'Other_symptoms.Wk24',
        'Other_symptoms.Wk48',
        'Tasks.BL',
        'Tasks.Wk4',
        'Tasks.Wk8',
        'Tasks.Wk12',
        'Tasks.Wk24',
        'Tasks.Wk48',
        'Non_drug.BL',
        'Non_drug.Wk4',
        'Non_drug.Wk8',
        'Non_drug.Wk12',
        'Non_drug.Wk24',
        'Non_drug.Wk48'))

# Quick check
glimpse(SE6_clean)

# Convert '9999' missing to <NA>
SE6_clean <- SE6_clean %>%    
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .)))

# Remove Group E participants
SE6_clean %<>%
    filter(!ID %in% group_e)

# Fix column classes
SE6_clean %<>%
    mutate_at(.vars = 2:37, 
              .funs = as.integer)

# Have a look
glimpse(SE6_clean)
skim(SE6_clean)

# Save outputs
write_rds(x = SE6_clean, 
          path = './data/se6.rds')
write_csv(x = SE6_clean,
          path = './data/se6.csv')

############################################################
#                                                          #
#                           Simmonds                       #
#                                                          #
############################################################
# Group E (for removal later)
group_e <- c('J2', 'J8', 'J13', 'J14', 'J15', 'J16', 'J20', 'J21', 'J25', 
             'J26', 'J27', 'J28', 'J39', 'J47', 'J48', 'J50', 'J58', 
             'J63', 'J64', 'J65', 'J66', 'J69', 'J70')

# Extract Simmonds Battery information
Simmonds_scores <- read_excel(path = 'original-data/amalgamated_data.xlsx', 
                              sheet = "Simmonds")

# Remove 2 empty rows at top of sheet
Simmonds_clean <- Simmonds_scores[-c(1,2),]

# Fix column names by renaming appropriately. 
# Use setnames command with concatenation and assign to Simmonds_clean data frame:
Simmonds_clean <-
    setNames(Simmonds_clean, c(
        'ID', 
        'Preferred_speed.BL',
        'Preferred_speed.Wk4',
        'Preferred_speed.Wk8',
        'Preferred_speed.Wk12',
        'Preferred_speed.Wk24',
        'Preferred_speed.Wk48',
        'Fastest_speed.BL',
        'Fastest_speed.Wk4',
        'Fastest_speed.Wk8',
        'Fastest_speed.Wk12',
        'Fastest_speed.Wk24',
        'Fastest_speed.Wk48',
        'Unloaded_reach.BL', 
        'Unloaded_reach.Wk4',
        'Unloaded_reach.Wk8',
        'Unloaded_reach.Wk12',
        'Unloaded_reach.Wk24',
        'Unloaded_reach.Wk48',
        'Loaded_reach.BL',
        'Loaded_reach.Wk4',
        'Loaded_reach.Wk8',
        'Loaded_reach.Wk12',
        'Loaded_reach.Wk24',
        'Loaded_reach.Wk48',
        'Sit_to_stand1.BL',
        'Sit_to_stand1.Wk4',
        'Sit_to_stand1.Wk8',
        'Sit_to_stand1.Wk12',
        'Sit_to_stand1.Wk24',
        'Sit_to_stand1.Wk48',
        'Sit_to_stand2.BL',
        'Sit_to_stand2.Wk4',
        'Sit_to_stand2.Wk8',
        'Sit_to_stand2.Wk12',
        'Sit_to_stand2.Wk24',
        'Sit_to_stand2.Wk48',
        'Sock_test.BL',
        'Sock_test.Wk4',
        'Sock_test.Wk8',
        'Sock_test.Wk12',
        'Sock_test.Wk24',
        'Sock_test.Wk48',
        'Reach_up.BL',
        'Reach_up.Wk4',
        'Reach_up.Wk8',
        'Reach_up.Wk12',
        'Reach_up.Wk24',
        'Reach_up.Wk48',
        'Distance.BL',
        'Distance.Wk4',
        'Distance.Wk8',
        'Distance.Wk12',
        'Distance.Wk24',
        'Distance.Wk48',
        'Belt_tie.BL',
        'Belt_tie.Wk4',
        'Belt_tie.Wk8',
        'Belt_tie.Wk12',
        'Belt_tie.Wk24',
        'Belt_tie.Wk48'))

# Quick look
glimpse(Simmonds_clean)

# Convert '9999' missing to <NA>
Simmonds_clean %<>%
    mutate_all(funs(ifelse(. == '9999',
                           yes = NA,
                           no = .)))

# Remove Group E participants
Simmonds_clean %<>%
    filter(!ID %in% group_e)

# Col 2-13 and 26-37 must be numeric with 2 decimal points
Simmonds_clean %<>%
    mutate_at(.vars = c(2:13, 26:61), 
              .funs = as.numeric) %>%
    mutate_at(.vars = c(2:13, 26:61), 
              .funs = round, digits = 2)
    
# Col 14-25 must be integers
Simmonds_clean %<>%
    mutate_at(.vars = 14:25, 
              .funs = as.integer) 

# Have a look
glimpse(Simmonds_clean)
skim(Simmonds_clean)

# Save outputs
write_rds(x = Simmonds_clean, 
          path = './data/simmonds.rds')
write_csv(x = Simmonds_clean,
          path = './data/simmonds.csv')
