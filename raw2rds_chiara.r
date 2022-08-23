# import PsyToolKit data
library(tidyverse)

switch(Sys.info()["nodename"],
              "DESKTOP-MDU4B4V" = setwd('C:/Users/Gregor/Filr/Meine Dateien/CrossModalCor/ExpV_Judy'),
              "PC1012101290"    = setwd('C:/Users/LocalAdmin/Filr/Meine Dateien/CrossModalCor/ExpV_Judy'))

### conmat to data frame
conmat  = read_delim('./conmat-2.txt', delim = ' ', col_names = c('pic1', 'pic2', 'pic3', 'tmp')) %>%
            separate(tmp, c('sound', 'freq'), sep = '_') %>%
            mutate(sound = as_factor(sound),
                   freq = fct_recode(as_factor(freq),
                           "750"  = '1',
                           "1250" = '2',
                           "2000" = '3',
                           "2750" = '4'),
                   row_index = 1:360) %>%
          select(c('pic1', 'pic2', 'pic3', 'sound', 'freq', 'row_index'))

subs = read_csv('./rawdata/data.csv') %>%
       filter(!is.na(TIME_total) & !(TIME_total < 30) & !is.na(survey_code)) %>%
       rename('age' = 'MyQuestion1:1', 'gender' = 'MyQuestion2:1', 'occupation' = 'MyQuestion3:1',
              'studyProgram' = 'MyQuestion4:1','filename' = 'test:1') %>%
       mutate(gender = fct_recode(as_factor(gender),
                                  female  = '1',
                                  male    = '2',
                                  diverse = '3'),
              TIME_start = as.POSIXct(TIME_start, format = '%Y-%m-%d-%H-%M'))

# read individual data files and add condition information
df = NULL
for (vp in 1:nrow(subs)){
dat = read_delim(paste('./rawdata/', subs$filename[vp], sep = ''), delim = ' ',
             col_names = c('row_index', 'rt', 'STATUS', 'response', 'trial_length', 'block')) %>%
      mutate(timeout = (STATUS == 3),
             trialNo = 1:360,
             block   = as_factor(block)) %>%
      select(., c('row_index', 'rt', 'response', 'timeout', 'trial_length', 'block', 'trialNo')) %>%
      left_join(., conmat, 'row_index')%>%
      mutate(match = case_when(
         response == 1 ~ pic1,
         response == 2 ~ pic2,
         response == 3 ~ pic3)) %>%
      separate(match, into = c('m_type', 'pic_nr'), sep = '_', remove = FALSE, convert = TRUE) %>%
      mutate(m_name  = paste(
                       fct_recode(as_factor(m_type),
                          rund = 'rund',
                          rechteckig = 'rech',
                          dreieckig  = 'drei'),
                       pic_nr, sep = ''),
             age = subs$age[vp],
             gender = subs$gender[vp],
             occupation = subs$occupation[vp],
             studyProgram = subs$studyProgram[vp],
             duration = subs$TIME_total[vp],
             starttime = subs$TIME_start[vp],
             filename  =  subs$filename[vp],
             survey_code =  subs$survey_code[vp],
             subject    = paste('S', vp, sep = '')) %>%
      mutate_if(is.character, as_factor) %>%
      select(subject, age, gender, occupation, trialNo, row_index, sound, freq, pic1:pic3, 
             m_type, m_name, rt, timeout, block, studyProgram, duration, survey_code)
      df = rbind(df, dat)
} 

df = df %>%
       mutate(m_type = fct_expand(m_type, 'rund', 'drei', 'rech'),
       m_type = fct_relevel(m_type, 'rund', 'drei', 'rech'),
       sound  = fct_relevel(sound, 'sin', 'saw', 'squ'))  


saveRDS(df, 'ExpV_Judy.rds')
write_excel_csv2(df, 'ExpV_Judy.csv')

