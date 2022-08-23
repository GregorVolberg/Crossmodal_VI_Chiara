# import PsyToolKit data
library(tidyverse)

# conmat to data frame
conmat  = read_delim('./conmat-2.txt', 
                     delim = ' ',
                     col_names = c('snd1', 'snd2', 'snd3', 'pic_name')) %>%
            separate(pic_name, c('pic', NA), sep = '_', remove = F) %>%
            separate(snd1, c('snd1', 'freq'), sep = '_') %>%
            separate(snd2, c('snd2', NA), sep = '_') %>%
            separate(snd3, c('snd3', NA), sep = '_') %>%
            mutate(pic = as_factor(pic),
                   freq = fct_recode(as_factor(freq),
                           "750"  = '1',
                           "1250" = '2',
                           "2000" = '3',
                           "2750" = '4'),
                   row_index = 1:360) %>%
          select(c('snd1', 'snd2', 'snd3', 'freq', 'pic', 'pic_name', 'row_index'))

subs = dir('./psytoolkit/', full.names = TRUE)

## read individual data files and add condition information
df = NULL
for (vp in 1:length(subs)){
    dat = read_delim(subs[vp],
                     delim = ' ',
                     col_names = c('row_index', 'rt', 'STATUS',
                                   'response', 'trial_length', 'block')) %>%
          mutate(timeout = (STATUS == 3),
                 trialNo = 1:360,
                 block   = as_factor(block)) %>%
         select(., c('row_index', 'rt', 'response',
                     'timeout', 'trial_length', 'block', 'trialNo')) %>%
         left_join(., conmat, 'row_index')%>%
         mutate(match = case_when(
         response == 1 ~ snd1,
         response == 2 ~ snd2,
         response == 3 ~ snd3),
         id = paste('S', vp, sep='')) %>%
      mutate_if(is.character, as_factor) %>%
      select(id, trialNo, row_index, snd1:snd3, freq, pic, pic_name, match, rt, timeout, block)
      df = rbind(df, dat)
} 

df = df %>%
       mutate(pic = fct_relevel(pic, 'rund', 'drei', 'rech'),
       match  = fct_relevel(match, 'sin', 'saw', 'squ'))  


saveRDS(df, 'ExpVI_ChiaraT.rds')
write_excel_csv2(df, 'ExpVI_ChiaraT.csv')

