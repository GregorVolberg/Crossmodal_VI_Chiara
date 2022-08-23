# import soscisurvey data
library(tidyverse)

source('./soscisurv/import_TheartofJudy_2022-08-11_16-32.r') 
picnames <- read_delim('jody_filenames.txt', delim = ' ', col_names = c('name', 'code'))

ds = ds %>% 
  select(age = SD02_01, sex = SD01, SD35_01:SD57_06) %>%
  drop_na() %>%
  pivot_longer(SD35_01:SD57_06, names_to = "nam", values_to = 'val') %>%
  separate(nam, c('code', 'nr'), sep='_') %>%
  left_join(., picnames, by = 'code') %>%
  separate(name, c(NA, 'pic_type', NA), sep = '_', remove = F) %>%
  select(age, sex, pic_type, pic_name = name, val) %>%
  mutate(question = as_factor(rep(c('freudig', 'traurig', 'zornig',
                                    'aengstlich', 'ekelhaft', 'ueberraschend'),
                                    10 * 3 * 37)),
         vp = paste('P', rep(1:37, each=30*6), sep='')) %>%
         #sex = fct_recode(as_factor(sex), female = '1', male = '2', diverse = '3'),
         #nam = NULL) %>%
  pivot_wider(., names_from = question, values_from = val) %>%
  select(vp, age, sex, pic_type, pic_name, freudig:ueberraschend) 


write_excel_csv2(ds, './soscisurv/SemKonnot_ExpVI_ChiaraT.csv')
write_rds(ds, './soscisurv/SemKonnot_ExpVI_ChiaraT.rds')

