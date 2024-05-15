
library(tidyverse)
library(readxl)
library(flextable)
library(ggtext)


## Denne fanen inneholder Tabell 1, 2 og 3. Tabell 2 og 3 viser her ikke p-verdier. P-verdiene vises under fanen "statistiske tester.R".  

### Tabell 1: Deskriptiv statistikk ########################

dxadataset <- Tidy_data_dxa_Masterthesis <- read_excel("data/Tidy_data_dxa_Masterthesis.xlsx", 
                                                       col_types = c("text", "text", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric"))

glimpse(dxadataset)


test <- dxadataset %>%
  select(participant, sex, age, Height_cm, Weight_kg, BMI_kgm2, Fat_mass_percent, Fat_mass_kg, Lean_mass_kg) %>%
  
  group_by(sex) %>%
  mutate(n = n()) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = age:n) %>%
  group_by(sex, variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = if_else(variable == "n",
                      as.character(m), 
                      paste0(signif(m, 3),
                             "(", 
                             signif(s, 3),
                             ")"))) %>%
  ungroup() %>%
  select(-m, -s) %>%
  pivot_wider(names_from = sex,
              values_from = ms) %>%
  mutate(variable = factor(variable, levels = c("n", "age", "Weight_kg", "Height_cm", "BMI_kgm2", "Fat_mass_kg", "Fat_mass_percent", "Lean_mass_kg"),
                           labels = c("N", "Alder (år)", "kroppsvekt (kg)", "høyde (cm)", "BMI (kg/m2)", "Fettmasse (kg)", "Fettmasse (%)", "Fettfri masse (kg)"))) %>%
  
  select(variable, female, male) %>%
  
  arrange(variable) %>%
  flextable() %>%
  set_header_labels(variable = "",
                    female = "Kvinner",
                    male = "Menn") %>%
  add_footer_row(values = "Tabell 1: Verdiene viser gjennomsnitt og (SD)", colwidths = 3) %>%
  autofit() %>%
  # flextable::save_as_docx(path = "descriptives/dxadatatabell.docx") %>%
  print()





### Hvilemetabolisme og VO2-verdier ########################

rmr <- Tidy_data_RMR_new <- read_excel("data/Tidy_data_RMR-new.xlsx", 
                                       col_types = c("numeric", "text", "text", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))

glimpse(rmr)

### Tabell 2 ########################

### Første del: Snitt dag 1 og 2 ###


testtable <- rmr %>%
  select(subject, mean_oxycon_measures_weir, mean_measures_vo2master, mean_vo2_measure_oxycon, mean_vo2_measure_vo2master) %>%
  mutate(change = mean_measures_vo2master - mean_oxycon_measures_weir) %>%
  mutate(diff = mean_vo2_measure_vo2master - mean_vo2_measure_oxycon) %>%
  mutate(changepercent = ((mean_measures_vo2master - mean_oxycon_measures_weir)/mean_oxycon_measures_weir)*100) %>%
  mutate(diffpercent = ((mean_vo2_measure_vo2master - mean_vo2_measure_oxycon)/mean_vo2_measure_oxycon)*100) %>%
  
  mutate(n = n()) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = mean_oxycon_measures_weir:n) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = if_else(variable == "n",
                      as.character(m), 
                      paste0(signif(m, 3),
                             "(", 
                             signif(s, 3),
                             ")"))) %>%
  ungroup() %>%
  select(-m, -s) %>%
  
  
  pivot_wider(names_from = variable,
              values_from = ms) %>%
  
  
  select(n, mean_oxycon_measures_weir, mean_measures_vo2master, change, changepercent, mean_vo2_measure_oxycon, mean_vo2_measure_vo2master, diff, diffpercent) %>%
  arrange() %>%
  flextable() %>%
  set_header_labels(n = "N",
                    mean_oxycon_measures_weir = "Oxycon Pro",
                    mean_measures_vo2master = "VO2 Master",
                    change = "Diff",
                    changepercent = "Diff%",
                    mean_vo2_measure_oxycon = "Oxycon Pro",
                    mean_vo2_measure_vo2master = "VO2 Master",
                    diff = "Diff",
                    diffpercent = "Diff%") %>%
  add_header_row(values = c("", "RMR", "RMR", "RMR", "RMR", "VO2", "VO2", "VO2","VO2")) %>%
  merge_at(part = "header", i = 1, j = 2:5) %>%
  merge_at(part = "header", i = 1, j = 6:9) %>%
  add_footer_row(values = "Tabell 2, del 1: Snitt dag 1 og 2", colwidths = 9) %>%
  autofit() %>%
  # flextable::save_as_docx(path = "descriptives/meandiffs.docx") %>%
  print()


### Andre del: Laveste måling ###


testtable2 <- rmr %>%
  select(subject, lowest_measure_oxycon_weir, lowest_measure_vo2master_rq85, lowest_vo2_measure_oxycon, lowest_vo2_measure_vo2master) %>%
  mutate(changelow = lowest_measure_vo2master_rq85 - lowest_measure_oxycon_weir) %>%
  mutate(difflow = lowest_vo2_measure_vo2master - lowest_vo2_measure_oxycon) %>%
  mutate(changepercentlow = ((lowest_measure_vo2master_rq85 - lowest_measure_oxycon_weir)/lowest_measure_oxycon_weir)*100) %>%
  mutate(diffpercentlow = ((lowest_vo2_measure_vo2master - lowest_vo2_measure_oxycon)/lowest_vo2_measure_oxycon)*100) %>%
  
  
  mutate(n = n()) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = lowest_measure_oxycon_weir:n) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = if_else(variable == "n",
                      as.character(m), 
                      paste0(signif(m, 3),
                             "(", 
                             signif(s, 3),
                             ")"))) %>%
  ungroup() %>%
  select(-m, -s) %>%
  
  
  pivot_wider(names_from = variable,
              values_from = ms) %>%
  
  select(n, lowest_measure_oxycon_weir, lowest_measure_vo2master_rq85, changelow, changepercentlow, lowest_vo2_measure_oxycon, lowest_vo2_measure_vo2master, difflow, diffpercentlow) %>%
  arrange() %>%
  flextable() %>%
  set_header_labels(n = "N",
                    lowest_measure_oxycon_weir = "Oxycon Pro",
                    lowest_measure_vo2master_rq85 = "VO2 Master",
                    changelow = "Diff",
                    changepercentlow = "Diff%",
                    lowest_vo2_measure_oxycon = "Oxycon Pro",
                    lowest_vo2_measure_vo2master = "VO2 Master",
                    difflow = "Diff",
                    diffpercentlow = "Diff%") %>%
  add_header_row(values = c("", "RMR", "RMR", "RMR", "RMR", "VO2", "VO2", "VO2","VO2")) %>%
  merge_at(part = "header", i = 1, j = 2:5) %>%
  merge_at(part = "header", i = 1, j = 6:9) %>%
  add_footer_row(values = "Tabell 2, del 2: Laveste måling", colwidths = 9) %>%
  autofit() %>%
  # flextable::save_as_docx(path = "descriptives/meandiffslow.docx") %>%
  print()


### Tabell 3 ########################

### Første del: Reliabilitet Oxycon Pro ###


testtable4 <- rmr %>%
  select(subject, rmr_oxycon_weir_day1, rmr_oxycon_weir_day2, vo2_oxycon_day1, vo2_oxycon_day2) %>%
  mutate(change2 = rmr_oxycon_weir_day2 - rmr_oxycon_weir_day1) %>%
  mutate(diff2 = vo2_oxycon_day2 - vo2_oxycon_day1) %>%
  mutate(changepercent2 = ((rmr_oxycon_weir_day2 - rmr_oxycon_weir_day1)/rmr_oxycon_weir_day1)*100) %>%
  mutate(diffpercent2 = ((vo2_oxycon_day2 - vo2_oxycon_day1)/vo2_oxycon_day1)*100) %>%
  
  
  mutate(n = n()) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = rmr_oxycon_weir_day1:n) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = if_else(variable == "n",
                      as.character(m), 
                      paste0(signif(m, 3),
                             "(", 
                             signif(s, 3),
                             ")"))) %>%
  ungroup() %>%
  select(-m, -s) %>%
  
  
  pivot_wider(names_from = variable,
              values_from = ms) %>%
  
  
  select(n, rmr_oxycon_weir_day1, rmr_oxycon_weir_day2, change2, changepercent2, vo2_oxycon_day1, vo2_oxycon_day2, diff2, diffpercent2) %>%
  arrange() %>%
  flextable() %>%
  set_header_labels(n = "N",
                    rmr_oxycon_weir_day1 = "Oxycon Pro dag 1",
                    rmr_oxycon_weir_day2 = "Oxycon Pro dag 2",
                    change2 = "Diff",
                    changepercent2 = "Diff%",
                    vo2_oxycon_day1 = "Oxycon Pro dag 1",
                    vo2_oxycon_day2 = "Oxycon Pro dag 2",
                    diff2 = "Diff",
                    diffpercent2 = "Diff%") %>%
  add_header_row(values = c("", "RMR", "RMR", "RMR", "RMR", "VO2", "VO2", "VO2","VO2")) %>%
  merge_at(part = "header", i = 1, j = 2:5) %>%
  merge_at(part = "header", i = 1, j = 6:9) %>%
  add_footer_row(values = "Tabell 3, del 1: Reliabilitet Oxycon Pro", colwidths = 9) %>%
  autofit() %>%
  # flextable::save_as_docx(path = "descriptives/meandiffslow.docx") %>%
  print()



### Andre del: Reliabilitet VO2 Master ###


testtable3 <- rmr %>%
  select(subject, vo2master_rq85_day1, vo2master_rq85_day2, vo2_vo2master_day1, vo2_vo2master_day2) %>%
  mutate(change1 = vo2master_rq85_day2 - vo2master_rq85_day1) %>%
  mutate(diff1 = vo2_vo2master_day2 - vo2_vo2master_day1) %>%
  mutate(changepercent1 = ((vo2master_rq85_day2 - vo2master_rq85_day1)/vo2master_rq85_day1)*100) %>%
  mutate(diffpercent1 = ((vo2_vo2master_day2 - vo2_vo2master_day1)/vo2_vo2master_day1)*100) %>%
  
  
  mutate(n = n()) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = vo2master_rq85_day1:n) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            s = sd(value)) %>%
  mutate(ms = if_else(variable == "n",
                      as.character(m), 
                      paste0(signif(m, 3),
                             "(", 
                             signif(s, 3),
                             ")"))) %>%
  ungroup() %>%
  select(-m, -s) %>%
  
  
  pivot_wider(names_from = variable,
              values_from = ms) %>%
  
  
  select(n, vo2master_rq85_day1, vo2master_rq85_day2, change1, changepercent1, vo2_vo2master_day1, vo2_vo2master_day2, diff1, diffpercent1) %>%
  arrange() %>%
  flextable() %>%
  set_header_labels(n = "N",
                    vo2master_rq85_day1 = "VO2 Master dag 1",
                    vo2master_rq85_day2 = "VO2 Master dag 2",
                    change1 = "Diff",
                    changepercent1 = "Diff%",
                    vo2_vo2master_day1 = "VO2 Master dag 1",
                    vo2_vo2master_day2 = "VO2 Master dag 2",
                    diff1 = "Diff",
                    diffpercent1 = "Diff%") %>%
  add_header_row(values = c("", "RMR", "RMR", "RMR", "RMR", "VO2", "VO2", "VO2","VO2")) %>%
  merge_at(part = "header", i = 1, j = 2:5) %>%
  merge_at(part = "header", i = 1, j = 6:9) %>%
  add_footer_row(values = "Tabell 3, del 2: Reliabilitet VO2 Master", colwidths = 9) %>%
  autofit() %>%
  # flextable::save_as_docx(path = "descriptives/meandiffslow.docx") %>%
  print()



