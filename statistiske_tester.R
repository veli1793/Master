
library(cowplot)
library(BlandAltmanLeh)
library(tidyverse)
library(readxl)
library(psych)
library(ggtext)
library(dplyr)
library(ggthemes)








rmr <- Tidy_data_RMR_new <- read_excel("data/Tidy_data_RMR-new.xlsx", 
                                       col_types = c("numeric", "text", "text", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric"))
### Fig 4 A ##############################


ba <- rmr %>%
  select(mean_measures_vo2master, mean_oxycon_measures_weir)%>%
  print()

bland.altman.stats(ba$mean_measures_vo2master, ba$mean_oxycon_measures_weir, two = 1.96, mode = 1, conf.int = 0.95)

ba %>%
  mutate(diff = mean_measures_vo2master - mean_oxycon_measures_weir) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(mean_oxycon_measures_weir, mean_measures_vo2master)), # mean
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)    

meanrmr.ttest <- t.test(ba$mean_measures_vo2master, ba$mean_oxycon_measures_weir, paired = TRUE)

meanrmr.ttest$estimate

meanrmr.ttest$conf.int

meanrmr.ttest$p.value



### Figur 4 B ########################

ba1 <- rmr %>%
  select(lowest_measure_vo2master_rq85, lowest_measure_oxycon_weir)%>%
  print()

ba1 %>%
  mutate(diff = lowest_measure_vo2master_rq85 - lowest_measure_oxycon_weir) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(lowest_measure_oxycon_weir, lowest_measure_vo2master_rq85)), # mean
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  

lowrmr.ttest <- t.test(ba1$lowest_measure_vo2master_rq85, ba1$lowest_measure_oxycon_weir, paired = TRUE)

lowrmr.ttest$estimate

lowrmr.ttest$conf.int

lowrmr.ttest$p.value

### Figur 4 C ########################

bamvo2 <- rmr %>%
  select(mean_vo2_measure_vo2master, mean_vo2_measure_oxycon)%>%
  print()

bamvo2 %>%
  mutate(diff = mean_vo2_measure_vo2master - mean_vo2_measure_oxycon) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(mean_vo2_measure_oxycon, mean_vo2_measure_vo2master)), # mean
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  

meanvo2.ttest <- t.test(bamvo2$mean_vo2_measure_vo2master, bamvo2$mean_vo2_measure_oxycon, paired = TRUE)

meanvo2.ttest$estimate

meanvo2.ttest$conf.int

meanvo2.ttest$p.value

### Figur 4 D ########################

balvo2 <- rmr %>%
  select(lowest_vo2_measure_vo2master, lowest_vo2_measure_oxycon)%>%
  print()

balvo2 %>%
  mutate(diff = lowest_vo2_measure_vo2master - lowest_vo2_measure_oxycon) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(lowest_vo2_measure_oxycon, lowest_vo2_measure_vo2master)), # mean
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  


lowvo2.ttest <- t.test(balvo2$lowest_vo2_measure_vo2master, balvo2$lowest_vo2_measure_oxycon, paired = TRUE)

lowvo2.ttest$estimate

lowvo2.ttest$conf.int

lowvo2.ttest$p.value


### Figur 5 A ########################

bap <- rmr %>%
  select(vo2master_rq85_day1, vo2master_rq85_day2)%>%
  print()

bap %>%
  mutate(diff = vo2master_rq85_day2 - vo2master_rq85_day1) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(vo2master_rq85_day1, vo2master_rq85_day2)), # mean
            te = s / sqrt(2), # the typical error.
            cv_te = 100 * (te / m), # Coefficient of variation, of typical error
            cv_s = 100 * (s / m), # Coefficient of variation, of sd
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  



ICC(bap, missing = TRUE, alpha = .05, lmer = TRUE, check.keys = FALSE)

rmrvm.ttest <- t.test(bap$vo2master_rq85_day1, bap$vo2master_rq85_day2, paired = TRUE)

rmrvm.ttest$estimate

rmrvm.ttest$conf.int

rmrvm.ttest$p.value

shapiro.test(x = bap$vo2master_rq85_day1)
shapiro.test(x = bap$vo2master_rq85_day2)

hist(bap$vo2master_rq85_day1)
qqnorm(bap$vo2master_rq85_day1)

hist(bap$vo2master_rq85_day2)
qqnorm(bap$vo2master_rq85_day2)

wilcox.test(x = bap$vo2master_rq85_day2,
            y = bap$vo2master_rq85_day1,
            paired = TRUE)

### Figur 5 B ########################

ba4 <- rmr %>%
  select(rmr_oxycon_weir_day1, rmr_oxycon_weir_day2)%>%
  print()

ba4 %>%
  mutate(diff = rmr_oxycon_weir_day2 - rmr_oxycon_weir_day1) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(rmr_oxycon_weir_day1, rmr_oxycon_weir_day2)), # mean
            te = s / sqrt(2), # the typical error.
            cv_te = 100 * (te / m), # Coefficient of variation, of typical error
            cv_s = 100 * (s / m), # Coefficient of variation, of sd
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  

ICC(ba4, missing = TRUE, alpha = .05, lmer = TRUE, check.keys = FALSE)

rmrox.ttest <- t.test(ba4$rmr_oxycon_weir_day1, ba4$rmr_oxycon_weir_day2)

rmrox.ttest$estimate

rmrox.ttest$conf.int

rmrox.ttest$p.value

shapiro.test(x = ba4$rmr_oxycon_weir_day1)
shapiro.test(x = ba4$rmr_oxycon_weir_day2)

hist(ba4$rmr_oxycon_weir_day1)
qqnorm(ba4$rmr_oxycon_weir_day1)

hist(ba4$rmr_oxycon_weir_day2)
qqnorm(ba4$rmr_oxycon_weir_day2)

wilcox.test(x = ba4$rmr_oxycon_weir_day2,
            y = ba4$rmr_oxycon_weir_day1,
            paired = TRUE)

### Figur 5 C ########################

ba3 <- rmr %>%
  select(vo2_vo2master_day1, vo2_vo2master_day2)%>%
  print()

ba3 %>%
  mutate(diff = vo2_vo2master_day2 - vo2_vo2master_day1) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(vo2_vo2master_day1, vo2_vo2master_day2)), # mean
            te = s / sqrt(2), # the typical error.
            cv_te = 100 * (te / m), # Coefficient of variation, of typical error
            cv_s = 100 * (s / m), # Coefficient of variation, of sd
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  

ICC(ba3, missing = TRUE, alpha = .05, lmer = TRUE, check.keys = FALSE)


vm.ttest <- t.test(ba3$vo2_vo2master_day1, ba3$vo2_vo2master_day2, paired = TRUE)

vm.ttest$estimate

vm.ttest$conf.int

vm.ttest$p.value

shapiro.test(x = ba3$vo2_vo2master_day1)
shapiro.test(x = ba3$vo2_vo2master_day2)

hist(x = ba3$vo2_vo2master_day1)
qqnorm(y = ba3$vo2_vo2master_day1)

hist(x = ba3$vo2_vo2master_day2)
qqnorm(y = ba3$vo2_vo2master_day2)

wilcox.test(x = ba3$vo2_vo2master_day2,
            y = ba3$vo2_vo2master_day1,
            paired = TRUE)


### Figur 5 D ########################

ba5 <- rmr %>%
  select(vo2_oxycon_day1, vo2_oxycon_day2)%>%
  print()

ba5 %>%
  mutate(diff = vo2_oxycon_day2 - vo2_oxycon_day1) %>% # Change/difference score
  summarise(s = sd(diff),  # Summarize to calculate sd, and... 
            m = mean(c(vo2_oxycon_day1, vo2_oxycon_day2)), # mean
            te = s / sqrt(2), # the typical error.
            cv_te = 100 * (te / m), # Coefficient of variation, of typical error
            cv_s = 100 * (s / m), # Coefficient of variation, of sd 
            L = qt(0.975, 4) * s, # Calculate as a percentage of the mean
            bias = mean(diff),
            upper_loa = bias + 1.96 * s,
            lower_loa = bias - 1.96 * s)  

ICC(ba5, missing = TRUE, alpha = .05, lmer = TRUE, check.keys = FALSE)

p.ttest <- t.test(ba5$vo2_oxycon_day1, ba5$vo2_oxycon_day2, paired = TRUE)

p.ttest$estimate

p.ttest$conf.int

p.ttest$p.value

shapiro.test(x = ba5$vo2_oxycon_day1)

shapiro.test(x = ba5$vo2_oxycon_day2)

hist(ba5$vo2_oxycon_day1)
qqnorm(ba5$vo2_oxycon_day1)

hist(ba5$vo2_oxycon_day2)
qqnorm(ba5$vo2_oxycon_day2)

wilcox.test(x = ba5$vo2_oxycon_day2,
            y = ba5$vo2_oxycon_day1, 
            paired = TRUE)

