
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


## Get Bland-Altman statistics from the 
stats <- bland.altman.stats(ba$mean_measures_vo2master, ba$mean_oxycon_measures_weir, two = 1.96, mode = 1, conf.int = 0.95)
fig_a <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = mean_measures_vo2master - mean_oxycon_measures_weir, 
         mean = mean(c(mean_measures_vo2master, mean_oxycon_measures_weir)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = stats$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = stats$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (kcal·dag<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (kcal·dag<sup>-1</sup>)",) +
  ylim(-600, 600) +
  xlim(900, 2100) +
  
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown()) 

### Figur 4 B ########################

ba1 <- rmr %>%
  select(lowest_measure_vo2master_rq85, lowest_measure_oxycon_weir)%>%
  print()


stats1 <- bland.altman.stats(ba1$lowest_measure_vo2master_rq85, ba1$lowest_measure_oxycon_weir, two = 1.96, mode = 1, conf.int = 0.95)
fig_b <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = lowest_measure_vo2master_rq85 - lowest_measure_oxycon_weir, 
         mean = mean(c(lowest_measure_vo2master_rq85, lowest_measure_oxycon_weir)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = stats1$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = stats1$lines[c(2)], lty = 1) +
  
  # Fix labels
  labs(x = "Snitt av målingene (kcal·dag<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (kcal·dag<sup>-1</sup>)") +
  ylim(-600, 600) +
  xlim(900, 2100) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown()) 


### Figur 4 C ########################

bamvo2 <- rmr %>%
  select(mean_vo2_measure_vo2master, mean_vo2_measure_oxycon)%>%
  print()



statsmvo2 <- bland.altman.stats(bamvo2$mean_vo2_measure_vo2master, bamvo2$mean_vo2_measure_oxycon, two = 1.96, mode = 1, conf.int = 0.95)
fig_c <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = mean_vo2_measure_vo2master - mean_vo2_measure_oxycon, 
         mean = mean(c(mean_vo2_measure_vo2master, mean_vo2_measure_oxycon)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = statsmvo2$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = statsmvo2$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (ml·min<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (ml·min<sup>-1</sup>)") +
  ylim(-75, 75) +
  xlim(140, 300) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown()) 


### Figur 4 D ########################

balvo2 <- rmr %>%
  select(lowest_vo2_measure_vo2master, lowest_vo2_measure_oxycon)%>%
  print()


statslvo2 <- bland.altman.stats(balvo2$lowest_vo2_measure_vo2master, balvo2$lowest_vo2_measure_oxycon, two = 1.96, mode = 1, conf.int = 0.95)
fig_d <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = lowest_vo2_measure_vo2master - lowest_vo2_measure_oxycon, 
         mean = mean(c(lowest_vo2_measure_vo2master, lowest_vo2_measure_oxycon)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = statslvo2$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = statslvo2$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (ml·min<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (ml·min<sup>-1</sup>)",) +
  ylim(-75, 75) +
  xlim(140, 300) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown())





### Figur 5 A ########################

bap <- rmr %>%
  select(vo2master_rq85_day1, vo2master_rq85_day2)%>%
  print()


statsp <- bland.altman.stats(bap$vo2master_rq85_day2, bap$vo2master_rq85_day1, two = 1.96, mode = 1, conf.int = 0.95)
fig_a2 <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = vo2master_rq85_day2 - vo2master_rq85_day1, 
         mean = mean(c(vo2master_rq85_day2, vo2master_rq85_day1)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = statsp$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = statsp$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (kcal·dag<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (kcal·dag<sup>-1</sup>)",
       title = "VO2 Master <sub>(Dag 2 - Dag 1) </sub>") +
  ylim(-600, 600) +
  xlim(900, 2100) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown())


### Figur 5 B ########################

ba4 <- rmr %>%
  select(rmr_oxycon_weir_day1, rmr_oxycon_weir_day2)%>%
  print()


stats4 <- bland.altman.stats(ba4$rmr_oxycon_weir_day2, ba4$rmr_oxycon_weir_day1, two = 1.96, mode = 1, conf.int = 0.95)
fig_b2 <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = rmr_oxycon_weir_day2 - rmr_oxycon_weir_day1, 
         mean = mean(c(rmr_oxycon_weir_day2, rmr_oxycon_weir_day1)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = stats4$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = stats4$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (kcal·dag<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (kcal·dag<sup>-1</sup>)",
       title = "Oxycon Pro <sub>(Dag 2 - Dag 1) </sub>") +
  ylim(-600, 600) +
  xlim(900, 2100) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown())


### Figur 5 C ########################

ba3 <- rmr %>%
  select(vo2_vo2master_day1, vo2_vo2master_day2)%>%
  print()



stats3 <- bland.altman.stats(ba3$vo2_vo2master_day2, ba3$vo2_vo2master_day1, two = 1.96, mode = 1, conf.int = 0.95)
fig_c2 <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = vo2_vo2master_day2 - vo2_vo2master_day1, 
         mean = mean(c(vo2_vo2master_day2, vo2_vo2master_day1)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = stats3$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = stats3$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (ml·min<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (ml·min<sup>-1</sup>)",
       title = "VO2 Master <sub>(Dag 2 - Dag 1) </sub>") +
  ylim(-75, 75) +
  xlim(140, 300) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown())

### Figur 5 D ########################

ba5 <- rmr %>%
  select(vo2_oxycon_day1, vo2_oxycon_day2)%>%
  print()


stats5 <- bland.altman.stats(ba5$vo2_oxycon_day2, ba5$vo2_oxycon_day1, two = 1.96, mode = 1, conf.int = 0.95)
fig_d2 <- rmr %>%
  # Calculate differences and mean
  rowwise() %>%
  mutate(diff = vo2_oxycon_day2 - vo2_oxycon_day1, 
         mean = mean(c(vo2_oxycon_day2, vo2_oxycon_day1)), 
         # Change sex labels
         sex = factor(sex, levels = c("f", "m"), labels = c("Kvinne", "Mann"))) %>%
  
  # Create plot, mean vs diff with sex as fill in points
  ggplot(aes(mean, diff, fill = sex, shape = sex)) + 
  # Add points, must change shape to 21 for filled circles
  geom_point(size = 2) +
  # adjust the shape of the points between sex
  scale_fill_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(21, 21)) +
  # Add geom hline from Bland-Altman stats
  geom_hline(yintercept = stats5$lines[c(1,3)], lty = 2) +
  geom_hline(yintercept = stats5$lines[c(2)], lty = 1) +
  # Fix labels
  labs(x = "Snitt av målingene (ml·min<sup>-1</sup>)", 
       y = "Forskjell mellom målingene (ml·min<sup>-1</sup>)",
       title = "Oxycon Pro <sub>(Dag 2 - Dag 1) </sub>") +
  ylim(-75, 75) +
  xlim(140, 300) +
  
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 0, lineheight = 0),
        legend.justification = "left",
        legend.position = c(0.01, 1.0),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        #legend.background = element_blank(),
        #legend.key.width = unit(3, "pt"),
        legend.text = element_markdown(margin = margin(r=10))) +
  
  theme(axis.title.y = element_markdown(size = 8), 
        axis.title.x = element_markdown(size = 8),
        plot.title = element_markdown())



sex_legend <- get_legend(fig_d2)









### Figur 4 ########################

fig4 <- plot_grid(
  ## Første rad
  
  plot_grid(fig_a+ theme(legend.position = "none", 
                         plot.title = element_blank()), 
            NULL, 
            fig_b+ theme(legend.position = "none", 
                         plot.title = element_blank()), 
            rel_widths = c(1, 0.1, 1), nrow = 1), 
  ## Andre
  NULL,
  
  plot_grid(fig_c + theme(legend.position = "none", 
                          plot.title = element_blank()), 
            NULL, 
            fig_d+ theme(legend.position = "none", 
                         plot.title = element_blank()) ,  
            rel_widths = c(1, 0.1, 1), nrow = 1), 
  
  
  
  
  plot_grid(NULL, sex_legend, nrow = 2, rel_heights = c(1, 1)),
  
  rel_heights = c(1, 0.1, 1, 0.1, 1, 0.2),
  nrow = 4, 
  align = "vh") +
  
  draw_plot_label(label = c("a", "b", 
                            "c", "d"), 
                  x = c(0.01, 0.530, 
                        0.01, 0.530), 
                  y = c(1.01, 1.01, 
                        0.51, 0.51))


ggsave("figurer/fig4.pdf", 
       plot = fig4, 
       height =15, width =15, units = "cm")



### Figur 5 ########################


fig5 <- plot_grid(
  ## Første rad
  plot_grid(fig_a2+ theme(legend.position = "none",
                          plot.title = element_blank()), 
            NULL, 
            fig_b2+ theme(legend.position = "none",
                          plot.title = element_blank()), 
            rel_widths = c(1, 0.1, 1), nrow = 1), 
  ## Andre
  NULL,
  
  plot_grid(fig_c2 + theme(legend.position = "none", 
                           plot.title = element_blank()), 
            NULL, 
            fig_d2+ theme(legend.position = "none", 
                          plot.title = element_blank()) ,  
            rel_widths = c(1, 0.1, 1), nrow = 1), 
  
  
  
  
  plot_grid(NULL, sex_legend, nrow = 2, rel_heights = c(1, 1)),
  
  rel_heights = c(1, 0.1, 1, 0.1, 1, 0.2),
  nrow = 4, 
  align = "vh") +
  
  draw_plot_label(label = c("a", "b", 
                            "c", "d"), 
                  x = c(0.01, 0.530, 
                        0.01, 0.530), 
                  y = c(1.01, 1.01, 
                        0.51, 0.51))

ggsave("figurer/fig5.pdf", 
       plot = fig5, 
       height =15, width =15, units = "cm")
