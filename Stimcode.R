setwd("G:/My Drive/AU sager/Artikler og review/Elektrisk stimulation og træning/R")

# To dele af det her script - Første del er træningsdata og styrke
# Anden del er ultralydsscanninger og udvikling
# Two parts of this script - Part one on training data and strength, part two 
# on ultrasounds and evolution

# Libraries ---------------------------------------------------------------

library(pacman)
library(dplyr)
library(tidyverse)
library(purrrlyr)
library(rstatix)
library(ggpubr)
library(readxl)
library(forcats)
library(patchwork)
library(esquisse)
library(ggplot2)

# Første del - Part one----------------------------------------------------

strong <- readxl::read_xlsx("Dynamometer målinger (2).xlsx")

strong <- strong %>%
      mutate(testdays = case_when(testday == 1 ~ "Familiarization 1",
                                  testday == 2 ~"Familiarization 2",
                                  testday == 3 ~"Pre-test",
                                  testday == 4 ~"Post-test")) %>%
      rename(fmax_max = `fmax/max`) %>%
      group_by(fp, testday)

# Laver oversigt over MVIC og MVC over tid. Bruger subset med !is.na for at 
# undgå at skulle lave flere datasæt. Derudover fct_inorder for at undgå
# alfabetisme over tidspunkter. 
# Creating an overview of MVIC and MVC over time. Using subset !is.na to avoid
# having to create multiple datasets. Furthermore, fct_inorder to avoid alphabetic
# ordering of timepoints

graph_theme <- theme(
      axis.title.x = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 18),
      title = element_text(size = 16)
)

MVC <- ggplot(data = subset(strong, !is.na(testday)), aes(fct_inorder(testdays), mvc, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) + 
      labs(x = "", y = "MVC (Nm)", title = "Maximal Voluntary Isometric Contraction") +
      expand_limits(y=0) +
      theme_pubr(legend = "none") +
      graph_theme
MVC
ggsave("MVC.jpg", scale = 1.5, dpi = 600)
ggsave("Fig_3.jpg", scale = 1.5, dpi = 600)
MVC_norm <- ggplot(data = subset(strong, !is.na(testday)), aes(fct_inorder(testdays), mvc_norm, col(fp))) +
      geom_point(aes(colour = fp),size = 3) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      labs(x = "", y = "MVC / Kg", title = "Maximal Voluntary isometric contraction") +
      expand_limits(y=0) +
      theme_pubr(legend = "none")+
      graph_theme
MVC_norm

Dynamic <- ggplot(data = subset(strong, !is.na(dynmvc), testdays <="3"), 
                  aes(fct_inorder(testdays),dynmvc, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      labs(x = "", y = "Maximaxl torque (Nm)", 
           title = "Maximal dynamic voluntary torque measured during eccentric phase of fatigue protocol") + 
      expand_limits(y=0) +
      theme_pubr(legend = "none") +
      graph_theme
Dynamic
ggsave("Dynamic.jpg", scale = 1.5, dpi = 600)
ggsave("Fig_4.jpg", scale = 1.5, dpi = 600)

Dynamic_norm <- ggplot(data = subset(strong, !is.na(testday)), aes(fct_inorder(testdays), mvc, col(fp))) +
      geom_point(aes(colour = fp),size = 3) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      labs(x = "Time", y = "Dynamic / Kg", title = "Maximal dynamic voluntary torque measured during fatigue protocol") +
      expand_limits(y=0) +
      theme_pubr(legend = "none")+
      graph_theme
Dynamic_norm

strong <- strong %>%
      group_by(day)

Norm <- ggplot(data = subset(strong, !is.na(day)),
            aes(x = day)) +
      geom_line(aes(y = avgnormmax)) +
      labs(x = "Training", y = "Maximal torque as % of MVIC",
           title = "Average normalized maximal power per training") +
      scale_x_continuous(breaks = round(seq(min(1),max(12), by = 1),1 )) +
      expand_limits(y=0) +
      theme_pubr()+
      graph_theme
Norm
ggsave("Normalized.tiff", scale = 1.5)

Current <- ggplot(data = subset(strong, !is.na(day)),
                  aes(x = day)) +
      geom_line(aes(y = avgcurrent)) +
      labs(x = "Training", y = "Maximal current",
           title = "Average current per training") +
      scale_x_continuous(breaks = round(seq(min(1),max(12), by = 1),1 )) +
      expand_limits(y=0) + 
      theme_pubr()+
      graph_theme
Current

multiplier <- 2

training <- ggplot(data = subset(strong, !is.na(day)),
                   aes(x = day)) +
      geom_line(aes(y = avgcurrent / multiplier), color = "red") +
      geom_hline(linetype = 3 ,yintercept = 100) +
      geom_line(aes(y = avgnormmax), color = "blue") +
      geom_errorbar(aes(ymin = avgnormmax-sdnormmax, 
                          ymax = avgnormmax+sdnormmax,
                        width = 0.15), color = "blue") + 
      geom_errorbar(aes(ymin = (avgcurrent / multiplier)-sdcurrent,
                          ymax = (avgcurrent / multiplier)+sdcurrent,
                        width = 0.15), color = "red", position = position_nudge(x = .02)) +
      labs(x = "Training session",
           title = "Average normalized maximal torque and absolute current per training") +
      scale_x_continuous(breaks = round(seq(min(1),max(12), by = 1),1 )) +
      scale_y_continuous(name = "Maximal torque as % of MVIC", 
                         sec.axis = sec_axis( trans=~.*2, name="Maximal current [mA]")) +
      theme_pubr() + 
      theme(axis.line.y.right = element_line(color = "red"), 
                  axis.ticks.y.right = element_line(color = "red"),
            axis.line.y = element_line("blue"),
                  axis.ticks.y = element_line("blue")) +
      expand_limits(y=0)+
      graph_theme
training
ggsave("Training intensity.jpg", scale = 1.5, dpi = 600)
ggsave("Fig_5.jpg", scale = 1.5, dpi = 600)

# Statistik - Parrede T-test Præ->Post - Paired t-tests pre->post ----------

# Bruger den samme orden som før, og laver parrede t-test på hver værdi
# Same order as before, paired t-tests on all values

MVC_ttest <- t.test(data = subset(strong, testday == "3" | testday == "4"), 
                             mvc~testday, paired = TRUE)
MVC_mean <- group_by(strong,testdays) %>%
                           summarise(
                                 count = n(),
                                 mean = mean(mvc, na.rm = TRUE),
                                 sd = sd(mvc, na.rm = TRUE)
                           )
MVC_mean
MVC_ttest
# Ingen forskel mellem grupper - No group differences
Dynamic_ttest <- t.test(data = subset(strong, testday == "3" | testday == "4"), 
                        dynmvc~testday, paired = TRUE)
Dynamic_mean <- group_by(strong,testdays) %>%
      summarise(
            count = n(),
            mean = mean(dynmvc, na.rm = TRUE),
            sd = sd(dynmvc, na.rm = TRUE)
      )
Dynamic_mean
Dynamic_ttest
# Ingen forskel mellem grupper - No group differences
Norm_ttest <- t.test(data = subset(strong, training == "1" | training == "12"), 
                     fmax_max~training, paired = TRUE)
Norm_mean <- group_by(strong,training) %>%
      summarise(
            count = n(),
            mean = mean(fmax_max*100, na.rm = TRUE),
            sd = sd(fmax_max*100, na.rm = TRUE)
      )
Norm_mean
Norm_ttest
# Signifikant stigning i kraft fra start til slut. - Significant increase pre to
# post

# Prøv med esquisser()
# esquisser()


# Ultrasound --------------------------------------------------------------

ultra <- readxl::read_xlsx("Scanninger hovedforsøg.xlsx", sheet = 2)

ultra <- ultra %>%
      mutate(times = case_when(time == 1 ~"Pre-test",
                              time == 2 ~"Post-test")) %>%
      mutate(muscle = case_when(area == 1 ~ "Proximal",
                                area == 2 ~"Medial",
                                area == 3 ~"Distal")) %>%
      group_by(fp, times)


evolution_m <- ggplot(data = subset(ultra, !is.na(time)),
                    aes(fct_inorder(as.factor(times)), wholemuscle, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Average muscle thickness (mm)",
           title = "Muscle thickness", subtitle = "A") +
      expand_limits(y=0)+
      graph_theme
evolution_m
ggsave("evolution muscle.jpg", scale = 1.5, dpi = 600)

evolution_f <- ggplot(data = subset(ultra, !is.na(time)),
                      aes(fct_inorder(as.factor(times)), wholefat, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Average fat thickness (mm)",
           title = "Fat thickness", subtitle = "B") +
      expand_limits(y=0)+
      graph_theme
evolution_f
ggsave("evolution fat.jpg", scale = 1.5, dpi = 600)

ggarrange(evolution_m, evolution_f)
ggsave("evolution.jpg", scale = 1.5, dpi = 600)

localized <- ultra %>%
      group_by(fp_, muscle)
appendix_m <- ggplot(data = localized,
      aes(fct_inorder(as.factor(muscle)), value, col(fp_))) +
      geom_point(aes(color = fp_), size = 5) +
      geom_line(aes(group = fp_)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Percentage change from Pre", 
           title = "Change in muscle thickness at three scan sites")+
      graph_theme
appendix_m 
ggsave("appendix_m.jpg", scale = 1.5, dpi = 600)
ggsave("Fig_7.jpg", scale = 1.5, dpi = 600)

# Statistik (t-tests) på ultralyd - T-tests on ultrasound --------------------

# Bruger samme fremgangsmåde som før - Same approach as before

Muscle_ttest <- t.test(data = ultra, 
                    wholemuscle~time, paired = TRUE)
Muscle_mean <- group_by(ultra,times) %>%
      summarise(
            count = n(),
            mean = mean(wholemuscle, na.rm = TRUE),
            sd = sd(wholemuscle, na.rm = TRUE)
      )
Muscle_mean
Muscle_ttest

Fat_ttest<- t.test(data = ultra, 
                   wholefat~time, paired = TRUE)
Fat_mean <- group_by(ultra,times) %>%
      summarise(
            count = n(),
            mean = mean(wholefat, na.rm = TRUE),
            sd = sd(wholefat, na.rm = TRUE)
      )
Fat_mean
Fat_ttest
# Ingen forskelle i hverken muskel eller fedt på helmuskelniveau.
# No differences in either muscle or fat on whole muscle 


# Test uden "outlier" - Testing without the outlier --------------------------

ultra_o <- readxl::read_xlsx("Scanninger hovedforsøg.xlsx", sheet = 3)

ultra_o <- ultra_o %>%
      mutate(times = case_when(time == 1 ~"Pre-test",
                               time == 2 ~"Post-test")) %>%
      mutate(muscle = case_when(area == 1 ~ "Proximal",
                                area == 2 ~"Medial",
                                area == 3 ~"Distal")) %>%
      group_by(fp, times)


evolution_m_o <- ggplot(data = subset(ultra_o, !is.na(time)),
                      aes(fct_inorder(as.factor(times)), wholemuscle, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Average muscle thickness (mm)",
           title = "Muscle thickness", subtitle = "A") +
      expand_limits(y=0)+
      graph_theme
evolution_m_o
ggsave("evolution muscle_o.jpg", scale = 1.5, dpi = 600)

evolution_f_o <- ggplot(data = subset(ultra_o, !is.na(time)),
                      aes(fct_inorder(as.factor(times)), wholefat, col(fp))) +
      geom_point(aes(colour = fp),size = 5) + 
      geom_boxplot(alpha = 0.5, size = .4, width = .4, fill = "grey") +
      geom_line(aes(group = fp)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Average fat thickness (mm)",
           title = "Fat thickness", subtitle = "B") +
      expand_limits(y=0)+
      graph_theme
evolution_f_o
ggsave("evolution fat_o.jpg", scale = 1.5, dpi = 600)

ggarrange(evolution_m_o, evolution_f_o)
ggsave("evolution_o.jpg", scale = 1.5, dpi = 600)
ggsave("Fig_6.jpg", scale = 1.5, dpi = 600)

localized_o <- ultra_o %>%
      group_by(fp_, muscle)
appendix_m_o <- ggplot(data = localized_o,
                     aes(fct_inorder(as.factor(muscle)), value, col(fp_))) +
      geom_point(aes(color = fp_), size = 5) +
      geom_line(aes(group = fp_)) +
      theme_pubr(legend = "none") +
      labs(x = "", y = "Percentage change from Pre", 
           title = "Change in muscle thickness at three scan sites")+
      graph_theme
appendix_m_o 
ggsave("appendix_m_o.jpg", scale = 1.5, dpi = 600)

# Statistik (t-tests) på ultralyd - T-tests on ultrasound --------------------

# Bruger samme fremgangsmåde som før - Same approach as before

Muscle_ttest_o <- t.test(data = ultra_o, 
                       wholemuscle~time, paired = TRUE)
Muscle_mean_o <- group_by(ultra_o,times) %>%
      summarise(
            count = n(),
            mean = mean(wholemuscle, na.rm = TRUE),
            sd = sd(wholemuscle, na.rm = TRUE)
      )
Muscle_mean_o
Muscle_ttest_o

Fat_ttest_o <- t.test(data = ultra_o, 
                   wholefat~time, paired = TRUE)
Fat_mean_o <- group_by(ultra_o,times) %>%
      summarise(
            count = n(),
            mean = mean(wholefat, na.rm = TRUE),
            sd = sd(wholefat, na.rm = TRUE)
      )
Fat_mean_o
Fat_ttest_o

