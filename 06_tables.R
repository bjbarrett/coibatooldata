install.packages("gt")
install.packages("gtsummary")
install.packages("tidyverse")
library(gt)
library(gtsummary)
library(tidyverse)

# ##Coiba and Jicaron combined
# dk_tab <- dk %>% select(weight_g, length_mm_max, width_mm_max,thickness, used_tool)
# 
# dk_tab %>% tbl_summary(by = used_tool,
#                        label = list(weight_g ~ "Weight (g)",
#                                     length_mm_max ~ "Length (mm)",
#                                     width_mm_max ~ "Width (mm)", 
#                                     thickness ~ "Thickness (mm)"), 
#                        statistic = list(
#                             all_continuous() ~ "{mean} ({sd})"),
#                        digits = starts_with("weight") ~ 1,
#                        ) %>%
#      modify_header(label ~ "**Metric**")  %>%
#      modify_spanning_header(c("stat_1", "stat_2") ~ "**Used Tool**")  %>%
#      modify_caption("Table X. Tool and raw material metrics (all islands)")

#Jicaron only
jic_tab <- dk_j %>% select(weight_g, length_mm_max, width_mm_max,thickness, used_tool)

squirrel <- jic_tab %>% tbl_summary(by = used_tool,
                          label = list(weight_g ~ "Weight (g)",
                                       length_mm_max ~ "Length (mm)",
                                       width_mm_max ~ "Width (mm)", 
                                       thickness ~ "Thickness (mm)"), 
                          statistic = list(
                         all_continuous() ~ "{mean} ({min}; {max})")
) %>%
     modify_header(label ~ "**Characteristic**")  %>%
     modify_spanning_header(c("stat_1", "stat_2") ~ "**Used Tool**")  %>%
     modify_caption("Table 1. Jicarón tool and raw material characteristics")
  
as_gt(squirrel) %>% gt::as_latex()

#Coiba only
coiba_tab <- dk_c %>% select(weight_g, length_mm_max, width_mm_max,thickness, used_tool)

coiba_tab %>% tbl_summary(by = used_tool,
                       label = list(weight_g ~ "Weight (g)",
                                    length_mm_max ~ "Length (mm)",
                                    width_mm_max ~ "Width (mm)", 
                                    thickness ~ "Thickness (mm)"), 
                       statistic = list(
                            all_continuous() ~ "{mean} ({min}; {max})")
) %>%
     modify_header(label ~ "**Characteristic**")  %>%
     modify_spanning_header(c("stat_1", "stat_2") ~ "**Used Tool**")  %>%
     modify_caption("Table 2. Coiba tool and raw material characteristics")


###Table using xtable
library(xtable)
data(jic_tab)
xtable(summary(jic_tab))


