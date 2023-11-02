install.packages("gt")
install.packages("gtsummary")
install.packages("tidyverse")
library(gt)
library(gtsummary)
library(tidyverse)
library(xtable)

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


#tables for selectivity
tabz <- as.data.frame(precis(m_raw_tc3, digits=2 ))
str(tabz)
tabz@row.names <- c( "$\\alpha_{wg,tc}$","$\\alpha_{wg,raw}$","$\\phi_{wg,raw}$","$\\phi_{wg,tc}$","$\\alpha_{t,tc}$","$\\alpha_{t,raw}$","$\\phi_{t,raw}$","$\\phi_{t,tc}$",
                    "$\\alpha_{l,tc}$","$\\alpha_{l,raw}$","$\\phi_{l,raw}$","$\\phi_{l,tc}$","$\\alpha_{wd,tc}$","$\\alpha_{wd,raw}$","$\\phi_{wd,raw}$","$\\phi_{wd,tc}$" )
str(tabz)
tabz <- tabz[,1:4]
tabz@names <- c("mean","sd","5.5\\%","94.5\\%")

yams <- xtable(tabz ,
               digits=2 , 
               label="tab:model_sum_raw_dims" , 
               caption="Summaries parameter posteriors distributions
               (mean, standard deviation, and 89 \\% HPDI) estimated from a multivariate 
               gamma distributed GLM comparing availible raw materials to used 
              \\textit{T. catappa} hammerstomes" )
yams

print(yams , file="jicaron_tc_raw_mat.tex" , sanitize.text.function=function(x){x} ) #sanitize text makes math mode latex friendly
