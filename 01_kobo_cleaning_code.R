library(janitor)
library(stringr)
library(rethinking)
library(cmdstanr)
# install.packages("brms")
#fcns
numbers_only <- function(x) !grepl("\\D", x)

####load new data from kobo
dk_raw <- read.csv("~/Documents/coibatooldata/data/tool_data/coiba_capuchin_tool_survey_kobo/Capuchin_Tool_Surveying_-_all_versions_-_False_-_2023-09-21-13-51-36.csv", sep=";", header=T)
# dk$hardness <- rowMeans(dk[, c("hardness_1_hld", "hardness_2_hld", "hardness_3_hld", 
#                                "hardness_4_hld", "hardness_5_hld", "hardness_6_hld", "hardness_7_hld", 
#                                "hardness_8_hld", "hardness_9_hld") ] , na.rm = TRUE)
dk <-dk_raw

####coiba subset
# coiba is all with "location" containing "7.6"
# jicaron is all with "location" containing "7.2"
# some have no location data but can still be traced to island -- probably by date? We know what dates we were on which island

sort(dk$location)
lat <- as.numeric(substr(dk$location, 1, 6))
dk$island <- ifelse(lat > 7.3 , "coiba" , "jicaron")
which(dk$island=="coiba")
coiba_days <- sort(unique(dk$today[which(dk$island=="coiba")]))
jicaron_days <- sort(unique(dk$today[which(dk$island=="jicaron")]))


dk$island <- ifelse( dk$today %in% coiba_days , "coiba" , "jicaron")
dk$island_index <- as.integer(as.factor(dk$island ))

xx <- as.vector(sapply(dk_raw, function(x)all(any(is.na(x))))) #makes a vector of T/F if all NA
dk <- dk_raw[,xx==FALSE] #creat dk which removes all columns with all NAs
colnames(dk)
dk <- clean_names(dk)

dk <- dk[numbers_only(dk$site_id)==FALSE,]
dk$site_id

##to do, check all names in kobo
#dk <- dk[-(which(dk$site_id=="Cliff1"):which(dk$site_id=="Wtu3")),] #remove claudio to inspect

criteria <- str_detect(dk$site_id, "T")  # Extract matching rows with str_detect
dk$used_tool <- ifelse(criteria==TRUE , 1 , 0)

criteria <- str_detect(dk$site_id, "Clff")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
     dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}
sum(dk$used_tool)

criteria <- str_detect(dk$site_id, "Fridge")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
     dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "Wtu")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
     dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "ex")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
     dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

criteria <- str_detect(dk$site_id, "Cebus")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
     dk$used_tool[i] <- ifelse(criteria[i]==TRUE , 1 , dk$used_tool[i])
}

dk$weight_g
dk <- dk[which(dk$weight_g!=0 & dk$length_mm_max!=0 &  dk$width_mm_max!=0 & dk$thickness!=0),]

dk$used_tool

dk_tools <- dk[dk$used_tool==1,]

#need to remove from this: "cliff", "not a site", "D01", "D02"
dk_rm <- dk[dk$used_tool==0,]

##simple
dens(dk_tools$weight_g)
dens(exp(rnorm(100, 5.5,1)))     
dens(exp(rnorm(10000, 1,2)))  

##to do astrocaryum add after other data
sort(unique(dk_tools$comments))

astro  <- c("Astrocaryum spp, rio esc" , "Astrocaryum spp, rio esc" , "Astrocaryum spp., weathered, Rio Esc" , 
            "Astrocaryum, unknown round thin-shell fruit" , "Asttocaryum," , "Debris: unknown nut" ,
            "Debris: unknown nut, same as others found today" , "Debris: unknown nut. Adding camera here now." , 
            "Debris: unknown nut. See pictures." , "In forest, across from mudslide. Unknown fruit/nut. Collected for ID" ,
            "Other debris: astrocaryum" , "Palm of astrocaryum.Taken back for Meredith to measure. Pedro has it. I have debris in left pocket." , 
            "Rio escondido, fruit/nut unknown" , "Unidentified nut" , "Unknown nut: round with thin shell")
dk_tools$astro <- dk$astro <- 0
for (i in 1 : nrow(dk_tools)){
     dk_tools$astro[i] <- ifelse(dk_tools$comments[i] %in% astro , 1 , 0)
}
for (i in 1 : nrow(dk)){
     dk$astro[i] <- ifelse(dk$comments[i] %in% astro , 1 , 0)
}
### create simplified datalist to read in models
data_list <- list(
     weight = dk_tools$weight_g ,
     thickness = dk_tools$thickness ,
     length = dk_tools$length ,
     width = dk_tools$width ,
     almendra = dk_tools$debris_at_site_almendra ,
     nerite = dk_tools$debris_at_site_marine_snail ,
     herm_crab = dk_tools$debris_at_site_hermit_crabs ,
     halloween = dk_tools$debris_at_site_halloween_crabs ,
     river_snail = dk_tools$debris_at_site_river_snail ,
     bactris = dk_tools$debris_at_site_bactris_fruit , 
     astro = dk_tools$astro
     )

dk$weight_g[dk$used_tool==0 | dk$debris_at_site_almendra==1] 
dk$weight_g

data_list_raw_tc <- list(
     weight = dk$weight_g[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     thickness = dk$thickness[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     length = dk$length[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     width = dk$width[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     almendra = dk$debris_at_site_almendra[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     nerite = dk$debris_at_site_marine_snail[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     herm_crab = dk$debris_at_site_hermit_crabs[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     halloween = dk$debris_at_site_halloween_crabs[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     river_snail = dk$debris_at_site_river_snail[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     bactris = dk$debris_at_site_bactris_fruit[dk$used_tool==0 | dk$debris_at_site_almendra==1]  , 
     astro = dk$astro[dk$used_tool==0 | dk$debris_at_site_almendra==1] ,
     used_tool = dk$used_tool[dk$used_tool==0 | dk$debris_at_site_almendra==1] 
)

str(data_list_raw_tc)

dl_raw_tc_comp <- list(
     weight_raw = dk$weight_g[dk$used_tool==0 ]  ,
     thickness_raw = dk$thickness[dk$used_tool==0 ]  ,
     length_raw = dk$length[dk$used_tool==0 ]  ,
     width_raw = dk$width[dk$used_tool==0 ]  ,
     weight_tc = dk$weight_g[dk$used_tool==1 | dk$debris_at_site_almendra==1]  ,
     thickness_tc = dk$thickness[dk$used_tool==1 | dk$debris_at_site_almendra==1]  ,
     length_tc = dk$length[dk$used_tool==1 | dk$debris_at_site_almendra==1]  ,
     width_tc = dk$width[dk$used_tool==1 | dk$debris_at_site_almendra==1]  
     # almendra = dk$debris_at_site_almendra[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     # nerite = dk$debris_at_site_marine_snail[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     # herm_crab = dk$debris_at_site_hermit_crabs[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     # halloween = dk$debris_at_site_halloween_crabs[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     # river_snail = dk$debris_at_site_river_snail[dk$used_tool==0 | dk$debris_at_site_almendra==1]  ,
     # bactris = dk$debris_at_site_bactris_fruit[dk$used_tool==0 | dk$debris_at_site_almendra==1]  , 
     # astro = dk$astro[dk$used_tool==0 | dk$debris_at_site_almendra==1] ,
     #used_tool = dk$used_tool[dk$used_tool==0 | dk$debris_at_site_almendra==1] 
)