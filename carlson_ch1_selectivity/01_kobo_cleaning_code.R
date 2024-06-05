library(janitor)
library(stringr)
library(rethinking)
library(cmdstanr)
# install.packages("brms")
#fcns
numbers_only <- function(x) !grepl("\\D", x)

####load new data from kobo
dk_raw <- read.csv("~/Downloads/Capuchin_Tool_Surveying_-_all_versions_-_False_-_2024-02-19-19-33-46.csv", sep=";", header=T)

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


dk <- dk[which(dk$weight_g!=0 & dk$length_mm_max!=0 &  dk$width_mm_max!=0 & dk$thickness!=0),]
dk <- dk[!(dk$weight_g == 4.8), ]  #remove row 670 (collection error, lists weight as 4.8g)

#by trip 
unique(dk$today) #(jul21,jan22, jul22, jan23, mar23, jul23, sep23, jan24)

criteria <- str_detect(dk$today, "2021-07")  # Extract matching rows with str_detect
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 1 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2022-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 2 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2022-07") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 3 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 4 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-03") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 5 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-07") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 6 , dk$trip[i])
}

criteria <- str_detect(dk$today, "2023-09")  #mislabeled as september
for( i in 1:nrow(dk)){
        dk$today [i] <- ifelse(criteria[i]==TRUE , "2023-03" , dk$today[i])
}
criteria <- str_detect(dk$today, "2023-03") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 7 , dk$trip[i])
}


criteria <- str_detect(dk$today, "2024-01") 
for( i in 1:nrow(dk)){
        dk$trip [i] <- ifelse(criteria[i]==TRUE , 8 , dk$trip[i])
}

dk$trip_id <- as.integer(dk$trip)

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




#create separate data frames for each island
dk$island <- ifelse( dk$today %in% coiba_days , "coiba" , "jicaron")
dk$island_index <- as.integer(as.factor(dk$island ))
dk_c <- dk[dk$island_index==1,]
dk_j <- dk[dk$island_index==2,]

#add astrocaryum category for coiba
dk_c$astro <- 0
for (i in 1 : nrow(dk_c)){
        dk_c$astro[i] <- ifelse(dk_c$comments[i] %in% astro , 1 , 0)
}

dk_c$astro2 <- NA

for (i in 1 : nrow(dk_c)){
        dk_c$astro2[i] <- ifelse("astrocaryum" %in% dk_c$debris_at_site[i] , 1 , 0)
}

dk_c$astrocaryum <- dk_c$astro + dk_c$astro2


###add freshwater snail category for coiba

#snail based on comments
dk_c$shell <- NA

for (i in 1 : nrow(dk_c)){
        dk_c$shell[i] <- ifelse("hermit_crabs" %in% dk_c$debris_at_site[i] , 1 , 0)
}       

#fw snail officially logged
dk_c$river_snail <- NA

for (i in 1 : nrow(dk_c)){
        dk_c$river_snail[i] <- ifelse("river_snail" %in% dk_c$debris_at_site[i] , 1 , 0)
}       
 
#combine   
dk_c$fw_snail <- dk_c$shell + dk_c$river_snail



dk_jt <- dk_j[dk_j$used_tool==1,] #jicaron tools
dk_jrm <- dk_j[dk_j$used_tool==0,] #jicaron raw mat

dk_ct <- dk_c[dk_c$used_tool==1,] #coiba tools
dk_crm <- dk_c[dk_c$used_tool==0,] #coiba raw mat


### create simplified datalist to read in models

data_list <- list(
        weight = dk$weight_g,
        thickness = dk$thickness,
        length = dk$length_mm_max,
        width = dk$width_mm_max,
)


#Jicaron datalist
data_list_j <- list(
        weight = dk_jt$weight_g ,
        thickness = dk_jt$thickness ,
        length = dk_jt$length ,
        width = dk_jt$width ,
        almendra = dk_jt$debris_at_site_almendra ,
        nerite = dk_jt$debris_at_site_marine_snail ,
        herm_crab = dk_jt$debris_at_site_hermit_crabs ,
        halloween = dk_jt$debris_at_site_halloween_crabs ,
        river_snail = dk_jt$debris_at_site_river_snail ,
        bactris = dk_jt$debris_at_site_bactris_fruit ,
        trip = dk_jt$trip_id
)

#Coiba datalist
data_list_c <- list(
        weight = dk_ct$weight_g ,
        thickness = dk_ct$thickness ,
        length = dk_ct$length ,
        width = dk_ct$width ,
        almendra = dk_ct$debris_at_site_almendra ,
        nerite = dk_ct$debris_at_site_marine_snail ,
        halloween = dk_ct$debris_at_site_halloween_crabs ,
        river_snail = dk_ct$fw_snail ,
        bactris = dk_ct$debris_at_site_bactris_fruit , 
        astro = dk_ct$astrocaryum,
        trip = as.integer(as.factor(dk_ct$trip_id))
)

#Selectivity data list (raw material and tool)

data_j_select <- list(
        rm_wt = dk_jrm$weight_g  ,
        rm_th = dk_jrm$thickness ,
        rm_l = dk_jrm$length_mm_max,
        rm_wd = dk_jrm$width_mm_max ,
        t_wt = dk_jt$weight_g ,
        t_th = dk_jt$thickness,
        t_l = dk_jt$length_mm_max,
        t_wd = dk_jt$width_mm_max,
        alm_wt = dk_jt$weight_g[dk_jt$debris_at_site_almendra==1],
        sh_wt = dk_jt$weight_g[dk_jt$debris_at_site_hermit_crabs==1]
)

data_c_select <- list (
        rm_wt = dk_crm$weight_g,
        rm_th = dk_crm$thickness,
        rm_l = dk_crm$length_mm_max,
        rm_wd = dk_crm$width_mm_max ,
        t_wt = dk_ct$weight_g,
        t_th = dk_ct$thickness,
        t_l = dk_ct$length_mm_max,
        t_wd = dk_ct$width_mm_max,
        as_wt = dk_ct$weight_g[dk_ct$astro==1],
        sh_wt = dk_ct$weight_g[dk_ct$shell==1]
)

data_c <- list (
        rm_wt = dk_crm$weight_g,
        rm_th = dk_crm$thickness,
        rm_l = dk_crm$length_mm_max,
        rm_wd = dk_crm$width_mm_max,
        as_wt = dk_ct$weight_g[dk_ct$astro==1],
        as_th = dk_ct$thickness[dk_ct$astro==1],
        as_l = dk_ct$length_mm_max[dk_ct$astro==1],
        as_wd = dk_ct$width_mm_max[dk_ct$astro==1],
        fs_wt = dk_ct$weight_g[dk_ct$fw_snail==1],
        fs_th = dk_ct$thickness[dk_ct$fw_snail==1],
        fs_l = dk_ct$length_mm_max[dk_ct$fw_snail==1],
        fs_wd = dk_ct$width_mm_max[dk_ct$fw_snail==1]
)
