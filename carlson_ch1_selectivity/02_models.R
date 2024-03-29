#NEW MODELS JAN 2024 DATA AND BY ISLAND
#jic
#almendra vs. not almendra
mj1 <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra,
                a ~ normal(6,1.2), #prior for mean
                ba ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list_j, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#almendra and shell
mj2 <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bhc*herm_crab,
                a ~ normal(6,1.2), #prior for mean
                c(ba,bhc) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list_j, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)


#coiba
#astrocaryum
mc1 <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + bas*astro,
                a ~ normal(6,1.2), 
                bas ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list_c, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#astro and freshwater snail
mc2 <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + bas*astro + brs*river_snail,
                a ~ normal(6,1.2), #prior for mean
                c(bas,brs) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list_c, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)

#varying effects trip
mc2v <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + a_trip[trip] + bas*astro + brs*river_snail,
                a ~ normal(6,1.2), #prior for mean
                a_trip[trip] ~ normal(0, sigma),
                c(bas,brs) ~ normal(0,1),
                scale ~ dexp(0.005),
                sigma ~ dexp(1)
        ),

        data=data_list_c, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
) #not working
#
mj2v <- ulam(
        alist(
                weight ~ dgamma2(mu,scale),
                log(mu) <- a + a_trip[trip] + ba*almendra + bhc*herm_crab,
                a ~ normal(6,1.2), #prior for mean
                a_trip[trip] ~ normal(0, sigma),
                c(ba,bhc) ~ normal(0,1),
                scale ~ dexp(0.005),
                sigma ~ dexp(1)
        ),
        
        data=data_list_j, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)
##


#####RAW MATERIAL

m_select_j <-  ulam(
        alist(
                t_wt ~ dgamma2(mu_t,scale_t), #unique shape and scale for used tools
                log(mu_t) <- a_t, #the regression to which we need to make it just jicaron
                rm_wt ~ dgamma2(mu_rm,scale_rm) , #raw weight shape and scale
                log(mu_rm) <- a_rm ,

                c(a_rm,a_t) ~ normal(5,1.8), #prior for mean
                c(scale_t,scale_rm) ~ exponential(0.0005) # we need a big scale for weight
        ),

        data=data_j_select, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

precis(m_select_j)


######
##

# # intercept only model, mean of all tools
# mw0 <- ulam(
#         alist(
#                 weight ~ dgamma2(mu,scale),
#                 log(mu) <- a ,
#                 a ~ normal(6,1.2), #prior for mean
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
# )
# 
# 
# #almendra vs not almendra
# mw1 <- ulam(
#         alist(
#                 weight ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra,
#                 a ~ normal(6,1.2), #prior for mean
#                 ba ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# 
# # almendra, nerite, herm_crab, hallowwen crab, river snail ignoring co-occurnce
# mw2 <- ulam(
#         alist(
#                 weight ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
#                 a ~ normal(6,1.2), #prior for mean
#                 c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
# )
# 
# 
# #resources permitting cooccurence
# mw3 <- ulam(
#         alist(
#                 weight ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
#                 INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
#                 a ~ normal(6,1.2), #prior for mean
#                 c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# 
# #below is building up to a crazy fancy model
# mw4 <-  ulam(
#         alist(
#                 weight ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra ,
#                 
#                 thickness ~ dgamma2(mu_t,scale_t) ,
#                 log(mu_t) <- a_t + ba_t*almendra ,
#                 
#                 c(a,a_t) ~ normal(6,1.2), #prior for mean
#                 c(ba,ba_t) ~ normal(0,1),
#                 c(scale,scale_t) ~ exponential(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# #########length , width , thickness needed with rechecked priors
# #length
# ml0 <- ulam(
#         alist(
#                 length ~ dgamma2(mu,scale),
#                 log(mu) <- a ,
#                 a ~ normal(4,1), #prior for mean
#                 scale ~ dexp(1.2)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
# )
# 
# #almendra not-almendra
# ml1 <- ulam(
#         alist(
#                 length ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra,
#                 a ~ normal(4,1), #prior for mean
#                 ba ~ normal(0,1),
#                 scale ~ dexp(1.2)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# 
# #all resources, ignoring co-occurence
# ml2 <- ulam(
#         alist(
#                 length ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
#                 scale ~ dexp(1.2)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
# )
# 
# 
# #all resources, with co-occurence
# ml3 <- ulam(
#         alist(
#                 length ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
#                 INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
#                 scale ~ dexp(1.2)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# #width
# #mean of all tools
# md0 <- ulam(
#         alist(
#                 width ~ dgamma2(mu,scale),
#                 log(mu) <- a ,
#                 a ~ normal(4,1), #prior for mean
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
# )
# 
# md1 <- ulam(
#         alist(
#                 width ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra,
#                 a ~ normal(4,1), #prior for mean
#                 ba ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# #all resources, ignoring co-occurrence 
# md2 <- ulam(
#         alist(
#                 width ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
# )
# 
# #all resources, accounting for co-occurence
# md3 <- ulam(
#         alist(
#                 width ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
#                 INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# #thickness
# mt0  <- ulam(
#         alist(
#                 thickness ~ dgamma2(mu,scale),
#                 log(mu) <- a ,
#                 a ~ normal(4,1), #prior for mean
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
# )
# 
# #almendra vs non-almendra
# mt1<- ulam(
#         alist(
#                 thickness ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra,
#                 a ~ normal(4,1), #prior for mean
#                 ba ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )
# 
# #all resources, ignoring co-occurrence
# mt2 <- ulam(
#         alist(
#                 thickness ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
# )
# 
# #all resources, permitting co-occurrence
# mt3 <- ulam(
#         alist(
#                 thickness ~ dgamma2(mu,scale),
#                 log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
#                 INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
#                 a ~ normal(4,1), #prior for mean
#                 c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
#                 scale ~ dexp(0.005)
#         ),
#         
#         data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
# )





m_raw_tc2 <-  ulam(
        alist(
                thickness_tc ~ dgamma2(mu_t_tc,scale_t_tc), #unique shape and scale for almendras
                log(mu_t_tc) <- t_tc , #the regression to which we need to make it just jicaron
                thickness_raw ~ dgamma2(mu_t_raw,scale_t_raw) , #raw weight shape and scale
                log(mu_t_raw) <- t_raw ,
                
                c(t_raw,t_tc) ~ normal(3,2.5), #prior for mean
                c(scale_t_tc,scale_t_raw) ~ exponential(0.01) # we need a big scale for weight
        ),
        
        data=dl_raw_tc_comp, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

m_raw_tc3 <-  ulam(
        alist(
                weight_tc ~ dgamma2(mu_w_tc,scale_w_tc), #unique shape and scale for almendras
                log(mu_w_tc) <- w_tc , #the regression to which we need to make it just jicaron
                weight_raw ~ dgamma2(mu_w_raw,scale_w_raw) , #raw weight shape and scale
                log(mu_w_raw) <- w_raw ,
                
                thickness_tc ~ dgamma2(mu_t_tc,scale_t_tc), #unique shape and scale for almendras
                log(mu_t_tc) <- t_tc , #the regression to which we need to make it just jicaron
                thickness_raw ~ dgamma2(mu_t_raw,scale_t_raw) , #raw weight shape and scale
                log(mu_t_raw) <- t_raw ,
                
                length_tc ~ dgamma2(mu_l_tc,scale_l_tc), #unique shape and scale for almendras
                log(mu_l_tc) <- l_tc , #the regression to which we need to make it just jicaron
                length_raw ~ dgamma2(mu_l_raw,scale_l_raw) , #raw weight shape and scale
                log(mu_l_raw) <- l_raw ,
                
                width_tc ~ dgamma2(mu_wd_tc,scale_wd_tc), #unique shape and scale for almendras
                log(mu_wd_tc) <- wd_tc , #the regression to which we need to make it just jicaron
                width_raw ~ dgamma2(mu_wd_raw,scale_wd_raw) , #raw weight shape and scale
                log(mu_wd_raw) <- wd_raw ,
                
                
                c(w_raw,w_tc) ~ normal(5,1.8), #prior for mean
                c(scale_w_tc,scale_w_raw) ~ exponential(0.0005), # we need a big scale for weight
                
                c(t_raw,t_tc) ~ normal(3,2.5), #prior for mean
                c(scale_t_tc,scale_t_raw) ~ exponential(0.01), # we need a big scale for weight
                
                c(l_raw,l_tc) ~ normal(3,2.5), #prior for mean
                c(scale_l_tc,scale_l_raw) ~ exponential(0.01), # we need a big scale for weight
                
                c(wd_raw,wd_tc) ~ normal(3,2.5), #prior for mean
                c(scale_wd_tc,scale_wd_raw) ~ exponential(0.01) # we need a big scale for weight
                
        ),
        
        data=dl_raw_tc_comp, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4,
)

precis(m_raw_tc3)
