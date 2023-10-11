# intercept only model, mean of all tools
mw0 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a ,
          a ~ normal(6,1.2), #prior for mean
          scale ~ dexp(0.005)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
)


#almendra vs not almendra
mw1 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra,
          a ~ normal(6,1.2), #prior for mean
          ba ~ normal(0,1),
          scale ~ dexp(0.005)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)


# almendra, nerite, herm_crab, hallowwen crab, river snail ignoring co-occurnce
mw2 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
          a ~ normal(0,12), #prior for mean
          c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
          scale ~ dexp(0.005)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)


#resources permitting cooccurence
mw3 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
          INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
          a ~ normal(6,1.2), #prior for mean
          c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
          scale ~ dexp(0.005)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)


#below is building up to a crazy fancy model
mw4 <-  ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra ,

          thickness ~ dgamma2(mu_t,scale_t) ,
          log(mu_t) <- a_t + ba_t*almendra ,

          c(a,a_t) ~ normal(6,1.2), #prior for mean
          c(ba,ba_t) ~ normal(0,1),
          c(scale,scale_t) ~ exponential(0.005)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#########length , width , thickness needed with rechecked priors


#####RAW MATERIAL

m_raw_tc <-  ulam(
     alist(
          weight ~ dgamma2(mu_tc,scale_tc),
          log(mu_tc) <- a_tc,
          
          thickness ~ dgamma2(mu_raw,scale_raw) ,
          log(mu_raw) <- a_raw ,
          
          c(a_raw,a_tc) ~ normal(0,2), #prior for mean
          c(scale_tc,scale_raw) ~ exponential(1)
     ),
     
     data=data_list_raw_tc, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

dens(rgamma(10000 , rate=1 , shape=2 , scale=4))

     