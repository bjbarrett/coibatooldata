# intercept only model, mean of all tools
mw0 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a ,
          a ~ normal(5.5,1.5), #prior for mean
          scale ~ dexp(0.5)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
)


#almendra vs not almendra
mw1 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra,
          a ~ normal(5.5,1.5), #prior for mean
          ba ~ normal(0,1),
          scale ~ dexp(0.5)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)


# almendra, nerite, herm_crab, hallowwen crab, river snail ignoring cooccurnce
mw2 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
          a ~ normal(5.5,1.5), #prior for mean
          c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
          scale ~ dexp(0.5)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)


#resources permitting cooccurence
mw3 <- ulam(
     alist(
          weight ~ dgamma2(mu,scale),
          log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + INT,
          INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab ,
          a ~ normal(5.5,1.5), #prior for mean
          c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq) ~ normal(0,1),
          scale ~ dexp(0.5)
     ),
     
     data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

