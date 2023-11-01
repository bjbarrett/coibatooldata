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
          a ~ normal(6,1.2), #prior for mean
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
#length
ml0 <- ulam(
        alist(
                length ~ dgamma2(mu,scale),
                log(mu) <- a ,
                a ~ normal(4,1), #prior for mean
                scale ~ dexp(1.2)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
)

#almendra not-almendra
ml1 <- ulam(
        alist(
                length ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra,
                a ~ normal(4,1), #prior for mean
                ba ~ normal(0,1),
                scale ~ dexp(1.2)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)


#all resources, ignoring co-occurence
ml2 <- ulam(
        alist(
                length ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
                scale ~ dexp(1.2)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)


#all resources, with co-occurence
ml3 <- ulam(
        alist(
                length ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
                INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
                scale ~ dexp(1.2)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#width
#mean of all tools
md0 <- ulam(
        alist(
                width ~ dgamma2(mu,scale),
                log(mu) <- a ,
                a ~ normal(4,1), #prior for mean
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
)

md1 <- ulam(
        alist(
                width ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra,
                a ~ normal(4,1), #prior for mean
                ba ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#all resources, ignoring co-occurrence 
md2 <- ulam(
        alist(
                width ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)

#all resources, accounting for co-occurence
md3 <- ulam(
        alist(
                width ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
                INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#thickness
mt0  <- ulam(
        alist(
                thickness ~ dgamma2(mu,scale),
                log(mu) <- a ,
                a ~ normal(4,1), #prior for mean
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4 
)

#almendra vs non-almendra
mt1<- ulam(
        alist(
                thickness ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra,
                a ~ normal(4,1), #prior for mean
                ba ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)

#all resources, ignoring co-occurrence
mt2 <- ulam(
        alist(
                thickness ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4
)

#all resources, permitting co-occurrence
mt3 <- ulam(
        alist(
                thickness ~ dgamma2(mu,scale),
                log(mu) <- a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + bas*astro + INT,
                INT <- baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + basXrs*astro*river_snail,
                a ~ normal(4,1), #prior for mean
                c(ba,bn,bhc,bgq,brs,baXn,baXhc,baXhcXgq,basXrs,bas) ~ normal(0,1),
                scale ~ dexp(0.005)
        ),
        
        data=data_list, cores=4 , warmup=1000 , iter=2000 , sample=TRUE, chains=4, 
)




