precis(mw1) #gets summary stats of model
precis(mw2)
precis(mw3)
coeftab(mw0,mw1,mw2,mw3)


precis(m_raw_tc)
#mw0 
post_p <-extract.prior(mw0) #extract prior from intecpts only model visualize that predictions are reasonabl given biology and also predict some unrealist values i.e. 10+ kg stones
precis(mw0) #summarize output
trankplot(mw0) #diagnostics for mixing
dev.off()

#visualize prior predictive sims of intercept
dens(data_list$weight ) # raw data
dens(exp(post_p$a) , col="red"  ) 
#below visualize prior
curve(dgamma(x , shape=median(post_p$a) , scale=median(post_p$scale) ) ,  add=TRUE ,col="black")  #range of mean predictions
for ( i in 1:500 ) {
     curve(dgamma(x , shape=post_p$a[i] , scale=post_p$scale[i])  , add=TRUE ,  col=col.alpha("black",0.05))
}

#visualize prior predictive sims with slope
post_p <-extract.prior(mw1) #extract prior from intecpts only model visualize that predictions are reasonabl given biology and also predict some unrealist values i.e. 10+ kg stones
precis(mw1) #summarize output
trankplot(mw1) #diagnostics for mixing
dev.off()

#visualize prior predictive sims of intercept
dens(data_list$weight ) # raw data
dens(exp(post_p$a) , col="red"  )
#below visualize prior
curve(dgamma(x , shape=median(post_p$a + post_p$ba) , scale=median(post_p$scale) ) ,  add=TRUE ,col="black") 

for ( i in 1:500 ) {
     curve(dgamma(x , shape=(post_p$a[i] + post_p$ba[i] ) , scale=post_p$scale[i])  , add=TRUE ,  col=col.alpha("black",0.05))
}



post <- extract.samples(mw0)
str(post)
trankplot(mw0)
dev.off()
plot.new()
dens(exp(post$a)) #post density of mean stone weight

plot(density(data_list$weight, adjust=1.8)  , col="white" , ylim=c(-0.0002,0.0027) , xlim=c(0,5000) , main="" , xlab="mean stone tool weight (g)" , cex.lab=1.7, yaxt='n' , ylab="")
points(data_list$weight,rep(0, length(data_list$weight)))
for ( i in 1:100 ) {
     curve(dgamma(x , shape=post$a[i] , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("black",0.1))
     #curve(dgamma(x , shape=(post$a[i] + post$b_adult[i]) , scale=post$scale[i] ) , add=TRUE ,  col=col.alpha("blue",0.1) , lty=1)
}


PostExtractPreds1 <- function( model , funk , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , astro=0 ){
     post <- extract.samples(model)
     frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + bas*astro + basXrs*astro*river_snail,)
     print(funk(exp(frogs)))
}



###raw_tc
precis(m_raw_tc)
#below are objects we can call into overleaf doc

almendra_mass_mean <- PostExtractPreds1(model=mw3 , funk=mean , almendra=1)
almendra_mass_hpdi <- PostExtractPreds1(model=mw3 , funk=HPDI , almendra=1)

nerite_mass_mean <- PostExtractPreds1(model=mw3 , funk=mean , nerite=1)
nerite_mass_hpdi <- PostExtractPreds1(model=mw3 , funk=HPDI , nerite=1)

herm_crab_mass_mean  <- PostExtractPreds1(model=mw3 , funk=mean , herm_crab=1)
herm_crab_mass_hpdi  <- PostExtractPreds1(model=mw3 , funk=HPDI , herm_crab=1)

halloween_mass_mean  <- PostExtractPreds1(model=mw3 , funk=mean , halloween=1)
halloween_mass_hpdi  <- PostExtractPreds1(model=mw3 , funk=HPDI , halloween=1)

river_snail_mass_mean  <- PostExtractPreds1(model=mw3 , funk=mean , river_snail=1)
river_snail_mass_hpdi  <- PostExtractPreds1(model=mw3 , funk=HPDI , river_snail=1)

astro_mass_mean  <- PostExtractPreds1(model=mw3 , funk=mean , astro=1)
astro_mass_hpdi  <- PostExtractPreds1(model=mw3 , funk=HPDI , astro=1)



x<- rnorm(n=10000 , mean=3.4 , sd=2)
shart <- summary(x)
print(xtable(shart, type = "latex") , file = "filename2.tex")


library(xtable)
input1 <- c(0,0,0,1,1,1,1,2,2,2,2)
input2 <- c(0,0,0,0,0,1,0,0,0,1,2)
result <- table(input1, input2)
xtable(result)

