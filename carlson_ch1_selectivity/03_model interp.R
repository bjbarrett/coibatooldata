precis(mj1)
precis(mj2)
coeftab(mj1, mj2)

precis(mj2v, depth=2)
precis(mc2v, depth=2)

precis(mc1)
precis(mc2)

##combined island models
precis(mw1)
precis(mw2)
precis(mw3)
coeftab(mw0,mw1,mw2,mw3)

#mw0 
post_p <-extract.prior(mw0)
precis(mw0) #summarize output
trankplot(mw0) #diagnostics for mixing
dev.off()

dens(data_list$weight)
dens(exp(post_p$a))
for ( i in 1:500 ) {
     curve(dgamma(x , shape=post_p$a[i] , scale=post_p$scale[i])  , add=TRUE ,  col=col.alpha("black",0.1))
     #curve(dgamma(x , shape=(post$a[i] + post$b_adult[i]) , scale=post$scale[i] ) , add=TRUE ,  col=col.alpha("blue",0.1) , lty=1)
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


post <- extract.samples(mw3)

#selectivity models

post2c <- extract.samples(m2_select_c)
dens(exp(post2c$a_as))
dens(exp(post2c$a_sh))
dens(exp(post2c$a_rm))

post2j <- extract.samples(m2_select_j)
dens(exp(post2j$a_alm))
dens(exp(post2j$a_sh))
dens(exp(post2j$a_rm))



