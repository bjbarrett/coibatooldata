#install.packages("plotrix")
library(plotrix)
library(gtools)
library(janitor)
library(rethinking)
library(dplyr)
library(wesanderson)
## load data
mar2018 <- read.csv("/Users/BJB/Downloads/coibatooldata-master/jicaron_mar_2018.csv")
jul2018 <- read.csv("/Users/BJB/Downloads/coibatooldata-master/jicaron_july_2018.csv")
jul2017jan2018 <- read.csv("/Users/BJB/Downloads/coibatooldata-master/jicaron_july_2017_jan_2018.csv")
janmar2019 <- read.csv("/Users/BJB/Downloads/coibatooldata-master/Jan March 2019.csv")

##add nut to this
mar2018$NUT <- 0
janmar2019$NUT <- 0

str(mar2018)
str(jul2018)
str(jul2017jan2018)
str(janmar2019)

d <- smartbind(mar2018,jul2018,janmar2019)
d <- clean_names(d)

d <- d[is.na(d$length)==FALSE,] #drop nas length from smartbind
d <- d[d$hammer!="Wooden",] #get rid of Wooden Hammer
d$weight <- as.numeric(d$weight)
dl <- d$length
dw <- d$width
which(d$length > d$width)
which(d$length < d$width)
which(d$width  <  d$thickness)
which(d$length  <  d$thickness) #only one where length is greater than thickness
d$length[which(dl < dw)] <- dw[which(dl < dw)] #swap l and w
d$width[which(dl < dw)] <- dl[which(dl < dw)] #swap l and w
which(d$length < d$width) #check for zero if correct

###do site type stuff
unique(d$site_type)
d$site_type_index <- as.integer(d$site_type)
dens(d$weight)
mean(d$weight)

plot(length~width , data=d , ylim=c(0,210) , xlim=c(0,210))
abline(a = 0, b = 1, col="red")
plot(thickness~width , data=d , ylim=c(0,210) , xlim=c(0,210))
abline(a = 0, b = 1, col="red")
plot(thickness~length , data=d , ylim=c(0,210) , xlim=c(0,210))
abline(a = 0, b = 1, col="red")

plot(0,0 , xlim=c(-100,100) , ylim=c(-100,100) , xlab="width (mm)" , ylab="length (mm)" )
for (i in 1:nrow(d)){
  draw.ellipse(x=0 , y=0 , a = c(d$width[i])/2, b = c(d$length[i])[1]/2, angle = 0, 
               nv = 100, border = col.alpha(1,0.2), col = NA, lty = 1, lwd = 1)
}

plot(0,0 , xlim=c(-100,100) , ylim=c(-100,100)  , xlab="width (mm)" , ylab="thickness (mm)" )
for (i in 1:nrow(d)){
  draw.ellipse(x=0 , y=0 , a = c(d$width[i])/2, b = c(d$thickness[i])[1]/2, angle = 0, 
               nv = 100, border = col.alpha(1,0.2), col = NA, lty = 1, lwd = 1)
}

plot(0,0 , xlim=c(-100,100) , ylim=c(-100,100)  , xlab="length (mm)" , ylab="thickness (mm)" )
for (i in 1:nrow(d)){
  draw.ellipse(x=0 , y=0 , a = c(d$length[i])/2, b = c(d$thickness[i])[1]/2, angle = 0, 
               nv = 100, border = col.alpha(1,0.2), col = NA, lty = 1, lwd = 1)
}


str(d$almendra)
# create dummy variables for yes no to each item
d$almendras_yes <- ifelse(d$almendras=="Yes" , 1 , 0)
d$shells_yes <- ifelse(d$shells=="Yes" , 1 , 0)
d$hcrabs_yes <- ifelse(d$hcrabs=="Yes" , 1 , 0)
d$caracol_yes <- 0
d$caracol_yes[grep("caracol" , d$comments)] <- 1
d$caracol_yes[grep("snail" , d$comments)] <- 1
##anvil issues
d$stone_anvil <- 1
d$stone_anvil[grep("Wood" , d$anvil)] <- 0

dens(d$weight[d$caracol_yes==1])
dens(d$weight[d$hcrabs_yes==1])

dens(d$weight[d$almendras_yes==1], col="green" , ylim=c(0,0.003) , xlab="mass hammerstone (g)")
abline(v=mean(d$weight[d$almendras_yes==1]) , col="green")
dens(d$weight[d$shells_yes==1 & d$almendras_yes==0] , col="violet" ,  add=TRUE  )
abline(v=mean(d$weight[d$shells_yes==1 & d$almendras_yes==0]) , col="violet")

legend("topright" , c("almendras" , "hermit shells") , col= c("green" , "violet") , pch=19) 



dens(d$weight[d$shells_yes==1 & d$almendras_yes==0 & d$caracol_yes==0] , col="violet" ,  add=TRUE  )
dens(d$weight[d$caracol_yes==1] , col="black" ,  add=TRUE  )
dens(d$weight[d$hcrabs_yes==1] , col="orange" ,  add=TRUE  )

mean(d$weight)
#almendras
mean(d$weight[d$almendras_yes==1])
mean(d$weight[d$almendras_yes==1 & d$caracol_yes==0 & d$shells_yes==0 & d$hcrabs_yes==0])
#caracol
mean(d$weight[d$caracol_yes==1])
mean(d$weight[d$caracol_yes==1 & d$almendras_yes==0])
#shells
mean(d$weight[d$shells_yes==1])
mean(d$weight[d$shells_yes==1 & d$almendras_yes==0])
#hcrabs
mean(d$weight[d$hcrabs_yes==1])
mean(d$weight[d$hcrabs_yes==1 & d$almendras_yes==0])



dens(d$weight[d$almendras=="No"] , add=TRUE , col="brown")

draw.ellipse(c(10,10), c(20,20), c(0.5,100), c(1,5), border=1,
             angle=c(0,0), lty=1 , col=c(2,4) )

####redo gamma GLMS 

m <- map2stan(
  alist(
    weight ~ dgamma2(mu,scale),
    log(mu) ~ a ,
    a ~ dnorm(1,2),
    scale ~ dexp(0.5)
  ),
  
  data=d, cores=2 , warmup=1000 , iter=2000 , WAIC=TRUE, constraints=list(scale="lower=0") , sample=TRUE, 
)

post <- extract.samples(m)
plot.new()
#check prior to see it reasonable based on laws of physics and capuchin monkey biology
#scale prior

scale_prior <- rexp(n=10000, rate=0.005)
dens(scale_prior)

shape_prior <- rnorm(n=10000, mean=3.5 , sd=2)
plot.new()

plot(density(d$weight, adjust=1.8), xlim=c(0,3000))
for ( i in 1:100 ) {
  curve(dgamma(x , shape=shape_prior[i] , scale=scale_prior[i])  , add=TRUE ,  col=col.alpha("black",0.3))
}

curve(dgamma(x , shape=median(shape_prior) , scale=median(scale_prior))   , add=TRUE , col="red" , lw=2)

#####
prior <- extract.prior(m1)
str(prior)

plot.new()
plot(density(d$weight, adjust=1.8), xlim=c(0,3500))
for ( i in 1:100 ) {
  curve(dgamma(x , shape=prior$a[i] , scale=prior$scale[i])  , add=TRUE ,  col=col.alpha("black",0.3))
}
curve(dgamma(x , shape=mean(prior$a) , scale=mean(prior$scale))  , add=TRUE , col="red" , lw=2)

plot.new()
plot(density(d$weight, adjust=1.8), xlim=c(0,500))
for ( i in 1:100 ) {
  curve(dgamma(x , shape=prior$a[i] , scale=prior$scale[i])  , add=TRUE ,  col=col.alpha("black",0.3))
}

curve(dgamma(x , shape=3 , scale=100 ) , add=TRUE , col="blue" , lw=2)

#begin model plot

plot(density(d$weight), xlim=c(0,2500)  , col="green" , ylim=c(-0.0002,0.0027) , main="" , xlab="mean stone tool weight (g)" , cex.lab=1.5, yaxt='n' , ylab="")

str(post)
for ( i in 1:100 ) {
  curve(dgamma(x , shape=post$a[i] , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("black",0.05))
}

curve(dgamma(x , shape=mean(post$a) , scale=mean(post$scale))  , add=TRUE , col="black" , lw=2)

points( d$weight, rep(-0.0001, nrow(d)) , pch=19 , col=col.alpha(1 , 0.2) , cex=0.5)

m1 <- map2stan(
  alist(
    weight ~ dgamma2(mu,scale),
    log(mu) ~ a + bA*almendras_yes ,
    a ~ dnorm(1,2),
    bA  ~ dnorm(0,1),
    scale ~ dexp(0.5)
  ),
  
  data=d, cores=4 , warmup=1000 , iter=2000 , WAIC=TRUE, constraints=list(scale="lower=0") , sample=TRUE, 
)

m2 <- map2stan(
  alist(
    weight ~ dgamma2(mu,scale),
    log(mu) ~ a + bA*almendras_yes + (bS + bAS*almendras_yes)*shells_yes ,
    a ~ dnorm(1,2),
    c(bA,bS,bAS)  ~ dnorm(0,1),
    scale ~ dexp(0.5)
  ),
  
  data=d, cores=4 , warmup=1000 , iter=2000 , WAIC=TRUE, constraints=list(scale="lower=0") , sample=TRUE, 
)

m3 <- map2stan(
  alist(
    weight ~ dgamma2(mu,scale),
    log(mu) ~ a + (bA + bAH*hcrabs_yes)*almendras_yes + (bS + bAS*almendras_yes)*shells_yes +
      (bH + (bHS + bHSA*almendras_yes)*shells_yes )*hcrabs_yes ,
    a ~ dnorm(1,2),
    c(bA,bAH,bS,bAS,bH,bHS,bHSA)  ~ dnorm(0,1),
    scale ~ dexp(0.5)
  ),
  
  data=d, cores=4 , warmup=1000 , iter=2000 , WAIC=TRUE, constraints=list(scale="lower=0") , sample=TRUE, 
)

m4 <-
precis(m3)

lm(logweight~ hcrabs_yes*almendras_yes*shells_yes , data=d)
######m1######
precis(m1)
post <- extract.samples(m1)

dens(exp(with(post, a + bA*0)) , add=TRUE , col="grey")
dens(exp(with(post, a + bA*1)) , add=TRUE , col="green")
plot(density(d$weight), xlim=c(min(d$weight),max((d$weight)))  , col="grey" , ylim=c(-0.0002,0.014) , main="" , xlab="mean stone tool weight (g)" , cex.lab=1.5, yaxt='n' , ylab="")
xx <- exp(with(post, a + bA*0))
dens( xx , add=TRUE , col="orange")
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha("orange", 0.5))
xx <- exp(with(post, a + bA*1))
dens(xx , add=TRUE , col="green")
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha("green", 0.5))
pts <- d$weight[d$almendras_yes==0]
points( pts , rep(-0.0001, length(pts)) , pch=19 , col=col.alpha("orange" , 0.2) , cex=0.5)
pts <- d$weight[d$almendras_yes==1]
points( pts , rep(-0.0003, length(pts)) , pch=19 , col=col.alpha("green" , 0.2) , cex=0.5)
legend("topright", c("almendras" , "no almendras"), pch=15, col=c("green" , "orange"), box.col=NA, cex=1 )

##############m2
precis(m2)
post <- extract.samples(m2)
m2@formula[[2]] #get likelihood loop to copy formula
plot(density(d$weight), xlim=c(min(d$weight),max((d$weight)))  , col="grey" , ylim=c(-0.0002,0.014) , main="" , xlab="mean stone tool weight (g)" , cex.lab=1.5, yaxt='n' , ylab="")
#almendras, no shells
xx <- exp(with(post, a + bA * 1 + (bS + bAS * 1) * 0) ) 
farbe <- "green"
pts <- d$weight[d$almendras_yes==1 & d$shells_yes==0]
dens( xx , add=TRUE , col=farbe)
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0001, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)

#almendras and shells
xx <-exp(with(post, a + bA * 1 + (bS + bAS * 1) * 1) ) 
farbe <- "grey"
pts <- d$weight[d$almendras_yes==1 & d$shells_yes==1]
dens( xx , add=TRUE , col=farbe)
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0002, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)

#shells no almendras
xx <-exp(with(post, a + bA * 0 + (bS + bAS * 0) * 1) ) 
farbe <- "orange"
pts <- d$weight[d$almendras_yes==0 & d$shells_yes==1]
dens( xx , add=TRUE , col=farbe)
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0003, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)

#no shells no almendras
xx <-exp(with(post, a + bA * 0 + (bS + bAS * 0) * 0) ) 
farbe <- "blue"
pts <- d$weight[d$almendras_yes==0 & d$shells_yes==0]
dens( xx , add=TRUE , col=farbe)
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0004, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)


legend("topright", c("almendras, no shells" , "almendras & shells" , "shells, no almendras"), pch=15, col=c("green" , "grey" , "orange"), box.col=NA, cex=1 )


#######if one wanted to model distribution of all the tools // need to check this
###yes almendras
# for ( i in 1:100 ) {
#   curve(dgamma(x , shape=post$a[i] + post$bA[i]*1 , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("green",0.1))
# }
# curve(dgamma(x , shape=mean(post$a + post$bA*1) , scale=mean(post$scale))  , add=TRUE , col="green" , lw=2)
# curve(dgamma(x , shape=mean(post$a + post$bA*0) , scale=mean(post$scale))  , add=TRUE , col="orange" , lw=2)


###########
d$logweight <- log(d$weight)

m1 <- ulam(
  alist(
    logweight ~ dnorm(mu,sigma),
    mu <- a + bA*almendras_yes ,
    a ~ dnorm(6,2),
    bA  ~ dnorm(0,2),
    sigma ~ dexp(1)
  ),
  
  data=d, cores=2 , warmup=1000 , iter=2000 , sample=TRUE, 
)

m2 <- map2stan(
  alist(
   logweight ~ dnorm(mu,sigma),
    mu ~ a + bA*almendras_yes ,
    a ~ dnorm(6,2),
    bA  ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=d, cores=2 , warmup=1000 , iter=2000 , sample=TRUE)


post <- extract.samples(m2)
precis(m2)

dens(post$a + post$bA , xlim=c(5,7) , col="green")
dens(post$a , add=TRUE)


#look at if stone anvil is used more or less at varied sites
str(d)
sa1 <- glm(stone_anvil ~ almendras_yes + shells_yes + hcrabs_yes + caracol_yes + coco_yes, data=d , family='binomial')
summary(sa1)

stlm <- lm(logweight~site_type , data=d)
summary(stlm)
#tendency for lighter stones at type 3 sites
exp(stlm$coefficients[[1]])  
exp(stlm$coefficients[[1]] + stlm$coefficients[[2]])  
exp(stlm$coefficients[[1]] + stlm$coefficients[[3]])  

lm1 <- lm(logweight ~ almendras_yes , data=d)
summary(lm1)
precis(m1)
exp(6.06)
exp(6.06 + 0.39)

lm2 <- lm(logweight ~ almendras_yes*shells_yes , data=d)
summary(lm2)
exp(lm2$coefficients[[1]] ) #neither almendras or shells
exp(lm2$coefficients[[1]] + lm2$coefficients[[2]]) #just almendras
exp(lm2$coefficients[[1]] + lm2$coefficients[[3]]) #just shells
exp(lm2$coefficients[[1]] + lm2$coefficients[[2]] + lm2$coefficients[[3]] + lm2$coefficients[[4]]) #shells and almendras

lm2$coefficients

lmanvil <- lm(logweight ~ stone_anvil , data=d)
summary(lmanvil)
########read in macaques

##terminalia catappa
mtc <- read.csv("/Users/BJB/Downloads/coibatooldata-master/macaca_sea_almond.csv")
ctc <- d[d$almendras_yes==1,]
mtc$weight <- mtc$Weight_g
mtc$width <- mtc$Width_mm
mtc$length <- mtc$Length_mm
mtc$thickness <- mtc$Tickness_mm

mtc <- select(mtc, weight=Weight_g , width=Width_mm , length=Length_mm , thickness=Tickness_mm)
ctc <- select(ctc, weight, width, length , thickness)
ctc$genus <- "Cebus"
mtc$genus <- "Macaca"
tc <- rbind(ctc,mtc)
mtc$weight <- mtc$Weight_g

dens(tc$weight[tc$genus=="Cebus"])
dens(tc$weight[tc$genus=="Macaca"] , add=TRUE)
tc$genus_index <- as.integer(as.factor(tc$genus))

#model of weight differences
mtc1 <- map2stan(
  alist(
    weight ~ dgamma2(mu,scale),
    log(mu) ~ a[genus_index] ,
    a[genus_index] ~ dnorm(1,2),
    scale ~ dexp(0.5)
  ),
  
  data=tc, cores=4 , warmup=1000 , iter=2000 , WAIC=TRUE, constraints=list(scale="lower=0") , sample=TRUE, 
)
precis(mtc1 , depth=2)
post <- extract.samples(mtc1)

##plot it
pal <- wes_palette("Darjeeling1", 2 )

plot(density(tc$weight), xlim=c(min(tc$weight),max((tc$weight)))  , col="grey" , ylim=c(-0.0002,0.012) , main="" , xlab="mean T. catappa stone tool weight (g)" , cex.lab=1.5, yaxt='n' , ylab="")
#almendras, no shells
#cebus
xx <- exp(post$a[,1]) 
farbe <- pal[1]
dens( tc$weight[tc$genus_index==1]  , col=farbe , add=TRUE)
pts <- tc$weight[tc$genus_index==1]
dens( xx  , col=farbe , add=TRUE)
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0001, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)
##macacque
xx <- exp(post$a[,2]) 
farbe <- pal[2]
dens( tc$weight[tc$genus_index==2]  , col=farbe , add=TRUE)
pts <- tc$weight[tc$genus_index==2]
dens( xx , add=TRUE , col=farbe )
shade( density(xx) , lim= as.vector(HPDI(xx, prob=0.9999)) , col = col.alpha(farbe, 0.5))
points( pts , rep(-0.0003, length(pts)) , pch=19 , col=col.alpha(farbe , 0.2) , cex=0.5)

legend("topright", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )


#########Cebus vs. macaca dimensions

plot(length~width , data=tc , ylim=c(0,300) , xlim=c(0,300) , col=pal[tc$genus_index])
abline(a = 0, b = 1, col=1 , lw=1)
legend("bottomright", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )

plot(thickness~width , data=tc , ylim=c(0,300) , xlim=c(0,300) , col=pal[tc$genus_index])
abline(a = 0, b = 1, col=1)
legend("topleft", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )

plot(thickness~length , data=tc , ylim=c(0,300) , xlim=c(0,300) , col=pal[tc$genus_index])
abline(a = 0, b = 1, col=1)
legend("topleft", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )

####width by length
plot(0,0 , xlim=c(-150,150) , ylim=c(-150,150) , xlab="width (mm)" , ylab="length (mm)" )
for (i in 1:nrow(tc)){
  draw.ellipse(x=0 , y=0 , a = c(tc$width[i])/2, b = c(tc$length[i])/2, angle = 0, 
               nv = 100, border = col.alpha( pal[tc$genus_index[i]] , 0.2), col = NA, lty = 1, lwd = 1)
}

for (i in 1:2){
  draw.ellipse(x=0 , y=0 , a = mean(tc$width[tc$genus_index==i])/2, b = mean(tc$length[tc$genus_index==i])/2, angle = 0,  nv = 100, border = pal[i], col = NA, lty = 1, lwd = 3)
}
legend("topleft", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )


####width by thickness
plot(0,0 , xlim=c(-150,150) , ylim=c(-150,150)  , xlab="width (mm)" , ylab="thickness (mm)" )
for (i in 1:nrow(tc)){
  draw.ellipse(x=0 , y=0 , a = c(tc$width[i])/2, b = c(tc$thickness[i])/2, angle = 0, 
               nv = 100, border = col.alpha(pal[tc$genus_index[i]],0.2), col = NA, lty = 1, lwd = 1)
}

for (i in 1:2){
  draw.ellipse(x=0 , y=0 , a = mean(tc$width[tc$genus_index==i])/2, b = mean(tc$thickness[tc$genus_index==i])/2, angle = 0,  nv = 100, border = pal[i], col = NA, lty = 1, lwd = 3)
}
legend("topleft", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )


plot(0,0 , xlim=c(-150,150) , ylim=c(-150,150)  , xlab="length (mm)" , ylab="thickness (mm)" )
for (i in 1:nrow(tc)){
  draw.ellipse(x=0 , y=0 , a = c(tc$length[i])/2, b = c(tc$thickness[i])/2, angle = 0, 
               nv = 100, border = col.alpha(pal[tc$genus_index[i]],0.2), col = NA, lty = 1, lwd = 1)
}

for (i in 1:2){
  draw.ellipse(x=0 , y=0 , a = mean(tc$length[tc$genus_index==i])/2, b = mean(tc$thickness[tc$genus_index==i])/2, angle = 0,  nv = 100, border = pal[i], col = NA, lty = 1, lwd = 3)
}
legend("topleft", c("Cebus" , "Macaca"), pch=15, col=pal, box.col=NA, cex=1 )
