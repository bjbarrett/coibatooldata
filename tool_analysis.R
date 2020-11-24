#install.packages("plotrix")
library(plotrix)
library(gtools)
library(janitor)
library(rethinking)

# NOT RUN {
plot(c(0,10), c(0,10), type="n", main="test draw.ellipse")
draw.ellipse(c(3,7), c(8,8), c(0.5,1), c(1,0.5), col=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)))
draw.ellipse(c(3,7), c(6,6), c(0.5,1), c(1,0.5), col=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)), arc.only=FALSE)
draw.ellipse(c(3,7), c(4,4), c(0.5,1), c(1,0.5), border=c(2,4),
             angle=c(45,0), segment=rbind(c(0,45),c(45,360)), arc.only=FALSE)
draw.ellipse(c(3,7), c(2,2), c(0.5,1), c(1,0.5), border=1,
             angle=c(45,0), lty=3)
draw.ellipse(c(3,7), c(2,2), c(0.5,1), c(1,0.5), border=c(5,3),
             angle=c(45,0), nv=c(3,4), lty=2, lwd=2)
# }

plot(c(0,10), c(0,10), type="n", main="test draw.ellipse")
draw.ellipse(c(3,7), c(2,2), c(0.5,1), c(1,0.5), border=1,
             angle=c(45,0), lty=1)
draw.ellipse(c(3,9), c(2,2), c(0.5,1), c(1,0.5), border=1,
             angle=c(0,0), lty=1 , col=c(2,4) )

######33
# load data
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
d$almendras_yes <- ifelse(d$almendras=="Yes" , 1 , 0)
d$shells_yes <- ifelse(d$shells=="Yes" , 1 , 0)
d$hcrabs_yes <- ifelse(d$hcrabs=="Yes" , 1 , 0)
d$caracol_yes <- 0
d$caracol_yes[grep("caracol" , d$comments)] <- 1
d$caracol_yes[grep("snail" , d$comments)] <- 1
dens(d$weight[d$caracol_yes==1])

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
