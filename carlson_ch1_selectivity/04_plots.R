library(RColorBrewer)
palette <- brewer.pal( 5 , "Dark2")

##mw2
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , color="black" , graph_title="words"){
     post <- extract.samples(model)
     frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail)
     dens(exp(frogs) , xlim=c(0 , 3100) , col=color , xlab="mass (g)")
     shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
     scoops <- data_list$weight[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail]
     points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
     abline(v=mean(exp(frogs)) , col=color , lty=1)
     title(main=graph_title)
}

PlotToolPost(model= mw2 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= mw2 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= mw2 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= mw2 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= mw2 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")

###mw3
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , color="black" , graph_title="words"){
     post <- extract.samples(model)
     frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab ,)
     dens(exp(frogs) , xlim=c(0 , 3100) , col=color , xlab="mass (g)")
     # for ( i in 1:100 ) {
     #      curve(dgamma(x , shape=frogs[i] , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("black",0.1))
     # }
     shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
     scoops <- data_list$weight[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail]
     points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
     abline(v=mean(exp(frogs)) , col=color , lty=1)
     title(main=graph_title)
     
}

PlotToolPost(model= mw3 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= mw3 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= mw3 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= mw3 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= mw3 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")

###
#mj2
#jicaron model almendra and hermit crab
PlotToolPost_j <- function( model , almendra=0 , herm_crab=0 , color="black" , graph_title="words"){
        post <- extract.samples(model)
        guitar <- with(post , a + ba*almendra + bhc*herm_crab)
        dens(exp(guitar) , xlim=c(0 , 3100), ylim=c(0, 0.011) , col=color , xlab="mass (g)")
        shade( density(exp(guitar)) , col=col.alpha(color,0.7) , lim=PI( exp(guitar)  , prob=0.9999 ))
        scoops <- data_list_j$weight[data_list_j$almendra==almendra & data_list_j$herm_crab==herm_crab]
        points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
        abline(v=mean(exp(guitar)) , col=color , lty=1)
        title(main=graph_title)
}

PlotToolPost_j(model= mj2 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost_j(model= mj2 , herm_crab=1 , color=palette[2] , graph_title= "Hermit crab")

#mc2
#coiba model astrocaryum and hermit crab
PlotToolPost_c <- function( model , astro=0 , river_snail=0 , color="black" , graph_title="words"){
        post <- extract.samples(model)
        nebula <- with(post , a + bas*astro + brs*river_snail)
        dens(exp(nebula) , xlim=c(0 , 3100) , ylim=c(0, 0.011), col=color , xlab="mass (g)")
        shade( density(exp(nebula)) , col=col.alpha(color,0.7) , lim=PI( exp(nebula)  , prob=0.9999 ))
        scoops <- data_list_c$weight[data_list_c$astro==astro & data_list_c$river_snail==river_snail]
        points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
        abline(v=mean(exp(nebula)) , col=color , lty=1)
        title(main=graph_title)
}

PlotToolPost_c(model= mc2 , astro=1 , color=palette[3] , graph_title= "Astrocaryum spp")
PlotToolPost_c(model= mc2 , river_snail=1 , color=palette[4] , graph_title= "Freshwater snail")


#m2_select_c
#coiba selectivity shell, astro, rm
# PlotSelectPost_c <- function(model, ast=0, sh=0, rm=0, color="black", graph_title="words"){
#         post <- extract.samples(model)
#         elephant <- with(post, a_as + a_sh + a_rm)
#         dens(exp(elephant) , xlim=c(0, 3100), ylim=c(0, 0.011), col=color ,  xlab="weight (g)")
# }
# 
# PlotSelectPost_c(model=m2_select_c, ast=1, color=palette[3], graph_title="Astrocaryum spp")
# PlotSelectPost_c(model=m2_select_c, sh=1, color=palette[4], graph_title = "fw snail")
# PlotSelectPost_c(model=m2_select_c, rm=1, color=palette[1], graph_title="raw material")


#FIGURE 2 FOR CHAPTER
par(mfrow=c(1,2))

dens(exp(post2c$a_as), xlim=c(0, 2000), ylim=c(0, 0.015), col="#84a955", lwd="2", xlab="Weight (g)")
dens(exp(post2c$a_sh), col="#c36785", add=TRUE, lwd="2")
dens(exp(post2c$a_rm), col="#696969", add=TRUE, lwd="2", lty=3)
legend(1000, 0.015, legend=c("Astrocaryum spp.", "Freshwater snail", "Raw material"),
       col=c("#84a955", "#c36785", "#696969"), lty=c(1, 1, 3), lwd=2, cex=0.8, text.font=2, box.lty=0)


dens(exp(post2j$a_alm), xlim=c(0, 2000), ylim=c(0, 0.015), col="#9f9244", lwd=2, xlab="Weight (g)")
dens(exp(post2j$a_sh), col="#8961b3", add=TRUE, lwd=2)
dens(exp(post2j$a_rm), col="#696969", add=TRUE, lwd=2, lty=3)
legend(1000, 0.015, legend=c("Sea almond", "Hermit crab", "Raw material"),
       col=c("#9f9244", "#8961b3", "#696969"), lty=c(1, 1, 3), lwd=2, cex=0.8, text.font=2, box.lty=0)
