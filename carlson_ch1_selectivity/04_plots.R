library(RColorBrewer)
palette <- brewer.pal( 6 , "Dark2")

##mw2
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , color="black" , graph_title="words"){
     post <- extract.samples(model)
     frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail)
     dens(exp(frogs) , xlim=c(0 , 3100) , col=color , xlab="mass (g)" , ylab="")
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
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , astro=0 , color="black" , graph_title="words"){
     post <- extract.samples(model)
     frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + bas*astro + basXrs*astro*river_snail,)
     par(mar=c(3,3,1,1))
     dens(exp(frogs) , xlim=c(0 , 3100) , col=color , xlab="mass (g)" , ylab="posterior density" , cex.lab=1.5)
     # for ( i in 1:100 ) {
     #      curve(dgamma(x , shape=frogs[i] , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("black",0.1))
     # }
     shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
     scoops <- data_list$weight[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail & data_list$astro==astro]
     points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
     abline(v=mean(exp(frogs)) , col=color , lty=1)
     title(main=graph_title)
     
}
PlotToolPost(model= mw3 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= mw3 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= mw3 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= mw3 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= mw3 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")



plot_names <- c("mass_post_terminalia" , "mass_post_nerita" , "mass_post_ceonobita" , "mass_post_gecarcinus" , "mass_post_freshsnail" , "mass_post_astrocaryum" )
plot_names <- paste0("carlson_ch1_selectivity/figures/",plot_names,".pdf")

#fix and make a loop later to be clean and fancy, or not....
pdf(file=plot_names[1] , width=7 , height=7 )
PlotToolPost(model= mw3 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
dev.off()
pdf(file=plot_names[2] , width=7 , height=7 )
PlotToolPost(model= mw3 , nerite=1 , color=palette[2] , graph_title= "Nerita scabricosta")
dev.off()
pdf(file=plot_names[3] , width=7 , height=7 )
PlotToolPost(model= mw3 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
dev.off()
pdf(file=plot_names[4] , width=7 , height=7 )
PlotToolPost(model= mw3 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
dev.off()
pdf(file=plot_names[5] , width=7 , height=7 )
PlotToolPost(model= mw3 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")
dev.off()
pdf(file=plot_names[6] , width=7 , height=7 )
PlotToolPost(model= mw3 , astro=1 , color=palette[6] , graph_title="Astrocaryum standleyanum")
dev.off()

#ml2
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , color="black" , graph_title="words"){
        post <- extract.samples(model)
        frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail)
        dens(exp(frogs) , xlim=c(0 , 400) , col=color , xlab="length (mm)" , ylab="")
        shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
        scoops <- data_list$length[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail]
        points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
        abline(v=mean(exp(frogs)) , col=color , lty=1)
        title(main=graph_title)
}

PlotToolPost(model= ml2 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= ml2 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= ml2 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= ml2 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= ml2 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")

#ml3
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , astro=0 , color="black" , graph_title="words"){
        post <- extract.samples(model)
        frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail + baXn*almendra*nerite + baXhc*almendra*herm_crab + baXhcXgq*almendra*halloween*herm_crab + bas*astro + basXrs*astro*river_snail,)
        par(mar=c(3,3,1,1))
        dens(exp(frogs) , xlim=c(0 , 400) , col=color , xlab="length (mm)" , ylab="posterior density" , cex.lab=1.5)
        # for ( i in 1:100 ) {
        #      curve(dgamma(x , shape=frogs[i] , scale=post$scale[i])  , add=TRUE ,  col=col.alpha("black",0.1))
        # }
        shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
        scoops <- data_list$length[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail & data_list$astro==astro]
        points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
        abline(v=mean(exp(frogs)) , col=color , lty=1)
        title(main=graph_title)
        
}

PlotToolPost(model= ml3 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= ml3 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= ml3 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= ml3 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= ml3 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")


# #mt2
PlotToolPost <- function( model , almendra=0 , nerite=0 , herm_crab=0 , halloween=0 , river_snail=0 , color="black" , graph_title="words"){
        post <- extract.samples(model)
        frogs <- with(post , a + ba*almendra + bn*nerite + bhc*herm_crab + bgq*halloween + brs*river_snail)
        dens(exp(frogs) , xlim=c(0 , 150) , col=color , xlab="thickness (mm)" , ylab="")
        shade( density(exp(frogs)) , col=col.alpha(color,0.7) , lim=PI( exp(frogs)  , prob=0.9999 ))
        scoops <- data_list$thickness[data_list$almendra==almendra & data_list$nerite==nerite & data_list$herm_crab==herm_crab & data_list$halloween==halloween & data_list$river_snail==river_snail]
        points(scoops,rep(0, length(scoops)) , cex=0.7 , col=col.alpha(color,0.33) , pch=19)
        abline(v=mean(exp(frogs)) , col=color , lty=1)
        title(main=graph_title)
}
# 
PlotToolPost(model= mt2 , almendra=1 , color=palette[1] , graph_title= "Terminalia catappa")
PlotToolPost(model= mt2 , nerite=1 , color=palette[2] , graph_title= "Nerita sp.")
PlotToolPost(model= mt2 , herm_crab=1 , color=palette[3] , graph_title="Coenobita compressus")
PlotToolPost(model= mt2 , halloween=1 , color=palette[4] , graph_title="Gecarcinus quadratus")
PlotToolPost(model= mt2 , river_snail=1 , color=palette[5] , graph_title="Freshwater Snail")
##add to these cebus hand dimensions?



#######plot raw materials 
dens(dk$weight_g[dk$used_tool==1] , col="cornflowerblue" , xlim=c(0 , max(dk$weight_g)) , ylim=c(-.0001 , 0.003), lwd=3, xlab="weight (g)")
abline(v=median(dk$weight_g[dk$used_tool==1]) , col="cornflowerblue")
dens(dk$weight_g[dk$used_tool==0] , col="darkgrey" , add=TRUE, lwd=3)
abline(v=median(dk$weight_g[dk$used_tool==0]) , col="darkgrey")

scoops <- dk$weight_g[dk$used_tool==1]
points(scoops,rep(-.00004, length(scoops)) , cex=0.7 , col=col.alpha(acol = "cornflowerblue",0.33) , pch=19, add=T)
scoops <- dk$weight_g[dk$used_tool==0]
points(scoops,rep(-.00008, length(scoops)) , cex=0.7 , col=col.alpha(acol = "darkgrey",0.33) , pch=19, add=T)

legend(2700, .0028, legend=c("raw material", "used tools"),
       col=c("darkgrey", "cornflowerblue"), lty=1, lwd=3, cex=0.8)


#plot length, width, thickness
dens(dk$length_mm_max[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$length_mm_max)))
dens(dk$length_mm_max[dk$used_tool==0] , col="darkgrey" , add=TRUE)

dens(dk$width_mm_max[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$width_mm_max)))
dens(dk$width_mm_max[dk$used_tool==0] , col="darkgrey" , add=TRUE)

dens(dk$thickness[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$thickness)))
dens(dk$thickness[dk$used_tool==0] , col="darkgrey" , add=TRUE)

#####plot separately for resources
dens(dk$weight_g[dk$used_tool==0] , col="darkgrey" , xlim=c(0 , 3100) , ylim=c(-.0001 , 0.0041), ylab="weight(g)", lty="twodash")
dens(dk$weight_g[dk$debris_at_site_almendra==1] , col=palette[1] , add=TRUE)
dens(dk$weight_g[dk$debris_at_site_hermit_crabs==1], col=palette[3] , add=TRUE)
dens(dk$weight_g[dk$debris_at_site_river_snail==1] , col=palette[5] , add=TRUE)

pta <- dk$weight_g[dk$debris_at_site_almendra==1]
points(pta,rep(0, length(pta)) , cex=0.7 , col=col.alpha(palette[1],0.33) , pch=19, add=T)
pthc <- dk$weight_g[dk$debris_at_site_hermit_crabs==1]
points(pthc,rep(0, length(pthc)) , cex=0.7 , col=col.alpha(palette[3],0.33) , pch=19, add=T)
ptrs <- dk$weight_g[dk$debris_at_site_river_snail==1]
points(ptrs,rep(0, length(ptrs)) , cex=0.7 , col=col.alpha(palette[5],0.33) , pch=19, add=T)



######raw
post <- extract.samples(m_raw_tc)

pdf(file="carlson_ch1_selectivity/r_plots/weight_tc_raw.pdf" )
     dens(dl_raw_tc_comp$weight_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$weight_raw)) , ylim=c(-.0001 , 0.003), lwd=1, xlab="weight (g)" )
     abline(v=mean(dl_raw_tc_comp$weight_tc) , col="cornflowerblue")
     dens(dl_raw_tc_comp$weight_raw , col="darkgrey" , add=TRUE, lwd=1)
     abline(v=mean(dl_raw_tc_comp$weight_raw) , col="darkgrey")
     
     sharks <- mean(exp(post$a_tc))
     stingrays <- HPDI(exp(post$a_tc))
     points(sharks , 0.003 , col="cornflowerblue" , pch=1 , cex=0.75)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
     #raw below
     sharks <- mean(exp(post$a_raw))
     stingrays <- HPDI(exp(post$a_raw))
     points(sharks , 0.003 , col="darkgrey" , pch=1 , cex=0.75)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
     #posterior samples for raw
     for ( i in 1:200 ) {
          curve(dgamma2(x , mu=exp(post$a_raw[i]) , scale=post$scale_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
     }
     #posterior samples for tc
     for ( i in 1:200 ) {
          curve(dgamma2(x , mu=exp(post$a_tc[i]) , scale=post$scale_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
     }
     #plot raw data
     points(dl_raw_tc_comp$weight_tc , rep(-.00005, length(dl_raw_tc_comp$weight_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
     points(dl_raw_tc_comp$weight_raw , rep(-.00015, length(dl_raw_tc_comp$weight_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
     #legend
     legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
            fill=c("darkgrey" , "cornflowerblue") , bty='n' )
dev.off()
