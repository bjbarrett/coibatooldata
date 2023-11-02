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

#plot weight, separated by island 
#Jic
dens(dk_c$weight_g[dk_c$used_tool==0] , col="darkgrey" , xlim=c(0 , max(dk$weight_g)) , ylim=c(-.0001 , 0.003), lwd=3, xlab="weight (g)")
dens(dk_c$weight_g[dk_c$used_tool==1], col="cornflowerblue", add=T, lwd=3)

#coiba
dens(dk_j$weight_g[dk_j$used_tool==0] , col="dimgrey" , xlim=c(0 , max(dk$weight_g)) , ylim=c(-.0001 , 0.003), lwd=3, xlab="weight (g)")
dens(dk_j$weight_g[dk_j$used_tool==1], col="darkblue", add=T, lwd=3)


#plot length, width, thickness
dens(dk$length_mm_max[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$length_mm_max)))
dens(dk$length_mm_max[dk$used_tool==0] , col="darkgrey" , add=TRUE)

dens(dk$width_mm_max[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$width_mm_max)))
dens(dk$width_mm_max[dk$used_tool==0] , col="darkgrey" , add=TRUE)

dens(dk$thickness[dk$used_tool==1] , col="red" , xlim=c(0 , max(dk$thickness)))
dens(dk$thickness[dk$used_tool==0] , col="darkgrey" , add=TRUE)



#####plot separately for resources, by island (JICARÓN)
par(mfrow = c(2, 2), font.main=1, cex.main=1, mar= c(1,4,5,1))

dens(dk_j$weight_g[dk_j$used_tool==0] , col="darkgrey" , xlim=c(0 , 3100) , ylim=c(-.00012 , 0.0065), lty="dotdash", lwd=3, main="raw material", xlab="weight (g)")
abline(v=median(dk_j$weight_g[dk_j$used_tool==0]) , col="darkgrey")
prm <- dk_j$weight_g[dk_j$used_tool==0]
points(prm, rep(-.0001, length(prm)) , cex=0.7 , col=col.alpha("darkgrey",0.33) , pch=19, add=T)

dens(dk_j$weight_g[dk_j$debris_at_site_almendra==1] , col=palette[1], lwd=3, xlim=c(0 , 3100), ylim=c(-.00012 , 0.0065), main="sea almond", xlab="weight (g)" )
abline(v=median(dk_j$weight_g[dk_j$debris_at_site_almendra==1]) , col=palette[1])
pta <- dk_j$weight_g[dk_j$debris_at_site_almendra==1]
points(pta,rep(-.0001, length(pta)) , cex=0.7 , col=col.alpha(palette[1],0.33) , pch=19, add=T)

dens(dk_j$weight_g[dk_j$debris_at_site_hermit_crabs==1], col=palette[3], lwd=3, xlim=c(0 , 3100) , ylim=c(-.00012 , 0.0065), main="hermit crab", xlab="weight (g)")
abline(v=median(dk_j$weight_g[dk_j$debris_at_site_hermit_crabs==1]) , col=palette[3])
pthc <- dk_j$weight_g[dk_j$debris_at_site_hermit_crabs==1]
points(pthc,rep(-.0001, length(pthc)) , cex=0.7 , col=col.alpha(palette[3],0.33) , pch=19, add=T)


dens(dk_j$weight_g[dk_j$debris_at_site_marine_snail==1] , col=palette[5], lwd=3, xlim=c(0 , 3100), ylim=c(-.00012 , 0.0065), main="marine snail" , xlab="weight (g)")
abline(v=median(dk_j$weight_g[dk_j$debris_at_site_marine_snail==1]) , col=palette[5])
ptms <- dk_j$weight_g[dk_j$debris_at_site_marine_snail==1]
points(ptms,rep(-.0001, length(ptms)) , cex=0.7 , col=col.alpha(palette[5],0.33) , pch=19, add=T)



#####plot separately for resources, by island (COIBA)
# par(mfrow = c(2, 2), font.main=1, cex.main=1, mar= c(1,4,5,1))
# 
# dens(dk_c$weight_g[dk_c$used_tool==0] , col="darkgrey" , xlim=c(0 , 3100) , ylim=c(-.00012 , 0.0065), lty="dash", lwd=3, main="raw material", xlab="weight (g)")
# prm_c <- dk_c$weight_g[dk_c$used_tool==0]
# points(prm_c, rep(-.0001, length(prm)) , cex=0.7 , col=col.alpha("darkgrey",0.33) , pch=19, add=T)
# 
# dens(dk_tools$weight_g[dk_tools$astro==1] , col=palette[2], lwd=3, xlim=c(0 , 3100), ylim=c(-.00012 , 0.0065), main="Astrocaryum", xlab="weight (g)" )
# ptas <- dk_tools$weight_g[dk_tools$astro==1]
# points(ptas,rep(-.0001, length(ptas)) , cex=0.7 , col=col.alpha(palette[2],0.33) , pch=19, add=T)

######raw
post <- extract.samples(m_raw_tc)

pdf(file="carlson_ch1_selectivity/r_plots/weight_tc_raw.pdf" )
dens(dl_raw_tc_comp$weight_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$weight_raw)) , ylim=c(-.0001 , 0.003), lwd=1, xlab="weight (g)" )
abline(v=mean(dl_raw_tc_comp$weight_tc) , col="cornflowerblue")
dens(dl_raw_tc_comp$weight_raw , col="darkgrey" , add=TRUE, lwd=1)
abline(v=mean(dl_raw_tc_comp$weight_raw) , col="darkgrey")

sharks <- mean(exp(post$w_tc))
stingrays <- HPDI(exp(post$w_tc))
points(sharks , 0.003 , col="cornflowerblue" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
#raw below
sharks <- mean(exp(post$w_raw))
stingrays <- HPDI(exp(post$w_raw))
points(sharks , 0.003 , col="darkgrey" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
#posterior samples for raw
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$w_raw[i]) , scale=post$scale_w_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
}
#posterior samples for tc
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$w_tc[i]) , scale=post$scale_w_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(dl_raw_tc_comp$weight_tc , rep(-.00005, length(dl_raw_tc_comp$weight_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
points(dl_raw_tc_comp$weight_raw , rep(-.00015, length(dl_raw_tc_comp$weight_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
#legend
legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
       fill=c("darkgrey" , "cornflowerblue") , bty='n' )
dev.off()


###thickness raw avail
post <- extract.samples(m_raw_tc3)

#run below do this and plot model code to see implied prior preds
# post <- extract.prior(m_raw_tc2) 

pdf(file="carlson_ch1_selectivity/r_plots/thickness_tc_raw.pdf" )
dens(dl_raw_tc_comp$thickness_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$thickness_raw)) , ylim=c(-.001 , 0.03), lwd=1, xlab="thickness (mm)" )
abline(v=mean(dl_raw_tc_comp$thickness_tc) , col="cornflowerblue")
dens(dl_raw_tc_comp$thickness_raw , col="darkgrey" , add=TRUE, lwd=1)
abline(v=mean(dl_raw_tc_comp$thickness_raw) , col="darkgrey")

sharks <- mean(exp(post$t_tc))
stingrays <- HPDI(exp(post$t_tc))
points(sharks , 0.003 , col="cornflowerblue" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
#raw below
sharks <- mean(exp(post$t_raw))
stingrays <- HPDI(exp(post$t_raw))
points(sharks , 0.003 , col="darkgrey" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
#posterior samples for raw
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$t_raw[i]) , scale=post$scale_t_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
}
#posterior samples for tc
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$t_tc[i]) , scale=post$scale_t_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(dl_raw_tc_comp$thickness_tc , rep(-.0005, length(dl_raw_tc_comp$thickness_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
points(dl_raw_tc_comp$thickness_raw , rep(-.0015, length(dl_raw_tc_comp$thickness_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
#legend
legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
       fill=c("darkgrey" , "cornflowerblue") , bty='n' )
dev.off()


pdf(file="carlson_ch1_selectivity/r_plots/length_tc_raw.pdf" )
dens(dl_raw_tc_comp$length_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$length_raw)) , ylim=c(-.001 , 0.015), lwd=1, xlab="length (mm)" )
abline(v=mean(dl_raw_tc_comp$length_tc) , col="cornflowerblue")
dens(dl_raw_tc_comp$length_raw , col="darkgrey" , add=TRUE, lwd=1)
abline(v=mean(dl_raw_tc_comp$length_raw) , col="darkgrey")

sharks <- mean(exp(post$l_tc))
stingrays <- HPDI(exp(post$l_tc))
points(sharks , 0.003 , col="cornflowerblue" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
#raw below
sharks <- mean(exp(post$l_raw))
stingrays <- HPDI(exp(post$l_raw))
points(sharks , 0.003 , col="darkgrey" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
#posterior samples for raw
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$l_raw[i]) , scale=post$scale_l_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
}
#posterior samples for tc
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$l_tc[i]) , scale=post$scale_l_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(dl_raw_tc_comp$length_tc , rep(-.0005, length(dl_raw_tc_comp$length_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
points(dl_raw_tc_comp$length_raw , rep(-.0015, length(dl_raw_tc_comp$length_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
#legend
legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
       fill=c("darkgrey" , "cornflowerblue") , bty='n' )
dev.off()


##width
pdf(file="carlson_ch1_selectivity/r_plots/width_tc_raw.pdf" )
dens(dl_raw_tc_comp$width_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$width_raw)) , ylim=c(-.001 , 0.02), lwd=1, xlab="width (mm)" )
abline(v=mean(dl_raw_tc_comp$width_tc) , col="cornflowerblue")
dens(dl_raw_tc_comp$width_raw , col="darkgrey" , add=TRUE, lwd=1)
abline(v=mean(dl_raw_tc_comp$width_raw) , col="darkgrey")

sharks <- mean(exp(post$l_tc))
stingrays <- HPDI(exp(post$wd_tc))
points(sharks , 0.003 , col="cornflowerblue" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
#raw below
sharks <- mean(exp(post$wd_raw))
stingrays <- HPDI(exp(post$wd_raw))
points(sharks , 0.003 , col="darkgrey" , pch=1 , cex=0.75)
segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
#posterior samples for raw
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$wd_raw[i]) , scale=post$scale_wd_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
}
#posterior samples for tc
for ( i in 1:200 ) {
     curve(dgamma2(x , mu=exp(post$wd_tc[i]) , scale=post$scale_wd_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
}
#plot raw data
points(dl_raw_tc_comp$width_tc , rep(-.0005, length(dl_raw_tc_comp$width_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
points(dl_raw_tc_comp$width_raw , rep(-.0015, length(dl_raw_tc_comp$width_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
#legend
legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
       fill=c("darkgrey" , "cornflowerblue") , bty='n' )
dev.off()


###MULTIPANEL
post <- extract.samples(m_raw_tc3)
pdf(file="carlson_ch1_selectivity/r_plots/mutlipanel_dims_tc_raw.pdf" , height=8 , width=8)
     
     par(mfrow = c(2, 2))
     par(cex = 0.9)
     par(mar = c(2, 0, 1, 0)+ 0.25, oma = c(3, 1, 1, 1))
     
     ### weight
     dens(dl_raw_tc_comp$weight_tc , col="white" , xlim=c(0 , max(dl_raw_tc_comp$weight_raw)) , ylim=c(-.0001 , 0.003), lwd=1 , yaxt="n" , ylab="", xlab="")
     title("a. weight (g)")
     # dens(dl_raw_tc_comp$weight_tc , col="cornflowerblue" , xlim=c(0 , max(dl_raw_tc_comp$weight_raw)) , ylim=c(-.0001 , 0.003), lwd=1, xlab="weight (g)" , yaxt="n" , ylab="")
     # abline(v=mean(dl_raw_tc_comp$weight_tc) , col="cornflowerblue")
     # dens(dl_raw_tc_comp$weight_raw , col="darkgrey" , add=TRUE, lwd=1)
     # abline(v=mean(dl_raw_tc_comp$weight_raw) , col="darkgrey")
     
     sharks <- mean(exp(post$w_tc))
     stingrays <- HPDI(exp(post$w_tc))
     points(sharks , 0.0003 , col="cornflowerblue" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.0003 , y1=0.0003 , col="cornflowerblue")
     #raw below
     sharks <- mean(exp(post$w_raw))
     stingrays <- HPDI(exp(post$w_raw))
     points(sharks , 0.0003 , col="darkgrey" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.0003 , y1=0.0003 , col="darkgrey")
     #posterior samples for raw
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$w_raw[i]) , scale=post$scale_w_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
     }
     #posterior samples for tc
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$w_tc[i]) , scale=post$scale_w_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
     }
     #plot raw data
     points(dl_raw_tc_comp$weight_tc , rep(-.00005, length(dl_raw_tc_comp$weight_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
     points(dl_raw_tc_comp$weight_raw , rep(-.00015, length(dl_raw_tc_comp$weight_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
     #legend
     # legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
     #        fill=c("darkgrey" , "cornflowerblue") , bty='n' )
     
     ##thicknesss
     dens(dl_raw_tc_comp$thickness_tc , col="white" , xlim=c(0 , max(dk$thickness) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="" ,ylab="" , yaxt="n")
     title("b. thickness (mm)")
     
     # dens(dl_raw_tc_comp$thickness_tc , col="cornflowerblue" , xlim=c(0 , max(dk$length_mm_max) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="thickness (mm)"  , yaxt="n" , ylab="")
     # abline(v=mean(dl_raw_tc_comp$thickness_tc) , col="cornflowerblue")
     # dens(dl_raw_tc_comp$thickness_raw , col="darkgrey" , add=TRUE, lwd=1)
     # abline(v=mean(dl_raw_tc_comp$thickness_raw) , col="darkgrey")
     
     sharks <- mean(exp(post$t_tc))
     stingrays <- HPDI(exp(post$t_tc))
     points(sharks , 0.003 , col="cornflowerblue" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
     #raw below
     sharks <- mean(exp(post$t_raw))
     stingrays <- HPDI(exp(post$t_raw))
     points(sharks , 0.003 , col="darkgrey" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
     #posterior samples for raw
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$t_raw[i]) , scale=post$scale_t_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
     }
     #posterior samples for tc
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$t_tc[i]) , scale=post$scale_t_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
     }
     #plot raw data
     points(dl_raw_tc_comp$thickness_tc , rep(-.0005, length(dl_raw_tc_comp$thickness_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
     points(dl_raw_tc_comp$thickness_raw , rep(-.0015, length(dl_raw_tc_comp$thickness_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
     #legend
     # legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
     #        fill=c("darkgrey" , "cornflowerblue") , bty='n' )
     
     #l##########length
     dens(dl_raw_tc_comp$length_tc , col="white" , xlim=c(0 , max(dk$length_mm_max) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="" , yaxt="n" , ylab="")
     title("c. length (mm)")
     
     # dens(dl_raw_tc_comp$length_tc , col="cornflowerblue" , xlim=c(0 , max(dk$length_mm_max) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="length (mm)" , yaxt="n" , ylab="")
     # abline(v=mean(dl_raw_tc_comp$length_tc) , col="cornflowerblue")
     # dens(dl_raw_tc_comp$length_raw , col="darkgrey" , add=TRUE, lwd=1)
     # abline(v=mean(dl_raw_tc_comp$length_raw) , col="darkgrey")
     
     sharks <- mean(exp(post$l_tc))
     stingrays <- HPDI(exp(post$l_tc))
     points(sharks , 0.003 , col="cornflowerblue" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
     #raw below
     sharks <- mean(exp(post$l_raw))
     stingrays <- HPDI(exp(post$l_raw))
     points(sharks , 0.003 , col="darkgrey" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
     #posterior samples for raw
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$l_raw[i]) , scale=post$scale_l_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
     }
     #posterior samples for tc
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$l_tc[i]) , scale=post$scale_l_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
     }
     #plot raw data
     points(dl_raw_tc_comp$length_tc , rep(-.0005, length(dl_raw_tc_comp$length_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
     points(dl_raw_tc_comp$length_raw , rep(-.0015, length(dl_raw_tc_comp$length_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
     #legend
     # legend("topright" , legend=c("raw material" , "T. catappa hammerstones") , 
     #        fill=c("darkgrey" , "cornflowerblue") , bty='n' )
     
     ##width
     dens(dl_raw_tc_comp$width_tc , col="white" , xlim=c(0 , max(dk$width_mm_max) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="width (mm)" , yaxt="n" , ylab="" )
     title("d. width (mm)")
     # dens(dl_raw_tc_comp$width_tc , col="cornflowerblue" , xlim=c(0 , max(dk$length_mm_max) ) , ylim=c(-.001 , 0.03), lwd=1, xlab="width (mm)" , yaxt="n" , ylab="" )
     # abline(v=mean(dl_raw_tc_comp$width_tc) , col="cornflowerblue")
     # dens(dl_raw_tc_comp$width_raw , col="darkgrey" , add=TRUE, lwd=1)
     # abline(v=mean(dl_raw_tc_comp$width_raw) , col="darkgrey")
     
     sharks <- mean(exp(post$wd_tc))
     stingrays <- HPDI(exp(post$wd_tc))
     points(sharks , 0.003 , col="cornflowerblue" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="cornflowerblue")
     #raw below
     sharks <- mean(exp(post$wd_raw))
     stingrays <- HPDI(exp(post$wd_raw))
     points(sharks , 0.003 , col="darkgrey" , pch=18 , cex=0.99)
     segments(x0=stingrays[[1]] , x1=stingrays[[2]] , y0=0.003 , y1=0.003 , col="darkgrey")
     #posterior samples for raw
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$wd_raw[i]) , scale=post$scale_wd_raw[i])  , add=TRUE ,  col=col.alpha("darkgrey",0.05))
     }
     #posterior samples for tc
     for ( i in 1:100 ) {
          curve(dgamma2(x , mu=exp(post$wd_tc[i]) , scale=post$scale_wd_tc[i])  , add=TRUE ,  col=col.alpha("cornflowerblue",0.05))
     }
     #plot raw data
     points(dl_raw_tc_comp$width_tc , rep(-.0005, length(dl_raw_tc_comp$width_tc) ) , pch="|"  , col=col.alpha("cornflowerblue",0.25) )
     points(dl_raw_tc_comp$width_raw , rep(-.0015, length(dl_raw_tc_comp$width_raw) ) , pch="|"  , col=col.alpha("darkgrey",0.25) )
     #legend
     par(xpd=TRUE)
     legend(-75,-.006, legend=c("raw material" , "T. catappa hammerstones") , 
            fill=c("darkgrey" , "cornflowerblue") , bty='n' , xpd=NA)


dev.off()

