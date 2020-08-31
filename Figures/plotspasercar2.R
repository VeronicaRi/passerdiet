#biocLite("ggtree")
#biocLite("treeio")
library("ggplot2")
#library("RevGadgets")
library("wesanderson")

### Colors
cols<-c(wes_palette("Darjeeling1"),wes_palette("Darjeeling2"))
cols2<- c("#7b3294","#c2a5cf","#a6dba0","#008837","#ffffbf")
cols3<-c("#ece2f0", "#67a9cf","#e31a1c","#fd8d3c","#02818a","#014636")
#sampling<-seq(1,250000,100)
### Plots for diversification rates Bisse for absence/ presence of carotenoids
setwd("/Users/veronicari/Documentos/GitHub/passerdiet/Output")
source("multiplot.R")
setwd("/Users/veronicari/Documentos/GitHub/passerdiet/Figures")
source("plot_ancestral_states_2.R")


#############################
######## CAROTENOIDS ########
#############################

output.sse<-read.table("BiSSE_passercar.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("Absence Carotenoids","Presence "),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("Absence carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q01","q10"),each=length(output.sse$rate_1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[1]))

p3.00<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))
multiplot(p1,p3.00,p5,p2,p4, cols=2)

############################################################################################
#####Independet characters for carotenoids####

output.sse<-read.table("CID_HiSSE_car.log", header=TRUE)
# States 1=0A, 2=1A, 3=0B, 4=1B
# For CID-2 the assumption is that 0A=1A and 0B=1B  which means that 1=2 and 3=4

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q_01","q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))


p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[3],cols[5]))

p3.0<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))


multiplot(p1,p3.0,p5,p2,p4,p6, cols=2)



############################################################################################
### Plots for diversification rates Hisse carotenoids
# 1=0A= absence carotenoids A 2=1A= presence carotenoids A, 3=0B= absence carotenoids B, 4=1B presence carotenoids B
# Remember the order is always all the A's first and then all the B's

output.sse<-read.table("HiSSE_carotenoids.log", header=TRUE)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21) ,Type=rep(c("q_01", "q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))



p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))

multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)
##########




#############################
######## DIET ##########
#############################

output.sse<-read.table("BiSSE_passerdiet.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q01","q10"),each=length(output.sse$rate_1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[1]))

p3.2<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))
multiplot(p1,p3.2,p5,p2,p4, cols=2)

############################################################################################
 ###
output.sse<-read.table("CID_HiSSE_diet.log", header=TRUE)
# States 1=0A, 2=1A, 3=0B, 4=1B
# For CID-2 the assumption is that 0A=1A and 0B=1B  which means that 1=2 and 3=4
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q_01","q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[3],cols[5]))

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))


multiplot(p1,p3.3,p5,p2,p4,p6, cols=2)


##########
############################################################################################
### Plots for diversification rates Hisse Diet
# 1=0A= No frugivorous A; 2=1A= Frugivorous A; 3=0B= No frugivorous B; 4=1B Frugivorous B
# Remember the order is always all the A's first and then all the B's

output.sse<-read.table("HiSSE_diet.log", header=TRUE)
str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21) ,Type=rep(c("q_01", "q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))



p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[7],cols[9],cols[1],cols[6]))

p3.4<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))

multiplot(p1,p3.4,p5,p2,p4,p6, cols=2)
##########



#####################################
######## CAROTENOIDS & DIET #########
#####################################

############################################################################################~
### Plots for diversification rates Musse 
output.sse<-read.table("MuSSE_dietncar.log", header=TRUE)
# States 1=F0 C0 A; 2=F1 C0 A; 3= F1 C1 A; 4= F0 C1 A

str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1.,output.sse$extinction.2.,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("F0 C0 ","F1 C0", "F1 C1", "F0 C1"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1.,output.sse$speciation.2.,output.sse$speciation.3.,output.sse$speciation.4.),Type=rep(c("F0 C0 ","F1 C0", "F1 C1", "F0 C1"),each=length(output.sse$speciation.1.)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1.-output.sse$extinction.1.,output.sse$speciation.2.-output.sse$extinction.2., output.sse$speciation.3.-output.sse$extinction.3.,output.sse$speciation.4.-output.sse$extinction.4.),Type=rep(c("F0 C0 ","F1 C0", "F1 C1", "F0 C1"),each=length(output.sse$speciation.1.)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1./output.sse$speciation.1.,output.sse$extinction.2./output.sse$speciation.2.,output.sse$extinction.3./output.sse$speciation.3.,output.sse$extinction.4./output.sse$speciation.4.),Type=rep(c("F0 C0 ","F1 C0", "F1 C1", "F0 C1"),each=length(output.sse$speciation.1.)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_14,output.sse$rate_41,output.sse$rate_23,output.sse$rate_32,output.sse$rate_34,output.sse$rate_43) ,Type=rep(c("F0C0 to F1C0","F1C0 to F0C0", "F0C0 to F0C1", "F0C1 to F0C0", "F1C0 to F1C1", "F1C1 to F1C0", "F1C1 to F0C1", "F0C1 to F1C1" ),each=length(output.sse$rate_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4],cols[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4],cols[6]))

p3.5<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4],cols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols3[3],cols3[2],cols2[4],cols[6]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(cols[2],cols[4],"pink",cols2[5],cols3[3],cols3[2],cols2[4],cols[6]))

multiplot(p1,p3.5,p5,p2,p4, cols=2) 



############################################################################################~
### Plots for diversification rates cidMuHiSSE 
output.sse<-read.table("cidMuHiSSE.log", header=TRUE)
str(output.sse)
# States 1=F0 C0 A; 2=F1 C0 A; 3= F1 C1 A; 4= F0 C1 A; 5=F0 C0 B; 6=F1 C0 B; 7= F1 C1 B; 8= F0 C1 B; 

sse.extinction<-data.frame(dens=c(output.sse$extinction.1, output.sse$extinction.5),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.5-output.sse$extinction.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.5/output.sse$speciation.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_14,output.sse$rate_41,output.sse$rate_23,output.sse$rate_32,output.sse$rate_34,output.sse$rate_43) ,Type=rep(c("F0C0 to F1C0","F1C0 to F0C0", "F0C0 to F0C1", "F0C1 to F0C0", "F1C0 to F1C1", "F1C1 to F1C0", "F1C1 to F0C1", "F0C1 to F1C1" ),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p3.6<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5],cols3[3],cols3[2],cols2[4],cols[6]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))

multiplot(p1,p3.6,p5,p2,p4,p6, cols=2)

########

############################################################################################~
### Plots for diversification rates MuHiSSE DP and breeding systems no diploidization ~/Dropbox/solploidypersonal/muhisse250k/output/MuHiSSE_ploidysi250K.log
output.sse<-read.table("MuHiSSE_dietncar.log", header=TRUE)
# States 1=F0 C0 A; 2=F1 C0 A; 3= F1 C1 A; 4= F0 C1 A; 5=F0 C0 B; 6=F1 C0 B; 7= F1 C1 B; 8= F0 C1 B; 


str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1.,output.sse$extinction.2.,output.sse$extinction.3.,output.sse$extinction.4.,output.sse$extinction.5.,output.sse$extinction.6.,output.sse$extinction.7.,output.sse$extinction.8.),Type=rep(c("F0C0 A","F1C0 A", "F1C1 A", "F0C1 A","F0C0 B","F1C0 B", "F1C1 B", "F0C1 B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1.,output.sse$speciation.2.,output.sse$speciation.3.,output.sse$speciation.4.,output.sse$speciation.5.,output.sse$speciation.6.,output.sse$speciation.7.,output.sse$speciation.8.),Type=rep(c("F0C0 A","F1C0 A", "F1C1 A", "F0C1 A","F0C0 B","F1C0 B", "F1C1 B", "F0C1 B"),each=length(output.sse$speciation.1.)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1.-output.sse$extinction.1.,output.sse$speciation.2.-output.sse$extinction.2., output.sse$speciation.3.-output.sse$extinction.3.,output.sse$speciation.4.-output.sse$extinction.4.,output.sse$speciation.5.-output.sse$extinction.5.,output.sse$speciation.6.-output.sse$extinction.6., output.sse$speciation.7.-output.sse$extinction.7.,output.sse$speciation.8.-output.sse$extinction.8.),Type=rep(c("F0C0 A","F1C0 A", "F1C1 A", "F0C1 A","F0C0 B","F1C0 B", "F1C1 B", "F0C1 B"),each=length(output.sse$speciation.1.)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1./output.sse$speciation.1.,output.sse$extinction.2./output.sse$speciation.2.,output.sse$extinction.3./output.sse$speciation.3.,output.sse$extinction.4./output.sse$speciation.4.,output.sse$extinction.5./output.sse$speciation.5.,output.sse$extinction.6./output.sse$speciation.6.,output.sse$extinction.7./output.sse$speciation.7.,output.sse$extinction.8./output.sse$speciation.8.),Type=rep(c("F0C0 A","F1C0 A", "F1C1 A", "F0C1 A","F0C0 B","F1C0 B", "F1C1 B", "F0C1 B"),each=length(output.sse$speciation.1.)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_14,output.sse$rate_41,output.sse$rate_23,output.sse$rate_32,output.sse$rate_34,output.sse$rate_43) ,Type=rep(c("F0C0 to F1C0","F1C0 to F0C0", "F0C0 to F0C1", "F0C1 to F0C0", "F1C0 to F1C1", "F1C1 to F1C0", "F1C1 to F0C1", "F0C1 to F1C1" ),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3],cols[4],cols2[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3],cols[4],cols2[6]))

p3.7<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3],cols[4],cols2[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[1],cols[6],cols[7],cols[9],cols2[4],cols2[3],cols[4],cols2[6]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4],"pink",cols2[5],cols3[3],cols3[2],cols2[4],cols[6]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))

multiplot(p1,p3.7,p5,p2,p4,p6, cols=2)

########
##netdiversification
multiplot(p3.2, p3.3, p3.4, p3.00, p3.0, p3.1, p3.5, p3.6, p3.7, cols=3)

multiplot(p3.00, p3.2, p3.5, p3.0, p3.3, p3.6, p3.1, p3.4, p3.7, cols=3)



###INTENANDO HACER RECONSTRUCCIÓN ANCESTRAL

library(RevGadgets)
library(ggplot2)

