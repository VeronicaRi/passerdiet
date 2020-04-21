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
#setwd("~/Dropbox/paserdiet/passerdiet/Output")
source("multiplot.R")
#source("plot_ancestral_states_2.R")
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

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[1]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))
multiplot(p1,p3.1,p5,p2,p4, cols=2)

############################################################################################
### Plots for diversification rates Bisse DP without diploidization~
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

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[3],cols[5]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[2],cols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))


multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)


##########
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

p3.2<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[7],cols[9],cols[1],cols[6]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(cols[4],cols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c("pink","green"))

multiplot(p1,p3.2,p5,p2,p4,p6, cols=2)
##########
