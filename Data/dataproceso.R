library(ape)
library(treeplyr)

setwd("~/passerdiet/passerdiet/Data/")

##cargando datos
paserdat= read.csv("datosdoc.csv")
pasertree<-read.nexus("Unarbol.nex")
head(paserdat)

##Uniendo nombres de arbol y datos
matched<-make.treedata(pasertree, paserdat, name_column = "Name")
write.nexus(matched$phy, file="pasertree.nex")


#Creando datos para revbayes... En text edit remplazar comillas y borrar primera linea
paserdat<-data.frame(matched$dat$frugivoro)
row.names(paserdat)<-matched$phy$tip.label
head(paserdat)
write.table(paserdat, file="paserdat.tsv", sep="\t")
