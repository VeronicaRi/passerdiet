library(ape)
library(treeplyr)

setwd("~/passerdiet/passerdiet/Data/")

####Arbol
#arboles <- read.nexus("newtrees.nex")
#arbol <- maxCladeCred(arboles, rooted=F)
#setdiff(arbol$tip.label, as.character(DatosArt$Especie))
#setdiff(as.character(tamas$Especie), arbol$tip.label)
#setdiff(arbol$tip.label, as.character(tamas$Especie))
#write.nexus(arbol, file="ArtTree.tre", translate=T)


##cargando datos
paserdat= read.csv("datosdoc.csv")
pasertree<-read.nexus("passertree.nex")
str(paserdat)


##Uniendo nombres de arbol y datos
setdiff(paserdat$Name, pasertree$tip.label)
setdiff(pasertree$tip.label, paserdat$Name)
matched<-make.treedata(pasertree, paserdat, name_column = "Name")
write.nexus(matched$phy, file="pasertree.nex")


#Creando datos para revbayes... En text edit buscar->buscar y remplazar -> comillas y borrar primera linea
paserdat<-data.frame(matched$dat$frugivoro)
row.names(paserdat)<-matched$phy$tip.label
head(paserdat)
write.table(paserdat, file="paserdat.tsv", sep="\t")


####Ahora para carotenoides
pasercar<-data.frame(matched$dat$carotenoide)
row.names(pasercar)<-matched$phy$tip.label
head(pasercar)
str(pasercar)
write.table(pasercar, file="pasercar.tsv", sep="\t")
