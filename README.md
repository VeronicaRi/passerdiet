# passerdiet
Dieta y coloraci??n paseriformes
arbol<-read.nexus("passercompleto.nex")

summary(arbol)

arbol$tree_9082

unarbol<-arbol$tree_9082

write.nexus(unarbol,file="Unarbol.nex")
