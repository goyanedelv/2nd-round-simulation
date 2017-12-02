library(ggplot2)
library(gridExtra)

#Votos emitidos por candidatos

pinera=2417216
guillier=1497118
sanchez=1336824
jakast=523213
goic=387780
meo=376471
artes=33690
navarro=24019
blanco=39397
nulo=65020

#Transferencias de Criteria y Cadem
pinera_pinera=c(1,0.931)
pinera_guillier=c(0,0.021)

guillier_pinera=c(0,0)
guillier_guillier=c(1,0.962)

sanchez_pinera=c(0.09,0.078)
sanchez_guillier=c(0.77,0.691)

jakast_pinera=c(0.75,0.777)
jakast_guillier=c(0.06,0.05)

goic_pinera=c(0.19,0.22)
goic_guillier=c(0.63,0.493)

meo_pinera=c(0.42,0.096)
meo_guillier=c(0.47,0.698)

artes_pinera=c(0,0)
artes_guillier=c(0,0.794)

navarro_pinera=c(0,0)
navarro_guillier=c(0,0.292)

#Vectores de Transferencias
mu_p=c(mean(pinera_pinera),mean(guillier_pinera),mean(sanchez_pinera),mean(jakast_pinera),
	mean(goic_pinera),mean(meo_pinera),mean(artes_pinera),mean(navarro_pinera))
mu_g=c(mean(pinera_guillier),mean(guillier_guillier),mean(sanchez_guillier),mean(jakast_guillier),
	mean(goic_guillier),mean(meo_guillier),mean(artes_guillier),mean(navarro_guillier))

sd_p=c(sd(pinera_pinera),sd(guillier_pinera),sd(sanchez_pinera),sd(jakast_pinera),
	sd(goic_pinera),sd(meo_pinera),sd(artes_pinera),sd(navarro_pinera))
sd_g=c(sd(pinera_guillier),sd(guillier_guillier),sd(sanchez_guillier),sd(jakast_guillier),
	sd(goic_guillier),sd(meo_guillier),sd(artes_guillier),sd(navarro_guillier))


n=10000

df=data.frame(Pinera=c(runif(n,0)),
			Guillier=c(runif(n,0)),Total=c(runif(n,0)),percPinera=c(runif(n,0)),
			percGuillier=c(runif(n,0)))

for(i in 1:n){

	df$Pinera[i]=round(pinera*rnorm(1,mu_p[1],sd_p[1])+
					guillier*rnorm(1,mu_p[2],sd_p[2])+
					sanchez*rnorm(1,mu_p[3],sd_p[3])+
					jakast*rnorm(1,mu_p[4],sd_p[4])+
					goic*rnorm(1,mu_p[5],sd_p[5])+
					meo*rnorm(1,mu_p[6],sd_p[6])+
					artes*rnorm(1,mu_p[7],sd_p[7])+
					navarro*rnorm(1,mu_p[8],sd_p[8]),digits=0)

	df$Guillier[i]=round(pinera*rnorm(1,mu_g[1],sd_g[1])+
					guillier*rnorm(1,mu_g[2],sd_g[2])+
					sanchez*rnorm(1,mu_g[3],sd_g[3])+
					jakast*rnorm(1,mu_g[4],sd_g[4])+
					goic*rnorm(1,mu_g[5],sd_g[5])+
					meo*rnorm(1,mu_g[6],sd_g[6])+
					artes*rnorm(1,mu_g[7],sd_g[7])+
					navarro*rnorm(1,mu_g[8],sd_g[8]),digits=0)

	df$Total[i]=df$Guillier[i]+df$Pinera[i]
	df$percPinera[i]=df$Pinera[i]/df$Total[i]
	df$percGuillier[i]=df$Guillier[i]/df$Total[i]
}

	df$Diferencia=abs(df$Pinera-df$Guillier)
gana_pin=subset(df,df$Pinera>df$Guillier)
n_gana_pinera=nrow(gana_pin)
n_gana_pinera
rm(gana_pin)

q2=qplot(df$percPinera, geom="histogram", binwidth = 0.005, main = "10,000 simulaciones", 
	xlab = "",ylab="Piñera",  
    fill=I("navy"),
    xlim=c(0.4,0.6),ylim=c(0,1700))+

  geom_segment(aes(x =mean(df$percPinera), y = 0, xend = mean(df$percPinera), yend = 1500),
  linetype="dashed",  color="navy")+
  annotate("text", x=mean(df$percPinera), y=1600, label= round(mean(df$percPinera)*100,2),
            color="navy",size=7)

q3=qplot(df$percGuillier, geom="histogram", binwidth = 0.005, main = "", 
	xlab = "",ylab="Guillier",  
    fill=I("brown3"),
    xlim=c(0.4,0.6),ylim=c(0,1700))+

  geom_segment(aes(x =mean(df$percGuillier), y = 0, xend = mean(df$percGuillier), yend = 1500),
  linetype="dashed",  color="brown3")+
  annotate("text", x=mean(df$percGuillier), y=1600, label= round(mean(df$percGuillier)*100,2),
            color="brown3",size=7)


grid.arrange(q2,q3,ncol=1,nrow=2,bottom="Proporción de votos en Segunda Vuelta",left="Escenarios generados")

#############
#############
#Graficando la transferencia (Cadem) con Sankey de googleVis
library(googleVis)

transfers=c(pinera*0.931,
	pinera*0.021,
	pinera*0,
	pinera*0,

	guillier*0,
	guillier*0.962,
	guillier*0,
	guillier*0.012,

	sanchez*0.078,
	sanchez*0.691,
	sanchez*0.05,
	sanchez*0.125,

	jakast*0.777,
	jakast*0.05,
	jakast*0.011,
	jakast*0.084,

	goic*0.22,
	goic*0.493,
	goic*0,
	goic*0.211,

	meo*0.096,
	meo*0.698,
	meo*0,
	meo*0.14,

	artes*0,
	artes*0.794,
	artes*0,
	artes*0.206,

	navarro*0,
	navarro*0.292,
	navarro*0,
	navarro*0,

	(nulo+blanco)*0.093,
	(nulo+blanco)*0.076,
	(nulo+blanco)*0.552,
	(nulo+blanco)*0.127)

transfers=round(transfers,0)

niveles_to=4

dat <- data.frame(From=c(rep("Pinera (1)",niveles_to),
						 rep("Guillier (1)", niveles_to),
						 rep("Sanchez", niveles_to),
						 rep("JA Kast", niveles_to),
						 rep("Goic", niveles_to),
						 rep("ME-O", niveles_to),
						 rep("Artes", niveles_to),
						 rep("Navarro", niveles_to),
						 rep("Nulo/Blanco", niveles_to)), 
                  To=rep(c("Pinera (2)", "Guillier (2)", "Nulo/Blanco (2)","No participara"),9), 
                  Transferencia=transfers)

sk1 <- gvisSankey(dat, from="From", to="To", weight="Transferencia")
plot(sk1)
setwd("C:/Users/Gonzalo/Desktop/GitHub/Sankey-transfer")
saveWidget(lala, "index.html")

sk2 <- gvisSankey(dat, from="From", to="To", weight="Weight",
                options=list(sankey="{link: {color: { fill: '#d799ae' } },
                                     node: { color: { fill: '#a61d4c' },
                                     label: { color: '#871b47' } }}"))
plot(sk2)


#############
#############
#Versión Bayesiana (in progress)


n=10

df=data.frame(Pinera=rep(NA,n),Guillier=rep(NA,n),Total=rep(NA,n),percPinera=rep(NA,n),percGuillier=c(runif(n,0)))
dfmu=data.frame(
	p_p=rep(NA,n),p_g=rep(NA,n),p_s=rep(NA,n),p_k=rep(NA,n),p_gc=rep(NA,n),p_m=rep(NA,n),p_a=rep(NA,n),p_n=rep(NA,n),
	g_p=rep(NA,n),g_g=rep(NA,n),g_s=rep(NA,n),g_k=rep(NA,n),g_gc=rep(NA,n),g_m=rep(NA,n),g_a=rep(NA,n),g_n=rep(NA,n))
dfsd=data.frame(
	p_p=rep(NA,n),p_g=rep(NA,n),p_s=rep(NA,n),p_k=rep(NA,n),p_gc=rep(NA,n),p_m=rep(NA,n),p_a=rep(NA,n),p_n=rep(NA,n),
	g_p=rep(NA,n),g_g=rep(NA,n),g_s=rep(NA,n),g_k=rep(NA,n),g_gc=rep(NA,n),g_m=rep(NA,n),g_a=rep(NA,n),g_n=rep(NA,n))



dfmu[1,]=c(mean(pinera_pinera),mean(guillier_pinera),mean(sanchez_pinera),mean(jakast_pinera),
	mean(goic_pinera),mean(meo_pinera),mean(artes_pinera),mean(navarro_pinera))
dfsd[1,]=c(mean(pinera_guillier),mean(guillier_guillier),mean(sanchez_guillier),mean(jakast_guillier),
	mean(goic_guillier),mean(meo_guillier),mean(artes_guillier),mean(navarro_guillier))


# for(i in 1:n){

# 	df$Pinera[i]=pinera*rnorm(1,mean(dfmu$p_p,na.rm=TRUE),dfsd$p_p[i])+
# 					guillier*rnorm(1,mean(dfmu$p_g,na.rm=TRUE),dfsd$p_g[i])+
# 					sanchez*rnorm(1,mean(dfmu$p_s,na.rm=TRUE),dfsd$p_s[i])+
# 					jakast*rnorm(1,mean(dfmu$p_k,na.rm=TRUE),dfsd$p_k[i])+
# 					goic*rnorm(1,mean(dfmu$p_gc,na.rm=TRUE),dfsd$p_gc[i])+
# 					meo*rnorm(1,mean(dfmu$p_m,na.rm=TRUE),dfsd$p_m[i])+
# 					artes*rnorm(1,mean(dfmu$p_a,na.rm=TRUE),dfsd$p_a[i])+
# 					navarro*rnorm(1,mean(dfmu$p_n,na.rm=TRUE),dfsd$p_n[i])

# 	df$Guillier[i]=pinera*rnorm(1,mean(dfmu$g_p,na.rm=TRUE),dfsd$g_p[i])+
# 					guillier*rnorm(1,mean(dfmu$g_g,na.rm=TRUE),dfsd$g_g[i])+
# 					sanchez*rnorm(1,mean(dfmu$g_s,na.rm=TRUE),dfsd$g_s[i])+
# 					jakast*rnorm(1,mean(dfmu$g_k,na.rm=TRUE),dfsd$g_k[i])+
# 					goic*rnorm(1,mean(dfmu$g_gc,na.rm=TRUE),dfsd$g_gc[i])+
# 					meo*rnorm(1,mean(dfmu$g_m,na.rm=TRUE),dfsd$g_m[i])+
# 					artes*rnorm(1,mean(dfmu$g_a,na.rm=TRUE),dfsd$g_a[i])+
# 					navarro*rnorm(1,mean(dfmu$g_n,na.rm=TRUE),dfsd$g_n[i])

# 	df$Total[i]=df$Guillier[i]+df$Pinera[i]
# 	df$percPinera[i]=df$Pinera[i]/df$Total[i]
# 	df$percGuillier[i]=df$Guillier[i]/df$Total[i]

# 		#if(i>1){
# 		for(j in ncol(dfmu)){
# 			dfmu[i+1,j]=mean(dfmu[,j],na.rm=TRUE)
# 		}
# 		for(j in ncol(dfmu)){

# 			dfsd[i+1,j]=sd(dfmu[,j],na.rm=TRUE)
# 		}
# 	#}
# }