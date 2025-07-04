# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

#-------------------------------------------------------------------------------
# 2d SECTION OF THE ANALYSES: VOID OF ARCHAEOLOGICAL REMAINS
#-------------------------------------------------------------------------------

# Libraries

library(spatstat)
library(readxl)
library(ggplot2)
library(dplyr)


# 2.1. DATA UPLOADING ----

# Materials

selection <- read_excel("04. CDC_Void_of_arch_materials.xlsx")

sel <- selection%>%
  filter(Type1=="Geological")

# Window in plan view

Window_plan <- read_excel("02. CDC_window_xy.xlsx")

WXY <- owin(poly=list(list(x=Window_plan$x, y=Window_plan$y)))

# Window in section view

Window_section <- read_excel("03. CDC_window_yz.xlsx")

WYZ <- owin(poly=list(list(x=Window_section$y, y=Window_section$z)))

# 2.2. PPP CREATION----
# Plan view----

msel <- as.factor(sel$Size_ran_2)

SELm <- ppp(x = sel$x, 
            y= sel$y, 
            window = WXY, 
            marks= msel)

SEL <- unmark(SELm)

SELms<- split(SELm)

LB <- SELms$Large_boulder
MB <- SELms$Medium_boulder
SB <- SELms$Small_boulder

plot(SELm, cols = c("black","tan","tan4"),
     cex = c(3, 1.5,0.75),
     pch = c(16,18,16),
     main = "Clasts first rockfall episodes", 
     leg.side = "right")
  axis(1, c(432076,432078, 432080, 432082), 
     cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), 
     cex.axis=0.75)
  grid(lty=2) #fig.4.d

# Section view----

sSELm <- ppp(x = sel$y, 
             y= sel$z, 
             window = WYZ, 
             marks= msel)

sSELm <- unique(sSELm)

sSEL <- unmark(sSELm)

sSELms<- split(SELm)

sLB <- sSELms$Large_boulder
sMB <- sSELms$Medium_boulder
sSB <- sSELms$Small_boulder

plot(sSELm, cols = c("black","tan","tan4"),
     cex = c(3, 1.5,0.75),
     pch = c(16,18,16),
     main = "Clasts first rockfall episodes by size", 
     leg.side = "right")
axis(1, 4530736:4530748)
axis(2, 1108:1111)
grid(lty=2) # fig.4.c

# 2.3. MARKED PATTERN (Clasts by size)-------
# 2.4.1. Plan view
# A. Intensity ----
# A.1.Kernel density
# Small boulders

S <- SB
bS <- bw.ppl(S)

D1cS<-density(S, bw.ppl, adjust=0.5, positive=TRUE)

plot(D1cS,useRaster=FALSE, main= "D1\n0.5*bw.ppl") # fig 4.f
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig.4.g

# Medium boulders
M <- MB
bM <-bw.ppl(M)

D1cM<-density(M, bw.ppl, adjust=0.5, positive=T)

plot(D1cM,useRaster=FALSE, main= "D1\n0.5bw.ppl")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig.4.f

# A.2. Scan test
# Small boulders

LR1S<-scanLRTS(S,r=bS)
pvals1S<-eval.im(pchisq(LR1S,df=1,lower.tail=FALSE))

# Medium boulder 

LR1M<-scanLRTS(M,r=0.5*bM)
pvals1M<-eval.im(pchisq(LR1M,df=1,lower.tail=FALSE))

# Scan test medium and small superimpose

plot(pvals1S<0.05, main="", col= c(NA, "tan4")) 
  plot(pvals1M<0.05, main="", col= c(NA, "tan"), add=T)
  plot(WXY, add=T, lwd=1)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig 4.e

# B. Density lines ----

selms <- sel%>%
  filter(Size_ran_2!="Large_boulder")

ggplot(selms, aes(x=x, color=Size_ran_2, fill=Size_ran_2)) +
  geom_density(alpha=0.05) +
  scale_color_manual(values=c("tan", "tan4"), labels=c("Medium", "Small")) +
  scale_fill_manual(values=c("tan", "tan4"), labels=c("Medium", "Small")) +
  theme_bw() +
  labs(color=NULL, fill=NULL) # S.I. 4.a


