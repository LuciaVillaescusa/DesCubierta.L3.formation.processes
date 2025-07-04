# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

# --------------------------------------------------------------------------------
# 1st SECTION OF THE ANALYSES: COMPARISON ARCHAEOLOGICAL VS GEOLOGICAL PATTERN----
#---------------------------------------------------------------------------------

# Libraries

library(readxl)
library(spatstat)

# 1.1.DATA UPLOADING -----

# Materials

cdc3 <- read_excel("01.CDC_geo_arch_materials.xlsx")

# Window in plan view

Window_plan <- read_excel("02. CDC_window_xy.xlsx")

WXY <- owin(poly=list(list(x=Window_plan$x, y=Window_plan$y)))

# Window in section view

Window_section <- read_excel("03. CDC_window_yz.xlsx")

WYZ <- owin(poly=list(list(x=Window_section$y, y=Window_section$z)))

# 1.2. PPP CREATION ----

# 1.2.1. Section view 

cdc3_filtered <- cdc3[cdc3$z != 0, ]

m_cdc3filtered <- as.factor(cdc3_filtered$Type1)

sCDC3m <- ppp(cdc3_filtered$y, 
              cdc3_filtered$z, 
              window = WYZ, 
              marks = m_cdc3filtered)

sCDC3m <- unique(sCDC3m) # apply unique as it exist duplicated points. To avoid problems in subsequent analyses  
sCDC3 <- unmark(sCDC3m)
sCDC3ms <- split(sCDC3m) # create separte ppp for geological and archaeological sets

unitname(sCDC3) <- c("metre", "metres")
unitname(sCDC3m) <- c("metre", "metres")
unitname(sCDC3ms) <- c("metre", "metres")

sARCH <- sCDC3ms$Archaeological
sGEO <- sCDC3ms$Geological

plot(sARCH, chars= 16, cex=0.4,
     leg.side = "right",
     cols = "black",
     main = "Archaeological")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) #fig.2.a

plot(sGEO, cex = 0.7, chars = 5, 
     cols = "burlywood4",
     main = "Geological")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) # fig.2.b

plot(sCDC3m,
     leg.side="right",
     cols=c("black","burlywood4"),
     cex=c(0.2,0.7), chars=c(16,5),
     main = "ARQ3 Archaeological and natural") 
  plot(sARCH, cols="black", cex=0.2, chars=16, add=T)
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) # fig. 2.c

# 1.2.2. Plan view 

m_cdc3 <- as.factor(cdc3$Type1)

CDC3m <- ppp(cdc3$x, 
             cdc3$y, 
             window = WXY, 
             marks = m_cdc3)

CDC3m <- unique(CDC3m) # apply unique as it exist duplicated points. To avoid problems in subsequent analyses  
CDC3 <- unmark(CDC3m)
CDC3ms <- split(CDC3m) # create separet ppp for geological and archaeological sets

unitname(CDC3) <- c("metre", "metres")
unitname(CDC3m) <- c("metre", "metres")
unitname(CDC3ms) <- c("metre", "metres")

ARCH <- CDC3ms$Archaeological
GEO <- CDC3ms$Geological

plot(ARCH,
     leg.side = "right",
     cols = "black",
     main = "ARQ3 Archaeological",
     chars = 16,
     cex = 0.4)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.a

plot(GEO,
     leg.side = "right",
     cols = "burlywood4",
     main = "ARQ3 Geological",
     chars = 5)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.b

plot(GEO,
     leg.side="right",
     cols="burlywood4",
     cex=0.7, chars=5,
     main = "Archaeological and geological")
plot(ARCH, 
     cols="black",
     cex=0.2, chars=16,
     add=T)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.c

#-------------------------------------------------------------------------------
# 1.3. UNMARKED PATTERNS ARCHAEOLOGICAL OR GEOLOGICAL----

# 1.3.1. Section view ----

# A. Summary ----

summary(sARCH)
summary(sGEO)

# B. Testing for CSR ----

# Chi-square test, Clark-Evans test, and Hopkins-Skellam index - ARCH

chi0sarch<-quadrat.test(sARCH,nx=4,ny=2)

chi1sarch<-quadrat.test(sARCH,nx=4, ny=2, CR=-1/2) #Freeman-Tukey

chi2sarch<-quadrat.test(sARCH,nx=4, ny=2, CR=-2) # Neyman test modified

chi3sarch<-quadrat.test(sARCH,4,2,method="MonteCarlo", nsim=39) # MonteCarlo simulations

clarsarch<-clarkevans.test(sARCH
                           ,alternative="clustered", correction="cdf",nsim=39) 

hopsarch<-hopskel.test(sARCH,alternative="clustered",method="MonteCarlo",nsim=999)

# get result in a table
testing_csr_sarch <- data.frame(c("chi0","chi1","chi2","chi3","clark-ev","hopskel"),
                                "chi_p" = c(chi0sarch$p.value,chi1sarch$p.value, 
                                            chi2sarch$p.value, chi3sarch$p.value, 
                                            clarsarch$p.value, hopsarch$p.value),
                                "statistic" = c(chi0sarch$statistic,chi1sarch$statistic, 
                                                chi2sarch$statistic, chi3sarch$statistic, 
                                                clarsarch$statistic, hopsarch$statistic))
colnames(testing_csr_sarch) <- c("test","p-value","statistic")

testing_csr_sarch # table 1

# Chi-square test, Clark-Evans test, and Hopkins-Skellam index - GEO

chi0sgeo<-quadrat.test(sGEO,nx=4,ny=2)

chi1sgeo<-quadrat.test(sGEO,nx=4, ny=2, CR=-1/2) #Freeman-Tukey

chi2sgeo<-quadrat.test(sGEO,nx=4, ny=2, CR=-2) # Neyman test modified

chi3sgeo<-quadrat.test(sGEO,4,2,method="MonteCarlo", nsim=39) # MonteCarlo simulations

clarsgeo<-clarkevans.test(sGEO,alternative="clustered", correction="cdf",nsim=39)

hopsgeo<-hopskel.test(sGEO,alternative="clustered",method="MonteCarlo",nsim=999)

# get results in a table
testing_csr_sgeo <- data.frame(c("chi0","chi1","chi2","chi3","clark-ev","hopskel"),
                               "chi_p" = c(chi0sgeo$p.value,chi1sgeo$p.value, 
                                           chi2sgeo$p.value, chi3sgeo$p.value, 
                                           clarsgeo$p.value, hopsgeo$p.value),
                               "statistic" = c(chi0sgeo$statistic,chi1sgeo$statistic, 
                                               chi2sgeo$statistic, chi3sgeo$statistic, 
                                               clarsgeo$statistic, hopsgeo$statistic))

colnames(testing_csr_sgeo) <- c("test","p-value","statistic")

testing_csr_sgeo # table 1

# C. Intensity ----

# C.1. Kernel density 

# C.1.1. Archaeological materials

bsarch <- bw.ppl(sARCH)
bsarch

D1sarch<-density(sARCH, bw.ppl, positive=TRUE) 
D1bsarch <- density(sARCH,bw.ppl,adjust=2, positive=TRUE) 

plot(D1bsarch) 
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) #fig.2.d

# C.1.2. Geological materials

bsgeo <- bw.ppl(sGEO)
bsgeo

D1asgeo <- density(sGEO, bw.ppl, positive=TRUE) 

plot(D1asgeo) 
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) #fig.2.e

# C.2. Scan test 

# C.2.1. Archaeological materials

LR1sarch <- scanLRTS(sARCH,r=bsarch)
pvals1sarch<-eval.im(pchisq(LR1sarch,df=1,lower.tail=FALSE))

# C.2.2. Geological materials    

LR1sgeo <- scanLRTS(sGEO, r=bsgeo)
pvals1sgeo <- eval.im(pchisq(LR1sgeo, df=1, lower.tail=FALSE))

# C.2.3. Scan test archaeological and geological superimpose

plot(pvals1sgeo<0.05, main="r=bw.ppl, pvals<0.05", col= c(NA, "burlywood4"))
plot(pvals1sarch<0.05, main="r=2*bw.ppl, pvals<0.05", col= c(NA, "black"), add=T)
  plot(WYZ, add=T, lwd=1)
  axis(1, 4530736:4530748)
  axis(2, 1107:1111)
  grid(lty=2) #fig.2.f

# D. Correlation ----
# D.1. L function
# D.1.1. Archaeological 
# L function adapted to archaeological-pattern inhomogeneity

D1sarch<-density(sARCH, bw.ppl, positive=TRUE)

mysLb<-function(X,...){ 
  D1<-density(X,bw.ppl, positive=TRUE) 
  Lis<-Linhom(X,D1,...) 
  return(Lis)
}

Ebs<-envelope(sARCH, mysLb,
              simulate=expression(rpoispp(D1sarch)),
              nsim=39, global=TRUE) 

plot(Ebs, . - mmean ~ r,lwd=2) # S.I. 3.b

# D.1.2. Geological 
# L function adapted to geological-pattern inhomogeneity

D1sgeo<-density(sGEO, bw.ppl, positive=TRUE)

Ebsgeo<-envelope(sGEO,mysLb, 
                  simulate=expression(rpoispp(D1sgeo)),
                  nsim=39, global=TRUE) 

plot(Ebsgeo, . - mmean ~ r,lwd=2) # S.I. 3.a

# 1.3.2. Plan view -----
# A. Summary ----

summary(ARCH)
summary(GEO)

# B. Testing for CSR ----

# Chi-square test, Clark-Evans test, and Hopkins-Skellam index - ARCH

chi0arch<-quadrat.test(ARCH,nx=4,ny=8)

chi1arch<-quadrat.test(ARCH,nx=4, ny=8, CR=-1/2) 

chi2arch<-quadrat.test(ARCH,nx=4, ny=8, CR=-2) 

chi3arch<-quadrat.test(ARCH,4,8,method="MonteCarlo", nsim=39) 

clararch<-clarkevans.test(ARCH,alternative="clustered", correction="cdf",nsim=39)

hoparch<-hopskel.test(ARCH,alternative="clustered",method="MonteCarlo",nsim=999)

# get result in a table
testing_csr_arch <- data.frame(c("chi0","chi1","chi2","chi3","clark-ev","hopskel"),
                               "chi_p" = c(chi0arch$p.value,chi1arch$p.value, 
                                           chi2arch$p.value, chi3arch$p.value, 
                                           clararch$p.value, hoparch$p.value),
                               "statistic" = c(chi0arch$statistic,chi1arch$statistic, 
                                               chi2arch$statistic, chi3arch$statistic, 
                                               clararch$statistic, hoparch$statistic))
colnames(testing_csr_arch) <- c("test","p-value","statistic")

testing_csr_arch # table 1

# Chi-square test, Clark-Evans test, and Hopkins-Skellam index - GEO

chi0geo<-quadrat.test(GEO,nx=4,ny=8)

chi1geo<-quadrat.test(GEO,nx=4, ny=8, CR=-1/2) 

chi2geo<-quadrat.test(GEO,nx=4, ny=8, CR=-2) 

chi3geo<-quadrat.test(GEO,4,8,method="MonteCarlo", nsim=39) 

clargeo<-clarkevans.test(GEO,alternative="clustered", correction="cdf",nsim=39)

hopgeo<-hopskel.test(GEO,alternative="clustered",method="MonteCarlo",nsim=999)

# get results in a table
testing_csr_geo <- data.frame(c("chi0","chi1","chi2","chi3","clark-ev","hopskel"),
                              "chi_p" = c(chi0geo$p.value,chi1geo$p.value, 
                                          chi2geo$p.value, chi3geo$p.value, 
                                          clargeo$p.value, hopgeo$p.value),
                              "statistic" = c(chi0geo$statistic,chi1geo$statistic, 
                                              chi2geo$statistic, chi3geo$statistic, 
                                              clargeo$statistic, hopgeo$statistic))

colnames(testing_csr_geo) <- c("test","p-value","statistic")

testing_csr_geo # table 1

# C. Intensity ----

# C.1. Kernel density 

# C.1.1.Archaeological materials 

barch <- bw.ppl(ARCH)
barch

D1barch <- density(ARCH,bw.ppl,adjust=2, positive=TRUE)

plot(D1barch, useRaster=FALSE)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.d

# C.1.2. Geological materials

bgeo <- bw.ppl(GEO)
bgeo

D1ageo <- density(GEO, bw.ppl, positive=TRUE)

plot(D1ageo, useRaster=FALSE)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.e

# C.2. Scan test 

# C.2.1. Archaeological materials

LR1arch <- scanLRTS(ARCH,r=barch)
pvals1arch<-eval.im(pchisq(LR1arch,df=1,lower.tail=FALSE))

# C.2.2. Geological materials    

LR1geo <- scanLRTS(GEO, r=bgeo)
pvals1geo <- eval.im(pchisq(LR1geo, df=1, lower.tail=FALSE))

# C.2.3. Scan test archaeological and geological superimpose

plot(pvals1geo<0.05, main="r=bw.ppl, pvals<0.05", col= c(NA, "burlywood4"))
plot(pvals1arch<0.05, main="r=2*bw.ppl, pvals<0.05", col= c(NA, "black"), add=T)
  plot(WXY, add=T, lwd=1)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) #fig.3.f

# D. Correlation ----
# D.1. L function

# D.1.1. Archaeological 

# L function adapted to archaeological-pattern inhomogeneity

D1<-density(ARCH, bw.ppl, positive=TRUE)

Eb<-envelope(ARCH, mysLb, 
             simulate=expression(rpoispp(D1)),
             nsim=39, global=TRUE) 

plot(Eb, . - mmean ~ r,lwd=2) # S.I. 3.g

# D.1.2. Geological 

# L function adapted to geological-pattern inhomogeneity

D1geo <- density(GEO, bw.ppl, positive=TRUE)

Ebgeo <- envelope(GEO, mysLb, 
                  simulate=expression(rpoispp(D1geo)), 
                  nsim=39, global=TRUE) 

plot(Ebgeo, . - mmean ~ r, lwd=2) # S.I. 3.f

#-------------------------------------------------------------------------------
# 1.4. MARKED PATTERN (ARCHAEOLOGICAL AND GEOLOGICAL)----

# 1.4.1. Section view ----

# A. Segregation test ----
sts <- segregation.test(sCDC3m,sigma=bw.ppl,nsim=19)

sts

# B. Dominant and relative risk -------

Rels<-bw.relrisk(sCDC3m,method="likelihood")
R1s<-relrisk(sCDC3m, Rels, casecontrol=FALSE)

Probs <- R1s
dominants <- im.apply(Probs, which.max)
types <- levels(marks(sCDC3m))
dominants <- eval.im(factor(dominants, levels=1:2, labels=types))

plot(dominants, col=c("black", "burlywood4"), sigma=0.5, main="")
  axis(1, 4530736:4530748)
  axis(2, 1107:1111)
  grid(lty=2, lwd= 0.5) # fig.2.i.

#tolcon --> relative risk with tolerance contours
  
tolcon <- function(X, ..., nsim=19,
                     alternative=c("greater", "less", "two.sided"),
                     verbose=TRUE) {
    require(spatstat)
    require(spatstat.utils)
    stopifnot(is.ppp(X))
    stopifnot(is.marked(X))
    check.1.integer(nsim)
    alternative <- match.arg(alternative)
    Zdata <- as.solist(relrisk(X, ...))
    n <- length(Zdata)
    rankcount <- rep(list(0), n)
    if(verbose) 
      cat(paste("Simulating", nsim, "random labellings..."))
    pstate <- list()
    for(i in seq_len(nsim)) {
      Xsim <- rlabel(X)
      Zsim <- as.solist(relrisk(Xsim, ...))
      if(length(Zsim) != n)
        stop("Different numbers of images produced in data and simulation")
      for(j in seq_len(n)) 
        rankcount[[j]] <- rankcount[[j]] + (Zsim[[j]] >= Zdata[[j]])
      if(verbose)
        pstate <- progressreport(i, nsim, state=pstate)
    }
    if(verbose) 
      splat("Done.")
    result <- Zdata
    for(j in seq_len(n)) {
      y <- result[[j]]
      r <- rankcount[[j]]
      attr(y, "pvalues") <-
        switch(alternative,
               greater = { (r+1)/(nsim+1.0) },
               less    = { (nsim-r+1)/(nsim+1.0) },
               two.sided = {
                 pg <- (r+1)/(nsim+1.0)
                 pl <- (nsim-r+1)/(nsim+1.0)
                 eval.im(pmin(1, 2*pmin(pg, pl)))
               })
      attr(y, "nsim") <- nsim
      attr(y, "alternative") <- alternative
      class(y) <- c("tolcon", class(y))
      result[[j]] <- y
    }
    class(result) <- c("tolconlist", class(result))
    return(result)
  }
  
shift.tolcon <- function(X, ...) {
    y <- shift.im(X, ...)
    attr(y, "pvalues") <- shift.im(attr(X, "pvalues"), ...)
    return(y)
  }
  
plot.tolconlist <- function(x, ...) {
    plot.imlist(x, ..., plotcommand=plot.tolcon)
  }
  
plot.tolcon <- function(x, ...,
                          show.contour=TRUE,
                          levels=0.05) {
    result <- plot.im(x, ...)
    if(show.contour)
      do.call.matched(contour.im,
                      resolve.defaults(list(x=attr(x, "pvalues"),
                                            levels=levels * 1.0001,
                                            add=TRUE),
                                       list(...),
                                       list(col="white",
                                            drawlabels=FALSE)),
                      extrargs=names(formals(contour.default)))
    return(result)
  }
  
print.tolcon <- function(x, ...) {
    print.im(x, ...) 
    splat("\t[Includes tolerance p-values based on",
          attr(x, "nsim"),
          "random labellings]")
    return(invisible(NULL))
  }
  
tps <- tolcon(sCDC3m, sigma=bw.ppl(sCDC3m), eps = 0.01, casecontrol=FALSE)
  
layout(matrix(c(1,2),nrow = 1, ncol = 2, byrow = TRUE))
  plot(tps$Archaeological, main="Archaeological", useRaster=F)
    axis(1, 4530736:4530748)
    axis(2, 1107:1111)
    grid(lty=2, lwd= 0.5)
  plot(tps$Geological, main="Geological", useRaster=F)
    axis(1, 4530736:4530748)
    axis(2, 1107:1111)
    grid(lty=2, lwd= 0.5)
    par(mfrow=c(1,1)) # fig.2.g y h

# C. Lcross inhom adapted to CDC3ms inhomogeneity----
    
mysLcross<-function(X,i,j,...){
      dsgeo<-density(sGEO,bw.ppl, at="points")
      dsarch<-density(sARCH,bw.ppl, at="points")
      Ls<-Lcross.inhom(X,i,j,dsgeo,dsarch,...)
      return(Ls)
    }
    
Ecross_ps <- envelope(sCDC3m,mysLcross,
             nsim=39,i="Geological", j="Archaeological",
             simulate=expression(rlabel(sCDC3m)))
    
plot(Ecross_ps, . - mmean ~ r,lwd=2) # S.I.3.c 
    
# D. Nncount and nnequal ----

#nncount 
    require(spatstat)
    require(spatstat.utils)
    nncount <- function(X, i=1, j=2, ..., kmax=20, ratio=TRUE, cumulative=TRUE) {
      stopifnot(is.ppp(X))
      stopifnot(is.multitype(X))
      marx <- marks(X)
      lev <- levels(marx)
      if(is.numeric(i)) i <- lev[i]
      if(is.numeric(j)) j <- lev[j]
      if(is.na(match(i, lev)))
        stop(paste("Unrecognised value", i, "for argument i"))
      if(is.na(match(j, lev)))
        stop(paste("Unrecognised value", j, "for argument j"))
      iname <- make.parseable(paste(i))
      jname <- make.parseable(paste(j))
      N <- nnwhich(X, k=1:kmax)
      mi <- (marx == i)
      mj <- (marx == j)
      if(!any(mi)) {
        numer <- denom <- rep(0, kmax)
      } else if(!any(mj)) {
        numer <- rep(0, kmax)
        denom <- sum(mi)
      } else {
        Xsub <- X[mi]
        Nsub <- N[mi, , drop=FALSE]
        Dsub <- nndist(Xsub,  k=1:kmax)
        Bsub <- bdist.points(Xsub)
        observed <- (Dsub <= Bsub)
        counted <- matrix(mj[Nsub], ncol=kmax)
        if(cumulative) {
          numer <- rowSums(apply(observed & counted, 1, cumsum))
          denom <- rowSums(apply(observed,           1, cumsum))
        } else {
          numer <- colSums(observed & counted)
          denom <- colSums(observed)
        }
      }
      estimate <- ifelse(denom > 0, numer/denom, 0)
      
      pj <- mean(mj)
      df <- data.frame(k=1:kmax, theo=pj, bord=estimate)
      desc <- c("neighbour order k",
                "theoretical %s", 
                "border-corrected estimate of %s")
      labl <- c("k","{%s[%s]^{theo}}(k)", "{hat(%s)[%s]^{bord}}(k)")
      dendf <- data.frame(k=1:kmax, theo=denom, bord=denom)
      ylab <- substitute(N[i,j](r), list(i=iname,j=jname))
      yexp <- substitute(N[list(i,j)](k), list(i=iname,j=jname))
      fname <- c("N", paste0("list(", iname, ",", jname, ")"))
      Z <- ratfv(df, NULL, dendf, 
                 argu="k",
                 ylab=ylab,
                 valu="bord",
                 fmla = . ~ k,
                 alim=c(1,kmax),
                 labl=labl, desc=desc, fname=fname, yexp=yexp,
                 ratio=ratio,
                 unitname=c("neighbour step", "neighbour steps"))
      return(Z)
    }
    
plot(envelope(sCDC3m, nncount, simulate=expression(rlabel(sCDC3m)))) # S.I. 3.d
    
#nnequal
nnequal <- function(X, ..., kmax=20, ratio=TRUE, cumulative=TRUE) {
      stopifnot(is.ppp(X))
      stopifnot(is.multitype(X))
      N <- nnwhich(X, k=1:kmax)
      D <- nndist(X,  k=1:kmax)
      B <- bdist.points(X)
      observed <- (D <= B)
      marx <- marks(X)
      mI <- matrix(marx[row(N)], ncol=kmax)
      mJ <- matrix(marx[N], ncol=kmax)
      counted <- (mI == mJ)
      if(cumulative) {
        numer <- rowSums(apply(observed & counted, 1, cumsum))
        denom <- rowSums(apply(observed,           1, cumsum))
      } else {
        numer <- colSums(observed & counted)
        denom <- colSums(observed)
      }
      estimate <- ifelse(denom > 0, numer/denom, 0)
      
      m <- as.integer(table(marx))
      n <- npoints(X)
      pequal <- sum(m*(m-1)/(n*(n-1)))
      
      df <- data.frame(k=1:kmax, theo=pequal, bord=estimate)
      desc <- c("neighbour order k",
                "theoretical %s", 
                "border-corrected estimate of %s")
      labl <- c("k","%s[theo](k)", "hat(%s)[bord](k)")
      dendf <- data.frame(k=1:kmax, theo=denom, bord=denom)
      yexp <- ylab <- quote(E(k))
      fname <- "E"
      Z <- ratfv(df, NULL, dendf, 
                 argu="k",
                 ylab=ylab,
                 valu="bord",
                 fmla = . ~ k,
                 alim=c(1,kmax),
                 labl=labl, desc=desc, fname=fname, yexp=yexp,
                 ratio=ratio,
                 unitname=c("neighbour step", "neighbour steps"))
      return(Z)
    }
    
plot(envelope(sCDC3m, nnequal, simulate=expression(rlabel(sCDC3m)))) # S.I. 3.e

# 1.4.1. Plan view -----
# A. Segregation test ----

st<- segregation.test(CDC3m,sigma=bw.ppl,nsim=19)
st

# B. Dominant and relative risk -------

Relp<-bw.relrisk(CDC3m,method="likelihood")
R1p<-relrisk(CDC3m, Relp, casecontrol=FALSE)

Probp <- R1p
dominantp <- im.apply(Probp, which.max)
types <- levels(marks(CDC3m))

dominantp <- eval.im(factor(dominantp, levels=1:2, labels=types))

plot(dominantp, col=c("black", "burlywood4"), sigma=0.5, main="")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2, lwd= 0.5) # fig.3.i

#tolcon -> relative risk with tolerance contours

tp <- tolcon(CDC3m, sigma=bw.ppl(CDC3m), eps = 0.01, casecontrol=FALSE)

layout(matrix(c(1,2),nrow = 1, ncol = 2, byrow = TRUE))
  plot(tp$Archaeological, main="", useRaster=F)
    axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
    axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
    grid(lty=2, lwd= 0.5)
  plot(tp$Geological, main="", useRaster=F)
    axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
    axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
    grid(lty=2, lwd= 0.5)
    par(mfrow=c(1,1)) #fig. 3.g y h

# C. Lcross inhom adapted to CDC3m inhomogeneity----

myLcross<-function(X,i,j,...){
  dgeo<-density(GEO,bw.ppl, at="points")
  darch<-density(ARCH,bw.ppl, at="points")
  L<-Lcross.inhom(X,i,j,dgeo,darch,...)
  return(L)
}

Ecross_p<-envelope(CDC3m,myLcross,nsim=39,i="Geological", j="Archaeological",
                   simulate=expression(rlabel(CDC3m)))

plot(Ecross_p, . - mmean ~ r,lwd=2) # S.I. 3.f

# D. Nncount and nnequal ----

#nncount 

plot(envelope(CDC3m, nncount, simulate=expression(rlabel(CDC3m)))) # S.I. 3.i

#nnequal

plot(envelope(CDC3m, nnequal, simulate=expression(rlabel(CDC3m)))) # S.I. 3.j


