
setwd("C:/Users/Parimal/Documents/R work")
#===========================================================================
#Type of Test
f=10
p=5
#Band Selection limits
a=0.20
b=0.80
#M=2 means long CCY pair, M=1 means short
M=2
#---------------------------------------------------------------------------
if(M==1){
    N <- 2
}else if(M==2){
    N <- 1
}

base <- read.csv("NZDUSD.csv",header=TRUE)
base <- base[,1:3]
base$Date <- strptime(base$Date,"%m / %d / %Y")
base <- base[with(base,order(Date)),]

base$Fwd <- ((-1)^M)*fwd(data=base,days=f)
base$TestVar <- prior(data=base,days=p)
base <- base[! is.na(base$Fwd),]
base <- base[! is.na(base$TestVar),]
base2 <- base
base2$type1 <- 0
base2$type2 <- 0
base2$p <- 0
k1 <- 0
k2 <- 0
k3 <- 0
k4 <- 0
r <- nrow(base2)
BMTM <- c(0,0,0,0,0)
NBMTM <- c(0,0,0,0,0)

for(i in 900:r){
    tempbase <- base[1:i,]
    Regime <- RegimeFilter(data=tempbase,type=2,Desc=0.1, col=4)
    NoRegime <- subset(tempbase,!tempbase$Date %in% Regime$Date)
    signif <- ks.test(NoRegime$TestVar,Regime$TestVar)
    base2$p[i] <- signif$p
    if(signif$p<=0.1){
        
        k1 <- k1+1
        RegInBand <- bandfilter(Regime, LLimit=a,ULimit=b,col=5)
        NonRegInBand <- bandfilter(NoRegime, LLimit=a,ULimit=b,col=5)
        band <- rbind(RegInBand,NonRegInBand)
        band <- band[with(band,order(Date)),] 
        NoBand <- subset(tempbase,!tempbase$Date %in% band$Date)
        if(band[nrow(band),1]==base[i,1]){
            
            k2 <- k2+1
            band5=tail(band)
            NoBand5=tail(NoBand)
            B1 <- which(base2$Date == band5[6,1])
            NB1<- which(base2$Date == NoBand5[6,1])
            for(j in 1:5){
                B2 <- which(base2$Date == band5[(6-j),1])
                if((B1-B2)<f){
                    BMTM[j] <- (band5[6,2]-band5[(6-j),2])/band5[(6-j),3]
                }else{
                    BMTM[j] <- band5[6,4]
                }
                NB2<- which(base2$Date == NoBand5[(6-j),1])
                if((NB1-NB2)<f){
                    NBMTM[j] <- (NoBand5[6,2]-NoBand5[(6-j),2])/NoBand5[(6-j),3]
                }else{
                    NBMTM[j] <- NoBand5[6,4]
                }
            }
            t1 <- BMTM[1]
            t2 <- BMTM[2]
            t3 <- BMTM[3]
            t4 <- BMTM[4]
            t5 <- BMTM[5]
            t <- c(t1,t2,t3,t4,t5)
            Bmean <- mean(t)
            Bmedian <- median(t)
            t1 <- NBMTM[1]
            t2 <- NBMTM[2]
            t3 <- NBMTM[3]
            t4 <- NBMTM[4]
            t5 <- NBMTM[5]
            t <- c(t1,t2,t3,t4,t5)
            NBmean <- mean(t)
            NBmedian <- median(t)
            if((Bmean>NBmean)&(Bmedian>NBmedian)){
                k3=k3+1
                
                if((Bmean>0)&(Bmedian>0)){
                    k4=k4+1
                    base2$type1[i] <- 1               
                }
                
            }
        }
    }
}
band <- base2[base2$type1==1,]
band$Equity <- cumsum(band$Fwd)
parameters <- read.csv("PrintData.csv",header=T)
row.names(parameters) <- parameters[,1]
parameters$Summary.Statistics <- c(nrow(base2),
                   k4,
                   round(max(band$Fwd),3),
                   round(min(band$Fwd),3),
                   round((nrow(band[band$Fwd>0,]))/k4,3),
                   round(mean(band$Fwd)/sd(band$Fwd),3),
                   round(mean(band$Fwd)/sd(band[band$Fwd<0,4]),3),
                   8)
parameters <- subset(parameters,select=-X)
mydate.ch <- format(band$Date,"%Y")
mydate.fac <- factor(mydate.ch, unique(mydate.ch))
p1 <- qplot(Date, Equity,data=band,geom=c("point","line"),main="Equity chart", color=mydate.fac)+
   # geom_line(aes(group=1), colour="#D55E00") +  
    #geom_point(size=3, colour="#CC0000")+
    geom_hline(aes(yintercept=0), colour="#0072B2", linetype="dashed")

p2 <- qplot(Date, Fwd,data=band,geom="boxplot",color=mydate.fac) + 
   # theme(axis.text.x=element_blank(), axis.title.x=element_blank())+
    ylab("Returns")+ labs(title = "Boxplot") + theme(legend.position="none")+
    geom_hline(aes(yintercept=0), colour="#0072B2", linetype="dashed")

p3 <- qplot(1:10, 1:10, geom = "blank") + 
    labs(title = "Summary Statistics") +
    theme_bw() +
    theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
    theme(line = element_blank(),text = element_blank()) +
    annotation_custom(grob = tableGrob(parameters,
                                       gpar.coretext =gpar(fontsize=8),
                                       gpar.coltext=gpar(fontsize=8), 
                                       gpar.rowtext=gpar(fontsize=8)),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

multiplot(p3,p2,p1, layout=matrix(c(1,2,3,3), nrow=2, byrow=TRUE))


