
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
tempbase <- base

top <- tempbase$Fwd>quantile(tempbase$Fwd,0.9)
bottom <- tempbase$Fwd<quantile(tempbase$Fwd,0.1)

tempbase$type <- "other"; tempbase$type[top] <- "top"; tempbase$type[bottom] <- "bottom"
tempbase$type <- as.factor(tempbase$type)

t <- tempbase[tempbase$type %in% "top" ,"TestVar"]
b <- tempbase[tempbase$type %in% "bottom" ,"TestVar"]
o <- tempbase[tempbase$type %in% "other" ,"TestVar"]

br <- seq(floor(min(t,o,b)), round(max(t,o,b)),by=0.05)
t1 <- hist(t, breaks=br, plot=F)
b1 <- hist(b, breaks=br, plot=F)
o1 <- hist(o, breaks=br, plot=F)
br=br[1:length(t1$density)]
dens <- data.frame(br=br, top=t1$density, bottom=b1$density, other=o1$density)
temp <- smooth.spline(x=br,y=t1$density)
dens$smt1 <- temp$y
temp <- smooth.spline(x=br,y=b1$density)
dens$smb1 <- temp$y
temp <- smooth.spline(x=br,y=o1$density)
dens$smo1 <- temp$y

ggplot(dens,aes(x=br))+geom_line(aes(y=smt1),color="red") +
    geom_line(aes(y=smb1),color="blue") + 
    geom_line(aes(y=smo1), color="green") 
