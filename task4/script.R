setwd("C:/Users/Paul/Desktop/Econometrics2/task4")
data=read.csv2("data.csv")
install.packages('car')
install.packages('lmtest')
library(lmtest)
library(car)

panel.hist <- function(x, ...)                                   
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor,...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 3)
}

pairs(data,upper.panel=panel.smooth,diag.panel=panel.hist,lower.panel=panel.cor)      #aiskiai matosi, jog kaina nuo ploto turi tiesiskumo priklausomybe        

cor(data$atstumasIkiPrekybosCentro,data$kaina)


z=numeric(0)
x=names(data)
for(i in 1:length(x)){
  y=boxplot(data[,i])
  y$out
  z=c(z,length(y$out))
}

remove_outliers <- function(x) {              # ideja gauta is http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)       #na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}
newdata=lapply(data,remove_outliers)
newdata=data.frame(newdata)

row.has.na <- apply(newdata, 1, function(x){any(is.na(x))})
newdata<- newdata[!row.has.na,]




mod=lm(newdata$kaina~newdata$plotas + newdata$aukstas+newdata$garsoIzoliacija+newdata$silumosLaidumas+newdata$atstumasIkiPrekybosCentro)
summary(mod) # du nereiksmingus rodo
install.packages('lmtest')
resettest(mod,power=2:3,type="regressor")   #nereikia kelti laipsniais kintamuosius


vif(mod)  # kolinearumo problema su silumos ir garso laidumu
cor(data$garsoIzoliacija,data$silumosLaidumas)    # kadangi ju koreliacija arti vieneto, galime viena kintamaji tiesiog ismest When the correlation between x1 and x2 is exactly one, the situation is called perfect collinearity, and one covariate needs to be removed from the regression model in order to estimate a solution. http://www.nature.com/bdj/journal/v199/n7/full/4812743a.html
cor.test(data$garsoIzoliacija,data$silumosLaidumas) #nors 95 percent confidence interval:0.9409172 0.9635591 bet vistiek ismetame viena is koreliuotu kintamuju
#ismetame garso laiduma ir atstuma iki prekyb

aukstasn=factor(newdata$aukstas)


mod11=lm(kaina~-1+plotas +aukstasn+garsoIzoliacija,data=newdata)
AIC(mod11)
summary(mod11)
mod12=lm(kaina~-1+plotas +aukstas+garsoIzoliacija,data=newdata)
AIC(mod12)
summary(mod12)   ## reik atskirt pirma auksta nuo kitu, nezinau kaip irodyt kad 2-9 aukstai vienoda itaka turi

mod13=lm(kaina~-1+plotas +aukstasn+log(garsoIzoliacija),data=newdata)
summary(mod13)
AIC(mod13)

#informacija rasta http://r-statistics.co/Outlier-Treatment-With-R.html
outlierTest(mod11)
newdata[20,]

cooksd <- cooks.distance(mod11)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels, na.rm  a logical value indicating whether NA values should be stripped before the computation proceeds.
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) 

newdata2=newdata[-c(influential), ]
aukstasn2=factor(newdata2$aukstas)
mod111=lm(kaina~-1+plotas +aukstasn2+garsoIzoliacija,data=newdata2)

summary(mod111)
plot(mod111)
AIC(mod111)

aukstas2=newdata2$aukstas==1
mod112=lm(kaina~-1+plotas +aukstas2+garsoIzoliacija,data=newdata2)
summary(mod112)
plot(mod112)
AIC(mod112)

cooksd <- cooks.distance(mod112)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels, na.rm  a logical value indicating whether NA values should be stripped before the computation proceeds.
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) 
newdata3=newdata2[-c(influential), ]

aukstas3=newdata3$aukstas==1
mod113=lm(kaina~-1+plotas +aukstas3+garsoIzoliacija,data=newdata3)
summary(mod113)
plot(mod113)
AIC(mod113)
vif(mod113)

bptest(mod113)            # neatmetam hipotezes kad homoskedastiskas

durbinWatsonTest(mod113)  # neatmetam hipotezes kad nera autokoreliacijos 


kable(summary(lm(y~x))$coef, digits=2) #grazesne lentele





