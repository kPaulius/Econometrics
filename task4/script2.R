paklaust dėl lietuviškų rašmenų


mod=lm(newdata$kaina~newdata$plotas + newdata$aukstas+newdata$garsoIzoliacija+newdata$silumosLaidumas+newdata$atstumasIkiPrekybosCentro)
summary(mod) # du nereiksmingus rodo

resettest(mod,power=2:3,type="regressor")   #nereikia kelti laipsniais kintamuosius
resettest(mod,power=1/2,type="regressor")   #nereikia kvadratines saknies
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






