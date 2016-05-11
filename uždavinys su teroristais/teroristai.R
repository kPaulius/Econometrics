#Yra 700000 žmonių, 50 iš jų - teroristai. Detektorius suskamba 99.05%, jei žmogus yra teroristas. O suskamba, kai žmogus 
#nėra teroristas 0.05% kartų. Praėjo žmogus ir suskambėjo detektorius, kokia tikimybė, jog žmogus yra teroristas?
data=numeric(0)
montecarlo = function(n){
  for(i in 1:n){    
    suskambejo = 0
    while(suskambejo==0){                                            #laukiame kol suskambės detektorius, parenkami žmonės tol kol suskambės
      praeivis=sample(c(1,0), 1, prob=c(50/700000,699950/700000))    #parenkamas praeinantysis žmogus
      if(praeivis==1) {                                              #jei žmogus teroristas
        suskambejo=(sample(c(1,0), 1, prob=c(1-0.0095,0.0095)))
      }
      if(praeivis==0){                                               #jei žmogus nėra teroristas
        suskambejo=(sample(c(1,0), 1, prob=c(0.005,1-0.005)))
      }
    }
    data[i]=praeivis                                                 #kai suskamba, pažymime koks tai žmogus
  }
  return(data)

}

mean(montecarlo(100000))     #tikimybė, jog žmogus yra teroristas, apskaičiuota iš 100 tūkst. simuliacijos, kantrybės laukiant atsakymo

