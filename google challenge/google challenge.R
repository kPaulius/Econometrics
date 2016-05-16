https://support.rstudio.com/hc/en-us/articles/205612627-Debugging-with-RStudio
data=scan("A-large-practice.in")    # kodo paradoxas su A ir B samples
sink('analysis-output.txt')          

n=1
N = data[n]

funkcija = function(data,n,ilgis){
  susikirtimai=0
  
  if(ilgis>1){
    ilgis=ilgis-1

    for(i in 1:ilgis){
      
      for(u in i:ilgis){
        
        if((data[n+i*2-1]>data[n+u*2+1]) && (data[n+i*2]<data[n+u*2+2])){
          susikirtimai=susikirtimai+1
          
        }
        if((data[n+i*2-1]<data[n+u*2+1]) && (data[n+i*2]>data[n+u*2+2])){
          susikirtimai=susikirtimai+1

        }
      }
    }
  }
  return(susikirtimai)
}


for(i in 1:N){
  n=n+1
  ilgis=data[n]
  sink('analysis-output.txt', append=TRUE)
  cat(paste(c("Case #",i,": ",funkcija(data,n,ilgis)),collapse=""))
  cat("\n")
  sink()
  n=n+ilgis*2
}
