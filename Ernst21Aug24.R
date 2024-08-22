loc<-unique(dat[,5])
tms<-unique(dat[,1])
nl<-length(loc)
nt<-length(tms)

dta<-matrix(NA,ncol=nl,nrow=nt)
dimnames(dta)<-list(tms,loc)


ri<-match(dat[,1],tms)
ci<-match(dat[,5],loc)
for (i in 1:nrow(dat)){
  dta[ri[i],ci[i]]<-dat[i,2]
}

sum(is.na(dta))
sum(is.na(dat[,2]))


sum(is.na(data_matrix))


dta<-as.data.frame(dta)
dta.new<-dta
for (i in 1:nl){
  mss<-is.na(dta[,i])
  if (sum(mss)>0){
    print(i)
    #fit model
    available.vars<-apply(dta[mss,],2,function(x){sum(is.na(x))==0})
    fml<-as.formula(paste0("`",loc[i],"`","~`",paste0(loc[available.vars],collapse = "`+`"),"`"))
    model.i<-lm(fml, subset= -mss,data=dta)
    #apply model to missing values and impute
    new.data<-dta[mss,]
    dta.new[mss,i]<-predict(model.i,newdata=new.data)
  }
}
i<-2
mss<-is.na(dta[,i])
plot(dta.new[,i],col=mss+1)
