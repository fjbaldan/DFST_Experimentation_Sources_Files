# TS creation script for performance testing of Distributed FastShapelet Transform algorithm:
# ARIMAS
stA<-function(n_ts,l_ts,class,list_model){
  min=100000
  num_class=length(class)
  n_train_init=as.integer(n_ts*80/100)
  n_test_init=n_ts-n_train_init
  first_time=TRUE

  randImp<-function(l_ts,class,id_class,list_model){
    # Creation of the input sequence to the ARIMA model
    innov=sample(0:1, l_ts, replace=T)
    serie <- arima.sim(n=l_ts, model=list_model[[id_class]], innov=innov, start.innov = rep(0,l_ts))
    serie=round(serie,4)[1:l_ts]
    print(length(serie))
    print(list_model[[id_class]])
    return(c(class[id_class],serie))
  }
  
  dec=round((n_ts/min-as.integer(n_ts/min))*min)
  if(dec==0){
    dec=min
  }
  iterations= 1:(ceiling(n_ts/min))
  
  n_train_aux=n_train_init
  n_test_aux=n_test_init
  last=TRUE
  for (aux in iterations){
    if(n_ts < min){
      n_ts_aux=n_ts
      last=TRUE
    }else{
      n_ts_aux=min
    }
    #Problema Aqu?
    if(aux==length(iterations) && aux > 1 ){
      n_ts_aux=dec
      last=TRUE
    }else{
      last=FALSE
      if(n_ts <= min){
        last=TRUE
      }
    }
    
    dataset=matrix(data=0,nrow=n_ts_aux,ncol=l_ts+1)
    assign("dataset",dataset)
    
    x=1
    for (i in 1:num_class){
      pos=x:(x+(as.integer(n_ts_aux/num_class))-1)
      sapply(pos,function(y){
        dataset[y,]<<-randImp(l_ts,class,i,list_model)
      })
      x=tail(pos, n=1)+1
    }
    
    x=x-1
    rest=n_ts_aux-x
    x=x+1
    if(rest>0){
      for(i in 1:rest){
        dataset[x,]<<-randImp(l_ts,class,i,list_model)
        x=x+1
      }
    }
    random=sample(1:nrow(dataset))
    dataset=dataset[random,]
    
    n_train=as.integer(n_ts_aux*80/100)
    n_test=n_ts_aux-n_train
    
    n_train_aux=n_train_aux-n_train
    n_test_aux=n_test_aux-n_test
    
    if(is.null(dim(dataset[1:n_train,]))==FALSE){
      write.table(dataset[1:n_train,], file = paste("train_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_train_init,".txt",sep=""), append = !first_time, quote = TRUE, sep = " ", row.names = FALSE,
                  col.names = FALSE)
    }else{
      write.table(t(dataset[1:n_train,]), file = paste("train_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_train_init,".txt",sep=""), append = !first_time, quote = TRUE, sep = " ", row.names = FALSE,
                  col.names = FALSE)
    }
    
    if(is.null(dim(dataset[(n_train+1):(length(dataset[,1])),]))==FALSE){
      write.table(dataset[(n_train+1):(length(dataset[,1])),], file = paste("test_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_test_init,".txt",sep=""), append = !first_time, quote = TRUE, sep = " ", row.names = FALSE,
                  col.names = FALSE)
    }else{
      write.table(t(dataset[(n_train+1):(length(dataset[,1])),]), file = paste("test_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_test_init,".txt",sep=""), append = !first_time, quote = TRUE, sep = " ", row.names = FALSE,
                  col.names = FALSE) 
    }
    if(last==TRUE){
      file.rename(paste("train_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_train_init,".txt",sep=""), paste("train_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_train_init,"_comp",".txt",sep=""))
      file.rename(paste("test_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_test_init,".txt",sep=""), paste("test_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_test_init,"_comp",".txt",sep=""))
    }
    
    output=c("Done: ", paste("train_randImpulseReponse_",num_class,"class_lengthTS",l_ts,"_numTS",n_train,".txt",sep=""))
    if(first_time==TRUE){
      first_time= !first_time
    }
  }
}

#ARIMA models
list_model=list(list(ar=c(.6), ma=0.1),list(ar=.5, ma=c(-.6, .6)),list(ar=c(.5, -.7), ma=c(-.6, .5)),list(order = c(2,1,2),ar=c(.5, -.7), ma=c(-.6, .5)))
class=c(0,1,2,3)

#Length TS
ts_dim=c(100)

# Train
# 100,000 to 3,000,000 by 100,000
# 4,000,000 to 20,000,000 by 1,000,000
ts_num=125000*c((1:30),seq(40,200,10))


library(parallel)
casos=1:length(ts_num)
mclapply(casos,function(i){
  for(j in 1:length(ts_dim)){
    stA(ts_num[i],ts_dim[j],class,list_model)
  }
},mc.cores = 1 )

