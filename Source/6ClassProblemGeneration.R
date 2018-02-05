# #TS creation script for performance testing of Distributed FastShapelet Transform algorithm:
# 6Class
# Selection of datasets and binding in a single problem, where the class label refers to each original dataset
datasets=list(list("UCR_TS_Archive_2015/ECG5000/ECG5000_TRAIN",
                   "UCR_TS_Archive_2015/ECG5000/ECG5000_TEST"),
              list("UCR_TS_Archive_2015/PhalangesOutlinesCorrect/PhalangesOutlinesCorrect_TRAIN",
                   "UCR_TS_Archive_2015/PhalangesOutlinesCorrect/PhalangesOutlinesCorrect_TEST"),
              list("UCR_TS_Archive_2015/Two_Patterns/Two_Patterns_TRAIN",
                   "UCR_TS_Archive_2015/Two_Patterns/Two_Patterns_TEST"),
              list("UCR_TS_Archive_2015/Gun_Point/Gun_Point_TRAIN",
                   "UCR_TS_Archive_2015/Gun_Point/Gun_Point_TEST"),
              list("UCR_TS_Archive_2015/wafer/wafer_TRAIN",
                   "UCR_TS_Archive_2015/wafer/wafer_TEST"),
              list("UCR_TS_Archive_2015/ElectricDevices/ElectricDevices_TRAIN",
                   "UCR_TS_Archive_2015/ElectricDevices/ElectricDevices_TEST"))

max_len=150

create_problem<-function(datasets){
  out_train=NULL
  out_test=NULL
  class=NULL
  
  class=0
  for(i in datasets){
    aux_train=read.table(i[[1]],sep=",")
    aux_test=read.table(i[[2]],sep=",")
    
    # Length
    length_fix<-function(dataset){
      aux=dataset
      if(length(aux[1,-1])>=max_len){
        aux=aux[,(1:(max_len+1))]
      }else{
        for(z in (length(aux[1,-1]):(max_len+1))){
          aux[,z]=0
        }
      }
      return(aux)
    }
    aux_train=length_fix(aux_train)
    aux_test=length_fix(aux_test)
    
    aux_train[,1]=class
    aux_test[,1]=class
    # Classes
    clases=class
    out_train=rbind.data.frame(out_train,aux_train)
    out_test=rbind.data.frame(out_test,aux_test)
    class=class+1
  }
  return(list(out_train,out_test,class))
}
aux=create_problem(datasets)
train=aux[[1]]
test=aux[[2]]

write.table(train,"6dataset_orig_6class.txt", sep = " ",col.names = F,row.names = F)
write.table(test,"6dataset_orig_6class_test.txt", sep = " ",col.names = F,row.names = F)

# Creating BigData problems with white noise
perc_noise=10
out_text="10Noise"
num_ts=100000*c((1:30),seq(40,200,10))

library(parallel)
mclapply(num_ts,function(num_ts_p){
  
  file="6dataset_orig_6class.txt"
  dataset=read.table(file, sep = " ")
  
  data_test="6dataset_orig_6class_test.txt"
  dataset_test=read.table(data_test, sep = " ")
  
  num_ts_p=as.integer(num_ts_p)
  x=0
  len=length(dataset[1,-1])
  firstTime=T
  classes=unique(dataset[,1])
  num_class=length(unique(dataset[,1]))
  
  num_ts_class=as.integer(num_ts_p/num_class)
  
  out=NULL
  
  i=0
  while(i<=num_class){
    x=x+num_ts_class
    i=i+1
    aux=NULL
    
    if(x<=num_ts_p){
      # We select the TSs of the desired classes, replicate them and add the noise
      ts_s=dataset[which(dataset[,1]==classes[i]),]
      dataset_aux=dataset[sample(which(dataset[,1]==classes[i]),num_ts_class,replace = T),]
      aux=t(apply(dataset_aux,1,function(y){
        out=c(y[1],y[2:(len+1)]+rnorm(len,mean = 0,sd=1)*perc_noise/100)
        out
      }))
    }else{
      sel=num_ts_p-(x-num_ts_class)
      dataset_aux=dataset[sample(1:length(dataset[,1]),sel,replace = T),]
      aux=t(apply(dataset_aux,1,function(y){
        out=c(y[1],y[2:(len+1)]+rnorm(len,mean = 0,sd=1)*perc_noise/100)
        out
      }))
    }
    if(dim(aux)[2]!=0){
      if(firstTime){
        firstTime=!firstTime
        write.table(aux,paste0(file,num_ts_p,out_text,".txta"),col.names = F,row.names = F,append = firstTime, sep = " ")
      }else{
        write.table(aux,paste0(file,num_ts_p,out_text,".txta"),col.names = F,row.names = F,append = !firstTime, sep = " ")
      }
    }
  }

},mc.cores = 1)
