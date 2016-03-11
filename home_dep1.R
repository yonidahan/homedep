
#Read, prepare and preprocess the data
dir<-"C:/Users/Sarah/Desktop/Data Science/Projects/Home_depot"

if(getwd()!=dir){setwd(dir)}

#Libraries
lapply(c("dplyr","caret","Matrix","tm","stringdist","doParallel","foreach",
         "rARPACK","gbm"),require,character.only=T)

if (Sys.getenv("JAVA_HOME")!="")#Java dependencies (RWeka package)
        Sys.setenv(JAVA_HOME="")
library("RWeka")

#Loading data sets
test<-read.csv("test.csv",stringsAsFactors=F)
train<-read.csv("train.csv",stringsAsFactors=F)
product_descr<-read.csv("product_descriptions.csv",stringsAsFactors=F)
attrib<-read.csv("attributes.csv",stringsAsFactors=F)

#Prepare the data
help_data<-list(test_id=test$id,train_id=train$id,y=train$relevance,
                n_train=nrow(train))

data_hd<-rbind(select(train,-relevance),test)#Stack 

data_hd<-merge(data_hd,product_descr,by="product_uid")

brand<-attrib[grep("MFG Brand Name",attrib$name),]#Append brand
data_hd<-merge(data_hd,brand[,c("value","product_uid")],
               by="product_uid",all.x=T,all.y=F)

names(data_hd)[names(data_hd)=="value"]<-"brand"

#Preprocessing
source("./preproc.R")
data_hd[,!(names(data_hd)%in%c("id","product_uid"))]<-
        apply(data_hd[,!(names(data_hd)%in%c("id","product_uid"))],
              2,preproc)

data_hd$brand[data_hd$brand=="NA"]<-""


#Save processed files in directory 
if(!dir.exists("./proc_files")){dir.create("./proc_files")}

if(!exists("./proc_files/data_hd")){
        save(data_hd,file="./proc_files/data_hd")
}
if(!exists("./proc_files/help_data")){
        save(help_data,file="./proc_files/help_data")
}

