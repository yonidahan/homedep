
#home_depot1
#Loading and preprocessing 


#Working directory

#Put your working directory here
dir<-"C:/Users/Sarah/Desktop/Data Science/Projects/Home_depot"

if(getwd()!=dir){setwd(dir)}

#Libraries
lapply(c("dplyr","caret","Matrix","tm","stringdist","doParallel","foreach",
         "rARPACK","gbm"),require,character.only=T)

if (Sys.getenv("JAVA_HOME")!="")#Java dependencies (RWeka package)
        Sys.setenv(JAVA_HOME="");
library("RWeka")

#Loading data sets
test<-read.csv("test.csv",stringsAsFactors=F)
train<-read.csv("train.csv",stringsAsFactors=F)
product_descr<-read.csv("product_descriptions.csv",stringsAsFactors=F)
attrib<-read.csv("attributes.csv",stringsAsFactors=F)

#Prepare the data
y<-train$relevance
help_data<-list(test_id=test$id,train_id=train$id,y=y,
                      n_train=nrow(train))

data_hd<-rbind(select(train,-relevance),test)

data_hd<-merge(data_hd,product_descr,by="product_uid")

attrib<-attrib[grep("MFG Brand Name",attrib$name),#Keep only brand indicator
               c("name","product_uid")]
data_hd<-merge(data_hd,attrib,by="product_uid",all.x=T,all.y=F)

#Preprocessing
preproc<-function(x){
        x<-iconv(enc2utf8(x),sub="byte")#UTF8 encoding
        x<-tolower(x)
        x<-removePunctuation(x,preserve_intra_word_dashes=T)
        
        x<-removeWords(x,stopwords("english"))
        
        #Stemming
        x<-PlainTextDocument(x)
        x<-stemDocument(x)
        x<-as.character(x)
        
        x<-stripWhitespace(x)#remove extra-whitespaces
        x
}
data_hd[,!(names(data_hd)%in%c("name","id"))]<-
        apply(data_hd[,!(names(data_hd)%in%c("name","id"))],
                                       2,preproc)

#Save in a directory called proc_files
if(!dir.exists("./proc_files")){dir.create("./proc_files")}

if(!exists("./proc_files/data_hd")){
        save(data_hd,file="./proc_files/data_hd")
}
if(!exists("./proc_files/help_data")){
        save(help_data,file="./proc_files/help_data")
}
