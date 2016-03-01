
#home_depot2
#feature extraction

#Libraries
lapply(c("dplyr","caret","Matrix","tm","stringdist","doParallel","foreach",
         "rARPACK","gbm"),require,character.only=T)

if (Sys.getenv("JAVA_HOME")!="")#Java dependencies (RWeka package)
        Sys.setenv(JAVA_HOME="");
library("RWeka")

#Put your working directory here
dir<-"C:/Users/Sarah/Desktop/Data Science/Projects/Home_depot"

if(getwd()!=dir){setwd(dir)}

if(!exists("data_hd")){load("./proc_files/data_hd")}
if(!exists("help_data")){load("./proc_files/help_data")}

#Create binary variable that indicates if result has a 
#brand name
names(data_hd)[names(data_hd)=="name"]<-"brand"
data_hd$brand[is.na(data_hd$brand)]<-0
data_hd$brand[data_hd$brand!=0]<-1
data_hd$brand<-as.numeric(data_hd$brand)

#Similarity/distance features
#cosine, Jaccard, Jaro-Winkler
#search_term - product_title
#search_term - product_description
data_hd$cos_title<-stringdist(data_hd$search_term,
                              data_hd$product_title,method="cosine")
data_hd$cos_descr<-stringdist(data_hd$search_term,
                              data_hd$product_description,method="cosine")
data_hd$jac_title<-stringdist(data_hd$search_term,
                              data_hd$product_title,method="jaccard")
data_hd$jac_descr<-stringdist(data_hd$search_term,
                              data_hd$product_description,method="jaccard")
data_hd$jw_title<-stringdist(data_hd$search_term,
                              data_hd$product_title,method="jw")
data_hd$jw_descr<-stringdist(data_hd$search_term,
                              data_hd$product_description,method="jw")

#Inf entries-->1
#1 is no similarity
data_hd$cos_title[data_hd$cos_title==Inf]<-0
data_hd$cos_descr[data_hd$cos_descr==Inf]<-0
data_hd$jac_title[data_hd$jac_title==Inf]<-0
data_hd$jac_descr[data_hd$jac_descr==Inf]<-0


#Number of matching characters search_term/product_title,
#search_term/product_description
#word counts in search_term,product_title and product_description
match_length<-function(search_term,product_title,
                       product_descr){
        
        search<-unlist(strsplit(search_term," "))
        n_search<-length(search)
        n_title<-length(unlist(strsplit(product_title," ")))
        n_descr<-length(unlist(strsplit(product_descr," ")))
        
        match_title<-0
        match_descr<-0
        for(i in 1:n_search){
                pattern<-paste("(^| )",search[i],"($| )",sep="")
                match_title<-match_title+grepl(pattern,product_title,
                                               perl=TRUE,ignore.case=TRUE)
                match_descr<-match_descr+grepl(pattern,product_descr,
                                               perl=TRUE,ignore.case=TRUE)
        }
        return(c(n_search,n_title,n_descr,match_title,match_descr))
}
match_feat<-as.data.frame(t(mapply(match_length,
                                         data_hd$search_term,
                                         data_hd$product_title,
                                         data_hd$product_description)))
data_hd$n_search<-match_feat[,1]
data_hd$n_title<-match_feat[,2]
data_hd$n_descr<-match_feat[,3]
data_hd$match_title<-match_feat[,4]
data_hd$match_descr<-match_feat[,5]


#TfIdf features
#more information about latent semantic analysis
#here:http://www.ling.ohio-state.edu/~reidy/LSAtutorial.pdf
tfidf<-function(text,sparse=0.95,n_gram=2){
        
        tokenizer<-function(x)NGramTokenizer(x,Weka_control(min=1,max=n_gram))
        control<-list(tokenize=tokenizer,weighting=function(x)
                weightTfIdf(x,normalize=T))
        
        dtm<-Corpus(VectorSource(text))
        dtm<-DocumentTermMatrix(dtm,control=control)
        
        #Remove terms with at least sparse% of empty
        dtm<-removeSparseTerms(dtm,sparse=sparse)
        
        #Sparse matrix, more efficient
        dtm<-Matrix(as.matrix(dtm),sparse=T)
        
        return(dtm)
        
}
text<-paste(data_hd$search_term,
            data_hd$product_title,data_hd$product_description)


#Distance, match and counting features
dist_feat<-select(data_hd,-product_uid,-product_title,
                  -search_term,-product_description)

#Keep ids in data_hd
help_data$id_data_hd<-data_hd$id


#TfIdf features
tfidf_feat<-tfidf(text)
tfidf_feat@Dimnames$Docs<-help_data$id_data_hd#Keep initial ids

#Get tfidf train and test features
tfidf_train<-tfidf_feat[rownames(tfidf_feat)%in%help_data$train_id,]
tfidf_train<-tfidf_train[order(rownames(tfidf_train)),]#get initial order

tfidf_test<-tfidf_feat[rownames(tfidf_feat)%in%help_data$test_id,]
tfidf_test<-tfidf_test[order(rownames(tfidf_test)),]

#Get distances train and test features
dist_train<-dist_feat[dist_feat$id%in%help_data$train_id,]
dist_train<-dist_train[order(dist_train$id),];
dist_train<-dist_train[,!(names(dist_train)%in%"id")]

dist_test<-dist_feat[dist_feat$id%in%help_data$test_id,]
dist_test<-dist_test[order(dist_test$id),];
dist_test<-dist_test[,!(names(dist_test)%in%"id")]

#Save everything
save(help_data,file="./proc_files/help_data")
save(dist_feat,file="./proc_files/dist_feat")
save(tfidf_feat,file="./proc_files/tfidf_feat")
save(dist_train,file="./proc_files/dist_train")
save(dist_test,file="./proc_files/dist_test")
save(tfidf_train,file="./proc_files/tfidf_train")
save(tfidf_test,file="./proc_files/tfidf_test")



