
#starting script

#Working directory
setwd("C:/Users/Sarah/Desktop/Data Science/Projects/Home_depot")


#Packages to be used
lapply(c("dplyr","tm","stringdist",
         "Matrix","doParallel","foreach",
         "rARPACK","caret","gbm"),require,character.only=TRUE)

if (Sys.getenv("JAVA_HOME")!="")#Java dependencies (RWeka package)
        Sys.setenv(JAVA_HOME="");
library("RWeka")

#Loading data sets

test<-read.csv("test.csv");train<-read.csv("train.csv")

#Merge train and test with product_descriptions
#I chose not to use the attributes data set in this starting script,
#for simplicity purpose
train<-merge(train,read.csv("product_descriptions.csv"),by="product_uid")
test<-merge(test,read.csv("product_descriptions.csv"),by="product_uid")

#Prepare the data
y<-train$relevance
train_id<-train$id
test_id<-test$id
n_train<-nrow(train)

train<-select(train,-product_uid,-id,-relevance)
test<-select(test,-product_uid,-id)


#Variables are categorical, convert them to character
train<-as.data.frame(lapply(train,as.character),stringsAsFactors=F)
test<-as.data.frame(lapply(test,as.character),stringsAsFactors=F)


#Slight preprocessing; 
#using tm package
#More information here: https://cran.r-project.org/web/packages/tm/tm.pdf
trans<-function(x){
        x<-iconv(enc2utf8(x),sub="byte")#UTF8 encoding
        x<-tolower(x)#convert upper cases to lower ones
        x<-removePunctuation(x)
        x<-removeNumbers(x)
        #remove stopwords against english dictionary
        #SMART is available, it's longer
        #viewable using either stopwords("english")
        #or stopwords("SMART")
        x<-removeWords(x,stopwords("english"))
        x<-stripWhitespace(x)#remove extra-whitespaces
}
train<-as.data.frame(apply(train,2,trans),stringsAsFactors=F)
test<-as.data.frame(apply(test,2,trans),stringsAsFactors=F)


#Create features

#Cosine distance between search_term and product_title
train$cos_title<-stringdist(train$search_term,train$product_title,
                                   method="cosine")
test$cos_title<-stringdist(test$search_term,test$product_title,
                                  method="cosine")

#Cosine distance between search_term and product_descriptions
train$cos_descr<-stringdist(train$search_term,train$product_description,
                                   method="cosine")
test$cos_descr<-stringdist(test$search_term,test$product_description,
                                  method="cosine")


#Some search_terms are stopwords (often it's "to")
#We can replace these Inf values with 1
train$cos_title[train$cos_title==Inf]<-1
train$cos_descr[train$cos_descr==Inf]<-1
test$cos_title[test$cos_title==Inf]<-1
test$cos_descr[test$cos_descr==Inf]<-1


#Number of matching words between query and product_title and
#query and product_description
#Number of words in query
match_length<-function(search,product_title,
                         product_descr){
        match_title<-0
        match_descr<-0
        search<-unlist(strsplit(search," "))
        n_search<-length(search)
        for(i in 1:n_search){
                pattern<-paste("(^| )",search[i],"($| )",sep="")
                match_title<-match_title+grepl(pattern,product_title,
                                           perl=TRUE,ignore.case=TRUE)
                match_descr<-match_descr+grepl(pattern,product_descr,
                                         perl=TRUE,ignore.case=TRUE)
        }
        return(c(n_search,match_title,match_descr))
}

#On train
train$n_search<-
        as.data.frame(t(mapply(match_length,
                             train$search_term,
                             train$product_title,train$product_description)))[,1]

train$match_title<-
        as.data.frame(t(mapply(match_length,
                             train$search_term,
                             train$product_title,train$product_description)))[,2]

train$match_descr<-
        as.data.frame(t(mapply(match_length,
                             train$search_term,
                             train$product_title,train$product_description)))[,3]

#On test
test$n_search<-
        as.data.frame(t(mapply(match_length,
                             test$search_term,
                             test$product_title,test$product_description)))[,1]

test$match_title<-
        as.data.frame(t(mapply(match_length,
                             test$search_term,
                             test$product_title,test$product_description)))[,2]

test$match_descr<-
        as.data.frame(t(mapply(match_length,
                             test$search_term,
                             test$product_title,test$product_description)))[,3]

#Vector space model
#Create a document-term matrix with n-grams, 
#from n=1 to 2
#weighting: term-frequency inverse document frequency
#more information about latent semantic analysis
#here:http://www.ling.ohio-state.edu/~reidy/LSAtutorial.pdf
vs<-function(text,sparse=0.95){
        
        #Tokenization here: n-grams from n=1 to 2
        tokenizer<-function(x)NGramTokenizer(x,Weka_control(min=1,max=2))
        control<-list(tokenize=tokenizer,weighting=function(x)
                weightTfIdf(x,normalize=F))#No normalization at this stage
        
        dtm<-Corpus(VectorSource(text))#Create a corpus
        dtm<-DocumentTermMatrix(dtm,control=control)
        
        #Remove terms with at least sparse% of empty
        dtm<-removeSparseTerms(dtm,sparse=sparse)
        
        #Convert to sparse matrix (Matrix package)
        dtm<-Matrix(as.matrix(dtm),sparse=T)
        
        return(dtm)
        
}


#Create character vectors 
text<-paste(train$search_term,
            train$product_title,train$product_description)
text<-c(text,paste(test$search_term,test$product_title,
                   test$product_description))

#Create vector space model and store it as a .csv file
#Be patient here, it can take up to 20 minutes
dtm<-vs(text)

#It can be stored as a .csv file using:
#write.csv(as.data.frame(dtm),"dtm.csv",quote=F,row.names=F)


#Remove train and test data sets
#keep the additional features only
train_dist<-select(train,cos_title,cos_descr,n_search,
                   match_title,match_descr)
test_dist<-select(test,cos_title,cos_descr,n_search,
                  match_title,match_descr)
rm(train,test,text)


#Model

#Gradient boosting in parallel
#best number of components and shrinkage (regularization parameter)
#are found using cross-validation in parallel

n_folds<-5


#Parallel backend
n_cores<-detectCores()
clust<-makeCluster(n_cores)
registerDoParallel(clust)

#Tuning grid
tune_grid<-expand.grid(n_comp=c(floor(ncol(dtm)*1/10),
                                floor(ncol(dtm)*1/4)),
                       shrink=c(0.2,1))

n.trees<-100

#run it in parallel!
#more information on foreach package and its unfancy grammar
#here: https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
results<-
        foreach(gridID=c(1:nrow(tune_grid)),
                
                #packages to be used inside the foreach task
                #Note: I use kernlab for SVM (the fastest), rARPACK for SVD
                #wich provides a scalable routine for high dimensional
                #SVD tasks
                .packages=c("gbm","caret","rARPACK",
                            "Metrics","Matrix"),
                .combine=rbind,.multicombine=T)%dopar%{
                        
                        set.seed(1308)
                        
                        folds<-createFolds(y,n_folds)
                        
                        svd_ncomp<-tune_grid[gridID,"n_comp"]
                        shrink<-tune_grid[gridID,"shrink"]
                        
                        
                        #cross-validation
                        rmse_cv<-0
                        
                        for(i in 1:n_folds){
                                
                                #Get the folds
                                dtm_train_cv<-dtm[1:n_train,][-folds[[i]],]
                                dtm_test_cv<-dtm[-c(1:n_train),][folds[[i]],]
                                
                                dist_train_cv<-train_dist[-folds[[i]],]
                                dist_test_cv<-train_dist[folds[[i]],]
                                
                                y_train_cv<-y[-folds[[i]]]
                                y_test_cv<-y[folds[[i]]]
                                
                                #SVD
                                train_svd_cv<-svds(dtm_train_cv,
                                                   k=svd_ncomp,#(call mapping)
                                                   nv=svd_ncomp,nu=0)
                                
                                #Same mapping to train and test
                                dtm_train_cv<-dtm_train_cv%*%train_svd_cv$v
                                dtm_test_cv<-dtm_test_cv%*%train_svd_cv$v
                                
                                #Include similarity features
                                dtm_train_cv<-cbind(dtm_train_cv,
                                                    as.matrix(dist_train_cv))
                                dtm_test_cv<-cbind(dtm_test_cv,
                                                   as.matrix(dist_test_cv))
                                
                                #Train GBM
                                model_cv<-gbm.fit(dtm_train_cv,y_train_cv,
                                                  distribution="gaussian",
                                                  interaction.depth=1,shrinkage=shrink,
                                                  n.trees=n.trees)
                                
                                
                                preds_cv<-predict(model_cv,
                                                  newdata=as.data.frame(as.matrix(dtm_test_cv)),
                                                  n.trees=n.trees)
                                
                                #Get metrics
                                rmse_cv<-rmse_cv+rmse(y_test_cv,preds_cv)
                                
                        }
                        return(c(
                                "rmse"=rmse_cv/n_folds,
                                "svd_ncomp"=svd_ncomp,
                                "shrinkage"=shrink
                                ))
                        
                }
stopCluster(clust)

#Get best results
results<-data.frame(results,row.names=NULL)
best<-results[order(results$rmse,decreasing=F),][1,]

#SVD with best n_comp
train_svd<-svds(dtm[1:n_train,],k=best$svd_ncomp,
                nv=best$svd_ncomp,nu=0)

#Map to train and test
dtm_train<-dtm[1:n_train,]%*%train_svd$v
dtm_test<-dtm[-c(1:n_train),]%*%train_svd$v

#Add additional features
dtm_train<-cbind(dtm_train,as.matrix(train_dist))
dtm_test<-cbind(dtm_test,as.matrix(test_dist))

#Train GBM
model<-gbm.fit(dtm_train,y,
               distribution="gaussian",
               interaction.depth=1,shrinkage=best$shrink,
               n.trees=n.trees)

#Get predictions
preds<-predict(model,newdata=as.matrix(dtm_test),
               n.trees=n.trees)

#Submission
submission<-write.csv(data.frame("Id"=test_id,"Prediction"=preds),
                      "submission.csv",quote=F,row.names=F)


