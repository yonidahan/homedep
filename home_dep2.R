
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


#Similarity/distance features

#Cosine search_term-product_title
data_hd$cos_title1<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="cosine",q=1)
data_hd$cos_title2<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="cosine",q=2)
data_hd$cos_title3<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="cosine",q=3)
data_hd$cos_title4<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="cosine",q=4)

data_hd$cos_title1[data_hd$cos_title1==Inf]<-1#when search_term=""
data_hd$cos_title2[data_hd$cos_title2==Inf]<-1
data_hd$cos_title3[data_hd$cos_title3==Inf]<-1
data_hd$cos_title4[data_hd$cos_title4==Inf]<-1

#Cosine search_term-product_description
data_hd$cos_descr1<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="cosine",q=1)
data_hd$cos_descr2<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="cosine",q=2)
data_hd$cos_descr3<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="cosine",q=3)
data_hd$cos_descr4<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="cosine",q=4)
data_hd$cos_descr1[data_hd$cos_descr1==Inf]<-1#when search_term=""
data_hd$cos_descr2[data_hd$cos_descr2==Inf]<-1
data_hd$cos_descr3[data_hd$cos_descr3==Inf]<-1
data_hd$cos_descr4[data_hd$cos_descr4==Inf]<-1

#Cosine search-term-brand
data_hd$cos_brand1<-stringdist(data_hd$search_term,data_hd$brand,
                               method="cosine",q=1)
data_hd$cos_brand2<-stringdist(data_hd$search_term,data_hd$brand,
                           method="cosine",q=2)
data_hd$cos_brand3<-stringdist(data_hd$search_term,data_hd$brand,
                           method="cosine",q=3)
data_hd$cos_brand4<-stringdist(data_hd$search_term,data_hd$brand,
                           method="cosine",q=4)
data_hd$cos_brand1[data_hd$cos_brand1==Inf]<-1
data_hd$cos_brand2[data_hd$cos_brand2==Inf]<-1
data_hd$cos_brand3[data_hd$cos_brand3==Inf]<-1
data_hd$cos_brand4[data_hd$cos_brand4==Inf]<-1


#Jaccard search_term - product_title
data_hd$jac_title1<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="jaccard",q=1)
data_hd$jac_title2<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="jaccard",q=2)
data_hd$jac_title3<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="jaccard",q=3)
data_hd$jac_title4<-stringdist(data_hd$search_term,data_hd$product_title,
                               method="jaccard",q=4)

data_hd$jac_title1[data_hd$jac_title1==Inf]<-1
data_hd$jac_title2[data_hd$jac_title2==Inf]<-1
data_hd$jac_title3[data_hd$jac_title3==Inf]<-1
data_hd$jac_title4[data_hd$jac_title4==Inf]<-1

#Jaccard search_term - product_description
data_hd$jac_descr1<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="jaccard",q=1)
data_hd$jac_descr2<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="jaccard",q=2)
data_hd$jac_descr3<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="jaccard",q=3)
data_hd$jac_descr4<-stringdist(data_hd$search_term,data_hd$product_description,
                               method="jaccard",q=4)
data_hd$jac_descr1[data_hd$jac_descr1==Inf]<-1
data_hd$jac_descr2[data_hd$jac_descr2==Inf]<-1
data_hd$jac_descr3[data_hd$jac_descr3==Inf]<-1
data_hd$jac_descr4[data_hd$jac_descr4==Inf]<-1

#Jaccard search_term - brand
data_hd$jac_brand1<-stringdist(data_hd$search_term,data_hd$brand,
                               method="jaccard",q=1)
data_hd$jac_brand2<-stringdist(data_hd$search_term,data_hd$brand,
                               method="jaccard",q=2)
data_hd$jac_brand3<-stringdist(data_hd$search_term,data_hd$brand,
                               method="jaccard",q=3)
data_hd$jac_brand4<-stringdist(data_hd$search_term,data_hd$brand,
                               method="jaccard",q=4)
data_hd$jac_brand1[data_hd$jac_brand1==Inf]<-1
data_hd$jac_brand2[data_hd$jac_brand2==Inf]<-1
data_hd$jac_brand3[data_hd$jac_brand3==Inf]<-1
data_hd$jac_brand4[data_hd$jac_brand4==Inf]<-1

#Jaro-Wrinkler search_term - product_title
data_hd$jw_title<-stringdist(data_hd$search_term,data_hd$product_title,
                             method="jw")

#Jaro-Wrinkler search_term - product_description
data_hd$jw_descr<-stringdist(data_hd$search_term,data_hd$product_description,
                             method="jw")

#Jaro-Wrinkler search_term - brand
data_hd$jw_brand<-stringdist(data_hd$search_term,data_hd$brand,
                             method="jw")

#OSA search_term - product_title
data_hd$osa_title<-stringdist(data_hd$search_term,data_hd$product_title,
                              method="osa")

#OSA search_term - product_description
data_hd$osa_descr<-stringdist(data_hd$search_term,data_hd$product_description,
                              method="osa")

#OSA search_term - brand
data_hd$osa_brand<-stringdist(data_hd$search_term,data_hd$brand,
                              method="osa")


#Full Damerau Levenshtein distance search_term - product_title
data_hd$dl_title<-stringdist(data_hd$search_term,data_hd$product_title,
                             method="dl")

#Full Damerau Levenshtein distance search_term - product_description
data_hd$dl_descr<-stringdist(data_hd$search_term,data_hd$product_description,
                             method="dl")

#Full Damerau Levenshtein distance search_term - brand
data_hd$dl_brand<-stringdist(data_hd$search_term,data_hd$brand,
                             method="dl")

#Levenshtein distance search_term - product_title
data_hd$lv_title<-stringdist(data_hd$search_term,data_hd$product_title,
                             method="lv")

#Levenshtein distance search_term - product_description
data_hd$dl_descr<-stringdist(data_hd$search_term,data_hd$product_description,
                             method="dl")

#Levenshtein distance search_term - brand
data_hd$dl_brand<-stringdist(data_hd$search_term,data_hd$brand,
                             method="dl")

#Longest common substring distance search_term - product_title
data_hd$lcs_title<-stringdist(data_hd$search_term,data_hd$product_title,
                             method="lcs")

#Longest common substring distance search_term - product_description
data_hd$lcs_title<-stringdist(data_hd$search_term,data_hd$product_description,
                              method="lcs")

#Longest common substring distance search_term - brand
data_hd$lcs_rand<-stringdist(data_hd$search_term,data_hd$brand,
                              method="lcs")



#Matching and counting features
#Matching: same as before
#number of words in search_term, product_title, product_description, brand
source("./match_feat.R")
match_feat<-as.data.frame(t(mapply(match_length,
                                   data_hd$search_term,
                                   data_hd$product_title,
                                   data_hd$product_description,
                                   data_hd$brand)))
data_hd$n_search<-match_feat[,1]
data_hd$n_title<-match_feat[,2]
data_hd$n_descr<-match_feat[,3]
data_hd$n_brand<-match_feat[,4]
data_hd$match_title<-match_feat[,5]
data_hd$match_descr<-match_feat[,6]
data_hd$match_brand<-match_feat[,7]

dist_feat<-select(data_hd,-product_title,-product_uid,
                  -search_term,-product_description,-brand)

help_data$id_data_hd<-data_hd$id#Keep ids in data_hd


#TfIdf features
source("./tfidf.R")
text<-paste(data_hd$search_term,
            data_hd$product_title,data_hd$brand)
tfidf_feat<-tfidf(text)
tfidf_feat@Dimnames$Docs<-help_data$id_data_hd#Keep initial ids

#tfidf score
dist_feat$tfidf_score<-apply(tfidf_feat,1,sum)

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


