
#TfIdf features
#more information about latent semantic analysis
#here:http://www.ling.ohio-state.edu/~reidy/LSAtutorial.pdf

tfidf<-function(text,sparse=0.995,n_gram=4){
        
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
