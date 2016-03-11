
#match_feat
#matching and counting features
match_length<-function(search_term,product_title,
                       product_descr,brand){
        
        search<-unlist(strsplit(search_term," "))
        n_search<-length(search)
        n_title<-length(unlist(strsplit(product_title," ")))
        n_descr<-length(unlist(strsplit(product_descr," ")))
        n_brand<-length(unlist(strsplit(brand," ")))
        
        match_title<-0
        match_descr<-0
        match_brand<-0
        for(i in 1:n_search){
                pattern<-paste("(^| )",search[i],"($| )",sep="")
                match_title<-match_title+grepl(pattern,product_title,
                                               perl=TRUE,ignore.case=TRUE)
                match_descr<-match_descr+grepl(pattern,product_descr,
                                               perl=TRUE,ignore.case=TRUE)
                match_brand<-match_brand+grepl(pattern,brand,
                                               perl=TRUE,ignore.case=TRUE)
        }
        return(c(n_search,n_title,n_descr,n_brand,
                 match_title/n_title,match_descr/n_descr,match_brand/n_brand))
}