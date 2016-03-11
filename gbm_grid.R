
#gbm model
#grid search in parallel

gbm_grid<-function(
        y=y,
        tfidf_train=tfidf_train,
        dist_train=dist_train,
        n_folds=10,
        n_comp,
        shrink,
        ntrees,
        depth,
        n_minob,
        bag_frac){
        
        
        #Parallel backend
        n_cores<-detectCores()
        clust<-makeCluster(n_cores)
        registerDoParallel(clust)
        
        tune_grid<-expand.grid(n_comp=n_comp,
                               shrink=shrink,
                               depth=depth,
                               n_minob=n_minob,
                               bag_frac=bag_frac)
        
        best<-data.frame()
        #Each core does a k-cross-validation task
        for(j in 0:(nrow(tune_grid)/n_cores-1)){
                
                tune_grid_j<-tune_grid[(n_cores*j+1):(n_cores*j+n_cores),]
                
                results<-
                        foreach(gridID=c(1:nrow(tune_grid_j)),
                                .packages=c("gbm","caret","rARPACK",
                                            "Metrics","Matrix"),
                                .combine=rbind,.multicombine=T)%dopar%{
                                        
                                        set.seed(1308)
                                        folds<-createFolds(y,n_folds)
                                        
                                        svd_ncomp<-tune_grid_j[gridID,"n_comp"]
                                        shrink<-tune_grid_j[gridID,"shrink"]
                                        depth<-tune_grid_j[gridID,"depth"]
                                        n_minob<-tune_grid_j[gridID,"n_minob"]
                                        bag_frac<-tune_grid_j[gridID,"bag_frac"]
                                        
                                        #Cross-validation
                                        rmse_cv<-0
                                        
                                        for(i in 1:n_folds){
                                                
                                                #Get the folds
                                                tfidf_train_cv<-tfidf_train[-folds[[i]],]
                                                tfidf_test_cv<-tfidf_train[folds[[i]],]
                                                dist_train_cv<-dist_train[-folds[[i]],]
                                                dist_test_cv<-dist_train[folds[[i]],]
                                                y_train_cv<-y[-folds[[i]]]
                                                y_test_cv<-y[folds[[i]]]
                                                
                                                #SVD
                                                svd_train_cv<-svds(tfidf_train_cv,
                                                                   k=svd_ncomp,
                                                                   nv=svd_ncomp,
                                                                   nu=0)
                                                
                                                #Mapping
                                                tfidf_train_cv<-
                                                        tfidf_train_cv%*%svd_train_cv$v
                                                tfidf_test_cv<-
                                                        tfidf_test_cv%*%svd_train_cv$v
                                                
                                                #Include distance and other features
                                                #in tfidf matrix
                                                tfidf_train_cv<-cbind(tfidf_train_cv,
                                                                      as.matrix(dist_train_cv))
                                                tfidf_test_cv<-cbind(tfidf_test_cv,
                                                                     as.matrix(dist_test_cv))
                                                
                                                #Train gradient boosting
                                                model_cv<-gbm.fit(tfidf_train_cv,
                                                                  y=y_train_cv,
                                                                  distribution="gaussian",
                                                                  interaction.depth=depth,
                                                                  shrinkage=shrink,
                                                                  n.minobsinnode=n_minob,
                                                                  bag.fraction=bag_frac,
                                                                  n.trees=ntrees)
                                                
                                                #Predictions
                                                preds_cv<-predict(model_cv,
                                                                  newdata=as.data.frame(
                                                                          as.matrix(tfidf_test_cv)),
                                                                  n.trees=ntrees)
                                                preds_cv[preds_cv>3]<-3
                                                preds_cv[preds_cv<1]<-1
                                                
                                                #Get metrics
                                                rmse_cv<-rmse_cv+rmse(y_test_cv,preds_cv)
                                                        
                                        }
                                        return(c(
                                                "rmse"=rmse_cv/n_folds,
                                                "svd_ncomp"=svd_ncomp,
                                                "shrinkage"=shrink,
                                                "depth"=depth,
                                                "nminob"=n_minob,
                                                "bag_frac"=bag_frac
                                                
                                        ))
                                }
                on.exit(stopCluster(clust))
                
                #Get best results
                results<-data.frame(results,row.names=NULL)
                best<-rbind(best,results[order(results$rmse,decreasing=F),][1,])                
        }
        best
}


