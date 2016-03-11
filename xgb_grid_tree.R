
#xgb_grid

xgb_grid_tree<-function(
        y=y,
        tfidf_train=tfidf_train,
        dist_train=dist_train,
        n_folds=10,
        n_comp,
        eta,
        nrounds,
        max_depth,
        min_child_weight,
        colsample_bytree){
        
        
        #Parallel backend
        n_cores<-detectCores()
        clust<-makeCluster(n_cores)
        registerDoParallel(clust)
        
        tune_grid<-expand.grid(n_comp=n_comp,
                               eta=eta,
                               nrounds=nrounds,
                               max_depth=max_depth,
                               min_child_weight=min_child_weight,
                               colsample_bytree=colsample_bytree)
        
        best<-data.frame()
        #Each core does a k-cross-validation task
        for(j in 0:(nrow(tune_grid)/n_cores-1)){
                
                tune_grid_j<-tune_grid[(n_cores*j+1):(n_cores*j+n_cores),]
                
                results<-
                        foreach(gridID=c(1:nrow(tune_grid_j)),
                                .packages=c("xgboost","caret","rARPACK",
                                            "Metrics","Matrix"),
                                .combine=rbind,.multicombine=T)%dopar%{
                                        
                                        set.seed(1308)
                                        folds<-createFolds(y,n_folds)
                                        
                                        svd_ncomp<-tune_grid_j[gridID,"n_comp"]
                                        eta<-tune_grid_j[gridID,"eta"]
                                        max_depth<-tune_grid_j[gridID,"max_depth"]
                                        min_child_weight<-tune_grid_j[gridID,"min_child_weight"]
                                        nrounds<-tune_grid_j[gridID,"nrounds"]
                                        colsample_bytree<-tune_grid_j[gridID,"colsample_bytree"]
                                        
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
                                                
                                                #Create an xgb.DMatrix object
                                                tfidf_train_cv<-xgb.DMatrix(as.matrix(tfidf_train_cv),
                                                                            label=as.matrix(y_train_cv))
                                                
                                                #Train gradient boosting
                                                params=list(
                                                        booster="gbtree",
                                                        eta=eta,
                                                        max_depth=max_depth,
                                                        min_child_weight=min_child_weight,
                                                        colsample_bytree=colsample_bytree)
                                                
                                                watchlist<-list(eval=tfidf_train_cv,
                                                                train=tfidf_train_cv)
                                                
                                                set.seed(1308)
                                                model_cv<-xgb.train(data=tfidf_train_cv,
                                                                    nrounds=nrounds,
                                                                    params=params,
                                                                    watchlist=watchlist)
                                                
                                                #Predictions
                                                preds_cv<-predict(model_cv,
                                                                  newdata=as.matrix(tfidf_test_cv))
                                                preds_cv[preds_cv>3]<-3
                                                preds_cv[preds_cv<1]<-1
                                                
                                                #Get metrics
                                                rmse_cv<-rmse_cv+rmse(y_test_cv,preds_cv)
                                                
                                        }
                                        return(c(
                                                "rmse"=rmse_cv/n_folds,
                                                "svd_ncomp"=svd_ncomp,
                                                "eta"=eta,
                                                "nrounds"=nrounds,
                                                "max_depth"=max_depth,
                                                "min_child_weight"=min_child_weight,
                                                "colsample_bytree"=colsample_bytree
                                                
                                        ))
                                }
                on.exit(stopCluster(clust))
                
                #Get best results
                results<-data.frame(results,row.names=NULL)
                best<-rbind(best,results[order(results$rmse,decreasing=F),][1,])                
        }
        best
}
