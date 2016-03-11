#home_dep3
#models 

#Put your working directory here
dir<-"C:/Users/Sarah/Desktop/Data Science/Projects/Home_depot"

if(getwd()!=dir){setwd(dir)}

if(!exists("tfidf_train")){load("./proc_files/tfidf_train")}
if(!exists("dist_train")){load("./proc_files/dist_train")}
if(!exists("help_data")){load("./proc_files/help_data")}
if(!exists("gbm_grid")){source("./gbm_grid.R")}


#Best models gbm
#Keep info about results of grid search
params=list(n_comp=c(2,3),shrink=c(1,2),
            ntrees=200,depth=c(1,2),n_minob=c(100,300),
            bag_frac=c(0.7,0.9))

gbm_results<-gbm_grid(
        y=help_data$y,
        tfidf_train=tfidf_train,
        dist_train=dist_train,
        n_folds=10,
        n_comp=params$n_comp,
        shrink=params$shrink,
        ntrees=params$ntrees,
        depth=params$depth,
        n_minob=params$n_minob,
        bag_frac=params$bag_frac)



