#-----------------------------------------------------------------------------------------
# load packages, install if not already installed
pload = function(package_name){
  if(!require(package_name, character.only=T)){ 
    install.packages(package_name, dependencies=T, repos="http://cran.cnr.berkeley.edu/")
    require(package_name, character.only=T, quietly=T)
    # suppressPackageStartupMessages()
  }
}
#-----------------------------------------------------------------------------------------
# split data into two fraction (1) train and (2) test
test_train_indicies = function(sample_size, fractional_test_size){
  
  if(fractional_test_size < 1.1) stop("fractional_test_size must be above 1.1")
  
  idx = 1:sample_size
  fractional_test_size = floor(sample_size/ fractional_test_size)
  permute_idx = sample(idx)
  
  test_set = permute_idx[ 1:fractional_test_size ]
  train_set = permute_idx [ (fractional_test_size+1):sample_size ]
  
  out = list(test = test_set, train = train_set)
  return(out)
  
  # test_train_indicies(100, 4)
}


#-----------------------------------------------------------------------------------------
# returns the model with one SE of global min

get_cp = function(rpart_obj_in){
  # returns the simplest model within one SE of the global min
  row_min = which(rpart_obj_in$cptable[,"xerror"] == min(rpart_obj_in$cptable[,"xerror"]))
  cp_min = rpart_obj_in$cptable[row_min,"CP"]
  one_se_rule = rpart_obj_in$cptable[row_min,"xerror"] + rpart_obj_in$cptable[row_min,"xstd"]
  smallest_model_row = min(which(rpart_obj_in$cptable[,"xerror"] < min(one_se_rule) ))
  cp_min_smallest = rpart_obj_in$cptable[smallest_model_row,"CP"]
  error_min_smallest = rpart_obj_in$cptable[smallest_model_row,"xerror"]
  out = list(
    "cp" = cp_min_smallest, 
    "xerror" = error_min_smallest, 
    "global_min_cp" = cp_min, 
    "global_min_err" = rpart_obj_in$cptable[row_min,"xerror"], 
    "one_se_rule" = one_se_rule,
    "nsplit" = rpart_obj_in$cptable[row_min,"nsplit"]
  )
  return(out)
}

get_cp_miss = function(rpart.obj, test_data, plot_on = F){
  
  # get smallest model within 1 SE of global min CP
  rpart.obj.cp = get_cp(rpart.obj)
  
  # prune model
  rpart.obj.cp.pruned = prune(rpart.obj, cp = rpart.obj.cp$cp)
  
  # run model on test data
  rpart.obj.cp.pruned.pred = predict(rpart.obj.cp.pruned, test_data, type="vector")
  
  # missclassification_rate
  dv = as.character(rpart.obj$call[[2]][[2]])
  missclassification_rate = 1 - (sum(rpart.obj.cp.pruned.pred == as.numeric(test_data[,dv])) / length(rpart.obj.cp.pruned.pred))
  print("Missclassification rate")
  # misclassification_rate
  
  # out
  out = data.frame(best_cp = rpart.obj.cp$cp, missclassification_rate = missclassification_rate, nsplit = rpart.obj.cp$nsplit)
  
  # plot
  if(plot_on == T){
    par(cex=.25, mar = c(5, 4, 6, 2) + 0.1)
    plot(rpart.obj.cp.pruned, branch = 0.01)
    text(rpart.obj.cp.pruned, all = F, pretty = T)
  }
  
  return(out)
}


#-----------------------------------------------------------------------------------------
# get 1SE model, purne model, run on test data, and plots result
plot_rpart = function(rpart.obj, test_data, plot_on = T){
  
  # get smallest model within 1 SE of global min CP
  rpart.obj.cp = get_cp(rpart.obj)$cp
  
  # prune model
  rpart.obj.cp.pruned = prune(rpart.obj, cp = rpart.obj.cp)
  
  # run model on test data
  rpart.obj.cp.pruned.pred = predict(rpart.obj.cp.pruned, test_data, type="vector")
  
  # missclassification_rate
  dv = as.character(rpart.obj$call[[2]][[2]])
  missclassification_rate = 1 - (sum(rpart.obj.cp.pruned.pred == as.numeric(test_data[,dv])) / length(rpart.obj.cp.pruned.pred))
  
  # plots
  if(plot_on == T){
    par(mfrow=c(1,3), cex=.25)
    x11()
    plot(rpart.obj)
    text(rpart.obj)
    x11()
    plotcp(rpart.obj)
    x11()
    plot(rpart.obj.cp.pruned)
    text(rpart.obj.cp.pruned)
  }
  
  print(list("best_cp" = rpart.obj.cp, "missclassification_rate" = missclassification_rate))
  
  return(rpart.obj.cp.pruned)
}


mse = function(x) mean(x^2)

