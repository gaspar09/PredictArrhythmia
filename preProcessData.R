#-----------
# Fri nov 14
options(width=300)
# load packages, install if not already installed
pload = function(package_name){
  if(!require(package_name, character.only=T)){ 
    install.packages(package_name, dependencies=T, repos="http://cran.cnr.berkeley.edu/")
    require(package_name, character.only=T, quietly=T)
    # suppressPackageStartupMessages()
  }
}

# import data
pload("RCurl")
data_url = getURL("https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data")
data_raw = read.csv(text = data_url, stringsAsFactors=F, header=F, sep=",")

#exprsFile = "~/Desktop/Project/arrhythmiaMultinomial.txt"
exprsFile = "~/Desktop/Project/arrhythmiaBinary.txt"
data_clean <- data.frame(read.table(exprsFile, header = F, sep=","))
# some are factors
factor_vars = c(2,22:27,34:39,46:51,58:63,72:75,82:87,94:99,106:111,118:123,130:135,142:147,154:159,280)
for(i in factor_vars) data_clean[,i]=as.factor(data_clean[,i])


# enforce a minimum of 2 levels per factor
data_clean_bkup = data_clean
for(i in 1:(ncol(data_clean)-1))
{
  if( is.factor(data_clean[,i]) ){
    if( length(levels(data_clean[,i])) < 2 )
    {
      levels(data_clean[,i]) = c(0,1) #bad practice to do this without checking
    }
  }
}
#levels(data_clean[,280]) = c(1:16)

data_clean_bkup = data_clean
cols_to_remove = c()
cols_NA = c()
for(i in 1:ncol(data_clean)){
  
  if(!is.factor(data_clean[,i]))
  {
    range_i = range( data_clean[,i], na.rm=T )
    range_i_diff = range_i[2] - range_i[1]
    print( range_i_diff )
    nNA = sum(is.na(data_clean[,i]))

      
        if ( (range_i_diff == 0)  | (nNA>0)){
     if ((nNA>0)){
        cols_NA = append(cols_NA, i) # omit the column
      }
      cols_to_remove = append(cols_to_remove, i) # omit the column
    }
  }
}

data_clean <- data_clean[, -14]

rows_NA = c()
for(i in 1:nrow(data_clean)){
  nNA = sum(is.na(data_clean[i,]))
  if ((nNA>0)){
    rows_NA = append(rows_NA, i) # omit the rowumn
  }
}
data_clean = data_clean[-rows_NA,]
# sanity check
data_clean[,cols_to_remove] # they are all 0
#data_clean = data_clean[,-cols_to_remove] #skip this line to avoid removing columns with na since we already remove the rows

write.table(data_clean, "~/Desktop/Project/clean_data.csv", sep=",", row.names=F, col.names=T, quote=F)

