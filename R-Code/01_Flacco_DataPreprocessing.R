# Group 03
# Team Members
# git test
#
#
#

#Part01 Data Preprocessing
#
#All methods which are necessary to preprocess the flacco data for further analysis in Part02.

#-------------------------------------------------------------------------------------------------------
#1.1 Flacco-specific preprocessing
#Special Preprocessing techniques for the special needs of the flacco dataset

#load dataset
load("../3-flacco1.RData")
str(feats)

#exclude the technical features at the beginning of the dataset (such as repl or seed) -> first 7 features
#these features are input parameter to the dataset generator and thus not subject to most of further analysis
#in some analysis it is paid attention to and then we will access by getting this information from metadata
#topology 6 is kept in the dataset as it is some feature of the problem instance (see papers)
afeats = feats[,-c(seq(1,5),7)]
metadata = feats[,c(seq(1,5),7)]

#exclude features with variance of zero
#these do not give further information on the problem instances
#furthermore these features will lead to problems in PCA, etc. because the variance cannot be normalized
#following features are excluded due to that:
#cm_angle.costs_fun_evals, cm_conv.costs_fun_evals, cm_grad.costs_fun_evals, ela_conv.lin_prob,
#ela_conv.costs_fun_evals, ela_curv.sample_size, ela_local.n_loc_opt.abs, ela_local.n_loc_opt.rel
var0feats = which(sapply(1:length(afeats), function(x) {var(afeats[,x])}) == 0)
bfeats = afeats[,-var0feats]

#result is a dataset of 58 expensive features based on the flacco dataset which is used for further tasks
#feature groups (used for detailed inter-group / intra-group analysis)
bfeats.cm_angle = bfeats[,seq(2,10)]
bfeats.cm_conv = bfeats[,seq(11,15)]
bfeats.cm_grad = bfeats[,seq(16,18)]
bfeats.ela_conv = bfeats[,seq(19,22)]
bfeats.ela_curv = bfeats[,seq(23,45)]
bfeats.ela_local= bfeats[,seq(46,59)]

#------------------------------------------------------------------------------------------------------------

#1.1 Visualization



#-----------------------------------------------------------------------------------------------------------

#1.2 Outlier Detection


#-----------------------------------------------------------------------------------------------------------

#1.3 Tests for Normality


#-----------------------------------------------------------------------------------------------------------

#1.4 Transformation to Normality


#-----------------------------------------------------------------------------------------------------------


