# Group 03
# Team Members
# Christian Siemen (394724)
# Lucas Stegger (394881)
# Yongsun Park (425844)
#

#Responsiblity Code:    Alltogether
#
#Specific Questions:
#   1.1 Visualization   Yongsun  Park
#   1.2, 1.3 Normality  Gino  S.  Colletti
#   1.4 Outliers        Lucas Stegger
#   2.1 PCA             Martin Kubicki
#   2.2 MDS             Christian Siemen
#   2.3 Cluster         Daniel  Camiola

#Part01 Data Preprocessing
#
#All methods which are necessary to preprocess the flacco data for further analysis in Part02.

#load custom functions
load("../3-customFunctions.RData")

#-------------------------------------------------------------------------------------------------------
#1.0 Flacco-specific preprocessing
#Special Preprocessing techniques for the special needs of the flacco dataset
section.new("1-1")

#load dataset and inspect
load("../3-flacco1.RData")
str(feats)
summary(feats)

#exclude the technical features at the beginning of the dataset (such as prob.seed or repl) -> first 7 features
#these features are input parameters to the dataset generator and thus not subject to most of further analysis
#in some analysis it is paid attention to and then we will access by getting this information from metadata
#Column 6(topology) is kept in the dataset as it is some feature of the problem instance (see papers)
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

#convert categorical var "topology" into numerical equivalent
bfeats[,1] = as.integer(factor(bfeats[,1], labels=c(0,1)))

#set rownames accoring to rownumber
attributes(bfeats)$row.names = as.character(1:nrow(bfeats))

#result is a dataset of 58 expensive features based on the flacco dataset which is used for further tasks
#feature groups (used for detailed inter-group / intra-group analysis)
bfeats.topology = bfeats[,1]
bfeats.cm_angle = bfeats[,seq(2,10)]
bfeats.cm_conv = bfeats[,seq(11,15)]
bfeats.cm_grad = bfeats[,seq(16,18)]
bfeats.ela_conv = bfeats[,seq(19,22)]
bfeats.ela_curv = bfeats[,seq(23,45)]
bfeats.ela_local= bfeats[,seq(46,59)]

#inspect after pre-preprocessing
str(bfeats)
summary(bfeats)
#no NaN are in the dataset, no columns with var = 0 left
#------------------------------------------------------------------------------------------------------------

#1.1 Visualization
#Visualization of the dataset

#Scatterplots
#correlation within the feature-groups is relatively high. This retrieves that it makes sense in further steps to
#reduce dimensionality
pairs.custom(bfeats.cm_angle, m="Correlation of Features in the group of cm_angle")
#influence of blocks metadata on CM-angle features (clusters for certain features):
pairs.custom(bfeats.cm_angle, m="Correlation of Features in the group of cm_angle",col=colors[metadata[,1]], 
             legend.title="Number of blocks", legend.text=c("3 blocks", "5 blocks", "7 blocks"), 
             legend.col = colors[c(3,5,7)])
pairs.custom(bfeats.cm_conv, m="Correlation of Features in the group of cm_conv")
pairs.custom(bfeats.cm_grad, m="Correlation of Features in the group of cm_grad")
pairs.custom(bfeats.ela_conv, m="Correlation of Features in the group of ela_conv")
pairs.custom(bfeats.ela_curv[,seq(1,7)], m="Correlation of Features in the group of ela_curv (extract)")
pairs.custom(bfeats.ela_curv[,c(1,8,15)], m="Correlation of Features in the group of ela_curv (extract)")
pairs.custom(bfeats.ela_local[,c(6,7,8,9,10,11,12)], m="Correlation of Features in the group of ela_local (extract)")

#it can be seen that the with increasing number of peaks the costs of the ELA functions increase
pairs.custom(data.frame(ela_conv.costs_runtime=bfeats.ela_conv[,4],
                        ela_curv.costs_runtime=bfeats.ela_curv[,23],
                        ela_local.costs_runtime=bfeats.ela_local[,14],
                        peak=metadata[,4]), m="Correlation of the number of peaks with ELA runtime")
#it can be seen that the with increasing number of blocks the costs of the CM functions increase
pairs.custom(data.frame(cm_angle.costs_runtime=bfeats.cm_angle[,9],
                        cm_conv.costs_runtime=bfeats.cm_conv[,5],
                        cm_grad.costs_runtime=bfeats.cm_grad[,3],
                        blocks=metadata[,1]), m="Correlation of the number of blocks with CM runtime")


#by analyzing some of this feature with regard to the repl metadata, one can see the only stochastic parameters are
#the costs_runtime ones. For all other features the different repl are equal.
pairs.custom(bfeats.cm_conv, m="Correlation of Features in the group of cm_conv", col=colors[metadata[,6]])

#examine amount of pairwise correlation within groups
#high for cm_grad
pairs.cor(bfeats.cm_angle)  #0.4126
pairs.cor(bfeats.cm_conv)   #0.4015
pairs.cor(bfeats.cm_grad)   #0.5945
pairs.cor(bfeats.ela_conv)  #0.3032
pairs.cor(bfeats.ela_curv)  #0.1954
pairs.cor(bfeats.ela_local) #0.2519

#examine the correlations between different feature groups by scatterplots
#for each feature group the first PC is included in the scatterplot to cover as much variance as
#possible by one datacolumn in the scatterplot
princomp_feat_groups = data.frame(
bfeats.topology,
princomp(bfeats.cm_angle, corr=TRUE, scores=TRUE)$scores[,1],
princomp(bfeats.cm_conv, corr=TRUE, scores=TRUE)$scores[,1],
princomp(bfeats.cm_grad, corr=TRUE, scores=TRUE)$scores[,1],
princomp(bfeats.ela_conv, corr=TRUE, scores=TRUE)$scores[,1],
princomp(bfeats.ela_curv, corr=TRUE, scores=TRUE)$scores[,1],
princomp(bfeats.ela_local, corr=TRUE, scores=TRUE)$scores[,1])
#check the amount of variance covered by first PC for each group
summary(princomp(bfeats.cm_angle, corr=TRUE, scores=TRUE))  #93.30%
summary(princomp(bfeats.cm_conv, corr=TRUE, scores=TRUE))  #73.38%
summary(princomp(bfeats.cm_grad, corr=TRUE, scores=TRUE))  #95.55%
summary(princomp(bfeats.ela_conv, corr=TRUE, scores=TRUE))  #99.99%
summary(princomp(bfeats.ela_curv, corr=TRUE, scores=TRUE))  #60.06%
summary(princomp(bfeats.ela_local, corr=TRUE, scores=TRUE))  #99.80%
#column names
colnames(princomp_feat_groups) <- c("topology", "cm_angle", "cm_conv", "cm_curv", "ela_conv", "ela_curv", "ela_local")

#view scatterplot
#it can be seen that most of the feature_groups have low correlation between each other
#BUT there are obviously some dependencies:
#topology <> ela_curv
#topology <> ela_local
#cm_angle <> cm_curv
#cm_conv <> ela_conv
#ela_curv <> ela_local
pairs.custom(princomp_feat_groups, m="Correlation between Feature Groups")
pairs.cor(princomp_feat_groups)   #0.3365  correlation of certain groups drives overall correlation

#The blocks argument of the metadata has some effect on the CM-features
pairs.custom(princomp_feat_groups[2:4], m="Correlation between Feature Groups", color=colors[metadata[,1]],
             legend.title="Number of blocks", legend.text=c("3 blocks", "5 blocks", "7 blocks"), 
             legend.col = colors[c(3,5,7)])
#the number of peaks has no significant influence; prob.seed and repl sare neither.


#scatterplot3d
#include package
require(scatterplot3d) 

#analyizing the distance to best, worst and the ratio of best and worst in a 3d scatterplot results
#in showing up three distinct clusters, consisting of a different number of blocks
#explanation: cells in a setting with smaller number of blocks are larger, distances therefore larger too
scatterplot3d.custom(bfeats.cm_angle[,1], bfeats.cm_angle[,3], bfeats.cm_angle[,7],
                     angle=35, main="3D Scatterplot on features within cm_angle", xlab="dist_ctr2best.mean", ylab="dist_ctr2worst.mean", 
                     zlab="y_ratio_best2worst.mean", col=colors[metadata[,1]], legend.title="Number of blocks", 
                     legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])

#also between feature groups in CM there are some dependencies
scatterplot3d.custom(bfeats.cm_angle[,7], bfeats.cm_conv[,4], bfeats.cm_grad[,1],
                     angle=35, main="3D Scatterplot on features within CM", xlab="y_ratio_best2worst.mean", ylab="concave.soft", 
                     zlab="cm_grad.mean", col=colors[metadata[,1]], legend.title="Number of blocks", 
                     legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])

#runtimes within ELA feature are commonly increasing(depending on number of peaks)
scatterplot3d.custom(bfeats.ela_conv[,4], bfeats.ela_curv[,23], bfeats.ela_local[,14],
                     angle=35, main="3D Scatterplot on runtime features within ela features", xlab="ela_conv.runtime", ylab="ela_curv.runtime", 
                     zlab="ela_local.runtime",col=colors[metadata[,4]/20], legend.col=colors[1:10],
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                     "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"))

#dependencies within ELA_curv featureset
scatterplot3d.custom(bfeats.ela_curv[,3], bfeats.ela_curv[,10], bfeats.ela_curv[,17],
                     angle=35, main="3D Scatterplot on features within ela_curv features", xlab="grad_norm.mean", ylab="grad_scale.mean", 
                     zlab="hessian_cond.mean", col=colors[bfeats.topology], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[1:2])

#also between ELA features there are some dependencies. clusters are dependent upon the topology of function
scatterplot3d.custom(bfeats.ela_conv[,1], bfeats.ela_curv[,3], bfeats.ela_local[,3],
                     angle=35, main="3D Scatterplot between features within ela features (by topology)", xlab="ela_conv.conv_prob", ylab="ela_curv.grad_norm.mean", 
                     zlab="ela_local.basin_sizes.avg_best", col=colors[bfeats.topology], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[1:2])


#scatterplots for displaying different feature groups against each other
#analysis of ELA features in scatterplot
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
              angle=35, main="3D Scatterplot on ELA-features", xlab="ela_conv",
              ylab="ela_curv", zlab="ela_local",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)

#although the number of blocks and repl do not have influence on distributrion of ela features,
#the generators seed seems to produce clusters overlying each other (small clusters in seed = 4)
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
                     angle=35, main="3D Scatterplot on ELA-features (prob.seed)", xlab="ela_conv",
                     ylab="ela_curv", zlab="ela_local",col=colors[metadata[,5]], legend.col=colors[1:5],
                     legend.title = "Prob.seed", legend.text = c("1", "2", "3", "4", "5"))

#deeper analyis with regard to the number of peaks by adding information (apparently influence on ELA-features)
#outlier clusters can be seen at number of peaks = 20
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
                     angle=35, main="3D Scatterplot on ELA-features (the number of peaks)", xlab="ela_conv",
                     ylab="ela_curv", zlab="ela_local",col=colors[metadata[,4]/20], legend.col=colors[1:10],
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                      "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"))

#scatterplot for cell mapping feature groups
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
              angle=55, main="3D Scatterplot on CM-features", xlab="cm_angle",
              ylab="cm_conv", zlab="cm_grad",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)

#adding the information of how many blocks were created shows up three distinct clusters
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
                  angle=55, main="3D Scatterplot on CM-features (by the number of blocks)", xlab="cm_angle",
                  ylab="cm_conv", zlab="cm_grad", col=colors[metadata[,1]], legend.title="Number of blocks", 
                  legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])

#adding other metadata like repl or seed does not cluster the data likewise, the features do not depend on which 
#replication or seed is used. Also peaks does not have influence on cm features:
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
                     angle=35, main="3D Scatterplot on CM-features (by the number of peaks)", xlab="cm_angle",
                     ylab="cm_conv", zlab="cm_grad", col=colors[metadata[,4]/20], legend.col=colors[1:10],
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                     "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"))


#find all correlations
cor.detect(bfeats)

#find cor>=0.9
cor.detect(bfeats, l=.9)

#high correlations exist especially between measures in the following feature groups:
#ela_curv and ela_curv
#ela_local and ela_local
#ela_curv and ela_local

#moreover, the following correlations are also high:
#"cor( cm_angle.dist_ctr2best.mean , cm_angle.dist_ctr2worst.mean ) = 0.96488130456667"
#"cor( cm_angle.y_ratio_best2worst.mean , cm_grad.mean ) = -0.941673351717285"
#"cor( cm_conv.convex.soft , cm_conv.concave.soft ) = -1"
#"cor( ela_conv.conv_prob , ela_conv.lin_dev.orig ) = -0.948706591111724"
#"cor( ela_conv.costs_runtime , ela_curv.costs_runtime ) = 0.956381579042816"
#"cor( ela_conv.costs_runtime , ela_local.costs_runtime ) = 0.961137737157008"

#-----------------------------------------------------------------------------------------------------------

#1.2 Tests for normality
section.new("1-2")

#apply the normality tests to single features of the dataset (in groups of 5 for displaying issues)
#it can be seen that assuming a significance niveau of 0.01 for each of the features
#the hypothesis of a normal distribution would be rejected
layout.custom(matrix(1:10, ncol=5,nrow=2), main = "Tests for normality (single features)", plotbreak = 5)
#1-5: p-value close to zero
mapply(normal.custom, x=bfeats[,1:5], title=colnames(bfeats)[1:5])
#6-10: p-value close to zero
mapply(normal.custom, x=bfeats[,6:10], title=colnames(bfeats)[6:10])
#11-15: p-value close to zero
mapply(normal.custom, x=bfeats[,11:15], title=colnames(bfeats)[11:15])
#16-20: p-value close to zero
mapply(normal.custom, x=bfeats[,16:20], title=colnames(bfeats)[16:20])
#21-25: p-value close to zero
mapply(normal.custom, x=bfeats[,21:25], title=colnames(bfeats)[21:25])
#26-30: p-value close to zero
mapply(normal.custom, x=bfeats[,26:30], title=colnames(bfeats)[26:30])
#32-35: p-value close to zero, 31 p-value of 0.04% still very low
mapply(normal.custom, x=bfeats[,31:35], title=colnames(bfeats)[31:35])
#36,37,39,40: p-value close to zero, 38 p-value of 0.0928% still very low
mapply(normal.custom, x=bfeats[,36:40], title=colnames(bfeats)[36:40])
#41-45: p-value close to zero
mapply(normal.custom, x=bfeats[,41:45], title=colnames(bfeats)[41:45])
#46-50: p-value close to zero
mapply(normal.custom, x=bfeats[,46:50], title=colnames(bfeats)[46:50])
#51-55: p-value close to zero
mapply(normal.custom, x=bfeats[,51:55], title=colnames(bfeats)[51:55])
#56-59: p-value close to zero
mapply(normal.custom, x=bfeats[,56:59], title=colnames(bfeats)[56:59])
#manually close device as last plot only contains 8 instead of 10 plots
if(pdf.create == T) dev.off()

#deeper analysis of p-values of SW-Tests for single features
#all of the tests are rejected with sig niv of 0.01
#mean: 0.02261%
bfeats.sw_pvalue = sapply(bfeats[,1:59], function(x) {shapiro.test(as.numeric(x))$p.value})
summary(bfeats.sw_pvalue)
which(bfeats.sw_pvalue > 0.01)

#also whan applying the tests to the PC of the feature groups (see 1.1 Visualization)
#the hypothesis of normal distribution must be rejected for each
#nevertheless the CM seem to be closer to normal distributrion by looking on the graphs
layout.custom(matrix(1:6, ncol=3, nrow=2), main = "Tests for normality (PC of groups)", plotbreak = 3)
mapply(normal.custom, x=princomp_feat_groups[,2:4], title=colnames(princomp_feat_groups)[2:4])
mapply(normal.custom, x=princomp_feat_groups[,5:7], title=colnames(princomp_feat_groups)[5:7])

#
#OBSERVATION
#apparently we cannot hold the assumption that the features in the dataset are normally distributed
#when looking at the whole dataset.
#Indeed there are two reasons for not looking on the whole dataset when testing for normality:
#1. every setting of the metadata has been executed 10 times (see column repl in metadata)
#   since most features are not stochastic ten rows will have the same value
#   the assumption of the problem instances being iid does not hold
#2. every problem instance is executed 3 times with different blocks setting
#   however for most features they retrieve each time the same value
#Therefore when testing normality (for assuming the instances being iid) the subgroups should be analyzed separately
#This means analyzing all data from ONE BLOCK SETTING and ONE REPITION as one subgroups.
#This way of doing yields 30 subgroups in total. And indeed the data seems to be closer to normally distribution:

#Analyze the subset(blocks = 3, repl = 1) of the dataset wrt to normality
#it can be seen that most of the features are closer to being normally distributed as the SW-test and the graphs show
layout.custom(matrix(1:10, ncol=5,nrow=2), main = "Tests for normality (single features - subset blocks = 3 & repl = 1)", plotbreak = 5)
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),1:5], title=colnames(bfeats)[1:5])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),6:10], title=colnames(bfeats)[6:10])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),11:15], title=colnames(bfeats)[11:15])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),16:20], title=colnames(bfeats)[16:20])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),21:25], title=colnames(bfeats)[21:25])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),26:30], title=colnames(bfeats)[26:30])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),31:35], title=colnames(bfeats)[31:35])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),36:40], title=colnames(bfeats)[36:40])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),41:45], title=colnames(bfeats)[41:45])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),46:49], title=colnames(bfeats)[46:49])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),51:55], title=colnames(bfeats)[51:55])
mapply(normal.custom, x=bfeats[which(metadata[,6]==1 & metadata[,1]==3),56:59], title=colnames(bfeats)[56:59])
#manually close device as last plot only contains 8 instead of 10 plots
if(pdf.create == T) dev.off()

#also when taking a closer look at the average SW-test pvalues, they do increase significantly 
#contrary to looking at the whole dataset
#mean of all 30 subgroups is calculated
#mean of new SW-Test pvalue:3.86% (0.02261% before)
temp_mat = matrix(rep(1, 56), nrow=56)
for(i in 1:3) {
  for(j in 1:10) {
    #appending pvalues of the next subset to matrix with pvalues of all subsets
    temp_mat = cbind(temp_mat, sapply(bfeats[which(metadata[,6]==j & metadata[,1]==1+2*i),c(1:47,51:59)], 
                                      function(x) {shapiro.test(as.numeric(x))$p.value}))
  }
}
#calculating average pvalues for each feature over all 30 subsets
bfeats.sw_pvalue.subgroups = rowMeans(temp_mat[,-1])
summary(bfeats.sw_pvalue.subgroups)
#assuming a significance niveau of 0.01 the normal distribution of following features cannot be rejected any longer:
#cm_angle.dist_ctr2best.sd    1.10%   
#cm_angle.angle.mean          1.12%
#cm_angle.angle.sd            4.44%
#ela_curv.grad_scale.lq       99.32%
#ela_curv.grad_scale.med      12.36%
#ela_curv.hessian_cond.lq     89.07%
#ela_curv.hessian_cond.uq     8.32%
#ela_curv.costs_fun_evals     7.15%
which(bfeats.sw_pvalue.subgroups > 0.01)
bfeats.sw_pvalue.subgroups[which(bfeats.sw_pvalue.subgroups > 0.01)]

#also when looking onto the PC of the feature groups the pvalues do increase when looking on the subset separately
#example for repl=1 and blocks=3:
layout.custom(matrix(1:6, ncol=3, nrow=2), main = "Tests for normality (PC of groups - subset blocks = 3 & repl = 1)", plotbreak = 3)
mapply(normal.custom, x=princomp_feat_groups[which(metadata[,6]==1 & metadata[,1]==3),2:4], title=colnames(princomp_feat_groups)[2:4])
mapply(normal.custom, x=princomp_feat_groups[which(metadata[,6]==1 & metadata[,1]==3),5:7], title=colnames(princomp_feat_groups)[5:7])

#reset layout
par.reset()

#Tests for multivariate normal distribution:

#apply the chi-sq-plot to the whole dataset
#the datapoints do not seem to stick to the optimal line, so it is to assume that the data
#is not multivariate normal distributed
normal_multi.custom(bfeats, main="Test for multivariate normal distribution")

#A multivariate Shapiro-Wilk-Test does underline this assumption
#pvalue = 0 -> hypothesis of norm. dis. has to be rejected
require(mvShapiroTest)
mvShapiro.Test(as.matrix(bfeats))$p.value

#analysis of the feature groups for multivariate normal distribution
#cm_angle does differ a lot
normal_multi.custom(bfeats.cm_angle, main="Test for multivariate normal distribution (cm_angle)")
#cm_conv has outliers at the end
normal_multi.custom(bfeats.cm_conv, main="Test for multivariate normal distribution (cm_conv)")
#cm_grad also has outliers
normal_multi.custom(bfeats.cm_grad, main="Test for multivariate normal distribution (cm_grad)")
#differs for higher distances
normal_multi.custom(bfeats.ela_conv, main="Test for multivariate normal distribution (ela_conv)")
#differs for higher values
normal_multi.custom(bfeats.ela_curv, main="Test for multivariate normal distribution (ela_curv)")
#ela_local also differs for higher distances
normal_multi.custom(bfeats.ela_local, main="Test for multivariate normal distribution (ela_local)")

#SW-Test pvalues for feature group multivariate
#underline rejection of hypothesises
mvShapiro.Test(as.matrix(bfeats.cm_angle))$p.value  #2.1185e-227
mvShapiro.Test(as.matrix(bfeats.cm_conv))$p.value   #8.9253e-132
mvShapiro.Test(as.matrix(bfeats.cm_grad))$p.value   #6.3225e-115
mvShapiro.Test(as.matrix(bfeats.ela_conv))$p.value  #1.7269e-151
mvShapiro.Test(as.matrix(bfeats.ela_curv))$p.value  #0

#as stated above in the observation about better looking at subsets of the whole dataset
#because of the repl and the repitions with different numbers of blocks, we have to pay attention
#to this also when looking for multivariate normality:

#Example of subset block=3 and repl=3
#it does yield another more detailed look onto the chi-sq-plot
normal_multi.custom(bfeats[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution")
#mvShapiro.Test does not work because in subset there are too less observation in relation to dimensions

#analyizing subset feature groups for multinormal distr
#when looking at the iid subset the datapoints are closer to optimal line
#-> closer to multivariate normal distribution
normal_multi.custom(bfeats.cm_angle[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (cm_angle)")
normal_multi.custom(bfeats.cm_conv[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (cm_conv)")
normal_multi.custom(bfeats.cm_grad[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (cm_grad)")
normal_multi.custom(bfeats.ela_conv[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (ela_conv)")
normal_multi.custom(bfeats.ela_curv[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (ela_curv)")
normal_multi.custom(bfeats.ela_local[which(metadata[,1]==3 & metadata[,6]==3),], main="Test for multivariate normal distribution (ela_local)")

#also pvalues of SW-Tests are getting much higher, but still far from not rejecting hypothesises
mvShapiro.Test(as.matrix(bfeats.cm_angle[which(metadata[,1]==3 & metadata[,6]==3),]))$p.value  #4.0837e-08
mvShapiro.Test(as.matrix(bfeats.cm_conv[which(metadata[,1]==3 & metadata[,6]==3),]))$p.value  #3.3650e-22
mvShapiro.Test(as.matrix(bfeats.cm_grad[which(metadata[,1]==3 & metadata[,6]==3),]))$p.value  #1.2608e-24
mvShapiro.Test(as.matrix(bfeats.ela_conv[which(metadata[,1]==3 & metadata[,6]==3),]))$p.value  #1.0563e-20
mvShapiro.Test(as.matrix(bfeats.ela_curv[which(metadata[,1]==3 & metadata[,6]==3),]))$p.value  #7.8178e-121

#-----------------------------------------------------------------------------------------------------------

#1.3 Transformation to Normality
section.new("1-3")

#Since we know from the previous section that the data does not represent a normal distribution in the single
#features as well as not a multivariate distribution as a whole (even the subset not that much), we will apply
#box cox transformations in the following for adjusting it

#
#Box Cox Transformation
require(AID)

#transform features to normality by boxcox transformation
bfeats.boxcox = boxcox.custom(bfeats, c(2:10, 15:19, 21:29, 31:36, 38:43, 45, 47:49, 51, 53:56))

#SW-Test after transformation
#mean of 1.621% for pvalue (0.02261% before transformation)
#assuming sign niveau of 0.01 the following cannot be rejected from being norm distr.
#ela_curv.grad_scale.lq  
#ela_curv.grad_scale.med   
#ela_curv.grad_scale.uq 
#ela_curv.hessian_cond.lq
bfeats.boxcox.sw_pvalue = sapply(as.data.frame(bfeats.boxcox), function(x) {shapiro.test(as.numeric(x))$p.value})
summary(bfeats.boxcox.sw_pvalue)
summary(bfeats.sw_pvalue)
which(bfeats.boxcox.sw_pvalue > 0.01)
which(bfeats.sw_pvalue > 0.01)


#examining qqplot before and after boxcox transformation to dataset:
#some of the features seem to fit better to the optimal line of normal distribution
#although for most of the features a normal distribution still has to be rejected
layout.custom(matrix(1:10,ncol=5), main = "Boxcox - before vs after", plotbreak = 5)
mapply(boxcox_view.custom, before_data=bfeats[,1:5], after_data=bfeats.boxcox[,1:5], title=colnames(bfeats)[1:5])
mapply(boxcox_view.custom, before_data=bfeats[,6:10], after_data=bfeats.boxcox[,6:10], title=colnames(bfeats)[6:10])
mapply(boxcox_view.custom, before_data=bfeats[,11:15], after_data=bfeats.boxcox[,11:15], title=colnames(bfeats)[11:15])
mapply(boxcox_view.custom, before_data=bfeats[,16:20], after_data=bfeats.boxcox[,16:20], title=colnames(bfeats)[16:20])
mapply(boxcox_view.custom, before_data=bfeats[,21:25], after_data=bfeats.boxcox[,21:25], title=colnames(bfeats)[21:25])
mapply(boxcox_view.custom, before_data=bfeats[,26:30], after_data=bfeats.boxcox[,26:30], title=colnames(bfeats)[26:30])
#this features really have improved
mapply(boxcox_view.custom, before_data=bfeats[,31:35], after_data=bfeats.boxcox[,31:35], title=colnames(bfeats)[31:35])
mapply(boxcox_view.custom, before_data=bfeats[,36:40], after_data=bfeats.boxcox[,36:40], title=colnames(bfeats)[36:40])
mapply(boxcox_view.custom, before_data=bfeats[,41:45], after_data=bfeats.boxcox[,41:45], title=colnames(bfeats)[41:45])
mapply(boxcox_view.custom, before_data=bfeats[,46:50], after_data=bfeats.boxcox[,46:50], title=colnames(bfeats)[46:50])
mapply(boxcox_view.custom, before_data=bfeats[,51:55], after_data=bfeats.boxcox[,51:55], title=colnames(bfeats)[51:55])
mapply(boxcox_view.custom, before_data=bfeats[,56:59], after_data=bfeats.boxcox[,56:59], title=colnames(bfeats)[56:59])
#manually close device as last plot only contains 8 instead of 10 plots
if(pdf.create == T) dev.off()

#multivariate distribution after boxcox:
#comparison of chi-sq-plots before and after transformation
#after boxcox points are closer to optimal line (consider scale of y-axis!)
layout.custom(matrix(1:2, ncol=2, nrow=1), main = "Boxcox multivariate - before and after", plotbreak = 2)
normal_multi.custom(bfeats, main="Before Boxcox")
normal_multi.custom(bfeats.boxcox, main="After Boxcox")


#SUBSETS
#as seen in 1.2 Tests for normality, subsets of whole dataset should be used instead of the whole data for being iid
#following boxcox is applied to iid subsets of the data
#afterwards the data is concatenated to one dataset again
bfeats.boxcox2 = bfeats[1,]
for(i in 1:3) {
  for(j in 1:10) {
    bfeats.boxcox2 = rbind(bfeats.boxcox2, boxcox.custom(bfeats[which(metadata[,1]==1+2*i & metadata[,6]==j),], 
    c(2:10, 15:19, 21:29, 31:36, 38:43, 45, 47, 51, 53:56)))
  }
}
bfeats.boxcox2 = bfeats.boxcox2[2:nrow(bfeats.boxcox2),]
#save data because of long computing time
save(bfeats.boxcox,bfeats.boxcox2, file="../3-boxcox.RData")

#examining the SW-Test for this boxcox transformation data
#apparently apply the boxcox to the subsets does not retrieve a good result when looking at the SW-test for the
#whole dataset
#mean 1.031e-05
bfeats.boxcox2.sw_pvalue = sapply(as.data.frame(bfeats.boxcox), function(x) {shapiro.test(as.numeric(x))$p.value})
summary(bfeats.boxcox2.sw_pvalue)
which(bfeats.boxcox2.sw_pvalue > 0.01)

#examine features before and after boxcox transformation
#also the look upon qqplots before and after boxcox show that applying boxcox separately to subgroups is no good idea
layout.custom(matrix(1:10,ncol=5), main = "Boxcox per feature group - before vs after", plotbreak = 2)
mapply(boxcox_view.custom, before_data=bfeats[,1:5], after_data=bfeats.boxcox2[,1:5], title=colnames(bfeats)[1:5])
mapply(boxcox_view.custom, before_data=bfeats[,6:10], after_data=bfeats.boxcox2[,6:10], title=colnames(bfeats)[6:10])

#therefore we go on by using the boxcox data from applying to the whole data set for outlier detection
bfeats.boxcox

#-----------------------------------------------------------------------------------------------------------

#1.4 Outlier Detection
section.new("1-4")

#one dimensional outliers:

#scale data
bfeats.scale = apply(bfeats.boxcox, 2, scale)
bfeats.cm_angle.scale = as.data.frame(apply(bfeats.boxcox[,2:10], 2, scale))
bfeats.cm_conv.scale = as.data.frame(apply(bfeats.boxcox[,11:15], 2, scale))
bfeats.cm_grad.scale = as.data.frame(apply(bfeats.boxcox[,16:18], 2, scale))
bfeats.ela_conv.scale = as.data.frame(apply(bfeats.boxcox[,19:22], 2, scale))
bfeats.ela_curv.scale = as.data.frame(apply(bfeats.boxcox[,23:45], 2, scale))
bfeats.ela_local.scale = as.data.frame(apply(bfeats.boxcox[,46:59], 2, scale))
#get max value within feature groups
bfeats.cm_angle.scale_max = apply(bfeats.cm_angle.scale, 1, max)
bfeats.cm_conv.scale_max = apply(bfeats.cm_conv.scale, 1, max)
bfeats.cm_grad.scale_max = apply(bfeats.cm_grad.scale, 1, max)
bfeats.ela_conv.scale_max = apply(bfeats.ela_conv.scale, 1, max)
bfeats.ela_curv.scale_max = apply(bfeats.ela_curv.scale, 1, max)
bfeats.ela_local.scale_max = apply(bfeats.ela_local.scale, 1, max)


#therefore max value of each problem instance of all features within feature group is plotted
#for identifying outliers visually
#one displaying one repl (see metadata), because values are often equal, therefore only 1/10 of outliers identified
#by number
#cm_angle
#2 outliers can be easily identified -> 111, 851
dotplot.custom(bfeats.cm_angle.scale_max[which(metadata[,6]==1)], "Dotplot cm_angle", 
               highlight = bfeats.cm_angle.scale_max[which(bfeats.cm_angle.scale_max > 3 & metadata[,6] ==1)])
#cm_conv
#no real outliers can be seen
dotplot.custom(bfeats.cm_conv.scale_max[which(metadata[,6]==1)], "Dotplot cm_conv")
#cm_grad
#2 outliers at the right end -> 581
dotplot.custom(bfeats.cm_grad.scale_max[which(metadata[,6]==1)], "Dotplot cm_grad",
               highlight = bfeats.cm_grad.scale_max[which(bfeats.cm_grad.scale_max > 3 & metadata[,6]==1)])
#ela_conv
#no outliers
dotplot.custom(bfeats.ela_conv.scale_max[which(metadata[,6]==1)], "Dotplot ela_conv",
               highlight = bfeats.ela_conv.scale_max[which(bfeats.ela_conv.scale_max > 4 & metadata[,6]==1)])
#ela_curv
#3 outliers at the right -> 191, 2127, 2331
dotplot.custom(bfeats.ela_curv.scale_max[which(metadata[,6]==1)], "Dotplot ela_curv",
               highlight = bfeats.ela_curv.scale_max[which(bfeats.ela_curv.scale_max > 5 & metadata[,6]==1)])
#ela_local
#5 outliers at the right -> 561, 571, 1561, 2561, 2571
dotplot.custom(bfeats.ela_local.scale_max[which(metadata[,6]==1)], "Dotplot ela_local",
               highlight = bfeats.ela_local.scale_max[which(bfeats.ela_local.scale_max > 5.5 & metadata[,6]==1)])


#visualization by means of boxplots
#cm_angle
#no outliers can be seen
par.reset()
pdf.init("Boxplot of cm_angle Features")
boxplot(bfeats.cm_angle.scale[,1:9], range=4, outline=TRUE, names=c(1:9), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of cm_angle Features", col=colors[1])
pdf.write()
#cm_conv
#one outlier can be seen in feat 1
layout(matrix(1:1, ncol=1))
pdf.init("Boxplot of cm_conv Features")
boxplot(bfeats.cm_conv.scale[,1:5], range=4, outline=TRUE, names=c(1:5), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of cm_conv Features", col=colors[1])
pdf.write()
#cm_grad
#no outliers
layout(matrix(1:1, ncol=1))
pdf.init("Boxplot of cm_grad Features")
boxplot(bfeats.cm_grad.scale[,1:3], range=4, outline=TRUE, names=c(1:3), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of cm_grad Features", col=colors[1])
pdf.write()
#ela_conv
#no outliers
layout(matrix(1:1, ncol=1))
pdf.init("Boxplot of ela_conv Features")
boxplot(bfeats.ela_conv.scale[,1:4], range=4, outline=TRUE, names=c(1:4), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of ela_conv Features", col=colors[1])
pdf.write()
#ela_curv
#3 outliers in feat 8, 3 outliers in feat 15
pdf.init("Boxplot of ela_curv Features")
layout(matrix(1:2, ncol=2))
boxplot(bfeats.ela_curv.scale[,1:12], range=4, outline=TRUE, names=c(1:12), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of ela_curv Features", col=colors[1])
boxplot(bfeats.ela_curv.scale[,13:23], range=4, outline=TRUE, names=c(13:23), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of ela_curv Features", col=colors[1])
pdf.write()
#ela_local
#1 outlier in feat 5, several in feats 1, 3, 4, 12, 14
pdf.init("Boxplot of ela_local Features")
layout(matrix(1:2, ncol=2))
boxplot(bfeats.ela_local.scale[,1:7], range=4, outline=TRUE, names=c(1:7), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of ela_local Features", col=colors[1])
boxplot(bfeats.ela_local.scale[,8:14], range=4, outline=TRUE, names=c(8:14), ylab="Index of Feature", 
        horizontal=TRUE, main="Boxplot of ela_local Features", col=colors[1])
pdf.write()


#identify maximum sigma-value per row /problem instance
#ignore feat12 from ela_local, because as seen by boxplots this feature cannot be normally distributed
bfeats.scale_max = apply(bfeats.scale[,-57], 1, max)
#identify those > 4.5 sigma
#we decided to use 4.5 sigma instead of 3.5 sigma in the lecture due to the large number of rows
out = which(abs(bfeats.scale_max) > 4.5)

#thus there are 65 outliers identified which represent an outlier in at least one of the features from
#an one dimensional perspective
#1D outliers:
#28  102  191  194  195  196  207  240  245  249  436  437  476  502  624  628  629  687  774  854 1034 1111 1179 1197 1200 1236 1309
#1397 1406 1407 1433 1435 1553 1565 1567 1593 1624 1625 1627 1693 1754 1852 1937 1959 1975 2032 2171 2172 2176 2216 2296 2331 2399 2512
#2566 2569 2619 2621 2622 2624 2625 2628 2778 2779 2794
#exclude outliers
bfeats2 = bfeats[-out,]
metadata2 = metadata[-out,]
bfeats2.cm_angle = bfeats.cm_angle[-out,]
bfeats2.cm_conv = bfeats.cm_conv[-out,]
bfeats2.cm_grad = bfeats.cm_grad[-out,]
bfeats2.ela_conv = bfeats.ela_conv[-out,]
bfeats2.ela_curv = bfeats.ela_curv[-out,]
bfeats2.ela_local = bfeats.ela_local[-out,]

#2-dimensional outliers
par.reset()
#since there is a large number of dimension we will basically examine the outliers
#on scatterplots within the feature groups
#cm_angle: no outliers can be directly identified
pairs.custom(bfeats2.cm_angle, m="Outliers within Feature Group cm_angle")
#cm_conv: no outliers can be directly identified
pairs.custom(bfeats2.cm_conv, m="Outliers within Feature Group cm_conv")
#cm_grad: 2 outliers can be identified: 220 904
pairs_out.custom(bfeats2.cm_grad, m="Outliers within Feature Group cm_grad", 
                 outl = which(bfeats2.cm_grad[,3]>0.08))
#ela_conv: 1 outlier: 2606
pairs_out.custom(bfeats2.ela_conv, m="Outliers within Feature Group ela_conv", 
             outl = which(bfeats2.ela_conv[,4]>400))
#ela_curv: 1 outlier: 2751
pairs.custom(bfeats2.ela_curv[,seq(1,7)], m="Outlier within Feature Group ela_curv (extract)")
pairs.custom(bfeats2.ela_curv[,seq(8, 14)], m="Outlier within Feature Group ela_curv (extract)")
pairs_out.custom(bfeats2.ela_curv[,seq(15, 23)], m="Outlier within Feature Group ela_curv (extract)",
                 outl = which(bfeats2.ela_curv[,18]>4))
#ela_local: no outliers can be directly identified
pairs.custom(bfeats2.ela_local[,seq(1,7)], m="Correlation within Feature Group ela_local (extract)")
pairs.custom(bfeats2.ela_local[,seq(8,14)], m="Correlation within Feature Group ela_local (extract)")

#view scatterplot of corr between features (see 1.1)
#1 outlier: 2668
pairs_out.custom(princomp_feat_groups, m="Correlation between Feature Groups",
                 outl = which(princomp_feat_groups[,5]>400))


#Multidimensional outliers
#chi-sq-plot and distance calculation

#identify multidimensional outliers from the whole dataset
#there are 11 outliers
#559  560  561  566 1539 1540 1541 1544 2515 2516 2606
layout(matrix(1, ncol=1))
outlier_multi.custom(bfeats2, main="Multidimensional outliers", num_outl=11)

#identify outliers when looking at the feature groups
#outliers are identified from each feature group
#cm_angle
#10 outliers: 597 598 599 600 601 602 603 604 605 606
outlier_multi.custom(bfeats2.cm_angle, main="Multidimensional outliers (cm_angle)", num_outl=10)
#cm_conv
#4 outliers: 298 315 811 888
outlier_multi.custom(bfeats2.cm_conv, main="Multidimensional outliers (cm_conv)", num_outl=4)
#cm_grad
#2 outliers: 220 904
outlier_multi.custom(bfeats2.cm_grad, main="Multidimensional outliers (cm_grad)", num_outl=2)
#ela_conv
#1 outlier: 2606
outlier_multi.custom(bfeats2.ela_conv, main="Multidimensional outliers (ela_conv)", num_outl=1)
#ela_curv
#10 outliers: 559  560  561  566 1539 1540 1541 1544 2515 2516
outlier_multi.custom(bfeats2.ela_curv, main="Multidimensional outliers (ela_curv)", num_outl=10)
#ela_local
#7 outliers: 549  554  555 1530 2508 2511 2513
outlier_multi.custom(bfeats2.ela_local, main="Multidimensional outliers (ela_local)", num_outl=7)


#calculate distances for whole dataset
bfeats2.distances = distances.custom(bfeats2)
#calculate 99.99 % quantile for dataset
bfeats2.limit = qchisq(0.9999, df=ncol(bfeats2))
#identify outliers based on limit
#47 outliers: 
#356  439  444  446  453  458  470  547  548  549  552  554  555  559  560  561  563 
#564  566  610  668  676 1222 1420 1423 1424 1530 1531
#1537 1538 1539 1540 1541 1542 1543 1544 1590 1637 2398 2508 2511 2513 2515 2516 2517 2518 2606 
out = which(bfeats2.distances > bfeats2.limit)

#identify outliers in feature groups of CM (most are the same as in whole dataset)
#calculate distances
bfeats2.distances.cm = distances.custom(bfeats2[,2:18])
#calculate 99.99 % quantile for dataset
bfeats2.limit.cm = qchisq(0.9999, df=ncol(bfeats2[,2:18]))
#identify outliers based on limit
#45 outliers: 
#356  439  444  446  453  458  470  547  548  549  552  554  555  559  560  561  
#563  564  566  610  668  676 1222 1420 1423 1424 1530 1531
#1537 1538 1539 1540 1541 1542 1543 1544 1590 1637 2398 2508 2511 2513 2515 2516 2517 2518 2606  
out.cm = which(bfeats2.distances.cm > bfeats2.limit.cm)

#identify outliers in feature groups of ELA (some additional outliers)
#calculate distances
bfeats2.distances.ela = distances.custom(bfeats2[,19:59])
#calculate 99.99 % quantile for dataset
bfeats2.limit.ela = qchisq(0.9999, df=ncol(bfeats2[,19:59]))
#identify outliers based on limit
#91 outliers: 
#231  280  323  336  360  362  364  366  369  400  402  407  438  440  443  446  454  456  
#457  459  462  540  541  542  543  544  545  546 
#547  634  642 1118 1323 1325 1329 1330 1333 1363 1365 1369 1389 1401 1402 1414 1415 
#1416 1417 1502 1503 1504 1505 1506 1507 1509 1533 1545 
#1578 1581 1596 1598 1614 2095 2135 2137 2281 2282 2300 2317 2322 2326 2345 2360 
#2361 2365 2382 2383 2386 2468 2469 2470 2471 2472 2473 2474 
#2475 2476 2477 2478 2514 2562 2564
out.ela = which(bfeats2.distances.ela > bfeats2.limit.ela)

#exclude multidimensional outliers
bfeats3 = bfeats2[-c(out, out.cm, out.ela),]
metadata3 = metadata2[-c(out, out.cm, out.ela),]
bfeats3.cm_angle = bfeats2.cm_angle[-c(out, out.cm, out.ela),]
bfeats3.cm_conv = bfeats2.cm_conv[-c(out, out.cm, out.ela),]
bfeats3.cm_grad = bfeats2.cm_grad[-c(out, out.cm, out.ela),]
bfeats3.ela_conv = bfeats2.ela_conv[-c(out, out.cm, out.ela),]
bfeats3.ela_curv = bfeats2.ela_curv[-c(out, out.cm, out.ela),]
bfeats3.ela_local = bfeats2.ela_local[-c(out, out.cm, out.ela),]
#2810 observation out of 30000 observation left for further steps

#exclude those feature which have var = 0 after outlier elamination
which(sapply(bfeats3[,1:length(bfeats3)], var)==0)
bfeats3 = bfeats3[,-50]
bfeats3.ela_local = bfeats3.ela_local[,-5]

#save data for further tasks
save(list = ls(all=TRUE), file="../3-DataPreprocessing.RData")
#-----------------------------------------------------------------------------------------------------------

