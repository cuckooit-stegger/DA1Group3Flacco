# Group 03
# Team Members
# Christian Siemen (394724)
# 
#
#

#Part01 Data Preprocessing
#
#All methods which are necessary to preprocess the flacco data for further analysis in Part02.

#-------------------------------------------------------------------------------------------------------
#1.1 Flacco-specific preprocessing
#Special Preprocessing techniques for the special needs of the flacco dataset

#load dataset and inspect
load("../3-flacco1.RData")
str(feats)
summary(feats)

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

#convert categorical var "topology" into numerical equivalent
bfeats[,1] = as.integer(factor(bfeats[,1], labels=c(0,1)))

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

# colors for the functions
colors = c('#1abc9c', '#2ecc71', '#3498db', '#9b59b6', '#34495e', '#16a085', '#27ae60', '#2980b9', '#8e44ad', '#2c3e50', '#f1c40f', '#e67e22', '#e74c3c', '#95a5a6', '#f39c12', '#d35400', '#c0392b', '#bdc3c7', '#7f8c8d', '#666666', '#FF0000', '#00FF00', '#0000FF', '#FFFF00')

#------------------------------------------------------------------------------------------------------------

#1.1 Visualization
#Visualization of the dataset

#Scatterplots
#function to print histograms to the diagonal panels of a scatterplot
panel.hist2 <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  #window settings
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  #draw rects of the histogram
  rect(breaks[-nB], 0, breaks[-1], y,  ...)
}

#function to print corelations to the upper panels of a scatterplot
panel.cor2 <- function(x, y, digits=2, prefix="", cex.cor=1, ...) { 
  usr <- par("usr"); on.exit(par(usr))
  #window settings
  par(usr = c(0, 1, 0, 1))
  #calculate pearson correlation coefficient
  r <- abs(cor(x, y))
  #calculate spearman correlation coefficient
  rsp <- abs(cor(x,y,method="spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt2 <- format(c(rsp, 0.123456789), digits=digits)[1]
  text(0.5, 0.5, paste(txt," / ", txt2,sep=""), cex = cex.cor,col="blue")
}

#function to draw a scatterplot with custom upper and diagonal panels and with legend
pairs.custom <- function(x, m, color=colors[1], legend.title="no", legend.text=NULL, legend.col=NULL) {
  #use full window for legend
  par(xpd=TRUE)
  oma = c(4,4,6,6)
  #different margins in case of legend
  if(legend.title != "no") {
    oma = c(4,4,6,12)
  }
  #plot the scatterplots
  pairs(x, panel = function (x, y, ...) {
    points(x, y, ...)
    abline(lm(y ~ x), col = "blue") 
    #include correlation coefficients in upper panel and histograms on diagonal
  }, pch=19, upper.panel=panel.cor2, diag.panel=panel.hist2, main=m, col=color, oma=oma)
  #put legend
  #only if legend required
  if(legend.title != "no") {
    legend("right",legend=legend.text, col=legend.col, pch=19, title=legend.title, cex=0.8)
  }
}

#function for determining the amount of pairwise correlation (pearson) within a certain dataset
pairs.cor <- function(x) {
  sum.corr = 0
  #sum up pairwise correlations
  for(i in 1:length(x)) {
    for(j in 1:length(x)) {
      if(i != j) {
        sum.corr = sum.corr + abs(cor(x[,i],x[,j]))
      }
    }
  }
  #divide by number of pairs evaluated
  sum.corr / (length(x)*(length(x) - 1))
}

#csiemen
#correlation within the feature-groups is relatively high. This retrieves that it makes sense in further steps to
#reduce dimensionality
pairs.custom(bfeats.cm_angle, m="Correlation within Feature Group cm_angle")
#influence of blocks metadata on CM-angle features (clusters for certain features):
pairs.custom(bfeats.cm_angle, m="Correlation within Feature Group cm_angle",col=colors[metadata[,1]], 
             legend.title="Number of blocks", legend.text=c("3 blocks", "5 blocks", "7 blocks"), 
             legend.col = colors[c(3,5,7)])
pairs.custom(bfeats.cm_conv, m="Correlation within Feature Group cm_conv")
pairs.custom(bfeats.cm_grad, m="Correlation within Feature Group cm_grad")
pairs.custom(bfeats.ela_conv, m="Correlation within Feature Group ela_conv")
pairs.custom(bfeats.ela_curv[,seq(1,7)], m="Correlation within Feature Group ela_curv (extract)")
pairs.custom(bfeats.ela_curv[,c(1,8,15)], m="Correlation within Feature Group ela_curv (extract)")
pairs.custom(bfeats.ela_local[,c(6,7,8,9,10,11,12)], m="Correlation within Feature Group ela_local (extract)")

#it can be seen that the with increasing number of peaks the costs of the ELA functions increase
pairs.custom(data.frame(ela_conv.costs_runtime=bfeats.ela_conv[,4],
                        ela_curv.costs_runtime=bfeats.ela_curv[,23],
                        ela_local.costs_runtime=bfeats.ela_local[,14],
                        peak=metadata[,4]), m="Correlation number of peaks with ELA runtime")
#it can be seen that the with increasing number of blocks the costs of the CM functions increase
pairs.custom(data.frame(cm_angle.costs_runtime=bfeats.cm_angle[,9],
                        cm_conv.costs_runtime=bfeats.cm_conv[,5],
                        cm_grad.costs_runtime=bfeats.cm_grad[,3],
                        blocks=metadata[,1]), m="Correlation number of blocks with CM runtime")


#by analyzing some of this feature wrt to the repl metadata, one can see the only stochastic parameters are
#the costs_runtime ones. For all other features the different repl are equal.
pairs.custom(bfeats.cm_conv, m="Correlation within Feature Group cm_conv", col=colors[metadata[,6]])

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
pairs.cor(princomp_feat_groups)   #0.3365  correlation of cerrtain groups drives overall correlation

#The blocks argument of the metadata has some effect on the CM-features
pairs.custom(princomp_feat_groups[2:4], m="Correlation between Feature Groups", color=colors[metadata[,1]],
             legend.title="Number of blocks", legend.text=c("3 blocks", "5 blocks", "7 blocks"), 
             legend.col = colors[c(3,5,7)])
#number of peaks has no significant influence; prob.seed and repl neither.

#csiemen
#scatterplot3d
#include package
require(scatterplot3d) 

#custom function for scatterplot3d including legend on the bottom
scatterplot3d.custom <- function(x, y, z, angle, main, xlab, ylab, zlab, col, legend.text, legend.col, legend.title) {
  layout(rbind(1,2), heights=c(7,1))
  scatterplot3d(x, y, z, angle=angle, pch=19, cex.lab=1, type="p", main=main, xlab=xlab,
                ylab=ylab, zlab=zlab, color=col)
  par(mar=c(0,0,0,0))
  plot.new()
  if(legend.title != "no") {
    legend('center','groups',legend=legend.text, col=legend.col, pch=19, title=legend.title, bty ="n", horiz=TRUE, cex=0.8)
  }
  #reset normal settings
  layout(matrix(1, ncol=1))
  par(mar=c(8,5,5,5))
}


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

#runtimes within ELA feature are increasing commonly (depending on number of peaks)
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

#also between ELA features there are some dependencies. clusters dependent on topology of function
scatterplot3d.custom(bfeats.ela_conv[,1], bfeats.ela_curv[,3], bfeats.ela_local[,3],
                     angle=35, main="3D Scatterplot between features within ela features (by topology)", xlab="ela_conv.runtime", ylab="ela_curv.runtime", 
                     zlab="ela_local.runtime", col=colors[bfeats.topology], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[1:2])


#scatterplots for displaying different feature groups against each other
#analysis of ELA features in scatterplot
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
              angle=35, main="3D Scatterplot on ELA-features", xlab="ela_conv",
              ylab="ela_curv", zlab="ela_local",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)

#although num of blocks and repl dont have influence on distributrion of ela features,
#the generators seed seems to produce clusters overlying eacch other (small clusters in seed = 4)
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
                     angle=35, main="3D Scatterplot on ELA-features (prob.seed)", xlab="ela_conv",
                     ylab="ela_curv", zlab="ela_local",col=colors[metadata[,5]], legend.col=colors[1:5],
                     legend.title = "Prob.seed", legend.text = c("1", "2", "3", "4", "5"))

#deeper analyis wrt to number of peaks as add information (apparently influence on ELA-features)
#outlier clusters can be seen at number of peaks = 20
scatterplot3d.custom(princomp_feat_groups$ela_conv, princomp_feat_groups$ela_curv, princomp_feat_groups$ela_local,
                     angle=35, main="3D Scatterplot on ELA-features (number of peaks)", xlab="ela_conv",
                     ylab="ela_curv", zlab="ela_local",col=colors[metadata[,4]/20], legend.col=colors[1:10],
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                      "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"))

#scatterplot for cell mapping feature groups
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
              angle=55, main="3D Scatterplot on CM-features", xlab="cm_angle",
              ylab="cm_conv", zlab="cm_grad",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)

#adding the information of how many blocks were created shows up three distinct clusters
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
                  angle=55, main="3D Scatterplot on CM-features (by num of blocks)", xlab="cm_angle",
                  ylab="cm_conv", zlab="cm_grad", col=colors[metadata[,1]], legend.title="Number of blocks", 
                  legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])

#adding other metadata like repl or seed does not cluster the data likewise, the features do not depend on which 
#replication or seed is used. Also peaks does not have influence on cm features:
scatterplot3d.custom(princomp_feat_groups$cm_angle, princomp_feat_groups$cm_conv, princomp_feat_groups$cm_grad,
                     angle=35, main="3D Scatterplot on CM-features (by num of peaks)", xlab="cm_angle",
                     ylab="cm_conv", zlab="cm_grad", col=colors[metadata[,4]/20], legend.col=colors[1:10],
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                     "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"))


#mkubicki
#function:
#cor.detect: display all correlations (pearson) between columns in given dataset that have at least value l
#parameters:
#x: data.frame to inspect
#l: minimum correlation. display correlation if absolute of cor is >=l
cor.detect <- function(x, l=0) {
  for(c in 1:ncol(x)) {
    col1 = colnames(bfeats)[c]
    if(c+1 < ncol(x)){
      for(d in (c+1):ncol(x)) {
        col2 = colnames(bfeats)[d]
        cur.cor = cor(x[c], x[d])
        #print(paste("cor(",col1, ",", col2,")=", cur.cor))
        if(abs(cur.cor) >= l) {
          print(paste("cor(",col1, ",", col2,") =", cur.cor))
        }
      }
    }
  }
}


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

#CSiemen
#method for different normality test
#method for displaying qqplot, histogram and p-value of Shapiro-Wilk-Test of feature given as parameter
normal.custom <- function(x, title, col=colors[1], round=6) {
  #draw qqplot
  qqnorm(x, main = title,pch=19,
         cex.lab=1,cex.main=1,ylab="Sample Quantiles", col=col)
  #insert "optimal" line
  qqline(x,lwd=2,col="red")
  #view histogram and p-value of SW-Test
  hist(x, main = paste("SW-Test: ", round(shapiro.test(x)$p.value, round)), col="cyan", xlab="")
}

#apply the normality tests to single features of the dataset (in groups of 5 for displaying issues)
#it can be seen that assuming a significance niveau of 0.01 for each of the features
#the hypothesis of a normal distribution would be rejected
layout(matrix(1:10, ncol=5,nrow=2))
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

#deeper analysis of p-values of SW-Tests for single features
#all of the tests are rejected with sig niv of 0.01
#mean: 0.02261%
bfeats.sw_pvalue = sapply(bfeats[,1:59], function(x) {shapiro.test(as.numeric(x))$p.value})
summary(bfeats.sw_pvalue)
which(bfeats.sw_pvalue > 0.01)

#also whan applying the tests to the PC of the feature groups (see 1.1 Visualization)
#the hypothesis of normal distribution must be rejected for each
#nevertheless the CM seem to be closer to normal distributrion by looking on the graphs
layout(matrix(1:6, ncol=3, nrow=2))
mapply(normal.custom, x=princomp_feat_groups[,2:4], title=colnames(princomp_feat_groups)[2:4])
mapply(normal.custom, x=princomp_feat_groups[,5:7], title=colnames(princomp_feat_groups)[5:7])

#CSiemen
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
layout(matrix(1:10, ncol=5,nrow=2))
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
summary(bfeats.sw_pvalue.s1)
#assuming a significance niveau of 0.01 the normal distribution of following features cannot be rejected any longer:
#cm_angle.dist_ctr2best.sd    1.10%   
#cm_angle.angle.mean          1.12%
#cm_angle.angle.sd            4.44%
#ela_curv.grad_scale.lq       99.32%
#ela_curv.grad_scale.med      12.36%
#ela_curv.hessian_cond.lq     89.07%
#ela_curv.hessian_cond.uq     8.32%
#ela_curv.costs_fun_evals     7.15%
which(bfeats.sw_pvalue.s1 > 0.01)
bfeats.sw_pvalue.s1[which(bfeats.sw_pvalue.s1 > 0.01)]

#also when looking onto the PC of the feature groups the pvalues do increase when looking on the subset separately
#example for repl=1 and blocks=3:
layout(matrix(1:6, ncol=3, nrow=2))
mapply(normal.custom, x=princomp_feat_groups[which(metadata[,6]==1 & metadata[,1]==3),2:4], title=colnames(princomp_feat_groups)[2:4])
mapply(normal.custom, x=princomp_feat_groups[which(metadata[,6]==1 & metadata[,1]==3),5:7], title=colnames(princomp_feat_groups)[5:7])


#Tests for multivariate normal distribution:

#custom function for display x^2 plot for testing multivariate normal distribution
require(MASS)
normal_multi.custom <- function(data, main, outl = FALSE, col=colors[1]) {
  cm = colMeans(data)
  S = cov(data)
  #calculating distances in the multidimensional room
  dis = apply(data, 1, function(x) t(x-cm) %*% ginv(S) %*% (x-cm))
  
  #plotting x^2 plot with datpoints
  #df = number of cols in data
  plot(qc <- qchisq((1:nrow(data)-1/2)/nrow(data), df=ncol(data)),
       sd <-  sort(dis), xlab=expression(paste(chi^2, " Quantile")),
       ylab="Ordered Distances", main=main,
       pch=19,cex.lab=1,cex.axis=1,cex=1, col=col)
  
  #adding "optimal normally distribtion" line
  abline(a=0,b=1,col= "red",lwd=2)
  
  #in case of outliers mark them
  if(outl) {
    out =  which(rank(abs(qc-sd), ties= "random") > nrow(data)-3)
    print(out)
    #show up rownumbers instead of rownames
    text(qc[out], sd[out]-1.5, 
         which(attributes(data)$row.names == names(out)),cex=1,col="blue")
  }
  
}

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

#Since we know from the previous section that the data does not represent a normal distribution in the single
#features as well as not a multivariate distribution as a whole (even the subset not that much), we will apply
#box cox transformations in the following for adjusting it

#CSiemen
#Box Cox Transformation
require(AID)

#custom function for applying boxcox transformation to features of a given dataset
#the columns for which the boxcox shall be applied can be specified
#because for some features it will not be possible due to computation limits or negative values
boxcox.custom <- function(data, cols=1:length(data)) {
  print("new")
  data[,cols] = 
    sapply(data[,cols], function(x) {
    lambda <- boxcoxnc(as.numeric(x), method='sw', plotit=FALSE, lam=seq(-7,7,0.01))$result[[1]]
    print(lambda)
    tr <- (x^{lambda}-1)/lambda
  })
  data
}

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

#function for displaying qqplot and SW pvalue before and after boxcox transformation
boxcox_view.custom <- function(before_data, after_data, title, col=colors[1], round=6) {
  #draw qqplot before
  qqnorm(before_data, main = title ,pch=19,
         cex.lab=1,cex.main=1,ylab="Sample Quantiles", xlab= paste("SW-Test: ", round(shapiro.test(before_data)$p.value, round)),
         col=col)
  #insert "optimal" line
  qqline(before_data,lwd=2,col="red")
  #view histogram and p-value of SW-Test
  ##qqplot after transformation
  qqnorm(after_data, pch=19, xlab="",
         cex.lab=1,cex.main=1,ylab="", main= paste("SW-Test: ", round(shapiro.test(after_data)$p.value, 6)),
         col=col)
  #insert "optimal" line
  qqline(after_data,lwd=2,col="red")
}

#examining qqplot before and after boxcox transformation to dataset:
#some of the features seem to fit better to the optimal line of normal distribution
#although for most of the features a normal distribution still has to be rejected
layout(matrix(1:10,ncol=5))
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

#multivariate distribution after boxcox:
#comparison of chi-sq-plots before and after transformation
#after boxcox points are closer to optimal line (consider scale of y-axis!)
layout(matrix(1:2, ncol=2, nrow=1))
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
layout(matrix(1:10,ncol=5))
mapply(boxcox_view.custom, before_data=bfeats[,1:5], after_data=bfeats.boxcox2[,1:5], title=colnames(bfeats)[1:5])
mapply(boxcox_view.custom, before_data=bfeats[,6:10], after_data=bfeats.boxcox2[,6:10], title=colnames(bfeats)[6:10])

#therefore we go on by using the boxcox data from applying to the whole data set for outlier detection
bfeats.boxcox

#-----------------------------------------------------------------------------------------------------------

#1.4 Outlier Detection



#-----------------------------------------------------------------------------------------------------------

