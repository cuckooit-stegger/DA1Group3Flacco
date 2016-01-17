# Group 03
# Team Members:
# Christian Siemen (394724)
#
#
#

#Part02 Unsupervised Learning
#
#Uses the preprocessed data from Part01 and analysis the data by applying techniques from the
#unsupervised learning.

#load data from part 01 preprocessing
load("../3-DataPreprocessing.RData")

#IMPORTANT
#bfeats3, bfeats3.cm_angle, bfeats3.cm_conv, etc. is used, because this is the orig dataset without outliers

#2.1 Principal Component Analysis (PCA)

#function to draw a scatterplot without regression lines, because 
#correlation of PCs is 0 from definition
pairs_noreg.custom <- function(x, m, color=colors[1], legend.title="no", legend.text=NULL, legend.col=NULL) {
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
    #include correlation coefficients in upper panel and histograms on diagonal
  }, pch=19, diag.panel=panel.hist2, main=m, col=color, oma=oma)
  #put legend
  #only if legend required
  if(legend.title != "no") {
    legend("right",legend=legend.text, col=legend.col, pch=19, title=legend.title, cex=0.8)
  }
}

#PCA analyis of the whole dataset
#first based on covariance matrix (exclude topology since it is input parameter)
bfeats3.pca_cov = princomp(bfeats3[,-1], cor=FALSE)
#examine reulting PCs
#only 2 PCs needed for explaining 80% of variance
summary(bfeats3.pca_cov)
biplot(bfeats3.pca_cov, main="Biplot of PCA based on cov")
#analysing these PC it can be seen that due to the large scale of some features
#these PC do rely on a very small number of features
#1.PC : ela_curv.grad_scale.max & ela_curv.hessian_cond.max  
#2.PC : ela_curv.grad_scale.max & ela_curv.hessian_cond.max & ela_local.costs_fun_evals
#thus the do not represent the dataset properly
round(summary(bfeats3.pca_cov, loadings = TRUE)$loadings[,1:2], 3)

#therefore the PCA should be based on the correlation matrix instead
#feat 49 has to be excluded as well since its variance is to low resulting in a non non-definite cor matrix
#(exclude topology since it is input parameter)
bfeats3.pca_cor = princomp(bfeats3[,-c(1,49)], cor=TRUE, scores=TRUE)
#examine resulting PCs
#11 PCs needed for explaining 80 % of overall variance
summary(bfeats3.pca_cor)
#plot scree diagram
#for deciding on appropriate number of PCs
plot(bfeats3.pca_cor$sdev, type="b", main="Scree diagram PCA on cor", xlab="Component number", ylab="Component variance")
#the scree diagram shows up a kink after PCs 6. So we consider that to be an appropriate number of PCs
#they result in explaining about 68% of overall variance and are still practical for analysis
plot(bfeats3.pca_cor$sdev, type="b", main="Scree diagram PCA on cor (elbow)", xlab="Component number", ylab="Component variance")
points(6, bfeats3.pca_cor$sdev[6], col="red", cex=2.5, lwd=1.5, pch=1)
#biplot
biplot(bfeats3.pca_cor, main="Biplot of PCA based on cor")
#show factors of components
round(summary(bfeats3.pca_cor, loadings = TRUE)$loadings[,1:6], 3)

#visualization of the 6 identified PCs
#scatterplot
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs")
#actually we can already see that there are some clusters 
#to ensure that these are not from the different metadata setting only 
#including metadata into the scatterplot
#dim and n.dim do not vary
#blocks
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[metadata3[,1]])
#from the color appearance we see that the clusters depend on the metadata argument num of blocks
#so we have to be careful when doing cluster analysis
#but when only including data with block == 3 in plot, other clusters are still there
pairs_noreg.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1:6], m="Scatterplot on PCs",
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#num peaks does not have influence on clusters
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[metadata3[,4]/20],
                   legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                   "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"),
                   legend.col=colors[1:10])
#prob.seed does not have influence on clusters
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[metadata[,5]], 
                   legend.title = "Prob.seed", legend.text = c("1", "2", "3", 
                  "4", "5"),legend.col=colors[1:5])
#repl does not have influence on clusters
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[metadata[,6]],
                   legend.title = "Repl", legend.text = c("1", "2", "3", 
                   "4", "5", "6", "7", "8","9", "10"),
                   legend.col=colors[1:10])
#topology seems to have influence. so that will be important parameter of the function
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 PC
require(scatterplot3d)
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)
#again influence of blocks visible
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[metadata3[,1]], legend.title="Number of blocks", 
                     legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#and again influence of topology as important parameter since it is "real" feature of function
#and not of evaluation process as num blocks
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3[,1]], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[c(1,2)])



#-----------------------------------------------------------------------------------------------------------

#2.2 Multidimensional Scaling





#-----------------------------------------------------------------------------------------------------------

#2.3 Cluster Analysis
#2.3.1 K-Means Algorithm



#-----------------------------------------------------------------------------------------------------------

#2.3.2 Hierarchical Clustering



#-----------------------------------------------------------------------------------------------------------