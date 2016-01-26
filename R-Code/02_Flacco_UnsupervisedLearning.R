# Group 03
# Team Members:
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

#Part02 Unsupervised Learning
#
#Uses the preprocessed data from Part01 and analysis the data by applying techniques from the
#unsupervised learning.

#load custom functions
load("../3-customFunctions.RData")

#load data from part 01 preprocessing
load("../3-DataPreprocessing.RData")

#IMPORTANT
#bfeats3, bfeats3.cm_angle, bfeats3.cm_conv, etc. is used, because this is the orig dataset without outliers

# --------------------------------------------------------------------------------------------------------------
#2.1 Principal Component Analysis (PCA)
section.new("2-1")

#PCA analyis of the whole dataset
#first based on covariance matrix (exclude topology since it is input parameter)
bfeats3.pca_cov = princomp(bfeats3[,-1], cor=FALSE)
#examine reulting PCs
#only 2 PCs needed for explaining 80% of variance
summary(bfeats3.pca_cov)
biplot(bfeats3.pca_cov, main="Biplot of PCA based on cov")
#analysing these PC it can be seen that these PC rely on a very small number of features
#In fact the large scale of some features speaks against using the covariance matrix as their basis
#1.PC : ela_curv.grad_scale.max & ela_curv.hessian_cond.max  
#2.PC : ela_curv.grad_scale.max & ela_curv.hessian_cond.max & ela_local.costs_fun_evals
#thus the do not represent the dataset properly
round(summary(bfeats3.pca_cov, loadings = TRUE)$loadings[,1:2], 3)

#Therefore the PCA should be based on the correlation matrix instead
#Feat 49 (ela_local.basin_sizes.avg_non_best) has to be excluded as well since its variance is too low resulting in a non non-definite cor matrix
#(exclude topology (feat 1) since it is input parameter)
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
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs", col=colors[metadata3[,1]],
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#from the color appearance we see that the clusters depend on the metadata argument num of blocks
#so we have to be careful when doing cluster analysis
#but when only including data with block == 3 in plot, other clusters are still there
pairs_noreg.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1:6], m="Scatterplot on PCs")
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

## (----------)

#based on the idea that CM and ELA are features of two different method 
#it would be a good start to do a PCA separately on each method features
#one for CM features, one for ELA features

#PCA for CM; features 2 to 18
bfeats3.pca_cor.cm = princomp(bfeats3[,2:18], cor=TRUE, scores=TRUE)
#examine resulting PCs
#4 PCs needed for explaining 80 % of overall variance
summary(bfeats3.pca_cor.cm)
#plot scree diagram
#for deciding on appropriate number of PCs
plot(bfeats3.pca_cor.cm$sdev, type="b", main="Scree diagram CM-PCA on cor", xlab="Component number", ylab="Component variance")
#the scree diagram shows up a kink after PCs 5. So we consider that to be an appropriate number of PCs
#they result in explaining about 88% of overall variance and are still practical for analysis
plot(bfeats3.pca_cor.cm$sdev, type="b", main="Scree diagram CM-PCA on cor (elbow)", xlab="Component number", ylab="Component variance")
points(5, bfeats3.pca_cor.cm$sdev[5], col="red", cex=2.5, lwd=1.5, pch=1)
#biplot
biplot(bfeats3.pca_cor.cm, main="Biplot of CM-PCA based on cor")
#show factors of components
round(summary(bfeats3.pca_cor.cm, loadings = TRUE)$loadings[,1:5], 3)

#visualization of CM-PCA
#scatterplot
pairs_noreg.custom(bfeats3.pca_cor.cm$scores[,1:5], m="Scatterplot on CM-PCs")
#actually again we can  see that there are some clusters 
#strong influence of blocks on CM-Features
pairs_noreg.custom(bfeats3.pca_cor.cm$scores[,1:5], m="Scatterplot on CM-PCs", col=colors[metadata3[,1]],
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#influence of topology
pairs_noreg.custom(bfeats3.pca_cor.cm$scores[,1:5], m="Scatterplot on PCs", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 CM-PC
require(scatterplot3d)
scatterplot3d.custom(bfeats3.pca_cor.cm$scores[,1], bfeats3.pca_cor.cm$scores[,2], bfeats3.pca_cor.cm$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. CM-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)
#again influence of blocks visible
scatterplot3d.custom(bfeats3.pca_cor.cm$scores[,1], bfeats3.pca_cor.cm$scores[,2], bfeats3.pca_cor.cm$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. CM-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[metadata3[,1]], legend.title="Number of blocks", 
                     legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#and again influence of topology as important parameter since it is "real" feature of function
#and not of evaluation process as num blocks
scatterplot3d.custom(bfeats3.pca_cor.cm$scores[,1], bfeats3.pca_cor.cm$scores[,2], bfeats3.pca_cor.cm$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. CM-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3[,1]], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[c(1,2)])



#PCA for ELA; features 19 to 58 (leave out 49 because of small var)
bfeats3.pca_cor.ela = princomp(bfeats3[,c(19:48,50:58)], cor=TRUE, scores=TRUE)
#examine resulting PCs
#9 PCs needed for explaining 80 % of overall variance
summary(bfeats3.pca_cor.ela)
#plot scree diagram
#for deciding on appropriate number of PCs
plot(bfeats3.pca_cor.ela$sdev, type="b", main="Scree diagram ELA-PCA on cor", xlab="Component number", ylab="Component variance")
#the scree diagram shows up a kink after PCs 5. So we consider that to be an appropriate number of PCs
#they result in explaining about 88% of overall variance and are still practical for analysis
plot(bfeats3.pca_cor.ela$sdev, type="b", main="Scree diagram ELA-PCA on cor (elbow)", xlab="Component number", ylab="Component variance")
points(5, bfeats3.pca_cor.ela$sdev[5], col="red", cex=2.5, lwd=1.5, pch=1)
#biplot
biplot(bfeats3.pca_cor.ela, main="Biplot of ELA-PCA based on cor")
#show factors of components
round(summary(bfeats3.pca_cor.ela, loadings = TRUE)$loadings[,1:5], 3)

#visualization of ELA-PCA
#scatterplot
pairs_noreg.custom(bfeats3.pca_cor.ela$scores[,1:5], m="Scatterplot on ELA-PCs")
#actually again we can  see that there are some clusters 
#no influence of blocks on ELA-Features
pairs_noreg.custom(bfeats3.pca_cor.ela$scores[,1:5], m="Scatterplot on ELA-PCs", col=colors[metadata3[,1]],
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
# strong influence of topology on clusters
pairs_noreg.custom(bfeats3.pca_cor.ela$scores[,1:5], m="Scatterplot on PCs", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 ELA-PC
require(scatterplot3d)
scatterplot3d.custom(bfeats3.pca_cor.ela$scores[,1], bfeats3.pca_cor.ela$scores[,2], bfeats3.pca_cor.ela$scores[,3],
                     angle=95, main="3D Scatterplot on 1., 2., 3. ELA-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)
#peaks has some influence on ELA features
scatterplot3d.custom(bfeats3.pca_cor.ela$scores[,1], bfeats3.pca_cor.ela$scores[,2], bfeats3.pca_cor.ela$scores[,3],
                     angle=95, main="3D Scatterplot on 1., 2., 3. ELA-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC",col=colors[metadata3[,4]/20], 
                     legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                     "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"),
                     legend.col=colors[1:10])
#and again influence of topology as important parameter since it is "real" feature of function
#and not of evaluation process as num blocks
scatterplot3d.custom(bfeats3.pca_cor.ela$scores[,1], bfeats3.pca_cor.ela$scores[,2], bfeats3.pca_cor.ela$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. ELA-PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3[,1]], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[c(1,2)])


#Finally an analysis with including first PC of each feature group
bfeats3.pca_fgroups = data.frame(
  princomp(bfeats3.cm_angle, corr=TRUE, scores=TRUE)$scores[,1],
  princomp(bfeats3.cm_conv, corr=TRUE, scores=TRUE)$scores[,1],
  princomp(bfeats3.cm_grad, corr=TRUE, scores=TRUE)$scores[,1],
  princomp(bfeats3.ela_conv, corr=TRUE, scores=TRUE)$scores[,1],
  princomp(bfeats3.ela_curv, corr=TRUE, scores=TRUE)$scores[,1],
  princomp(bfeats3.ela_local, corr=TRUE, scores=TRUE)$scores[,1])
#check the amount of variance covered by first PC for each group
summary(princomp(bfeats3.cm_angle, corr=TRUE, scores=TRUE))  #93.21%
summary(princomp(bfeats3.cm_conv, corr=TRUE, scores=TRUE))  #73.76%
summary(princomp(bfeats3.cm_grad, corr=TRUE, scores=TRUE))  #95.88%
summary(princomp(bfeats3.ela_conv, corr=TRUE, scores=TRUE))  #99.99%
summary(princomp(bfeats3.ela_curv, corr=TRUE, scores=TRUE))  #50.77%
summary(princomp(bfeats3.ela_local, corr=TRUE, scores=TRUE))  #99.88%
#column names
colnames(bfeats3.pca_fgroups) <- c("cm_angle", "cm_conv", "cm_curv", "ela_conv", "ela_curv", "ela_local")

#visualiztion of Feature Group PCA
#scatterplot
pairs_noreg.custom(bfeats3.pca_fgroups[1:6], m="Scatterplot on Feature Group-PCs")
#influence of topology
pairs_noreg.custom(bfeats3.pca_fgroups[1:6], m="Scatterplot on Feature Group-PCs", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#-----------------------------------------------------------------------------------------------------------

#2.2 Multidimensional Scaling
section.new("2-2")

#application of multidimensional scaling similar to PCA above

#methodology implemented
#parameters are distance matrix and number of dimensions
mds.custom <- function(dist_mat, dim) {
  #calculate A from distance matrix
  A = -0.5*as.matrix(dist_mat)^2
  #centering the matrix
  B = A - outer(rowMeans(A), colMeans(A), "+") + mean(A)
  #calculate eigenvalues
  eg = eigen(B)
  X = NULL
  #concatenate eigenvectors after transformationn by sqrt of eigenvalues
  for(i in 1:dim) {
    X = cbind(X, eg$vectors[,i] * sqrt(eg$values[i]))
  }
  #return dataframe of eigenvectors and eigenvalues
  data.frame(eig=eg, points=X)
}

#apply MDS to the whole dataset
#calculate distance matrix
bfeats3.dist = dist(bfeats3[,2:58])
#built in function
#choose a maximum of 20 dimensinos
bfeats3.mds = cmdscale(bfeats3.dist, k = 20, eig=TRUE, add = FALSE, x.ret = FALSE)
#examine resulting data
#2 dimensions for 80% of overall variance
cumsum(bfeats3.mds$eig[1:20])/sum(bfeats3.mds$eig[1:20])
#again we see that due to scaling issues only low number of components do have influence
#and strongly depend on small number of input features


#therefore similar to PCA we should scale data before applying MDS
#calculate distance matrix after scaling
bfeats3.dist_scale = dist(as.data.frame(apply(bfeats3[,2:58], 2, scale)))
#built in function
#choose a maximum of 20 dimension with knowledge of PCS above
bfeats3.mds_scale = cmdscale(bfeats3.dist_scale, k = 20, eig=TRUE, add = FALSE, x.ret = FALSE)
#examine resulting data
#9 dimensions for 80% of overall variance
cumsum(bfeats3.mds_scale$eig[1:20])/sum(bfeats3.mds_scale$eig[1:20])
#plot scree diagram
#for deciding on appropriate number of dimensions
plot(bfeats3.mds_scale$eig[1:20], type="b", main="Scree diagram MDS", xlab="Component number", ylab="Component value")
#the scree diagram shows up a kink after dimension 6. So we consider that to be an appropriate number of dimensions
#they result in explaining about 71% of overall variance and are still practical for analysis
plot(bfeats3.mds_scale$eig[1:20], type="b", main="Scree diagram MDS (elbow)", xlab="Component number", ylab="Component value")
points(6, bfeats3.mds_scale$eig[6], col="red", cex=2.5, lwd=1.5, pch=1)

#visualization of the 6 identified dimensions
#scatterplot
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS")
#the result is the same as the PCA scatterplot (besides rotation)
#again we can identify influence of some metadat
#blocks
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS (num blocks)", col=colors[metadata3[,1]],
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#num peaks does not have influence on clusters
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS (num peaks)", col=colors[metadata3[,4]/20],
                   legend.title = "Number of peaks", legend.text = c("20 peaks", "40 peaks", "60 peaks", 
                   "80 peaks", "100 peaks", "120 peaks", "140 peaks", "160 peaks","180 peaks", "200 peaks"),
                   legend.col=colors[1:10])
#prob.seed does not have influence on clusters
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS (prob.seed)", col=colors[metadata[,5]], 
                   legend.title = "Prob.seed", legend.text = c("1", "2", "3", 
                                                               "4", "5"),legend.col=colors[1:5])
#repl does not have influence on clusters
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS (repl)", col=colors[metadata[,6]],
                   legend.title = "Repl", legend.text = c("1", "2", "3", 
                                                          "4", "5", "6", "7", "8","9", "10"),
                   legend.col=colors[1:10])
#topology seems to have influence. so that will be important parameter of the function
pairs_noreg.custom(bfeats3.mds_scale$points[,1:6], m="Scatterplot on MDS (topology)", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 PC
require(scatterplot3d)
#comparison with PCA yields that it is the same besides rotation (as expected)
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.mds_scale$points[,1], bfeats3.mds_scale$points[,2], bfeats3.mds_scale$points[,3],
                     angle=75, main="3D Scatterplot on MDS (3 dim)", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim", color=colors[1], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC",color=colors[1], cex.lab=1, type="p", pch=19)
#influence of blocks
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.mds_scale$points[,1], bfeats3.mds_scale$points[,2], bfeats3.mds_scale$points[,3],
              angle=75, main="3D Scatterplot on MDS (num blocks)", xlab="1 dim",
              ylab="2 dim", zlab="3 dim", color=colors[metadata3[,1]], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
              angle=75, main="3D Scatterplot on PCs (num blocks)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[metadata3[,1]], cex.lab=1, type="p", pch=19)
#influence of topology
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.mds_scale$points[,1], bfeats3.mds_scale$points[,2], bfeats3.mds_scale$points[,3],
              angle=75, main="3D Scatterplot on MDS (topology)", xlab="1 dim",
              ylab="2 dim", zlab="3 dim", color=colors[bfeats3[,1]], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
              angle=75, main="3D Scatterplot on PCs (topology)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[bfeats3[,1]], cex.lab=1, type="p", pch=19)


## (----------------)

#use of custom mds function and comparison to built-in one
bfeats3.mds_custom = mds.custom(bfeats3.dist_scale, 20)
#same result as for built in-function
cumsum(bfeats3.mds_custom[1:20,1])/sum(bfeats3.mds_custom[1:20,1])
plot(bfeats3.mds_custom[1:20,1], type="b", main="Scree diagram method-MDS (elbow)", xlab="Component number", ylab="Component value")
points(6, bfeats3.mds_custom[6,1], col="red", cex=2.5, lwd=1.5, pch=1)

#comparison built-in and custom function for MDS
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.mds_scale$points[,1], bfeats3.mds_scale$points[,2], bfeats3.mds_scale$points[,3],
              angle=75, main="3D Scatterplot on MDS built-in", xlab="1 dim",
              ylab="2 dim", zlab="3 dim", color=colors[bfeats3[,1]], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.mds_custom[,2], bfeats3.mds_custom[,3], bfeats3.mds_custom[,4],
              angle=75, main="3D Scatterplot on MDS custom", xlab="1 dim",
              ylab="2 dim", zlab="3 dim",color=colors[bfeats3[,1]], cex.lab=1, type="p", pch=19)
#therefore we will go on using the results from built-in function as they are the same



#again we will conduct a MDS separately on CM / ELA features (like PCA)
#maybe when clustering in further tasks there will be better results with separate look onto methods

#MDS on CM features; featuers 2 to 18
#dist matrix (scaled features)
bfeats3.dist_scale.cm = dist(as.data.frame(apply(bfeats3[,2:18], 2, scale)))
#built in function
#choose a maximum of 20 dimension with knowledge of PCS above
bfeats3.mds_scale.cm = cmdscale(bfeats3.dist_scale.cm, k = 20, eig=TRUE, add = FALSE, x.ret = FALSE)
#examine resulting data
#4 dimensions for 80% of overall variance
cumsum(bfeats3.mds_scale.cm$eig[1:20])/sum(bfeats3.mds_scale.cm$eig[1:20])
#plot scree diagram
#for deciding on appropriate number of dimensions
plot(bfeats3.mds_scale.cm$eig[1:20], type="b", main="Scree diagram CM-MDS", xlab="Component number", ylab="Component value")
#the scree diagram shows up a kink after dimension 6. So we consider that to be an appropriate number of dimensions
#they result in explaining about 71% of overall variance and are still practical for analysis
plot(bfeats3.mds_scale.cm$eig[1:20], type="b", main="Scree diagram CM-MDS (elbow)", xlab="Component number", ylab="Component value")
points(6, bfeats3.mds_scale.cm$eig[6], col="red", cex=2.5, lwd=1.5, pch=1)

#visualization of CM-MDS
#scatterplot
pairs_noreg.custom(bfeats3.mds_scale.cm$points[,1:6], m="Scatterplot on CM-MDS")
#actually again we can  see that there are some clusters 
#strong influence of blocks on CM-Features
pairs_noreg.custom(bfeats3.mds_scale.cm$points[,1:6], m="Scatterplot on CM-MDS (num blocks)", col=colors[metadata3[,1]],
                   legend.title="Number of blocks", 
                   legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#influence of topology
pairs_noreg.custom(bfeats3.mds_scale.cm$points[,1:6], m="Scatterplot on CM-MDS (topology)", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 CM-dimensions
require(scatterplot3d)
scatterplot3d.custom(bfeats3.mds_scale.cm$points[,1], bfeats3.mds_scale.cm$points[,2], bfeats3.mds_scale.cm$points[,3],
                     angle=75, main="3D Scatterplot on CM-dimensions", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)
#again influence of blocks visible
scatterplot3d.custom(bfeats3.mds_scale.cm$points[,1], bfeats3.mds_scale.cm$points[,2], bfeats3.mds_scale.cm$points[,3],
                     angle=75, main="3D Scatterplot on CM-dimensions (num blocks)", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim", col=colors[metadata3[,1]], legend.title="Number of blocks", 
                     legend.text=c("3 blocks", "5 blocks", "7 blocks"), legend.col=colors[c(3,5,7)])
#and again influence of topology as important parameter since it is "real" feature of function
#and not of evaluation process as num blocks
scatterplot3d.custom(bfeats3.mds_scale.cm$points[,1], bfeats3.mds_scale.cm$points[,2], bfeats3.mds_scale.cm$points[,3],
                     angle=75, main="3D Scatterplot on CM-dimensions (topology)", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim", col=colors[bfeats3[,1]], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[c(1,2)])



#MDS on ELA features; featuers 19 to 58
#dist matrix (scaled features)
bfeats3.dist_scale.ela = dist(as.data.frame(apply(bfeats3[,19:58], 2, scale)))
#built in function
#choose a maximum of 20 dimension with knowledge of PCS above
bfeats3.mds_scale.ela = cmdscale(bfeats3.dist_scale.ela, k = 20, eig=TRUE, add = FALSE, x.ret = FALSE)
#examine resulting data
#9 dimensions for 80% of overall variance
cumsum(bfeats3.mds_scale.ela$eig[1:20])/sum(bfeats3.mds_scale.ela$eig[1:20])
#plot scree diagram
#for deciding on appropriate number of dimensions
plot(bfeats3.mds_scale.ela$eig[1:20], type="b", main="Scree diagram ELA-MDS", xlab="Component number", ylab="Component value")
#the scree diagram shows up a kink after dimension 5. So we consider that to be an appropriate number of dimensions
#they result in explaining about 71% of overall variance and are still practical for analysis
plot(bfeats3.mds_scale.ela$eig[1:20], type="b", main="Scree diagram ELA-MDS (elbow)", xlab="Component number", ylab="Component value")
points(5, bfeats3.mds_scale.ela$eig[5], col="red", cex=2.5, lwd=1.5, pch=1)

#visualization of ELA-MDS
#scatterplot
pairs_noreg.custom(bfeats3.mds_scale.ela$points[,1:5], m="Scatterplot on ELA-MDS")
#influence of topology
pairs_noreg.custom(bfeats3.mds_scale.ela$points[,1:5], m="Scatterplot on ELA-MDS (topology)", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#scatterplot3d of first 3 ELA-dimensions
require(scatterplot3d)
scatterplot3d.custom(bfeats3.mds_scale.ela$points[,1], bfeats3.mds_scale.ela$points[,2], bfeats3.mds_scale.ela$points[,3],
                     angle=135, main="3D Scatterplot on ELA-dimensions", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim",col=colors[1], legend.title="no", legend.col=NULL, legend.text=NULL)
#influence of topology again
scatterplot3d.custom(bfeats3.mds_scale.ela$points[,1], bfeats3.mds_scale.ela$points[,2], bfeats3.mds_scale.ela$points[,3],
                     angle=135, main="3D Scatterplot on ELA-dimensions (topology)", xlab="1 dim",
                     ylab="2 dim", zlab="3 dim", col=colors[bfeats3[,1]], legend.title="Topology", 
                     legend.text=c("funnel", "random"), legend.col=colors[c(1,2)])



#Finally an analysis with including first dimension of MDS of each feature group
bfeats3.mds_fgroups = data.frame(
  cmdscale(dist(as.data.frame(apply(bfeats3.cm_angle, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1],
  cmdscale(dist(as.data.frame(apply(bfeats3.cm_conv, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1],
  cmdscale(dist(as.data.frame(apply(bfeats3.cm_grad, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1],
  cmdscale(dist(as.data.frame(apply(bfeats3.ela_conv, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1],
  cmdscale(dist(as.data.frame(apply(bfeats3.ela_curv, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1],
  cmdscale(dist(as.data.frame(apply(bfeats3.ela_local, 2, scale)))
           , k = 1, eig=TRUE, add = FALSE, x.ret = FALSE)$points[,1])
#column names
colnames(bfeats3.mds_fgroups) <- c("cm_angle", "cm_conv", "cm_curv", "ela_conv", "ela_curv", "ela_local")

#visualiztion of Feature Group MDS dimensions
#scatterplot
pairs_noreg.custom(bfeats3.mds_fgroups[1:6], m="Scatterplot on Feature Group-MDS")
#influence of topology
pairs_noreg.custom(bfeats3.mds_fgroups[1:6], m="Scatterplot on Feature Group-MDS", col=colors[bfeats3[,1]],
                   legend.title = "Topology", legend.text = c("funnel", "random"),
                   legend.col=colors[1:10])

#-----------------------------------------------------------------------------------------------------------

#2.3 Cluster Analysis
#2.3.1 K-Means Algorithm
section.new("2-3-1")

#set seed for allow reproduction of results
set.seed(1906)


#standardize variables by their range
bfeats3.s = sweep(bfeats3, 2, sapply(bfeats3, function(x) diff(range(x))), FUN='/')
#cluster dataset when looking onto the whole dataset
kmeans_wss.custom(bfeats3.s, 12, main="whole dataset")
#from the screeplots it could be suggested to have 2 or 5 clusters
#apply both
bfeats3.clust2 = kmeans(bfeats3.s, centers=2, nstart=25)
bfeats3.clust5 = kmeans(bfeats3.s, centers=5, nstart=25)
#show centers of cluster
#2 cluster; 1. cluster 1458 obs , 2. cluster 1352 obs
bfeats3.clust2$centers * sapply(bfeats3, function(x) diff(range(x)))
#5 cluster: 1. cluster 473 obs, 2. cluster 439 obs, 3. cluster 591, 4.cluster 336, 5.cluster 971 obs
bfeats3.clust5$centers * sapply(bfeats3, function(x) diff(range(x)))

#the two cluster solution reminds us of the underlying topology
#for 96% of obs this is the case
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs (2 clusters)", col=colors[bfeats3.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
#scatterplot3d shows the same 
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC (2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.clust2$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)])

#the five cluster approach shows up clusters which seem to be well organized when looking at the first PCs
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs (5 clusters)", col=colors[bfeats3.clust5$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2", "3", "4", "5"), legend.col=colors[1:5])
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC (5 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.clust5$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2", "3", "4", "5"), legend.col=colors[1:5])



#as we saw a quite strong influence of the blocks metadata on the clusters in the PCs
#next we apply a cluster analysis within only one cluster setting, for excluding that influence
#which came from different CM-settings
kmeans_wss.custom(bfeats3.s[which(metadata3[,1]==3),], 12, main="blocks == 3")
#elbow at 2 clusters
#apply
bfeats3.block3.clust2 = kmeans(bfeats3.s[which(metadata3[,1]==3),], centers=2, nstart=25)
#show centers of cluster
bfeats3.block3.clust2$centers * sapply(bfeats3, function(x) diff(range(x)))

#visualize results
#still we can see the topology difference
pairs_noreg.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1:6], m="Scatterplot on PCs (block==3, 2 clusters)",
                   col=colors[bfeats3.block3.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
#scatterplot3d shows the same 
scatterplot3d.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1], bfeats3.pca_cor$scores[which(metadata3[,1]==3),2],
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3),3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC (block==3, 2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.block3.clust2$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)])




#even only choosing one topology setting is tested for excluding that influence
#funnel
kmeans_wss.custom(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==1),], 12, main="blocks=3, top=funnel")
bfeats3.top_fun.clust2 = kmeans(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==1),], centers=2, nstart=25)
#random
kmeans_wss.custom(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==2),], 12, main="blocks=3, top=random")
bfeats3.top_ran.clust2 = kmeans(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==2),], centers=2, nstart=25)

#funnel topology
#not really distinct clusters
pairs_noreg.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),1:6], 
                    m="Scatterplot on PCs (block=3, top=funnel, 2 clusters)",
                   col=colors[bfeats3.top_fun.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
scatterplot3d.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2],
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC (block=3, top=funnel, 2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.top_fun.clust2$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)])

#random topology
#there are two distinct clusters
pairs_noreg.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),1:6], 
                   m="Scatterplot on PCs (block=3, top=random, 2 clusters)",
                   col=colors[bfeats3.top_ran.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
scatterplot3d.custom(bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2],
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
                     angle=75, main="3D Scatterplot on 1., 2., 3. PC (block=3, top=random, 2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.top_ran.clust2$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)])




#another idea would be doing clustering only on CM features 
#because CM is another method than ELA
kmeans_wss.custom(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==1),2:18], 12, main="CM features")
#2 clusters is appropriate
bfeats3.cm.clust2 = kmeans(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==1),2:18], centers=2, nstart=25)

#comparison of overall-feature / CM-feature clustering on Funnel Data
#it shows that using only CM-Features for clustering is more precise in case of funnel data
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),2],
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),3],
              angle=20, main="Clustering using all features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.top_fun.clust2$cluster], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),2],
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3 & bfeats3[,1]==1),3],
              angle=20, main="Clustering using only CM-Features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[bfeats3.cm.clust2$cluster], cex.lab=1, type="p", pch=19)





#applying kmeans using the ELA features
kmeans_wss.custom(bfeats3.s[,19:58], 12, main="ELA features")
#2 clusters is strongly recommended
bfeats3.ela.clust2 = kmeans(bfeats3.s[,19:58], centers=2, nstart=25)

#
pairs_noreg.custom(bfeats3.pca_cor.ela$scores[,1:6], 
                   m="Scatterplot on ELA-PCs (2 clusters)",
                   col=colors[bfeats3.ela.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])

#comparison of clustering with ELA-features only and all features
#shows that ELA-features are enough for clustering data
#according to their landscape topology (funnel vs. random)
layout(matrix(1:2, ncol=2))
#clusters 96% correct
scatterplot3d(bfeats3.pca_cor.ela$scores[,1], 
              bfeats3.pca_cor.ela$scores[,2],
              bfeats3.pca_cor.ela$scores[,3],
              angle=115, main="Clustering using all features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.clust2$cluster], pch=19, cex.lab=1, type="p")
#clusters already 83% correctly
scatterplot3d(bfeats3.pca_cor.ela$scores[,1], 
              bfeats3.pca_cor.ela$scores[,2],
              bfeats3.pca_cor.ela$scores[,3],
              angle=115, main="Clustering using only ELA-Features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[bfeats3.ela.clust2$cluster], cex.lab=1, type="p", pch=19)



#also when clustering the problems with random topology
#using only ELA features is precise
bfeats3.ela_ran.clust2 = kmeans(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==2),19:58], centers=2, nstart=25)
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),2],
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),3],
              angle=340, main="Clustering using all features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.top_ran.clust2$cluster], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),2],
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3 & bfeats3[,1]==2),3],
              angle=340, main="Clustering using only ELA-Features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[bfeats3.ela_ran.clust2$cluster], cex.lab=1, type="p", pch=19)




#alternative approach would be using low dimension data for kmeans clustering
#thus we use first 3 PCs of the whole dataset
#based on these 3 dimensions we execute kmeans clustering
#scaling PC as well
kmeans_wss.custom(sweep(bfeats3.pca_cor$scores[,1:3], 2, apply(bfeats3.pca_cor$scores[,1:3], 2,  function(x) diff(range(x))), FUN='/'),
                  12, main="based on 3 PCs")
#from the screeplots it could be suggested to have 2 clusters (small elbow)
#apply
bfeats3.pca_cor.clust2 = kmeans(sweep(bfeats3.pca_cor$scores[,1:3], 2, 
                                      apply(bfeats3.pca_cor$scores[,1:3], 2,  function(x) diff(range(x))), FUN='/'),
                                centers=2, nstart=25)

#clusters are different from first kmeans with all dimension
#seem to be less appropriate now
#since clusters by blocks settings not even all block settings distinguished
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs (2 clusters from PC)", 
                   col=colors[bfeats3.pca_cor.clust2$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
#3d
#using all dimensions clusters topology of landscape -> real function feature
#using PCs is not appropriate
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor$scores[,1], 
              bfeats3.pca_cor$scores[,2],
              bfeats3.pca_cor$scores[,3],
              angle=115, main="Clustering using all features", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.clust2$cluster], pch=19, cex.lab=1, type="p")
scatterplot3d(bfeats3.pca_cor$scores[,1], 
              bfeats3.pca_cor$scores[,2],
              bfeats3.pca_cor$scores[,3],
              angle=115, main="Clustering using 3 PCs", xlab="1 PC",
              ylab="2 PC", zlab="3 PC",color=colors[bfeats3.pca_cor.clust2$cluster], cex.lab=1, type="p", pch=19)

#outcomes from kmeans-clustering:
#using all features for clustering whole dataset: 2 / 5 clusters
#   -2 cluster represent problem landscape
#
#using all features for clustering only one block setting: 2 cluster
#   -problem landscape still represented by clusters
#
#using all features for clustering only one block setting and one problem landscape topology: 2 cluster
#   -funnel topology -> clusters not precise
#   -random topology -> 2 precise clusters
#
#using only CM-Features for clustering one block setting and funnel topology:
#   -2 clusters are now precise (better than using all features for clustering)
#so when knowing that problem landscape has funnel topology only CM-Features should be used for clustering landscape
#
#using only ELA-Features is almost as precise as using all features for cluster
#when clustering the landscape topology of the problem instances
#also when clustering random topology setting using ELA-Features is enough
#in this cases using ELA-Features is enough and less costly


#-----------------------------------------------------------------------------------------------------------

#2.3.2 Hierarchical Clustering
section.new("2-3-2")

#apply methods to whole dataset and all features
#single: not appropriate
#complete: 2 clusters 
#average: not appropriate
#centroid: not appropriate
#ward.D1: 3 clusters -> seems to be good clustering (visualization below)
#ward.D2: 3 clusters 
aggl_dend.custom(bfeats3.s[,2:58])

#based on centroid linkage: 2 clusters
bfeats3.aggl.wardD =  hclust(dist(bfeats3.s[,2:58]), method="ward.D")
bfeats3.aggl.wardD$group = cutree(bfeats3.aggl.wardD, k=2)

#visualization of ward.D cluster method
#reveals two clusters, but differently from kmeans clustering in 2.3.1
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs (2 clusters)", 
                   col=colors[bfeats3.aggl.wardD$group],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
scatterplot3d.custom(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="Agglomerative clustering (2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", col=colors[bfeats3.aggl.wardD$group], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)])



#apply methods to one block setting and all features
#for elaminating influence of block setting
#ward.D is most appropriate when implementing -> 2 clusters
aggl_dend.custom(bfeats3.s[which(metadata3[,1]==3),2:58])
bfeats3.block3.aggl.wardD =  hclust(dist(bfeats3.s[which(metadata3[,1]==3),2:58]), method="ward.D")
bfeats3.block3.aggl.wardD$group = cutree(bfeats3.block3.aggl.wardD, k=2)

#same clusters as from kmeans obviously
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1], 
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3),2], 
                     bfeats3.pca_cor$scores[which(metadata3[,1]==3),3],
                     angle=200, main="Agglomerative Clustering (4 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.aggl.wardD$group],pch=19)
scatterplot3d(bfeats3.pca_cor$scores[which(metadata3[,1]==3),1], 
              bfeats3.pca_cor$scores[which(metadata3[,1]==3),2], 
              bfeats3.pca_cor$scores[which(metadata3[,1]==3),3],
              angle=200, main="Kmeans algorithm (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.clust2$cluster],pch=19)



#apply methods to one block setting + funnel topology problems and only CM-features
#as we know from 2.3.1 that CM-Features can split up these instances quite well
#average, complete, ward.D and ward.D2 methods all lead to two clusters
#example implementation of average linkage
aggl_dend.custom(bfeats3.s[which(metadata3[,1]==3 & bfeats3[,1]==1),2:18])
bfeats3.block3.fun.aggl.av =  hclust(dist(bfeats3.s[which(metadata3[,1]==3& bfeats3[,1]==1),2:18]), method="average")
bfeats3.block3.fun.aggl.av$group = cutree(bfeats3.block3.fun.aggl.av, k=2)

#visualization
#agglomerative splits up data differently from kmeans
#kmeans seems to be more appropriate from visualization
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
              angle=20, main="Agglomerative Clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.fun.aggl.av$group],pch=19)
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
              angle=20, main="KMeans Algorithm (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.cm.clust2$cluster],pch=19)




#apply methods to one block setting + random and only ELA-features
#based on wardD / wardD 2 clusters are appropriate
aggl_dend.custom(bfeats3.s[which(metadata3[,1]==3, bfeats3[,1]==2),19:58])
bfeats3.block3.ran.aggl.av =  hclust(dist(bfeats3.s[which(metadata3[,1]==3& bfeats3[,1]==2),19:58]), method="ward.D")
bfeats3.block3.ran.aggl.av$group = cutree(bfeats3.block3.ran.aggl.av, k=2)

#comparison to kmeans shows almost equal clusters
layout(matrix(1:2, ncol=2))
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
              angle=80, main="Agglomerative Clustering (4 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.ran.aggl.av$group],pch=19)
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
              angle=80, main="KMeans Algorithm (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.ela_ran.clust2$cluster],pch=19)


#findings from hierarchical clustering
#clusters are often not equal ot kmeans clusters
#does better show up "optimal" number of clusters by dendrograms
#kmeans cluster indeed seem to be more appropriate from visualizations perspective

#-----------------------------------------------------------------------------------------------------------

#2.3.3 Divisive Hierarchical Clustering
section.new("2-3-3")

#load package
require(cluster)

#apply methods to whole dataset and all features
bfeats3.dv = diana(bfeats3[,2:18], diss=FALSE, metric="euclidean", keep.diss=FALSE)
#dendrogram shows up 2 clusters
plot(bfeats3.dv)
#therefore split data to 3 clusters according to divisive clustering
bfeats3.dv$cluster = cutree(as.hclust(bfeats3.dv), k=2)

#visualization of clusters
#2 clusters do not represent landscape topology 
pairs_noreg.custom(bfeats3.pca_cor$scores[,1:6], m="Scatterplot on PCs (2 clusters divisive)", 
                   col=colors[bfeats3.dv$cluster],
                   legend.title="Cluster", 
                   legend.text=c("1", "2"), legend.col=colors[1:2])
#3d comparison to agglomerative clustering and kmeans
#kmeans is most appropriate, divisive still more appropriate than agglomerative
layout(matrix(1:3, ncol=3))
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="Agglomerative clustering (2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", color=colors[bfeats3.aggl.wardD$group], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)], pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
                     angle=75, main="Divisive clustering (2 clusters)", xlab="1 PC",
                     ylab="2 PC", zlab="3 PC", color=colors[bfeats3.dv$cluster], legend.title="Cluster", 
                     legend.text=c("1", "2"), legend.col=colors[c(1,2)], pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor$scores[,1], bfeats3.pca_cor$scores[,2], bfeats3.pca_cor$scores[,3],
              angle=75, main="Kmeans clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.clust2$cluster], legend.title="Cluster", 
              legend.text=c("1", "2"), legend.col=colors[c(1,2)], pch=19, cex.symbols=1.5)



#apply methods to one block setting + funnel topology problems and only CM-features
#as we know from 2.3.1 that CM-Features can split up these instances quite well
bfeats3.block3.fun.dv = diana(bfeats3[which(metadata3[,1]==3 & bfeats3[,1]==1),2:18], diss=FALSE, 
                              metric="euclidean", keep.diss=FALSE)
#dendrogram shows up 2 clusters
plot(bfeats3.block3.fun.dv)
#therefore split data to 3 clusters according to divisive clustering
bfeats3.block3.fun.dv$cluster = cutree(as.hclust(bfeats3.block3.fun.dv), k=2)

#comparison to kmeans and agglomerative
#divisive seems to be most appropriate from visualization perspective
layout(matrix(1:3, ncol=3))
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
              angle=80, main="Agglomerative Clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.fun.aggl.av$group],pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
              angle=80, main="Divisive Clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.fun.dv$cluster],pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),1], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),2], 
              bfeats3.pca_cor.cm$scores[which(metadata3[,1]==3& bfeats3[,1]==1),3],
              angle=80, main="KMeans Algorithm (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.top_fun.clust2$cluster],pch=19, cex.symbols=1.5)




#apply methods to one block setting + random and only ELA-features
#see 2.2 ELA features can split up random landscapes
bfeats3.block3.ran.dv = diana(bfeats3[which(metadata3[,1]==3 & bfeats3[,1]==2),19:58], diss=FALSE, 
                   metric="euclidean", keep.diss=FALSE)
#dendrogram shows up 2 clusters
plot(bfeats3.block3.ran.dv)
#therefore split data to 3 clusters according to divisive clustering
bfeats3.block3.ran.dv$cluster = cutree(as.hclust(bfeats3.block3.ran.dv$cluster), k=2)

#comparison to kmeans and agglomerative
#divisive not appropriate at all
layout(matrix(1:3, ncol=3))
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
              angle=80, main="Agglomerative Clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.ran.aggl.av$group],pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
              angle=80, main="Divisive Clustering (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.block3.ran.dv$cluster],pch=19, cex.symbols=1.5)
scatterplot3d(bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),1], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),2], 
              bfeats3.pca_cor.ela$scores[which(metadata3[,1]==3& bfeats3[,1]==2),3],
              angle=80, main="KMeans Algorithm (2 clusters)", xlab="1 PC",
              ylab="2 PC", zlab="3 PC", color=colors[bfeats3.ela_ran.clust2$cluster],pch=19, cex.symbols=1.5)

#findings from divisive hierarchical clustering:
#in most cases not appropriate due to distribution of data
#for CM-Features and funnel topology appropriate indeed

# ----------------------------------------------------------------------------------------------------------------
