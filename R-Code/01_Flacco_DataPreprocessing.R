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
  layout(matrix(1, ncol=1))
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



#-----------------------------------------------------------------------------------------------------------

#1.3 Transformation to Normality


#-----------------------------------------------------------------------------------------------------------

#1.4 Outlier Detection

#-----------------------------------------------------------------------------------------------------------

