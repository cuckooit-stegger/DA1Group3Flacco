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
  rect(breaks[-nB], 0, breaks[-1], y, col="red", ...)
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

#function to draw a scatterplot with custom upper and diagonal panels
pairs.custom <- function(x, m) {
  pairs(x, panel = function (x, y, ...) {
    points(x, y, ...)
    abline(lm(y ~ x), col = "blue") 
    #include correlation coefficients in upper panel and histograms on diagonal
  }, pch=19, upper.panel=panel.cor2, diag.panel=panel.hist2, main=m)
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

### Lucas:  which scatterplots do make sense? Scatterplots with feature groups won't tell us anything 
###         important I would guess. Maybe scatterplots between the several mean columns of the feature
###         groups? Or include some of the setup features to show which setup feature influences which 
###         other feature?
### Christian: I included scatterplot for displaying the within feature group corr and the between feature group
####           corr. Thus we can see that, how expected within them it is high and between low. But we also see
###            that there are some dependencies between feat groups
###           Maybe we can also include some measure on the average corr within a scatterplot

#correlation within the feature-groups is relatively high. This retrieves that it makes sense in further steps to
#reduce dimensionality
pairs.custom(bfeats.cm_angle, m="Correlation within Feature Group cm_angle")
pairs.custom(bfeats.cm_conv, m="Correlation within Feature Group cm_conv")
pairs.custom(bfeats.cm_grad, m="Correlation within Feature Group cm_grad")
pairs.custom(bfeats.ela_conv, m="Correlation within Feature Group ela_conv")
pairs.custom(bfeats.ela_curv[,seq(1,7)], m="Correlation within Feature Group ela_curv (extract)")
pairs.custom(bfeats.ela_curv[,c(1,8,15)], m="Correlation within Feature Group ela_curv (extract)")
pairs.custom(bfeats.ela_local[,c(6,7,8,9,10,11,12)], m="Correlation within Feature Group ela_local")

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
prcomp_feat_groups = data.frame(
bfeats.topology,
prcomp(bfeats.cm_angle, scale=TRUE)$x[,1],
prcomp(bfeats.cm_conv, scale=TRUE)$x[,1],
prcomp(bfeats.cm_grad, scale=TRUE)$x[,1],
prcomp(bfeats.ela_conv, scale=TRUE)$x[,1],
prcomp(bfeats.ela_curv, scale=TRUE)$x[,1],
prcomp(bfeats.ela_local, scale=TRUE)$x[,1])
#column names
colnames(prcomp_feat_groups) <- c("topology", "cm_angle", "cm_conv", "cm_curv", "ela_conv", "ela_curv", "ela_local")

#view scatterplot
#it can be seen that most of the feature_groups have low correlation between each other
#BUT there are obviously some dependencies:
#topology <> ela_curv
#topology <> ela_local
#cm_angle <> cm_curv
#cm_conv <> ela_conv
#ela_curv <> ela_local
pairs.custom(prcomp_feat_groups, m="Correlation between Feature Groups")
pairs.cor(prcomp_feat_groups)   #0.4809  correlation of cerrtain groups drives overall correlation

#scatterplot3d
#include package
require(scatterplot3d) 

#scatterplots for displaying different feature groups against each other
scatterplot3d(prcomp_feat_groups$ela_conv, prcomp_feat_groups$ela_curv, prcomp_feat_groups$ela_local,
              angle=35, pch=19, cex.lab=2, type="p", main="3D Scatterplot on ela features", xlab="ela_conv",
              ylab="ela_curv", zlab="ela_local")
scatterplot3d(prcomp_feat_groups$cm_angle, prcomp_feat_groups$cm_conv, prcomp_feat_groups$cm_grad,
              angle=35, pch=19, cex.lab=2, type="p", main="3D Scatterplot on ela features", xlab="cm_angle",
              ylab="cm_conv", zlab="cm_grad")


#-----------------------------------------------------------------------------------------------------------

#1.2 Outlier Detection


#-----------------------------------------------------------------------------------------------------------

#1.3 Tests for Normality


#-----------------------------------------------------------------------------------------------------------

#1.4 Transformation to Normality


#-----------------------------------------------------------------------------------------------------------


