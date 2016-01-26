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
#   1.2, 1.3 Normality  Gino  S.	Colletti
#   1.4 Outliers        Lucas Stegger
#   2.1 PCA             Martin Kubicki
#   2.2 MDS             Christian Siemen
#   2.3 Cluster         Daniel  Camiola

#Part00 Custom Functions
#
#Defines custom functions which are used in Part01 and Part02

#-------------------------------------------------------------------------------------------------------
#0.0 General settings
#debug?
debug = T

#method to print debug messages, if debug is true
debug.msg = function(s) {
  #only print message if debug is true
  if(debug == T) {
    print(s)
  }
}

#save default graphical parameters to be able to restore them before each new plot
par.defaults = par(no.readonly = T)

#write plots to pdf?
pdf.create = T

#pdf file settings
pdf.path = "../plots/"

pdf.filename.section = "1-1"
pdf.filename.index = 1
pdf.filename.main = ""

pdf.layout.settings = matrix(1:1, ncol=1)
pdf.layout.multiple = F
pdf.layout.plotcount = 1
pdf.layout.counter = 0

#method to restore default graphical parameters
par.reset = function() {
  par = par.defaults
  #set default settings for only one plot per pdf file
  pdf.layout.multiple <<- F
  pdf.layout.plotcount <<- 1
  pdf.layout.counter <<- 0
}

#method to set layout for plots including pdfs
layout.custom = function(layout = matrix(1:1, ncol=1), main = "", plotbreak = -1) {
  debug.msg(length(layout))
  par = par.defaults
  
  pdf.layout.settings <<- layout
  
  if(plotbreak == -1) {
    plotbreak = length(layout)
  }
  
  if(length(layout) == 1) {
    #set default settings for only one plot per pdf file
    pdf.layout.multiple <<- F
    pdf.layout.plotcount <<- 1
    pdf.layout.counter <<- 0
  } else {
    #set settings for multiple plots per pdf file
    pdf.layout.multiple <<- T
    pdf.layout.plotcount <<- plotbreak
    pdf.layout.counter <<- -1
    pdf.filename.main <<- main
    
    pdf.filename = paste(pdf.filename.section, pdf.filename.index, pdf.filename.main, sep="_")
    pdf(paste(pdf.path, pdf.filename, ".pdf", sep = ""), width = 11.69, height = 8.27)
    
    debug.msg(paste(pdf.path, pdf.filename, ".pdf", sep = ""))
    
    #increase index for pdf files
    pdf.filename.index <<- pdf.filename.index + 1
  }
  debug.msg(pdf.layout.plotcount)
  pdf.init(main)
  layout(layout)
}

#method to initialize new pdf device
pdf.init = function(main = "") {
  #write plot to pdf?
  debug.msg("pdf.layout.multiple: ")
  debug.msg(pdf.layout.multiple)
  debug.msg("pdf.layout.plotcount: ")
  debug.msg(pdf.layout.plotcount)
  debug.msg("pdf.layout.counter: ")
  debug.msg(pdf.layout.counter)
  
  if(pdf.create == T) {
    if(pdf.layout.multiple == T && pdf.layout.counter == pdf.layout.plotcount) {
      #begin new file
      debug.msg("Begin new file")
      layout.custom(pdf.layout.settings, main = pdf.filename.main, plotbreak = pdf.layout.plotcount)
    }
    
    #layout for multiple plots in one file set?
    if(pdf.layout.multiple == F) {
      pdf.open(main)
      
      #increase index for pdf files
      pdf.filename.index <<- pdf.filename.index + 1
    } 
    
    #increase layout counter 
    if(pdf.layout.multiple == T) {
      pdf.layout.counter <<- pdf.layout.counter + 1
      debug.msg(paste("PDF Counter:", pdf.layout.counter, sep = " "))
    }
  }
}

#create new empty pdf file
pdf.open = function(main) {
  
  pdf.filename.main <<- main
  
  pdf.filename = paste(pdf.filename.section, pdf.filename.index, pdf.filename.main, sep="_")
  pdf(paste(pdf.path, pdf.filename, ".pdf", sep = ""), width = 11.69, height = 8.27)
  
  debug.msg(paste(pdf.path, pdf.filename, ".pdf", sep = ""))
}

#method to close device and safe pdf to file
pdf.write = function() {
  #write plot to pdf?
  if(pdf.create == T && (pdf.layout.multiple == F || pdf.layout.plotcount == pdf.layout.counter)) {
    dev.off()
  }
}

#method to begin a new section
section.new = function(section = "1.1") {
  par.reset()
  #set section for pdf filename
  pdf.filename.section <<- section
  #reset filename index to 1
  pdf.filename.index <<- 1
}

# --------------------------------------------------------------------------------------------------------
#Custom functions with regard to content

# colors for the functions
colors = c('#1abc9c', '#FFD700', '#3498db', '#9b59b6', '#34495e', '#16a085', '#27ae60', '#2980b9', '#8e44ad', '#2c3e50', '#f1c40f', '#e67e22', '#e74c3c', '#95a5a6', '#f39c12', '#d35400', '#c0392b', '#bdc3c7', '#7f8c8d', '#666666', '#FF0000', '#00FF00', '#0000FF', '#FFFF00')


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
  #reset window settings
  pdf.init(main = m)
  #use full window for legend
  par(xpd=TRUE)
  oma = c(4,4,6,6)
  #different margins in case of legend
  if(legend.title != "no") {
    oma = c(4,4,6,18)
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
  
  pdf.write()
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


#custom function for scatterplot3d including legend on the bottom
scatterplot3d.custom <- function(x, y, z, angle, main, xlab, ylab, zlab, col, legend.text, legend.col, legend.title) {
  require(scatterplot3d)
  #reset window settings
  pdf.init(main = main)
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
  
  pdf.write()
}


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



#method for different normality test
#method for displaying qqplot, histogram and p-value of Shapiro-Wilk-Test of feature given as parameter
normal.custom <- function(x, title, col=colors[1], round=6) {
  #reset window settings
  pdf.init(main = title)
  
  #draw qqplot
  qqnorm(x, main = title,pch=19,
         cex.lab=1,cex.main=1,ylab="Sample Quantiles", col=col)
  #insert "optimal" line
  qqline(x,lwd=2,col="red")
  #view histogram and p-value of SW-Test
  hist(x, main = paste("SW-Test: ", round(shapiro.test(x)$p.value, round)), col="cyan", xlab="")
  
  pdf.write()
}


#custom function for display x^2 plot for testing multivariate normal distribution
require(MASS)
normal_multi.custom <- function(data, main, outl = FALSE, col=colors[1]) {
  #reset window settings
  pdf.init(main = main)
  
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
  
  pdf.write()
}



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

#function for displaying qqplot and SW pvalue before and after boxcox transformation
boxcox_view.custom <- function(before_data, after_data, title, col=colors[1], round=6) {
  #reset window settings
  pdf.init(main = title)
  
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
  
  pdf.write()
}

#custom function to plot dotplots of the different feature sets
dotplot.custom <- function(x, title, highlight = "no") {
  #reset window settings
  pdf.init(main = title)
  
  #dotplot
  stripchart(x,method="stack",pch=1,main=title)
  
  #hightlight outliers if highlight != "no"
  if(highlight != "no") {
    stripchart(highlight, col="red", cex=3, add=TRUE, pch=1)  
  }
  
  pdf.write()
}


#custom function for plotting outliers in scatterplots pairs
pairs_out.custom <- function(x, m, outl=NULL, color=colors[1]) {
  #reset window settings
  pdf.init(main = m)
  
  #plot the scatterplots
  pairs(x, panel = function (x, y, ...) {
    points(x, y, ...)
    points(x[outl], y[outl], cex=2, col="red", pch=1)
    abline(lm(y ~ x), col = "blue") 
    #include correlation coefficients in upper panel and histograms on diagonal
  }, pch=19, upper.panel=panel.cor2, diag.panel=panel.hist2, main=m, col=color)
  
  pdf.write()
}


#custom function for display x^2 plot for testing multivariate normal distribution
require(MASS)
outlier_multi.custom <- function(data, main, num_outl = 3, col=colors[1]) {
  #reset window settings
  pdf.init(main = main)
  
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
  
  #in case of outliers mark them
  if(num_outl > 0) {
    out =  which(rank(abs(sd), ties= "random") > nrow(data)-num_outl)
    print(which(attributes(data)$row.names %in% names(out)))
    #mark outliers
    text(qc[out], sd[out]-1.5, 
         which(attributes(data)$row.names %in% names(out)),cex=0.5,col="blue", pos=1)
    points(qc[out], sd[out], col="red", pch=1, cex=2)
  }
  
  pdf.write()
}


#calculate generalized suared distances
distances.custom <- function(data) {
  cm = colMeans(data)
  S = cov(data)
  #calculating distances in the multidimensional room
  dis = apply(data, 1, function(x) t(x-cm) %*% ginv(S) %*% (x-cm))
}

# ---------------------------------------------------------------------------------------------------------
#Custom functions mainly for Part02

#function to draw a scatterplot without regression lines, because 
#correlation of PCs is 0 from definition
pairs_noreg.custom <- function(x, m, color=colors[1], legend.title="no", legend.text=NULL, legend.col=NULL, 
                               pch=19, legend.pch = 19) {
  #reset window settings
  pdf.init(main = m)
  
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
  }, pch=pch, diag.panel=panel.hist2, main=m, col=color, oma=oma)
  #put legend
  #only if legend required
  if(legend.title != "no") {
    legend("right",legend=legend.text, col=legend.col, pch=legend.pch, title=legend.title, cex=0.8)
  }
  
  pdf.write()
}


#TODO include expected WSS as in tutorial of 21.01.2016

#custom function for calculating the within-group sum of squares (WSS)
#for range of number of clusters
#if required add expected wss of uniform data (only in case data is standardized)
kmeans_wss.custom <- function(data, max_clust, main="Kmeans WSS") {
  #reset window settings
  pdf.init(main = main)
  
  n = nrow(data)
  #variables for wss
  wss = rep(0,max_clust)
  #compute wss for one cluster
  wss[1] = (n - 1) * sum(apply(data,2,  var))
  #compute WSS for different number of clusters
  for(i in 2:max_clust) {
    #compute wss and use certain number of rep for centers
    wss[i] = sum(kmeans(data, centers=i, nstart=25)$withinss)
  }
  #screeplot for different WSS amounts
  #for deciding on number of clusters which is optimal
  #normal scale and log scale
  layout(matrix(1:2, ncol=2))
  plot(wss, type="b", main=paste("WSS of kmeans for ", main), xlab="Num of clusters", ylab="WSS")
  plot(log(wss), type="b", main=paste("log(WSS) of kmeans for ", main), xlab="Num of clusters", ylab="log(WSS)")
  
  pdf.write()
}


#custom function for creating dendrograms with agglomerative clustering to the
#methods defind in the parameter settings
aggl_dend.custom <- function(data, methods=c("single", "complete", "average", "centroid", "ward.D", "ward.D2")) {
  #reset window settings
  pdf.init(main = "Dendrograms")
  
  #set margin settings and layout
  par(mar = c(0, 5, 4, 2) + 0.1)
  layout(matrix(1:6, ncol=3, byrow=TRUE))
  #create dendrograms applying different methods
  sapply(methods,
         function(x) plot(hclust(dist(data), x),
                          hang = -1, 
                          labels = FALSE, 
                          las = 1,
                          xlab = "", 
                          sub = "",
                          main = paste("Cluster Dendrogram(", x,
                                       " linkage)"),
                          cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1))
  
  pdf.write()
}

#save functions in file for loading them in Part01 and Part02
save(list = ls(all=TRUE), file="../3-customFunctions.RData")
