### 1/21/2019####

##2/2/2019###

##Came back to run again to remove Lawn requring watering and removing underscores from variables.###

###Correlation for CH.2 Round 3#####

setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.2")
mastrdta = read.csv("CH2_Master_subset_analysis_current3.csv")
library(dplyr)




##Now that the data has been subsetted. Rename variables os they look better in the corrplot##

dta$PNW_climate_attitudes = dta$PNWClimateIndex              
dta$Pro_water_conservation = dta$pro_water_con4             
dta$Native_Plants = dta$Lawn.Native
dta$Drought_resistant_plants = dta$Lawn.Drought
dta$Water_saving_devices = dta$Water.Conserv.Device
dta$Water_efficient_appliances = dta$Water.Conserv.App
dta$Lawn_watering_frequency = dta$Water.Lawn
dta$Water_savings = dta$water_savings2
dta$Property_size = dta$sqm_prop
#dta$Lawn_requires_watering = dta$Water.Veg
dta$Age = dta$Age1
dta$Education = dta$Education1
dta$Political_Attitudes = dta$Political.Attitudes1
dta$Annual_Income = dta$Income

##Interesting. Can't have spaces between variables names. Good to know###

dta2 = dta[,c(96:108)]

##gsub worked well at removing underscores. Could use this for removing periods too.##

colnames(dta2) = gsub('_', ' ',colnames(dta2))
?gsub
###Make correlation matrix################
cordta<-dta2
cor1 <- cor(cordta, use="pairwise.complete.obs", method="pearson")


require(corrplot)
corrplot(cor1)
#Make correlation matrix
library(Hmisc)


rcorr(as.matrix(cor1))
correldata = cor1
res2<-rcorr(as.matrix(correldata))

res = cor(correldata,use="pairwise.complete.obs", method="pearson")

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)

##Make a corrplot#


corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

###Making a custom corplot 1/21/2019##

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(cordta)
head(p.mat[, 1:5])

M<-cor(cordta)
M <- cor(cordta, use="pairwise.complete.obs", method="pearson")

# Specialized the insignificant value according to the significant level
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col="black",tl.cex = 0.8, tl.srt=45,number.cex = 0.6, #Text label color and rotation
                   # Combine with significance
                   p.mat = p.mat, sig.level = 0.01, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE 
)

##I think the corrplot looks pretty good. Just take time to figure out how to 
###explain the relationships.

### Removing underscores ####

M = gsub('_', ' ',colnames(M))
