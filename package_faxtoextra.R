##########################################################################################
### Package: Factoextra 
##########################################################################################

##########################################################################################
## easy extraction and visualization of the output of multivariate data analyses
#
# 1.) Principal Component Analysis (PCA)
#     --> summarizes the information contained in a multivariate dataset reducing the dimensionality of the data without loosing important information.
# 2.) Correspondence Analysis (CA)
#     --> extension of the PCA suited to analyse a large contingency table formed by two qualitative variables.
# 3.) Multiple Correspondence Analysis (MCA)
#     --> adaptation of CA to a data table containing more than two categorical variables.
# 4.) Multiple Factor Analysis (MFA)
#     --> dedicated to datasets where variables are organized into groups (qualitative/quantitative).
# 5.) Hierarchical Multiple Factor Analysis (HMFA)
#     --> extension of MFA in a situation where the data are organized into a hierarchical structure.
# 6.) Factor Analysis of Mixed Data (FAMD)
#     --> a particular case of the MFA, dedicated to analyze a data set containing both quantitative and qualitative variables.
#########################################################################################

#load needed packages
#library(FactoMineR)
library(factoextra)
library(ggplot2)

#######################################################################################
# 1.) Principal component analysis (PCA)
#     summarizes the information contained in a multivariate dataset,
#     reducing the dimensionality of the data without loosing important information.
#######################################################################################
#Loading data
data("decathlon2")
df <- decathlon2[1:23, 1:10]  #storing only some disciplines and athletes in a dataframe

res.pca <- PCA(df,  graph = FALSE)

#Extract eigenvalues/variances
get_eig(res.pca)

#Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Extract the results for variables
var <- get_pca_var(res.pca)
var

# Coordinates of variables
head(var$coord)

# Contribution of variables
head(var$contrib)

# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)

# Compute PCA on the iris data set
# The variable Species (index = 5) is removed
# before PCA analysis
iris.pca <- PCA(iris[,-5], graph = FALSE)
# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


#######################################################################################
# 2.) Correspondence analysis (CA)
#     extension of the PCA,
#     suited to analyse a large contingency table formed by two qualitative variables.
#######################################################################################
library("FactoMineR")

# Loading data
data("housetasks")

# Computing CA
res.ca <- CA(housetasks, graph = FALSE)

#---------------------------------------------------
# Result for row variables
get_ca_row(res.ca)
# Result for column variables
get_ca_col(res.ca)

#Biplot of rows and columns
fviz_ca_biplot(res.ca, repel = TRUE)

#--------------------------------------------------
# Graph of row points
fviz_ca_row(res.ca, repel = TRUE)
# Graph of column points
fviz_ca_col(res.ca)
# Visualize row contributions on axes 1
fviz_contrib(res.ca, choice ="row", axes = 1)
# Visualize column contributions on axes 1
fviz_contrib(res.ca, choice ="col", axes = 1)

#######################################################################################
# 3.) Multiple correspondence analysis (MCA)
#     adaptation of CA to a data table containing more than two categorical variables.
#######################################################################################
#library(FactoMineR)

#Computing MCA:
data(poison)
res.mca <- MCA(poison, quanti.sup = 1:2,
               quali.sup = 3:4, graph=FALSE)

#-------------------------------------------------
# Extract the results for variable categories
get_mca_var(res.mca)
# Extract the results for individuals
get_mca_ind(res.mca)

#-------------------------------------------------
# Visualize variable categorie contributions on axes 1
fviz_contrib(res.mca, choice ="var", axes = 1)
# Visualize individual contributions on axes 1
# select the top 20
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)

#------------------------------------------------
# Color by groups
# Add concentration ellipses
# Use repel = TRUE to avoid overplotting
grp <- as.factor(poison[, "Vomiting"])
fviz_mca_ind(res.mca,  habillage = grp,
             addEllipses = TRUE, repel = TRUE)

#Graph of variable categories:
fviz_mca_var(res.mca, repel = TRUE)

#-----------------------------------------------
#Biplot of individuals and variables:
fviz_mca_biplot(res.mca, repel = TRUE)


#--------------------------------------------------------------------------------------------------
data(multishapes)
plot(multishapes[,1], multishapes[, 2],col = multishapes[, 3], pch = 19, cex = 0.8)
