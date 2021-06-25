# Insert the 2008-2009 NBA Regular Season data into R and store it as a 
# dataframe named "season0809".
season0809 <- read.csv("2008-2009 NBA Regular Season.csv", na = "")
str(season0809)

# PCA works best with numerical data
# Going to convert "Position" variable into numerical
# Going to drop "Team" variable and "Player" variable 

# Drop "Team" and "Player" variable 
names(season0809)
include_list <- season0809[c("Age", "Pos", "GP", "GM", "Usg", "FG.", "FT.", 
                             "X3P.", "TS.", "Tor", "Min", "M.Diff.", "Pts", 
                             "P.Diff.", "Rbd", "R.Diff.", "Ast", "A.Diff.")]
season0809 <- include_list
str(season0809)

season0809$Age <- as.numeric(season0809$Age)
season0809$GP <- as.numeric(season0809$GP)
season0809$GM <- as.numeric(season0809$GM)
str(season0809)

install.packages("caret")
library(caret)
library(plyr)
library(readr)
library(dplyr)
library(caret)

season0809$Pos



onehotencoding <- dummyVars(" ~ .", data = season0809)
season0809_transformed <- data.frame(predict(onehotencoding, newdata = season0809))

glimpse(season0809_transformed)


str(season0809_transformed)
# All variables are now numeric, ready for PCA 


# Remove NA's and zeros 
season0809_transformed <- na.omit(season0809_transformed)
season0809_transformed[rowSums(season0809_transformed[])>0,]


# PCA
install.packages("factoextra")
library(factoextra)

season0809_transformed.pca <- prcomp(season0809_transformed, scale = TRUE)
names(season0809_transformed.pca)

# PCA Results
season0809_transformed.pca$center
season0809_transformed.pca$rotation
season0809_transformed.pca$sdev
season0809_transformed.pca$scale
season0809_transformed.pca$x


# PCA Results
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(season0809_transformed.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(season0809_transformed.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(season0809_transformed.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# Visualize eigenvalues (CHANGE WORDING OF THESE STATEMENTS)
# Show the percentage of variances explained by each principal component
fviz_eig(season0809_transformed.pca)




fviz_pca_ind(season0809_transformed.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping)
             

fviz_pca_var(season0809_transformed.pca, 
             col.var = "contrib", #Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_pca_biplot(season0809_transformed.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color 
                )

biplot(season0809_transformed.pca, scale = 0)
             
             