# 2026-03-31 class script

# the reason for using pca is  because the data has a lot of dimensions - you use 
# pca is one method of data progression
# but compression can be difficult to understand
# go back to video for more info

library(tidyverse)
library(palmerpenguins)

head(penguins)

# pca just uses lines - linear algorithum 
# PCAs - AHHHHH

pen_drop_na = penguins %>%
  drop_na()

summary(pen_drop_na)

# now split dataset into 2 - 

pen_num = pen_drop_na %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
head(pen_num)

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)

# run our PCA 
# prcomp() from stats package
# scale normalizes datasets before feeding it into your pca

pen_pca = prcomp(pen_num, scale. = T, center = T)
class(pen_pca)
str(pen_pca) # str for structure 
summary(pen_pca)
head(pen_pca)
dim(pen_pca$x)
dim(pen_num)

# the stdev is used to calc the proportion of variance in the data set
# is pca right for you? Most people stop after PC1 and PC2 - but if the variation are super low, 
# you may want to consider a different analysis

str(summary(pen_pca))
summary(pen_pca)$importance[2,]
pen_pca$sdev
pen_pca$sdev^ 2 / sum(pen_pca$sdev^2) # prove it 

plot(pen_pca)

# plot a scree plot for our pca

pca_scree = data.frame(pc = seq(1,4),
                        var = pen_pca$sdev^ 2 / sum(pen_pca$sdev^2)  
)
pca_scree
ggplot(data = pca_scree, aes(x = pc, y = var)) +
  geom_bar(stat = "identity") +
  geom_point() +
  geom_line() +
  xlab("Principle Component") +
  ylab("Proportion of variance explained")


pen_pca$rotation
# PC1 has good representation from all variables, PC2 primarily stuck to bill length and bill depth
pen_pca$x
pen_pca_meta = cbind(pen_pca$x, pen_meta)
pen_pca_meta

ggplot(data = pen_pca_meta) +
  geom_point(aes(x = PC1, y = PC2, color = species, shape = sex))
  coord_fixed(ratio = 1)

install.packages("ggbiplot")
library(ggbiplot)

head(pen_pca_meta)
biplot(pen_pca)
ggbiplot(pen_pca, scale = 1, obs.scale = 1, groups = pen_meta$species, ellipse = T, 
          alpha = 0) +
  geom_point(data = pen_pca_meta, aes(x = PC1, y = PC2, color = species, shape = sex)) +
  theme_bw()
# ellipse encapsulating 95% of variability w/in the species

pen_pca$rotation
