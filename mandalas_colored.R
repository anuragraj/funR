#Code Adapted form the @aschinchon (Antonio Sánchez Chinchón)
#This is script for generation of Colored Mandala Art. This is based on Voronoi tesselations.
#Find more details at funR[https://github.com/anuragraj/funR]
#AnuragRaj-04/03/2021

# Load in libraries required for the image generation
library(ggplot2)
library(dplyr)
library(deldir)
library(colourlovers)
library(rlist)

# setting up the Parameters
iter=3 # Number of iterations (depth)
points=20 # Number of points
radius=1.15 # Factor of expansion/compression

# Angles of points from center
angles=seq(0, 2*pi*(1-1/points), length.out = points)+pi/2

# Initial center of Mandala
df=data.frame(x=0, y=0)

# Iterate over centers again and again to generate 
for (k in 1:iter)
{
  temp=data.frame()
  for (i in 1:nrow(df))
  {
    data.frame(x=df[i,"x"]+radius^(k-1)*cos(angles), 
               y=df[i,"y"]+radius^(k-1)*sin(angles)) %>% rbind(temp) -> temp
  }
  df=temp
}

# Function to extract id, coordinates and area of each polygon
crea = function(tile) {tile %>% list.match("ptNum|x|y|area") %>% as.data.frame()}

# Generate tesselation, obtain polygons and create a dataframe with results
# This dataframe will be the input of ggplot
df %>% 
  deldir(sort = TRUE)  %>% 
  tile.list() %>% 
  list.filter(sum(bp)==0) %>% 
  list.filter(length(intersect(which(x==0), which(y==0)))==0) %>% 
  lapply(crea) %>% 
  list.rbind() ->  df_polygon

# Pick a random top palette from colourLovers
#you can explore more palette with "top", "new" and "random"
palette <- sample(clpalettes('top'), 1)[[1]] %>% swatch %>% .[[1]]

# Draw mandala with geom_polygon. Colur depends on area
#explore more with changing size nad linetypes
ggplot(df_polygon, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = ptNum), 
               show.legend = FALSE, size=0.2, linetype=1)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void()

# Do you like the result? You can save it (change the filename):
ggsave("mandalaArt.png", height=5, width=5, units='in', dpi=600)
