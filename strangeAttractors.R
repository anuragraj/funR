#Strange Attractors
#The code is adapted & inspired from @aschinchon and @will-r-chase
#https://www.williamrchase.com/post/strange-attractors-12-months-of-art-february/
#Anurag Raj 
#12-March-2021

#packages to generate attractors
library(ggplot2)
library(dplyr)

#a hopalong attractor

#ggplot theme blank canvas
opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())


#attractor function
createTrajectory <- function(n, x0, y0, a, b, c) {
  #pre-initialize vectors of length n
  x <- vector(mode = "numeric", length = n)
  y <- vector(mode = "numeric", length = n)
  
  #starting values
  x[1] <- x0
  y[1] <- y0
  
  #fill vectors with values
  for(i in 2:n) {
    x[i] <- y[i-1]-1-sqrt(abs(b*x[i-1]-c))*sign(x[i-1]-1)
    y[i] <- a-x[i-1]-1
  }
  
  #make dataframe
  data.frame(x = x, y = y)
}

#constants
#these parameters can be changed for experiment
a=2
b=1
c=8
v=3

#calculate positions and plot
df=createTrajectory(3000000, 0, 0, a, b, c)
plot <- ggplot(df, aes(x, y)) + geom_point(color="#1E1E1E", shape=46, alpha=.05) + opt

#saving the plot
ggsave("strangeArt.png", plot, height = 4, width = 4, units = 'in', dpi = 600)

#coloring the image 

library(EBImage)
library(tidyverse)
library(viridis)

#read in the image and convert to greyscale
img <- readImage("C:/Users/anura/Desktop/strange2.png")
gray <- channel(img, "gray")

#map the color palette to the image
img_col <- colormap(gray, viridis::magma(256))
display(img_col, method = "raster")

#when you are satisfied with the image, save it
writeImage(img_col, "C:/Users/anura/Desktop/strange_color2.png")