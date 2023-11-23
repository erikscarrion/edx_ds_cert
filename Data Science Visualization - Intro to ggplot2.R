# ggplot tutorials
library(tidyverse)
data("midwest", package = "ggplot2")
theme_set(theme_bw())

str(midwest)
summary(midwest)

gg <- ggplot(midwest, aes(area, poptotal/(10^6))) +
  geom_point(aes(col = state, size = popdensity)) + 
  geom_smooth(method = "loess", se = F) +
  xlim(c(0,0.1)) + ylim(c(0,.5)) +
  labs(title="Area Vs Population", y="Population (millions)", x="Area", caption="Source: midwest")
gg


gg + theme(plot.title = element_text(size = 20,
                                     face = "bold",
                                     family = "American Typewriter",
                                     color = "tomato",
                                     hjust = 0.5,
                                     lineheight = 1.2),
           plot.subtitle = element_text(size = 15,
                                        family = "American Typewriter",
                                        face = "bold",
                                        hjust = 0.5),
           plot.caption = element_text(size = 15),
           axis.title.x = element_text(vjust = 1,
                                       size = 15),
           axis.title.y = element_text(size = 15),
           axis.text.x = element_text(size = 10, 
                                      angle = 45, 
                                      vjust = 0.5),
           axis.text.y = element_text(size = 10)
           ) +
  labs(size = "Density", color = "State")

# Making Chnages to the Legend

# Change the position of the labels using
# guides()

gg + guides(colour = guide_legend(order=1),
            size = guide_legend(order=2))

# Change order and color of labels
gg + guides(colour = guide_legend(order=1),
            size = guide_legend(order=2)) +
  scale_color_manual(
    # Set the name of the legend
    name="State",
    # Use the full state name
    labels = c("Illinois", "Indiana",
               "Michigan", "Ohio", "Wisconsin"),
    # vector of "label"="color" pairs
    values = c("IL"="#DC3220",
               "IN"="#FFC20A",
               "MI"="#0C7BDC",
               "OH"="#D35FB7",
               "WI"="#FEFE62"))
# Style the legend title, text, and key
# the legend's key is a figure, so it's set with element_rect

gg + theme(legend.key = element_rect(fill = "#40b0a6"),
           legend.title = element_text(family = "American Typewriter",
                                       size = 10),
           legend.text = element_text(family = "American Typewriter",
                                      face = "bold",
                                      size = 5))
# Change the position of the legend
# the position of the legend can be adjusted via theme
# if you want to put the legend inside the plot, you can 
# adjust the hinge point using legend.justification

# No Legend
gg + theme(legend.position = "None") + labs(subtitle = "No Legend")

# Left 
gg + theme(legend.position = "left") + labs(subtitle = "Left Side")

# Bottom horizontal
gg + theme(legend.position = "bottom",
           legend.box = "horizontal") + labs(subtitle = "Legend at bottom")

# legend bottom right, inside plot
gg + theme(legend.title = element_text(size=12, color = "salmon", face="bold"),
           legend.justification = c(1,0),
           legend.position = c(0.95, 0.05),
           legend.background = element_blank(),
           legend.key = element_blank()) +
  labs(subtitle = "Legend inside plot at bottom right")


# legend top left inside the plot
gg + theme(legend.title = element_text(size=8, color = "salmon", face="bold"),
           legend.justification = c(0,1),
           legend.position = c(0,1),
           legend.key.height = unit(5,"points"),
           legend.background = element_blank(),
           legend.key = element_blank()) +
  labs(subtitle = "Legend inside plot at bottom right")

# Add labels for the counties with populations greater than 400K
# First subset the dataframe

midwest400 <- midwest %>% filter(poptotal >= 300000)
midwest400$lrgcounty = ifelse(midwest400$poptotal>=300000, midwest400$county,"")

gg + geom_text(data=midwest400, aes(label = lrgcounty), 
               size = 4,
               alpha = 0.2) + 
  labs(subtitle="With ggplot2::geom_text") + 
  theme(legend.position = "None") 

# plot text and labels that repel
library("ggrepel")

gg + geom_text_repel(data = midwest400, aes(label = lrgcounty), size = 3, alpha = 0.2)
gg + geom_label_repel(data = midwest400, aes(label = lrgcounty), size = 3, alpha = 0.2)


# Adding Annotations anywhere within the plot
# using the grid package and annotation_custom()
library(grid)

# grid requires a blob
my_text <-"this text is at x = 0.7, y = .8"
my_grob <- grid.text(my_text, x =.7, y = .8,
                     gp = gpar(col="firebrick",
                               fontsize = 10, 
                               fontface = "bold"))
gg + annotation_custom(my_grob) + theme(legend.position = "none")
