# install.packages("palmerpenguins")
# install.packages('tidyverse')
library(ggplot2)
library(dplyr)
library(palmerpenguins)

# aesthetics: x, y, alpha, color, fill, shape, size, etc.
p <- ggplot(data = penguins,
            aes(x = bill_length_mm, y = bill_depth_mm))

p

# geom layer
p <- p + geom_point(aes(color = as.factor(species), # species 별 색 지정
                        size = body_mass_g, # body_mass_g별 점 크기 지정
                        alpha = 0.7))
p

# scale: change Label, Breaks, Limits, etc.
p + scale_y_continuous(
  "Bill depth (mm)",
  breaks = seq(0, 30, 1)
)

# color
p <- p +
  scale_color_brewer(palette = "Set1",
                     labels = c('myAdele', "myChinstrap", "myGentoo"))
p

# comfirm your setting
# p <- p +
#   scale_alpha_identity() +
#   scale_size_identity() # 정확한 값으로 인식된다.
# p

# legend
my_species <- guide_legend(title = "Species", ncol=3)
p <- p + 
  guides(color = my_species) +
  theme(legend.position = "bottom")
p

# facets
p <- p + facet_wrap(vars(island))
p

# title, subtitle, and captions
p <- p + labs(title = "Visualization of palmer penguins",
              subtitle = "Bill length vs depth by species",
              x = 'bill length', y = 'bill depth',
              caption = "http://theissaclee.com")
p

# ggrepel package
# install.packages('ggrepel')
library(ggrepel)

mypoint <- penguins %>% 
  filter(bill_depth_mm > 20, bill_length_mm > 40)

p <- p + 
  geom_label_repel(
    data = mypoint,
    aes(x = bill_length_mm,
        y = bill_depth_mm,
        label = paste("(",bill_length_mm, ", ", bill_depth_mm,")")),
    color = "black",
    size = 4)
p

# aspect ratio
tibble(x = 1:10, y = 2*x) %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_line() + coord_fixed()

# boxplot
ggplot(penguins, aes(x = species, y = body_mass_g)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = .2)

ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g)) + 
  geom_boxplot(aes(group = cut_width(bill_length_mm, 5)))
