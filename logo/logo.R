# Installation
# install.packages('rgeos')
# install.packages('ggvoronoi')
# devtools::install_github("nschiett/fishualize", force = TRUE)
# install.packages("harrypotter")

library(magick)
library(bunny)
library(ggplot2)
library(magrittr)
library(ggvoronoi)
library(fishualize)
library(harrypotter)

# Draw voronoi graph with ggvoronoi and fishualize
set.seed(7) # 45056
x <- sample(1:200,100)
y <- sample(1:200,100)
points <- data.frame(x, y,
                     distance = sqrt((x-100)^2 + (y-100)^2),
                     group = runif(100) %>% as.character())

circle <- data.frame(x = 100*(1+cos(seq(0, 2*pi, length.out = 2500))),
                     y = 100*(1+sin(seq(0, 2*pi, length.out = 2500))),
                     group = rep(1,2500))

fig.voronoi <- image_graph(width = 3000, height = 3000, res = 96)
ggplot(points) +
  geom_voronoi(aes(x=x,y=y,fill=group)) + 
  scale_fill_fish(option = "Coris_gaimard", discrete = TRUE) +
  theme(legend.position = "none") + 
  theme_void()
dev.off()

# Area plot
sales <- data.frame(x=1:20, y=(1:20 *  runif(20, .60, 1)) , type='a')

fig.area <- image_graph(width = 3000, height = 3000, res = 96, bg = 'transparent')
ggplot(sales, aes(x=x,y=y, fill = type)) +
  geom_area(position ="identity", linetype = 1, size = 20, colour = border.color, 
            fill = harrypotter(option = "DracoMalfoy", n =7)[6]) + 
  geom_point(colour = harrypotter(option = "DracoMalfoy", n =7)[2], size = 30) +
  ylim(0, 20) +
  theme(legend.position = "none") + 
  theme_void() 

dev.off()

fig.area2 <- fig.area %>% 
  image_crop('2000x2000+500+500')

fig.voronoi <- fig.voronoi %>% 
  image_crop('2000x2000+300+300')

border.color <- "#0d4448"
border.color <- "#283F99FF"

hex_canvas <- image_canvas_hex(border_color = border.color, 
                               border_size = 10, fill_color = "#ede6f2")
hex_border <- image_canvas_hexborder(border_color = border.color, 
                                     border_size = 10)

font <- 'Chilanka'

img_hex <- hex_canvas %>% 
  image_composite(fig.voronoi , offset = '+100+100') %>% 
  image_compose(fig.area2, gravity = 'west') %>% 
  image_annotate("awesome", size=200, gravity = "center", font = font, 
                 color = harrypotter(option = "RonWeasley", n =7)[1], degrees = -30, location  = '+310+650') %>% 
  image_annotate("R dataviz", size=200, gravity = "center", font = font, 
                 color = harrypotter(option = "RonWeasley", n =7)[1], degrees = -90, location  = '+700-50') %>% 
                 # color = harrypotter(option = "RonWeasley", n =7)[1], degrees = -90, location  = '+700-50') %>% 
    # image_annotate("dataviz", size=450, gravity = "center", font = "Aller", color = harrypotter(option = "RonWeasley", n =7)[1]) %>% 
  bunny::image_compose(hex_border, gravity = "center", operator = "Over")

  img_hex

img_hex %>%
  image_scale("200x200") %>%
  image_write(here::here("logo", "logo.png"), density = 600)