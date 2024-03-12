install.packages("jpeg")
install.packages("echarts4r")
install.packages("grid")
install.packages("ggimage")
install.packages("gridGraphics")
install.packages("showtext")
install.packages("extrafont")
install.packages("ggtext")

# loading the required libraries
library(showtext)
library(extrafont)
library(ggtext)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library("jpeg") 
library(grid)
library(gridGraphics)
library(ggimage)
 

font_add(family = "super", "SuperRugged-4nBy9.ttf")
font_add(family = "whiteTrashReg", "CfWhiteTrashRegular-rqoO.ttf")
# allows fonts to show up in plots You need to run this every time you add new fonts.
showtext_auto()

#data extraction
trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

#wrangling
trashwhee01 <- filter(trashwheel, Year == 2023, Name == "Mister Trash Wheel") 
trashwhee01 <- subset(trashwhee01, select = -ID)  #remove ID col

plasticBottles_sum <- sum(as.numeric(trashwhee01$PlasticBottles), na.rm = TRUE)
polystyrene_sum <- sum(as.numeric(trashwhee01$Polystyrene), na.rm = TRUE)
cigarettes_sum <- sum(as.numeric(trashwhee01$CigaretteButts), na.rm = TRUE)
glassBottles_sum <- sum(as.numeric(trashwhee01$GlassBottles), na.rm = TRUE)
plasticBags_sum <- sum(as.numeric(trashwhee01$PlasticBags), na.rm = TRUE)
wrappers_sum <- sum(as.numeric(trashwhee01$Wrappers), na.rm = TRUE)
sportsBalls_sum <- sum(as.numeric(trashwhee01$SportsBalls), na.rm = TRUE)
homesPowered_sum <- sum(as.numeric(trashwhee01$HomesPowered), na.rm = TRUE)

total <- plasticBottles_sum + polystyrene_sum + cigarettes_sum + 
  glassBottles_sum +plasticBags_sum +
  wrappers_sum + sportsBalls_sum + homesPowered_sum

ppb <- (plasticBottles_sum/total)*100
ppb <- round(ppb, digits = 2)
 
poly <- polystyrene_sum/total*100
poly <- round(poly, digits = 2)

cig <- cigarettes_sum/total*100
cig <- round(cig, digits = 2)

gb <- glassBottles_sum/total*100
gb <- round(gb, digits = 2)

pbs <- plasticBags_sum/total*100
pbs <- round(pbs, digits = 2)

wrap <- wrappers_sum/total*100
wrap <- round(wrap, digits = 2)

sports <- sportsBalls_sum/total*100
sports <- round(sports, digits = 2)

home <- homesPowered_sum/total*100
home <- round(home, digits = 2)


plasticBottle <- "https://www.amphorea.co.uk/wp-content/uploads/2022/06/60ml-Clear-PET-bottle.jpeg"
polystyrene <- "https://m.media-amazon.com/images/I/61M3YceJ1aL.__AC_SX300_SY300_QL70_ML2_.jpg"
cigarette <- "https://images.rawpixel.com/image_800/czNmcy1wcml2YXRlL3Jhd3BpeGVsX2ltYWdlcy93ZWJzaXRlX2NvbnRlbnQvbHIvZnJjaWdhcmV0dGVfc21va2luZ19zbW9rZV9hc2hfMi1pbWFnZS1reWJlMGh4MC5qcGc.jpg"
glassBottle <- "https://freepngimg.com/thumb/bottle/153618-glass-bottle-png-image-high-quality.png"
plasticbag <- "http://www.acua.com/uploadedImages/Site/Blog/ACUA_Blog/plastic-bag.png"  
wrappers <-  "https://content.govdelivery.com/attachments/fancy_images/UKBMD/2019/05/2598798/sweet-crisp-wrappers-600px_original.jpg" 
sportsballs <-  "https://cdn11.bigcommerce.com/s-4030hufb/images/stencil/1440x1440/products/753/3102/Sports_Balls__28591.1646919399.jpg?c=2" 
homesPowered <- "https://www.pveurope.eu/sites/default/files/styles/teaser_image_full__l/public/ezpublish/10-tips-to-use-the-power-from-your-solar-modules-most-effectively.jpg.webp?itok=ulj87Owo"  
  
trashwheel02 <- data.frame(
  items = c("Plastic Bottles", "Polystyrene", "Cigarette Butts", 
        "Glass Bottles", "Plastic Bags", "Wrappers", 
        "Sports Balls", "Home Powered"), 
  
  sums = c(ppb, poly, cig, gb, pbs,wrap,sports,home),
  
  symbol = c(plasticBottle,polystyrene,cigarette,glassBottle,plasticbag,
             wrappers,sportsballs,homesPowered)
  ) 


ggplot(trashwheel02, aes(x = items ,y = sums)) +
  theme_minimal() +
  theme( legend.position = "none" ) +
  ylim(-5, 45) +
  geom_image(aes(image = symbol), size = 0.1, asp = 2) +
  labs(title = "Percentage % of trash items disposed in <span style ='color:blue'>2023</span> by the Trashwheel",
       caption = "Data from: fiscalsponsordirectory.org\nCreated by sndaba.") +
  theme(plot.title = element_markdown(hjust = 0.5,family = "super", size = 30),
        plot.caption = element_markdown(hjust = 0, size = 20, family = "super"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = 'white', colour = 'white')) +
  annotate(geom = "text", x = 1, y = 35, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "44.22") +
  annotate(geom = "text", x = 2, y = 10, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "0.27") +
  annotate(geom = "text", x = 3, y = 10, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "0.72") +
  annotate(geom = "text", x = 4, y = -2, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "3") +
  annotate(geom = "text", x = 5, y = 20, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "27.8") +
  annotate(geom = "text", x = 6, y = 10, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "3.01") +
  annotate(geom = "text", x = 7, y = -4, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "0.26") +
  annotate(geom = "text", x = 8, y = 15, size = 15, family = "whiteTrashReg", lineheight = .6,
           label = "20.76")

ggsave("2024week10TidyTuesday.png", width = 12, height = 8)
