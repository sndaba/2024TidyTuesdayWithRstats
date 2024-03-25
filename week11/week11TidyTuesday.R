library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add(family = "super", "SuperRugged-4nBy9.ttf")
showtext_auto()


fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')

# Define the dataframes
fiscal_max <- data.frame(
  top_10_n_sponsored = c(850, 725, 600, 500, 475,350,300,250,230),
  sponsor_max = c("NewHorizonsFoundationInc","SocialGood","TheHackFoundation","HelunaHealth",
                  "PlayersPhilanthropyFund", "NewYorkFoundationfortheArts","TheField","WomenMakeMovies",
                  "TheFilmCollaborative"))

fiscal_min <- data.frame(
  bottom_10_n_sponsored = c(2,3,4,5,6,7,11,10,12),
  sponsor_min =c("AssociationBuildingCommunity","1stNoteMusicFoundation","BEMeditationGroup",
                 "CENTERSantaFe", "AlternativeNewsweeklyFoundation","AccessibleFestivals",
                 "AGodSend","BASICS","50CAN,Inc."))

# Define colors
colors <- c("gold", "salmon", "darkgreen", "orange", "brown", "#5499C7", "darkblue",
            "red",  "purple")

colours2 <- c("maroon", "chocolate", "#2980B9", "lightgreen", "#1F618D", "#CC9966", "black",
              "lightsalmon","pink1")

hsize <- 2 #sieze of hole in the middle of the donut

#Plot the donut chart
a <- ggplot(fiscal_max, aes(x = hsize, y = top_10_n_sponsored, fill = factor(sponsor_max))) +
  geom_bar(stat = "identity",color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme_void() +
  theme(legend.position = "none")+
  geom_text(aes(label = paste(sponsor_max, "\n",top_10_n_sponsored )),
            position = position_stack(vjust = 0.5),color = "white", face = "bold",size = 5) +
  annotate(geom = "text", x = 0.2, y = 0.9, size = 5, color = "white", lineheight = .9,
           label = "Maximum Times Sponsors\n have Donated" , fontface = "bold") +
  scale_fill_manual(values = colors) 


b <- ggplot(fiscal_min, aes(x = hsize, y = bottom_10_n_sponsored, fill = factor(sponsor_min))) +
  geom_bar(stat = "identity",color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme_void() +
  theme(legend.position = "none")+
  geom_text(aes(label = paste(sponsor_min, "\n",bottom_10_n_sponsored )),
            position = position_stack(vjust = 0.5),color = "white", face = "bold",size = 5) +
  annotate(geom = "text", x = 0.2, y = 0.8, size = 5, color = "white", lineheight = .9,
           label = "Minimum Times Sponsors\n have Donated" , fontface = "bold") +
  scale_fill_manual(values = colours2)
b

#combined plots
e <- plot_grid(a,b,ncol = 2,align = 'tb', rel_widths = c(6,6)) +
  labs(title = 'Maximum & Minimum Fiscal Donations by Sponsors\n\n\n',
       caption = 'Data:2024TidyTuesdayWeek11\nCreated By:Simisani Ndaba\n') + 
  theme(panel.background = element_blank(),
        plot.title = element_markdown(hjust = 0.5,family = "super",size=23,color = "white", face = "bold"),
        plot.caption = element_text(hjust = 0.5, size=11,color = "white", face = "bold"),
        plot.background = element_rect(fill = '#330033'))
e


ggsave("2024week11TidyTuesday.png", width = 35, height = 25, units = "cm")  


