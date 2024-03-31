
library(tidyverse)
library(ggplot2)
library(extrafont)
library(sysfonts)
library(showtextdb)
library(showtext)
library(ggimage)
library(ggtext)
library(cowplot)
library(jpeg)

# Name of the fonts we need
font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
font_add_google(family=font, font, db_cache = FALSE)
font_add_google(family=font2, font2, db_cache = FALSE)

theme_set(theme_minimal(base_family = font2, base_size = 3))

bg <- "white"
txt_col <- "black"

font_add(family = "xfighters", "XFightersRegular-3dlz.ttf")
font_add(family = "cartoon", "CartoonCharacter-aWvx.ttf")
showtext_auto(enable = TRUE)



mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')

mutant_moneyball %>% select('Member','TotalIssues')


# Calculate percentage of total issues
mutant_moneyball$Percent <- mutant_moneyball$Total / sum(mutant_moneyball$Total) * 100


devtools::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)
fishualize()
fish_palettes()

install.packages("devtools")
library(jcolors)

# Plot the pie chart
p <- ggplot(mutant_moneyball03, aes(x = "", y = Percent, fill = Member)) +
  geom_bar(stat = "identity", width = -0.5) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Member) +
  theme_void() +
  labs(title = "Issue Frequency of Member appearance between 1963 and 1992.")+
  theme(legend.position = "none",
        plot.title =  element_markdown(family = "cartoon", size = 15, face = "bold"))+
  labs(fill = "Member")+
  scale_fill_fish_d(option = "Cirrhilabrus_tonozukai") +
  theme(legend.position = "none")
p 


xmen <- data.frame(c =(1),d=(1),
  image = c("https://www.pngall.com/wp-content/uploads/5/X-Men-PNG-Transparent-HD-Photo.png"))
x <- ggplot(xmen,aes(x=c,y=d))+
  theme_void() +
  geom_image(aes(image = image),size =0.3, asp = 2)
x



plot_grid(x, p,align="hv", rel_heights = c(2,5),rel_widths = c(2,2)) +
  theme(plot.background = element_rect(fill = "lightblue1"))




ggsave("2024week13TidyTuesday.png", width = 10, height = 10, units = "cm")


  

