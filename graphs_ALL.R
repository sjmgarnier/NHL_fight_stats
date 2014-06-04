#+ license, echo=FALSE
# This script plots hockey fight statistics scraped from 
# http://dropyourgloves.com/ for various hockey leagues. 
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("data.table")
require("dplyr")
require("ggplot2")
library("png")
library("grid")
if (!require("graphZoo")) {
  require("devtools")
  install_github("morpionZ/graphZoo")
  require("graphZoo")
}
require("extrafont")
loadfonts()

#+ load.img, echo=FALSE
img <- readPNG("fight2.png")
img.grob <- rasterGrob(img, interpolate = TRUE)

#+ load.data, echo=FALSE
fightDB <- fread("fightDB_ALL.csv")

#+ plot1, echo=FALSE
fightDB.top10 <- subset(fightDB, 
                        name == "AHL" | name == "QMJHL" | name == "NLA" | 
                          name == "OHL" | name == "SkEL" | name == "CzEL"| 
                          name == "FEL" | name == "SEL" | name == "KHL" | 
                          name == "NHL") %>%
  mutate(name = factor(name, levels = c("NHL", "KHL", "SEL", 
                                        "FEL", "CzEL", "SkEL", 
                                        "OHL", "NLA", "QMJHL",
                                        "AHL")))

g <- ggplot(fightDB.top10,
            aes(x = season,
                y = ratio,
                color = name)) +
  xlim(1980, 2013) +
  xlab("Season") +
  ylab("Fights/Game") +
  ggtitle("Violence in Top 10 hockey leagues") +
  guides(color = guide_legend(title = "League")) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line(size = 1.5) + 
  theme_graphzoo(base_size = 28, family = "Open Sans Condensed Light") 
  
g <- addBanner(g, font.size = 5.83,
               l.txt = "GRAPHZOO.TUMBLR.COM", r.txt = "SOURCE: EIA, FRED")

#+ plot2, echo=FALSE
tmp <- fightDB %.%
  filter(season >= 2004) %.%
  group_by(name) %.%
  summarize(mean = mean(ratio)) %.%
  mutate(name = factor(name, levels = name[order(mean)]))

subtitle <- "(2004 - 2014)"

g <- ggplot(tmp,
       aes(x = name,
           y = mean,
           fill = name)) +
  theme_graphzoo(base_size = 24, family = "Open Sans Condensed Bold")  +
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip() +
  xlab("League") +
  ylab("Fights/Game") +
  guides(fill = FALSE) +
  #ggtitle("Violence in various hockey leagues") +
  geom_bar(stat = "identity", color = "white") +
  annotation_custom(img.grob, -12, 40, 0.25, 5.75) +
  ggtitle(bquote(atop("Violence in various hockey leagues", 
                      atop(italic(.(subtitle)), ""))))

g <- addBanner(g, font.size = 5.83, heights = c(1, 0.05 * 700 / 1200), family = "Open Sans Condensed Bold",
               l.txt = "GRAPHZOO.TUMBLR.COM", r.txt = "SOURCE: DROPYOURGLOVES.COM")

png("ALL_number_of_fights_per_game.png", width = 700, height = 1200, bg = "#F0F0F0")
g
dev.off()





