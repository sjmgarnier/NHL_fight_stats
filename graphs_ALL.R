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
require("ggthemes")
library("png")
library("grid")

#+ load.img, echo=FALSE
img <- readPNG("fight.png")
g <- rasterGrob(img, interpolate = TRUE)

#+ load.data, echo=FALSE
fightDB <- fread("fightDB_ALL.csv")

#+ plot1, echo=FALSE
fightDB.top10 <- subset(fightDB, 
                        name == "AHL" | name == "QMJHL" | name == "NLA" | 
                          name == "OHL" | name == "SkEL" | name == "CzEL"| 
                          name == "FEL" | name == "SEL" | name == "KHL" | 
                          name == "NHL")
fightDB.top10$name <- factor(fightDB.top10$name, levels = c("NHL", "KHL", "SEL", 
                                                            "FEL", "CzEL", "SkEL", 
                                                            "OHL", "NLA", "QMJHL",
                                                            "AHL"))
ggplot(fightDB.top10,
       aes(x = season,
           y = ratio,
           color = name)) +
  theme_minimal(base_size = 18) +
  xlim(1980, 2013) +
  xlab("Season") +
  ylab("Fights/Game") +
  ggtitle("Violence in Top 10 hockey leagues") +
  guides(color = guide_legend(title = "League")) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_line(size = 1.5)
  
#+ plot2, echo=FALSE
tmp <- fightDB %.%
  filter(season >= 2004) %.%
  group_by(name) %.%
  summarize(mean = mean(ratio)) %.%
  mutate(name = factor(name, levels = name[order(mean)]))

ggplot(tmp,
       aes(x = name,
           y = mean,
           fill = name)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(vjust = 0),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold", vjust = 2)) +
  coord_flip() +
  xlab("League") +
  ylab("Fights/Game") +
  guides(fill = FALSE) +
  ggtitle("Average violence\nin various hockey leagues (2004-2014)") +
  geom_bar(stat = "identity", color = "white") +
  annotation_custom(g, -10, 40, 0, 5) +
  annotate("text", x = 0, y = 5.25, label = "Data source: http://dropyourgloves.com/", angle = 90, size = 5, hjust = 0)
  
  




