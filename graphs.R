#+ license, echo=FALSE
# This script plots NHL fight statistics scraped from http://dropyourgloves.com/
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
img <- readPNG("hockey_lego.png")
g <- rasterGrob(img, interpolate = TRUE)

#+ load.data, echo=FALSE
fightDB <- fread("fightDB.csv")

#+ plot1, echo=FALSE
ggplot(fightDB) +
  theme_minimal(base_size = 18) +
  theme(legend.position=c(0.2, 0.9)) + 
  labs(color = NULL,
       x = "Year",
       y = "Number of fights per game") + 
  geom_line(data = filter(fightDB, !is.na(pre.perG)),
            aes(x = year,
                y = pre.perG, 
                color = "1 - Preseason"), 
            size = 1) +
  geom_line(data = filter(fightDB, !is.na(reg.perG)),
            aes(x = year,
                y = reg.perG, 
                color = "2 - Regular"), 
            size = 1) +
  geom_line(data = filter(fightDB, !is.na(playoffs.perG)),
            aes(x = year,
                y = playoffs.perG, 
                color = "3 - Playoffs"), 
            size = 1) +
  annotation_custom(g, 1920, 1970, 0.2, 2.3)

#+ plot2, echo=FALSE
ggplot(fightDB) +
  theme_minimal(base_size = 18) +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1)) + 
  labs(color = NULL,
       x = "Season",
       y = "Number of fights") + 
  geom_line(data = filter(fightDB, !is.na(pre)),
            aes(x = year,
                y = pre, 
                color = "1 - Preseason"), 
            size = 1) +
  geom_line(data = filter(fightDB, !is.na(reg)),
            aes(x = year,
                y = reg, 
                color = "2 - Regular season"), 
            size = 1) +
  geom_line(data = filter(fightDB, !is.na(playoffs)),
            aes(x = year,
                y = playoffs, 
                color = "3 - Playoffs"), 
            size = 1) +
  annotation_custom(g, 1900, 1970, 0, 900)



