#+ license, echo=FALSE
# This script scrapes NHL fight statistics from http://dropyourgloves.com/
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
require("XML")
require("data.table")
require("dplyr")

#+ prepareDB, echo=FALSE
fightDB <- data.table(year = 1918:2014) %.%
  mutate(ID = 1:length(year),
         URLs = paste0("http://dropyourgloves.com/Fights/LeagueSeason.aspx?League=1&Season=", year))

#+ loadHTML, echo=FALSE
HTMLs <- lapply(fightDB$URLs, function(x) htmlParse(x))

#+ scrape, echo=FALSE
fightDB <- fightDB %.%
  group_by(ID) %.%
  mutate(total = as.numeric(gsub("[^0-9]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Total')]", xmlValue))),
         pre = as.numeric(gsub("[^0-9]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Preseason')]", xmlValue))),
         pre.perG = as.numeric(gsub("[^0-9.]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Preseason')]/following::text()[contains(., 'per game')]", xmlValue)[1])),
         reg = as.numeric(gsub("[^0-9]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Regular')]", xmlValue))),
         reg.perG = as.numeric(gsub("[^0-9.]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Regular')]/following::text()[contains(., 'per game')]", xmlValue)[1])),
         playoffs = as.numeric(gsub("[^0-9]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Playoffs')]", xmlValue))),
         playoffs.perG = as.numeric(gsub("[^0-9.]", "", xpathSApply(HTMLs[[ID]], "//td/descendant::text()[contains(., 'Playoffs')]/following::text()[contains(., 'per game')]", xmlValue)[1])))

#+ save, echo=FALSE
write.csv(fightDB, "fightDB.csv")
