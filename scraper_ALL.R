#+ license, echo=FALSE
# This script scrapes hockey fight statistics from http://dropyourgloves.com/ 
# for various hockey leagues. 
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
require("stringr")

#+ leagueSelector, echo=FALSE
selector.HTML <- htmlParse("http://dropyourgloves.com/Fights/FightsPerGameChart.aspx")
selector <- data.table(name = xpathSApply(selector.HTML, "//option", xmlValue),
                       ID = as.numeric(xpathSApply(selector.HTML, "//option/@value"))) %.%
  mutate(URLs = paste0("http://dropyourgloves.com/Fights/FightsPerGameChart.aspx?League=", ID))

#+ loadHTML, echo=FALSE
HTMLs <- lapply(selector$URLs, function(x) htmlParse(x))

#+ scrape, echo=FALSE
if (exists("fightDB")) {
  rm(fightDB)
} 
for (i in 1:nrow(selector)) {
  if (exists("fightDB")) {
    script <- xpathSApply(HTMLs[[i]], "//script[@language='JavaScript']", xmlValue)
    if (length(script) > 0) {
      fight.games <- matrix(as.numeric(unlist(strsplit(gsub(" fights in ", " ", unlist(str_extract_all(script, "[0-9]+ fights in [0-9]+"))), "  *"))), ncol = 2, byrow = TRUE)
      tmp <- data.table(name = selector$name[i],
                        ID = selector$ID[i],
                        season = as.numeric(gsub("-[0-9]+", "", unlist(str_extract_all(script, "(\\d+)(-)(\\d+)")))),
                        fights = fight.games[,1],
                        games = fight.games[,2]) %.%
        mutate(ratio = fights / games)
      fightDB <- rbind(fightDB, tmp)
    }
  } else {
    script <- xpathSApply(HTMLs[[i]], "//script[@language='JavaScript']", xmlValue)
    if (length(script) > 0) {
      fight.games <- matrix(as.numeric(unlist(strsplit(gsub(" fights in ", " ", unlist(str_extract_all(script, "[0-9]+ fights in [0-9]+"))), "  *"))), ncol = 2, byrow = TRUE)
      fightDB <- data.table(name = selector$name[i],
                            ID = selector$ID[i],
                            season = as.numeric(gsub("-[0-9]+", "", unlist(str_extract_all(script, "(\\d+)(-)(\\d+)")))),
                            fights = fight.games[,1],
                            games = fight.games[,2]) %.%
        mutate(ratio = fights / games)
    }
  }
}

#+ save, echo=FALSE
write.csv(fightDB, "fightDB_ALL.csv")



