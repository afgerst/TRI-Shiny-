TRI1990 <- read.csv("C:\\Users\\Abby\\Documents\\SDSU\\Fall23\\STAT442\\testtesttest\\1990_us.csv", header=T)


TRI1990 <- TRI1990 %>%
  select(YEAR, FACILITY.NAME, CITY, ST, ZIP, LATITUDE, LONGITUDE, PARENT.CO.NAME, INDUSTRY.SECTOR, CHEMICAL, CLEAN.AIR.ACT.CHEMICAL, 
         METAL, CARCINOGEN, PBT, PFAS, UNIT.OF.MEASURE, FUGITIVE.AIR, STACK.AIR, WATER, UNDERGROUND, LANDFILLS,
         ON.SITE.RELEASE.TOTAL, OFF.SITE.RELEASE.TOTAL, PRODUCTION.RATIO)


grams2pounds <- function(grams) {
  pounds <- grams / 453.592  # 1 pound = 453.592 grams
  return(pounds)
}


TRI1990$UNIT.OF.MEASURE <- as.character(TRI1990$UNIT.OF.MEASURE)
grams_rows <- TRI2022$UNIT.OF.MEASURE == "Grams"

TRI1990$FUGITIVE.AIR[grams_rows] <- grams2pounds(TRI1990$FUGITIVE.AIR[grams_rows])
TRI1990$STACK.AIR[grams_rows] <- grams2pounds(TRI1990$STACK.AIR[grams_rows])
TRI1990$WATER[grams_rows] <- grams2pounds(TRI1990$WATER[grams_rows])
TRI1990$LANDFILLS[grams_rows] <- grams2pounds(TRI1990$LANDFILLS[grams_rows])
TRI1990$UNDERGROUND[grams_rows] <- grams2pounds(TRI1990$UNDERGROUND[grams_rows])
TRI1990$ON.SITE.RELEASE.TOTAL[grams_rows] <- grams2pounds(TRI1990$ON.SITE.RELEASE.TOTAL[grams_rows])
TRI1990$OFF.SITE.RELEASE.TOTAL[grams_rows] <- grams2pounds(TRI1990$OFF.SITE.RELEASE.TOTAL[grams_rows])


ColumnsReplace <- c("FUGITIVE.AIR", "STACK.AIR", "WATER", "LANDFILLS", "UNDERGROUND", "ON.SITE.RELEASE.TOTAL", "OFF.SITE.RELEASE.TOTAL")

TRI1990 <- TRI1990 %>%
  mutate(across(all_of(ColumnsReplace), ~ifelse(. == 0, NA, .)))

by_state1990 <- TRI1990 %>%
  group_by(ST) %>%
  summarise(
    FugitiveAir = mean(FUGITIVE.AIR, na.rm = TRUE),
    StackAir = mean(STACK.AIR, na.rm = TRUE),
    Water = mean(WATER, na.rm = TRUE),
    Landfills = mean(LANDFILLS, na.rm = TRUE),
    Underground = mean(UNDERGROUND, na.rm = TRUE),
    OnSite = mean(ON.SITE.RELEASE.TOTAL, na.rm = TRUE),
    OffSite = mean(OFF.SITE.RELEASE.TOTAL, na.rm = TRUE),
  ) %>%
  ungroup()

write.csv(by_state1990, file = "by_state1990.csv")


TRIHist <- read.csv("C:\\Users\\Abby\\Documents\\SDSU\\Fall23\\STAT442\\testtesttest\\historical.csv", header=T)


