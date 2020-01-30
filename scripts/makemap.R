
shh <- suppressPackageStartupMessages
shh(library(ggplot2))
shh(library(sf))
shh(library(lubridate))
shh(library(parallel))
shh(library(dplyr))
shh(library(glue))
shh(library(yaml))

makemap <- function(data, date){
   ggplot(data) +
      geom_sf(data = bg,fill = "#e4e5ef", size = 0.1) +
      geom_sf(size = 0.05, color="#FF3503") +
      theme_void() +
      geom_text(aes(x=-170, y = -30), size = 7, hjust = 0, color = "#2e324d",
         label = strftime(date,"%Y %b"))
}

protests <- readRDS("data/prepped/geocodedProtests.rds") 

bg <- read_sf("data/ne/ne_50m_admin_0_countries_lakes.shp") %>%
   st_simplify(dTolerance = 0.2)

months <- seq(min(protests$start_date),max(protests$end_date),"months")
i <- 0
months <- lapply(months, function(m){
   i <<- i + 1
   list(date = m, n = i)
})

maps <- mclapply(months, function(dateCrossection){
   dataCs <- filter(protests,
      start_date <= dateCrossection$date & end_date >= dateCrossection$date
   )
   makemap(dataCs, dateCrossection$date) %>%
      ggsave(glue("frames/frame_{dateCrossection$n}.png"),
         ., device = "png", width = 5, height = 2.5)
}, mc.cores=detectCores() - 1)

#system("gifski frames/frame_*.png --fps 2 --output map.gif")
