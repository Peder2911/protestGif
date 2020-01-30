
shh <- suppressPackageStartupMessages
shh(library(dplyr))
shh(library(readxl))
shh(library(glue))
shh(library(tidyr))
shh(library(sf))
shh(library(opencage))
shh(library(assertthat))

fixNumCat <- function(x) {
   x <- as.numeric(x)
   if(0 %in% x){
      x <- x + 1
   }
   ifelse(x>0,x,NA)
}

from1899 <- function(num){
   # What the actual fuck
   as.Date(as.numeric(num),origin=as.Date("1899-12-30"))
}

raw <- read_xlsx("data/raw/80f9f9cf3a1aba33beccc7bda049ee33.xlsx")[-1,]
raw[4,"start_month"] <- 2 # FIX ALGERIE MONTH

dat <- raw %>%
   mutate(
       start_date = as.Date(glue("{start_year}-{start_month}-{start_day}")),
       end_date = as.Date(glue("{end_year}-{end_month}-{end_day}")),
       cause = c("Political","Economic","Mixed","Other")[fixNumCat(Cause)],
       structure = c("Decentralized","Lead")[fixNumCat(`Participant’s structure`)],
       demands = c("Maximalist","Other")[fixNumCat(Demands)]
   ) %>%
   select(country_name = Location, 
          start_date, end_date,
          #iso_a3 = sftgcode,
          #cow = loc_cow,
          location = Geography_specific,
          cause, structure, demands) %>%
   filter(!country_name %in% c("Israel","Syria")) %>%
   separate_rows(location, sep = " *, *") %>%
   filter(!location %in% c("", "-1")) %>%
   mutate(querystring = glue("{location}, {country_name}")) %>%
   unique()

if(!"geocoding.rds" %in% list.files("cache")){
   Sys.setenv(OPENCAGE_KEY = readLines("keys/key"))

   writeLines(glue("\x1b[33mPulling geodata from opencage\x1b[0m"))
   geocoding <- lapply(dat$querystring, function(cityname){
      Sys.sleep(1.05)
      writeLines(glue("\x1b[33mGeocoding {cityname}\x1b[0m"))

      res <- opencage_forward(cityname)
      if(!is.null(res$results)){
         writeLines(glue("\x1b[32mGot {cityname}\x1b[0m"))
         res$results[1,]
      } else {
         writeLines(glue("\x1b[31m{cityname} not found!\x1b[0m"))
         NA
      }
   })
   saveRDS(geocoding,"cache/geocoding.rds")
} else {
   geocoding <- readRDS("cache/geocoding.rds")
}

geocoding <- geocoding %>%
   bind_rows() %>%
   select(querystring = query, lat = geometry.lat, lng = geometry.lng)

rows <- nrow(dat)

dat <- merge(dat,geocoding,"querystring", all.x = TRUE) %>%
   unique()
assert_that(nrow(dat) == rows)

rows <- nrow(dat)
dat <- dat %>%
   st_as_sf(coords = c("lng","lat"), crs = 4326, remove = FALSE) %>%
   filter(!is.na(date))
assert_that(nrow(dat) == rows)

saveRDS(dat,"data/prepped/geocodedProtests.rds")


