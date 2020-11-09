knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

# Read in new Social Determinants definitions
SocialDeterminants <- read_csv("SocialDeterminants.csv")

final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
chr.data.2019 <- readRDS("chr.data.2019.rds")
chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(final.determinants)

# Load name map and its inverse
chr.namemap.2019 <- SocialDeterminants %>% select("Code", "Name")
chr.namemap.2019 <- column_to_rownames(chr.namemap.2019, "Code")
names(chr.namemap.2019)[1] <- "name"

geo.namemap <- readRDS("geo.namemap.rds")

source("Theme.R")

date_of_study = Sys.Date()

chr <- read_csv("../Data/2020CHR.csv")
chr <- chr[, -grep("Quartile", colnames(chr))]
chr <- chr[, -grep("95", colnames(chr))]
chr <- subset(chr, select = c(FIPS, Population))

geo <- subset(geo.namemap, select = -c(state_fips))
geo$county_fips <- str_pad(geo$county_fips, 5, pad = "0")

# Historical data
covid_hist = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
covid_us_hist = subset(covid_us_hist, select = c(FIPS, Deaths))
covid_us_hist$FIPS <- str_pad(covid_us_hist$FIPS, 5, pad = "0")
covid_us_hist = merge(x = covid_us_hist,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
covid_us_hist = merge(x = covid_us_hist,y = chr, by = "FIPS", all.x = TRUE)
covid_us_hist$period = "03-22-2020"

date_of_all = format(seq(as.Date("2020-03-23"), as.Date(strptime(date_of_study,"%Y-%m-%d")), by = "month"),"%m-%d-%Y")

for (i in 1:length(date_of_all)){
  covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all[i],".csv")))
  covid_daily = subset(covid_daily,Country_Region == "US" & is.na(FIPS)==F)
  covid_daily = subset(covid_daily, select = c(FIPS, Deaths))
  covid_daily$FIPS <- str_pad(covid_daily$FIPS, 5, pad = "0")
  covid_daily = merge(x = covid_daily,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
  covid_daily = merge(x = covid_daily,y = chr, by = "FIPS", all.x = TRUE)
  covid_daily$period = date_of_all[i]
  covid_us_hist <- rbind(covid_us_hist, covid_daily)
}

covid_us_hist <- dplyr::rename(covid_us_hist, c(county_fips = FIPS, death_num = Deaths, population = Population))
covid_us_hist$death_rate <- covid_us_hist$death_num/covid_us_hist$population*100000


km.func <- function(data.mat, cluster.num=3, seed=200) {
  
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    tibble(
      county_fips = dplyr::pull(data.mat, county_fips),
      cluster = as.character(km.result$cluster)
    )
  )
}

kendall.func <- function(x.data, sd.data) {
  
  # TODO's:
  #   1. Align social determinants and "x" data by "county_fips"
  #   2. Re-separate "x column and sd data frame with select
  #   3. Do cor.test
  #   4. Rename the variables to VAR
  
  align <- dplyr::left_join(x.data, sd.data, by = "county_fips") %>% 
    dplyr::select(-dplyr::one_of(c("county_fips", "state_name", "county_name")))
  #browser()
  x <- as.numeric(dplyr::pull(align, 1))
  sd <- dplyr::select(align, -1)
  
  cor.res <- list()
  for (n in names(sd)) {
    y <- dplyr::pull(sd, n)
    if (is_character(y)) {
      # print(y)
      y <- readr::parse_number(y)
    }
    
    if (sum(is.na(y)) < 0.5 * length(y)) {
      # print(tibble(x, y, n))
      cor.res[[n]] <- cor.test(
        x = x, y = y, use = "pairwise.complete.obs", method = "kendall", exact = F
      )
    }
  }
  
  tibble(
    chr_code = names(cor.res),
    kendall_cor = sapply(cor.res, function(r) r$estimate),
    kendall_p = sapply(cor.res, function(r) r$p.value)
  )
}