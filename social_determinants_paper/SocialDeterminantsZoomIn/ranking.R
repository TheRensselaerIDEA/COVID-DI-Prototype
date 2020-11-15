source("Source.R")

state.list <- state.abb
names(state.list) <- state.name

aggregated_des = data.frame(matrix(ncol=0,nrow=0))
aggregated_pro = data.frame(matrix(ncol=0,nrow=0))

for (state_choice in state.list) {
  if (!state_choice %in% c("HI", "DE")){
    print(state_choice)
    covid.data <- subset(covid_us_hist, period %in% date_of_all)
    covid.data <- covid.data %>% drop_na(death_num,population)
    
    mort.rate <- covid.data %>% dplyr::filter(
      state_abbr == state_choice,
      period == tail(date_of_all, n=1)
    ) %>%
      dplyr::mutate(
        # death_rate = death_num / population * 10^5
        #death_rate = cut(death_rate, bin.geo.mort("Despair"))
      ) %>%
      dplyr::select(county_fips, death_rate)
    
    kendall.cor <- mort.rate %>% 
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit()
    
    # Sort by kendall.cor
    kendall.cor.new <- kendall.cor %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, abs(kendall_cor)) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    # print(kendall.cor.new)
        
    kendall.des <- kendall.cor.new[kendall.cor.new$DIR == "Destructive",]
    
    if (nrow(kendall.des) > 0){
      kendall.des <- select(kendall.des, c(chr_code, kendall_cor))
      kendall.des$des_rank <- 1:nrow(kendall.des)
      kendall.des <- kendall.des[, c(3, 1, 2)]
      
      if (ncol(aggregated_des) == 0) {
        aggregated_des <- kendall.des
      }else {
        aggregated_des <- merge(aggregated_des, kendall.des, by="des_rank", all = T) 
      }
      
      names(aggregated_des)[names(aggregated_des) == "chr_code"] <- paste(state_choice, "_chr_code", sep = "")
      names(aggregated_des)[names(aggregated_des) == "kendall_cor"] <- paste(state_choice, "_kendall_cor", sep = "")
    }
    
    kendall.pro <- kendall.cor.new[kendall.cor.new$DIR == "Protective",]
    
    if (nrow(kendall.pro) > 0){
      kendall.pro <- select(kendall.pro, c(chr_code, kendall_cor))
      kendall.pro$pro_rank <- 1:nrow(kendall.pro)
      kendall.pro <- kendall.pro[, c(3, 1, 2)]
      
      if (ncol(aggregated_pro) == 0) {
        aggregated_pro <- kendall.pro
      }else {
        aggregated_pro <- merge(aggregated_pro, kendall.pro, by="pro_rank", all = T) 
      }
      
      names(aggregated_pro)[names(aggregated_pro) == "chr_code"] <- paste(state_choice, "_chr_code", sep = "")
      names(aggregated_pro)[names(aggregated_pro) == "kendall_cor"] <- paste(state_choice, "_kendall_cor", sep = "")
    }
    
  }
}

write.csv(aggregated_des,"aggregated_des.csv", row.names = FALSE)
write.csv(aggregated_pro,"aggregated_pro.csv", row.names = FALSE)