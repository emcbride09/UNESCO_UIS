regional_af <- read.csv('C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/regional_africa.csv')


year_red <- regional_af %>% group_by(year) %>% summarise(
  mean_rate = sum(regional_ave)/length(regional_ave)
)

reg_red <- regional_af %>% group_by(subregion, year) %>% summarise(
  mean_rate = sum(regional_ave)/length(regional_ave)
)



reg_red2 <- reg_red %>% filter(year != 1971:2016)


west <- regional_af %>% filter(subregion == 'Western Africa') %>% group_by(year) %>% summarise(
  mean_rate = sum(regional_ave)/length(regional_ave)
)
