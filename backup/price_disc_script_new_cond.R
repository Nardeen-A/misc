# library ####
library(data.table)
library(dplyr)
library(ggnewscale)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(pals)
library(RColorBrewer)
library(svglite)
library(readxl)
library(reshape)
library(tidyr)
library(tidyverse)
library(viridis)
library(viridisLite)
library(rvest)
library(rmarkdown)
library(flexdashboard)
library(haven)
library(magrittr)
library(reshape2)
library(stargazer)
library(labelled)
library(Hmisc)
library(stargazer)
library(sandwich)
library(lmtest)
library(openxlsx)

# MFA ####
# loading data ####
MFA <- fread("C:\\Users\\Seller_King\\Desktop\\Code\\data\\OL.txt")

# filters and prelims (new items) ####
colnames(MFA)[13] <- "item_condition"
MFA <- MFA %>%
  filter(item_condition == 11)

# drawing asins 
MFA_asins <- MFA[, 17]

# keepa list
MFA_keepa_list <- as.character(MFA_asins$asin1)
MFA_keepa_list <- paste(MFA_keepa_list, collapse = " ")
writeClipboard(MFA_keepa_list)

# analysis ####
# keepa data
MFA_info <- read.csv("C:\\Users\\Seller_King\\Downloads\\info_1.csv")

# selection columns
MFA_info <- MFA_info[, c(3, 16, 18, 20, 26, 10, 30, 84, 95)]

# naming columns
colnames(MFA_info)[2] <- "buy_box_price"
colnames(MFA_info)[3] <- "buy_box_stock"
colnames(MFA_info)[4] <- "buy_box_seller"
colnames(MFA_info)[5] <- "buy_box_FBA"
colnames(MFA_info)[6] <- "past_month_sold"

MFA_info <- MFA_info[, c(1, 8, 2, 4, 5, 6, 3, 7, 9)]

# removing our buybox
MFA_info <- MFA_info[!grepl("\\(AMTS3G0O92WLE\\)", MFA_info$buy_box_seller), ]

# Price Discrimination
MFA_i_p_q <- MFA_info[, c(2, 3)]
MFA_p_q <- MFA[, c(17, 5)]

colnames(MFA_p_q)[1] <- "ASIN"
colnames(MFA_i_p_q)[2] <- "price"

MFA_i_p_q[[2]] <- as.numeric(gsub("\\$|\\s", "", MFA_i_p_q[[2]]))

price_disc_MFA <- inner_join(MFA_p_q, MFA_i_p_q, by = "ASIN", suffix = c("_ours", "_bb"))
price_disc_MFA$difference <- price_disc_MFA$price_bb - price_disc_MFA$price_ours
price_disc_MFA$percent_difference <- price_disc_MFA$difference / price_disc_MFA$price_ours * 100

colnames(MFA)[17] <- "ASIN"

price_disc_MFA <- price_disc_MFA %>%
  left_join(MFA %>% select(ASIN, quantity), by = "ASIN") %>%
  left_join(MFA_info %>% select(ASIN, Amazon..Stock), by = "ASIN")

price_disc_MFA <- price_disc_MFA[order(price_disc_MFA[[5]]), ]

write.xlsx(price_disc_MFA, "C:\\Users\\Seller_King\\Desktop\\MFA_price_disc.xlsx")

# FBA ####
FBA <- fread("C:\\Users\\Seller_King\\Downloads\\FBA.txt")

# filters and prelims (new items) ####
colnames(FBA)[4] <- "condition"

FBA <- FBA %>%
  filter(!`Quantity Available` == 0 & condition == "NewItem") 

# keepa list
MFA_keepa_list_2 <- as.character(FBA$asin)
MFA_keepa_list_2 <- paste(MFA_keepa_list_2, collapse = " ")
writeClipboard(MFA_keepa_list_2)

# analysis ####
# keepa data
FBA_info <- read.csv("C:\\Users\\Seller_King\\Downloads\\info_2.csv")

# selection columns
FBA_info <- FBA_info[, c(3, 16, 18, 20, 26, 10, 30, 84, 95)]
FBA <- read.csv("C:\\Users\\Seller_King\\Downloads\\test_2.csv")

# naming columns
colnames(FBA_info)[2] <- "buy_box_price"
colnames(FBA_info)[3] <- "buy_box_stock"
colnames(FBA_info)[4] <- "buy_box_seller"
colnames(FBA_info)[5] <- "buy_box_FBA"
colnames(FBA_info)[6] <- "past_month_sold"

FBA_info <- FBA_info[, c(1, 8, 2, 4, 5, 6, 3, 7, 9)]

FBA_p_q <- FBA[, c(4, 20, 42)]

# removing our buybox
FBA_info <- FBA_info[!grepl("\\(AMTS3G0O92WLE\\)", FBA_info$buy_box_seller), ]

# analysis ####
FBA_i_p_q <- FBA_info[, c(2, 3)]
FBA_p_q <- FBA_p_q[, c(1, 2)]

colnames(FBA_p_q)[1] <- "ASIN"
colnames(FBA_p_q)[2] <- "price"
colnames(FBA_i_p_q)[2] <- "price"
colnames(FBA)[4] <- "ASIN"

FBA_i_p_q[[2]] <- as.numeric(gsub("\\$|\\s", "", FBA_i_p_q[[2]]))

price_disc_FBA <- inner_join(FBA_p_q, FBA_i_p_q, by = "ASIN", suffix = c("_ours", "_bb"))
price_disc_FBA$difference <- price_disc_FBA$price_bb - price_disc_FBA$price_ours
price_disc_FBA$percent_difference <- price_disc_FBA$difference / price_disc_FBA$price_ours * 100

price_disc_FBA <- price_disc_FBA %>%
  left_join(FBA %>% select(ASIN, available), by = "ASIN") %>%
  left_join(FBA_info %>% select(ASIN, Amazon..Stock), by = "ASIN")

price_disc_FBA <- price_disc_FBA[order(price_disc_FBA[[5]]), ]


write.xlsx(price_disc_FBA, "C:\\Users\\Seller_King\\Desktop\\FBA_price_disc.xlsx")


