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
library(stringdist)

# loading data ####
MFA <- fread("C:\\Users\\Seller_King\\Desktop\\Code\\data\\OL.txt")
MFA_info <- read.csv("C:\\Users\\Seller_King\\Desktop\\Code\\data\\keepa.csv")
test <- read.csv("C:\\Users\\Seller_King\\Downloads\\products_export.csv")
categories <- read_excel("C:\\Users\\Seller_King\\Desktop\\Code\\data\\taxonomy.xls")

# prelim
colnames(MFA_info)[98] <- "product_group"
colnames(categories)[2] <- "root"

# BESTBUY filter, remove if not needed ####
MFA_info <- MFA_info %>%
  filter(product_group %in% c("Personal Computer", "Electronics, Office Product", "Wireless, Home Entertainment", "Baby Product", "Camera", "Electronics", "Home", "Home Entertainment", "Kitchen", "Luggage", "Major Appliances", "Musical Instruments", "Office Product", "Personal Care Appliances", "Personal Computer", "Sports", "Tools", "Toy", "Video Games", "Wireless"))


unique_categories <- categories %>%
  distinct(root) %>%
  pull(root)

Roots <- MFA_info %>%
  select(Categories..Root) %>%
  mutate(Roots = recode(Categories..Root,
                                  "Home & Kitchen" = "Home & Garden",
                                  "Automotive" = "Vehicles & Parts",
                                  "Electronics" = "Electronics",
                                  "Tools & Home Improvement" = "Hardware",
                                  "Office Products" = "Office Supplies",
                                  "Clothing, Shoes & Accessories" = "Apparel & Accessories",
                                  "Baby" = "Baby & Toddler",
                                  "Sports & Outdoors" = "Sporting Goods",
                                  "Books" = "Media",
                                  "Industrial & Scientific" = "Business & Industrial",
                                  "Beauty & Personal Care" = "Health & Beauty",
                                  "Toys & Games" = "Toys & Games",
                                  "Health & Personal Care" = "Health & Beauty",
                                  "Patio, Lawn & Garden" = "Home & Garden",
                                  "Movies & TV" = "Media",
                                  "Music" = "Media",
                                  "Musical Instruments, Stage & Studio" = "Arts & Entertainment",
                                  "Pet Supplies" = "Animals & Pet Supplies",
                                  "Software" = "Software"
  )) %>%
  distinct(Categories..Root, Roots)

Roots <- Roots[-c(13, 14, 17, 18, 19), ]
categories <- categories[, -c(1)]

categories <- categories %>%
  mutate(Duplicate_Column = .[[1]]) %>%
  unite("Combined", c(1:7), sep = " > ", na.rm = TRUE)
colnames(categories)[2] <- "Roots"

matching <- MFA_info %>%
  select(ASIN, Categories..Root, Categories..Tree)

matching <- matching %>%
  left_join(select(Roots, Categories..Root, Roots), by = "Categories..Root")
matching <- matching[, -c(2)]
colnames(matching)[2] <- "tree"


# cleaning 
matching <- matching %>%
  mutate(tree = str_replace_all(tree, "›", ">") %>% str_squish())

categories <- categories %>%
  mutate(Combined = str_replace_all(Combined, "›", ">") %>% str_squish())

matching <- matching %>% filter(!is.na(.[[3]]))

matching$Roots <- trimws(matching$Roots)
categories$Roots <- trimws(categories$Roots)


# Subsets
roots <- unique(matching$Roots)

root_subsets_prelim <- lapply(roots, function(r) {
  matching %>% filter(Roots == r)
})
names(root_subsets_prelim) <- roots

root_subsets_gog <- lapply(roots, function(r) {
  categories %>% filter(Roots == r)
})
names(root_subsets_gog) <- roots



# matching
root_subsets_matched <- lapply(roots, function(i) {
  df1_sub <- root_subsets_prelim[[i]]
  df2_sub <- root_subsets_gog[[i]]
  
  # Extract the trees to match (assuming 'CategoryTree' column for both)
  df1_trees <- df1_sub$tree
  df2_trees <- df2_sub$Combined

  # For each row in df1, find closest match in df2
  matches <- sapply(df1_trees, function(tree1) {
    # Calculate string distances
    dists <- stringdist::stringdist(tree1, df2_trees, method = "jw")  # Jaro-Winkler distance
    
    min_index <- which.min(dists)
    
    list(
      matched_tree = df2_trees[min_index],
      similarity = 1 - dists[min_index]  # similarity = 1 - distance (higher is better)
    )
  })
  
  # Unpack matches list
  matched_trees <- sapply(matches, `[[`, "matched_tree")
  similarities <- sapply(matches, `[[`, "similarity")
  
  # Add new columns to df1 subset
  df1_sub %>%
    mutate(
      MatchedTree = matched_trees,
      SimilarityScore = similarities
    )
})

# Name the list elements by root
names(root_subsets_matched) <- roots

