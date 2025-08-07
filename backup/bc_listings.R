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
categories <- read.csv("C:\\Users\\Seller_King\\Downloads\\categories.txt")

# prelim
colnames(MFA_info)[98] <- "product_group"

# categories ####
colnames(categories)[1] <- "raw"
categories <- categories %>%
  mutate(
    code = str_extract(raw, "(?<=TaxonomyCategory/)[^\\s:]+"),
    category = str_trim(str_extract(raw, "(?<=:\\s).*"))
  )
categories <- categories[-1, -1]

categories <- categories %>%
  mutate(cat_norm = str_replace_all(category, "›", ">") %>% str_squish())

MFA_info <- MFA_info %>%
  mutate(cat_norm = str_replace_all(Categories..Tree, "›", ">") %>% str_squish())

find_closest_code <- function(cat) {
  distances <- stringdist::stringdist(cat, categories$cat_norm, method = "jw") # Jaro-Winkler distance
  min_index <- which.min(distances)
  return(categories[min_index, c("code", "cat_norm")])
}

matched <- do.call(rbind, lapply(MFA_info$cat_norm, find_closest_code))

MFA_info <- cbind(MFA_info, matched)
MFA_info <- MFA_info %>% 
  select(-c(154, 156))

# BESTBUY filter, remove if not needed ####
MFA_info <- MFA_info %>%
  filter(product_group %in% c("Personal Computer", "Electronics, Office Product", "Wireless, Home Entertainment", "Baby Product", "Camera", "Electronics", "Home", "Home Entertainment", "Kitchen", "Luggage", "Major Appliances", "Musical Instruments", "Office Product", "Personal Care Appliances", "Personal Computer", "Sports", "Tools", "Toy", "Video Games", "Wireless"))

# Price and quantity list ####
MFA_p_q <- MFA[, c(17, 5, 6, 13)]
colnames(MFA_p_q)[1] <- "ASIN"
colnames(MFA_p_q)[4] <- "Tags"

# naming and adherence ####
colnames(MFA_info)[81] <- "Type"
colnames(MFA_info)[86] <- "UPC"
colnames(MFA_info)[87] <- "EAN"
colnames(MFA_info)[154] <- "Product.Category"

MFA_info$UPC <- sub(",.*", "", MFA_info$UPC)
MFA_info$EAN <- sub(",.*", "", MFA_info$EAN)

# selecting columns for bc data
bc <- MFA_info[, c(2, 3, 84, 86, 87, 88, 94, 98, 99, 101, 102, 109, 81, 131, 154)]

# joining prices and quantities 
bc <- bc %>%
  left_join(MFA_p_q %>% select(ASIN, price, quantity, Tags), by = "ASIN") %>%
  filter(Tags == 11) %>%
  distinct(ASIN, .keep_all = TRUE) %>%
  mutate(across(Tags, ~ ifelse(. == 11, "Brand New", .))) 

# UPC/EAN ####
bc <- bc %>%
  mutate(
    barcode = if_else(UPC == "" | is.na(UPC), EAN, UPC)
  )

# renaming
colnames(bc)[19] <- "Variant.Barcode"
colnames(bc)[7] <- "Vendor"
colnames(bc)[16] <- "Variant.Compare.At.Price"
colnames(bc)[17] <- "Variant.Inventory.Qty"
colnames(bc)[6] <- "Global Trade Number"

# photo separation ####
bc <- bc %>%
  separate(Image, into = paste0("image", 1:20), sep = ";", fill = "right", extra = "drop") %>%
  arrange(desc(Variant.Compare.At.Price))

# xAi List for discriptions ####
xAi_list <- bc[, c(22:28)]
write.csv(xAi_list, "C:\\Users\\Seller_King\\Desktop\\xAi_list.csv", row.names = FALSE, na = "")

# handles ####
bc <- bc %>%
  mutate(
    "Product URL" = Title %>%
      tolower() %>%                           # lowercase
      str_replace_all("[[:punct:]]", "") %>%  # remove punctuation
      str_replace_all("\\s+", "-") %>%        # spaces to dashes
      paste0("/", ., "/")                     # add / before and after
  )

# pivot for bc format ####
bc <- bc %>%
  pivot_longer(
    cols = starts_with("image"),       # or specify: c(image1, image2, image3)
    names_to = "Image.Position",       # new column for photo identifier
    values_to = "Image.Src"            # new column for URLs
  ) %>%
  filter(!is.na(Image.Src)) %>%      # remove rows with no image URL
  mutate(Image.Position = as.integer(str_remove(Image.Position, "image")))

# separating and aligning data values ####
product_info <- bc %>%
  distinct(across(1:19)) %>%
  mutate(
    Item = "Product", 
    Type = "physical",
    Options = NA_character_,
    "Inventory Tracking" = "none",
    "Retail Price" = NA_character_,
    "Sale Price" = as.numeric(Variant.Compare.At.Price) * 0.95,
    "Brand ID" = NA_character_,
    "Channels" = 1,
    "Categories" = NA_character_,
    "Description" = NA_character_,
    "Custom Fields" = NA_character_,
    "Page Title" = NA_character_,
    "Meta Description" = NA_character_,
    "Search Keywords" = NA_character_,
    "Meta Keywords" = NA_character_,
    "Product Condition" = "New",
    "Show Product Condition" = "TRUE",
    "Is Visible" = "TRUE",
    "Is Featured" = "FALSE",
    "Warranty" = NA_character_,
    "Free Shipping" = "FALSE",
    "Sort Order" = 0,
    "Low Stock" = 2,
    Package..Weight..g. = ifelse(is.na(Package..Weight..g.), 100, Package..Weight..g.)
    )

image_info <- bc %>%
  select(2, image_position = 20, image_url = 21) %>%
  mutate("Image is Thumbnail" = ifelse(. [[2]] == 1, "TRUE", "FALSE"),
         "Image Description" = paste0("image_", `image_position`))


# Bind the product row, followed by each of its image rows
data_raw <- product_info %>%
  rowwise() %>%
  do({
    asin <- .$ASIN  # column 2 as identifier, or replace with `.$ASIN` 
    product_row <- select(as.data.frame(.), everything())
    image_rows <- image_info %>%
      filter(ASIN == asin) %>%
      select(image_position, image_url, "Image is Thumbnail", "Image Description")
    
    bind_rows(product_row, image_rows)
  }) %>%
  ungroup() 

# setting up listings ####
data_raw$Item[is.na(data_raw$Item)] <- "Image"
data_raw$ID <- NA_character_
data_raw$"Cost Price" <- NA_character_


colnames(data_raw)[1] <- "Name"
colnames(data_raw)[2] <- "SKU"
colnames(data_raw)[8] <- "Manufacturer Part Number"
colnames(data_raw)[13] <- "Weight"
colnames(data_raw)[15] <- "Price"
colnames(data_raw)[16] <- "Current Stock"
colnames(data_raw)[18] <- "UPC/EAN"
colnames(data_raw)[42] <- "Image Sort Order"
colnames(data_raw)[43] <- "Image URL (Import)"


data_raw <- data_raw %>%
  mutate(Price = round(as.numeric(gsub("[$,]", "", Price))),
         Sale.Price= round(as.numeric(gsub("[$,]", "", Sale.Price)))) 

listings <- data_raw %>%
  select(
    "Item",
    "ID",
    "Name",
    "Type",
    "SKU",
    "Vendor",
    "Options",
    "Inventory.Tracking",
    "Current Stock",
    "Low.Stock",
    "Price",
    "Cost Price",
    "Retail.Price",
    "Sale.Price",
    "Brand.ID",
    "Channels",
    "Categories",
    "Description",
    "Custom.Fields",
    "Page.Title",
    "Product.URL",
    "Meta.Description",
    "Search.Keywords",
    "Meta.Keywords",
    "UPC/EAN",
    "Global.Trade.Number",
    "Manufacturer Part Number",
    "Free.Shipping",
    "Weight",
    "Is.Visible",
    "Is.Featured",
    "Warranty",
    "Product.Condition",
    "Show.Product.Condition",
    "Sort.Order",
    "Image URL (Import)",
    "Image is Thumbnail",
    "Image Sort Order",
    "Image Description"
  )

colnames(listings) <- gsub("\\.", " ", colnames(listings))

target_categories <- c(
  "Record Players & Turntables", "Distortion & Overdrive", "Compression & Sustain",
  "Component Subwoofers", "Trombones", "Powered Mixers",
  "Amplifiers", "Portable Electronic Keyboards", "Studio Monitors",
  "Stereo Shelf Systems", "Home Digital Pianos", "Portable & Arranger Keyboards",
  "Booms & Stands", "Surround Sound Systems", "Coaxial Speakers", "Electric Guitar Effects",
  "Receivers, AV Receivers & Amplifiers", "Acoustic Guitar Strings",
  "Internal Sound Cards", "Controllers & Drum Machines",
  "Amplifiers & Effects", "Combo Amplifiers", "Microphone Accessories, Uninterrupted Power Supplies",
  "Portable Recorders", "Signal Path & Pedal Tuners", "Computer Microphones", "Synthesizers",
  "Acoustic Steel-String Guitars", "Audio Interfaces", "Cabinets, Studio Recording Equipment",
  "Violas", "Heads, Studio Recording Equipment", "Subwoofers", "DJ Controllers & Interfaces",
  "Tabletop Synthesizers", "Wireless & Streaming Audio", "Acoustic Violins",
  "Acoustic Guitar Kits", "Drum Sets", "Delay & Reverb", "Stage Digital Pianos", "Trumpets",
  "Audio & Video Accessories, Component Subwoofers", "Dynamic Microphones", "MIDI Controllers",
  "Saxophones", "External Sound Cards", "Wah, Talk Box & Filters", "Turntable Cartridges",
  "Electric Guitar Gig Bags", "Pedals", "Flutes", "Electric Guitar Kits",
  "Speakers, Component Subwoofers", "Electric Guitar Cases", "Guitars, Combo Amplifiers"
)

newegg_request <- MFA_info %>%
  select(Title, ASIN, c(81)) %>%
  filter(Type %in% target_categories) %>%
  mutate(URL = paste0("https://www.amazon.ca/dp/", ASIN))
           

write.csv(listings, "C:\\Users\\Seller_King\\Desktop\\listings.csv", row.names = FALSE, na = "")
write.csv(newegg_request, "C:\\Users\\Seller_King\\Desktop\\newegg_request.csv", row.names = FALSE, na = "")

