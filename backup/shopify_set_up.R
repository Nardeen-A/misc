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

# loading data
MFA <- fread("C:\\Users\\Seller_King\\Desktop\\Code\\data\\OL.txt")
MFA_info <- read.csv("C:\\Users\\Seller_King\\Desktop\\Code\\data\\keepa.csv")
test <- read.csv("C:\\Users\\Seller_King\\Downloads\\products_export.csv")
categories <- read.csv("C:\\Users\\Seller_King\\Downloads\\categories.txt")

# prelim
colnames(MFA_info)[98] <- "product_group"

# categories
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

# BESTBUY filter, remove if not needed
MFA_info <- MFA_info %>%
  filter(product_group %in% c("Personal Computer", "Electronics, Office Product", "Wireless, Home Entertainment", "Baby Product", "Camera", "Electronics", "Home", "Home Entertainment", "Kitchen", "Luggage", "Major Appliances", "Musical Instruments", "Office Product", "Personal Care Appliances", "Personal Computer", "Sports", "Tools", "Toy", "Video Games", "Wireless"))

# Price and quantity list
MFA_p_q <- MFA[, c(17, 5, 6, 13)]
colnames(MFA_p_q)[1] <- "ASIN"
colnames(MFA_p_q)[4] <- "Tags"

# naming and adherence
colnames(MFA_info)[81] <- "Type"
colnames(MFA_info)[86] <- "UPC"
colnames(MFA_info)[87] <- "EAN"
colnames(MFA_info)[154] <- "Product.Category"

MFA_info$UPC <- sub(",.*", "", MFA_info$UPC)
MFA_info$EAN <- sub(",.*", "", MFA_info$EAN)

# selecting columns for shopify data
shopify <- MFA_info[, c(2, 3, 84, 86, 87, 94, 98, 99, 101, 102, 109, 81, 154)]

# joining prices and quantities 
shopify <- shopify %>%
  left_join(MFA_p_q %>% select(ASIN, price, quantity, Tags), by = "ASIN") %>%
  filter(Tags == 11) %>%
  distinct(ASIN, .keep_all = TRUE) %>%
  mutate(across(Tags, ~ ifelse(. == 11, "Brand New", .))) 

# UPC/EAN
shopify <- shopify %>%
  mutate(
    barcode = if_else(UPC == "" | is.na(UPC), EAN, UPC)
  )

# renaming
colnames(shopify)[17] <- "Variant.Barcode"
colnames(shopify)[6] <- "Vendor"
colnames(shopify)[14] <- "Variant.Compare.At.Price"
colnames(shopify)[15] <- "Variant.Inventory.Qty"

# photo separation 
shopify <- shopify %>%
  separate(Image, into = paste0("image", 1:20), sep = ";", fill = "right", extra = "drop") %>%
  arrange(desc(Variant.Compare.At.Price))

# xAi List for discriptions
xAi_list <- shopify[, c(21:27)]
write.csv(xAi_list, "C:\\Users\\Seller_King\\Desktop\\xAi_list.csv", row.names = FALSE, na = "")

# handles
shopify <- shopify %>%
  mutate(
    Handle = Title %>%
      tolower() %>%                           # lowercase
      str_replace_all("[[:punct:]]", "") %>%  # remove punctuation
      str_replace_all("\\s+", "-")            # spaces to dashes
  )

# pivot for shopify format
shopify <- shopify %>%
  pivot_longer(
    cols = starts_with("image"),       # or specify: c(image1, image2, image3)
    names_to = "Image.Position",       # new column for photo identifier
    values_to = "Image.Src"            # new column for URLs
  ) %>%
  filter(!is.na(Image.Src)) %>%      # remove rows with no image URL
  mutate(Image.Position = as.integer(str_remove(Image.Position, "image")))

# Shopify data columns
shopify <- shopify %>%
  mutate(
    `Body..HTML.` = "",
    Published = "True",
    Option1.Name = "Title",
    Option1.Value = "Default Title",
    Option1.Linked.To = NA_character_,
    Option2.Name = NA_character_,
    Option2.Value = NA_character_,
    Option2.Linked.To = NA_character_,
    Option3.Name = NA_character_,
    Option3.Value = NA_character_,
    Option3.Linked.To = NA_character_,
    Variant.SKU = NA_character_,
    Variant.Grams = 0,
    Variant.Inventory.Tracker = "shopify",
    Variant.Inventory.Policy = "deny",
    Variant.Fulfillment.Service = "manual",
    Variant.Requires.Shipping = "true",
    Variant.Taxable = "false",
    Image.Alt.Text = NA_character_,
    Gift.Card = "false",
    SEO.Title = NA_character_,
    SEO.Description = NA_character_,
    Google.Shopping...Google.Product.Category = NA_character_,
    Google.Shopping...Gender = NA_character_,
    Google.Shopping...Age.Group = NA_character_,
    Google.Shopping...MPN = NA_character_,
    Google.Shopping...Condition = NA_character_,
    Google.Shopping...Custom.Product = NA_character_,
    Google.Shopping...Custom.Label.0 = NA_character_,
    Google.Shopping...Custom.Label.1 = NA_character_,
    Google.Shopping...Custom.Label.2 = NA_character_,
    Google.Shopping...Custom.Label.3 = NA_character_,
    Google.Shopping...Custom.Label.4 = NA_character_,
    `Location..product.metafields.custom.location.` = NA_character_,
    `Google..Custom.Product..product.metafields.mm.google.shopping.custom_product.` = NA_character_,
    `Connection.type..product.metafields.shopify.connection.type.` = NA_character_,
    `Cosmetic.condition..product.metafields.shopify.cosmetic.condition.` = "new",
    `Memory.technology..product.metafields.shopify.memory.technology.` = NA_character_,
    `Operating.system..product.metafields.shopify.operating.system.` = NA_character_,
    `Processor.family..product.metafields.shopify.processor.family.` = NA_character_,
    Variant.Image = NA_character_,
    Variant.Weight.Unit = "kg",
    Variant.Tax.Code = NA_character_,
    Cost.per.item = NA_character_,
    Status = "active"
    
  )  
  
cols_to_clear <- setdiff(names(shopify), c("Image.Src", "Image.Position", "Handle"))

# data adherence
shopify <- shopify %>%
  group_by(Handle) %>%
  mutate(across(
    all_of(cols_to_clear),
    ~ {
      x <- as.character(.)             # convert to character
      if_else(row_number() == 1, x, "")  # keep first row value, else empty string
    }
  )) %>%
  ungroup()

# naming 
colnames(shopify)[2] <- "Asin..product.metafields.custom.asin."
colnames(shopify)[8] <- "Color..product.metafields.shopify.color.pattern."
colnames(shopify)[10] <- "Material..product.metafields.shopify.material."

# variant compare at price
shopify <- shopify %>%
  mutate(Variant.Price = as.numeric(Variant.Compare.At.Price) * 0.95)

# ordering columns 
listings <- shopify %>%
  select("Handle",
         "Title",
         "Body..HTML.",
         "Vendor",
         "Product.Category",
         "Type",
         "Tags",
         "Published",
         "Option1.Name",
         "Option1.Value",
         "Option1.Linked.To",
         "Option2.Name",
         "Option2.Value",
         "Option2.Linked.To",
         "Option3.Name",
         "Option3.Value",
         "Option3.Linked.To",
         "Variant.SKU",
         "Variant.Grams",
         "Variant.Inventory.Tracker",
         "Variant.Inventory.Qty",
         "Variant.Inventory.Policy",
         "Variant.Fulfillment.Service",
         "Variant.Price",
         "Variant.Compare.At.Price",
         "Variant.Requires.Shipping",
         "Variant.Taxable",
         "Variant.Barcode",
         "Image.Src",
         "Image.Position",
         "Image.Alt.Text",
         "Gift.Card",
         "SEO.Title",
         "SEO.Description",
         "Google.Shopping...Google.Product.Category",
         "Google.Shopping...Gender",
         "Google.Shopping...Age.Group",
         "Google.Shopping...MPN",
         "Google.Shopping...Condition",
         "Google.Shopping...Custom.Product",
         "Google.Shopping...Custom.Label.0",
         "Google.Shopping...Custom.Label.1",
         "Google.Shopping...Custom.Label.2",
         "Google.Shopping...Custom.Label.3",
         "Google.Shopping...Custom.Label.4",
         "Asin..product.metafields.custom.asin.",
         "Location..product.metafields.custom.location.",
         "Google..Custom.Product..product.metafields.mm.google.shopping.custom_product.",
         "Color..product.metafields.shopify.color.pattern.",
         "Connection.type..product.metafields.shopify.connection.type.",
         "Cosmetic.condition..product.metafields.shopify.cosmetic.condition.",
         "Material..product.metafields.shopify.material.",
         "Memory.technology..product.metafields.shopify.memory.technology.",
         "Operating.system..product.metafields.shopify.operating.system.",
         "Processor.family..product.metafields.shopify.processor.family.",
         "Variant.Image",
         "Variant.Weight.Unit",
         "Variant.Tax.Code",
         "Cost.per.item",
         "Status",
         "product_group")

# clean and round price
listings <- listings %>%
  mutate(Variant.Price = round(as.numeric(gsub("[$,]", "", Variant.Price))))  

# final naming
colnames(listings)[1:60] <- c(
  "Handle",
  "Title",
  "Body (HTML)",
  "Vendor",
  "Product Category",
  "Type",
  "Tags",
  "Published",
  "Option1 Name",
  "Option1 Value",
  "Option1 Linked To",
  "Option2 Name",
  "Option2 Value",
  "Option2 Linked To",
  "Option3 Name",
  "Option3 Value",
  "Option3 Linked To",
  "Variant SKU",
  "Variant Grams",
  "Variant Inventory Tracker",
  "Variant Inventory Qty",
  "Variant Inventory Policy",
  "Variant Fulfillment Service",
  "Variant Price",
  "Variant Compare At Price",
  "Variant Requires Shipping",
  "Variant Taxable",
  "Variant Barcode",
  "Image Src",
  "Image Position",
  "Image Alt Text",
  "Gift Card",
  "SEO Title",
  "SEO Description",
  "Google Shopping / Google Product Category",
  "Google Shopping / Gender",
  "Google Shopping / Age Group",
  "Google Shopping / MPN",
  "Google Shopping / Condition",
  "Google Shopping / Custom Product",
  "Google Shopping / Custom Label 0",
  "Google Shopping / Custom Label 1",
  "Google Shopping / Custom Label 2",
  "Google Shopping / Custom Label 3",
  "Google Shopping / Custom Label 4",
  "Metafield: custom.asin [single_line_text_field]",
  "Metafield: custom.location [list.single_line_text_field]",
  "Metafield: mm-google-shopping.custom_product [boolean]",
  "Metafield: shopify.color-pattern [list.metaobject_reference]",
  "Metafield: shopify.connection-type [list.metaobject_reference]",
  "Metafield: shopify.cosmetic-condition [list.metaobject_reference]",
  "Metafield: shopify.material [list.metaobject_reference]",
  "Metafield: shopify.memory-technology [list.metaobject_reference]",
  "Metafield: shopify.operating-system [list.metaobject_reference]",
  "Metafield: shopify.processor-family [list.metaobject_reference]",
  "Variant Image",
  "Variant Weight Unit",
  "Variant Tax Code",
  "Cost per item",
  "Status",
  "product_group"
)

# adding metafields
listings <- listings %>%
  mutate(
    `Metafield: title_tag [string]` = NA_character_,
    `Metafield: description_tag [string]` = NA_character_,
    `Metafield: shopify.operating-system [list.metaobject_reference]` = NA_character_,
    `Metafield: mm-google-shopping.google_product_category [string]` = NA_character_,
    `Metafield: mm-google-shopping.condition [string]` = NA_character_
  )

# refining data
listings <- listings %>%
  mutate(across(where(is.character), ~ replace(., is.na(.), "")))

listings <- listings %>%
  mutate(`Variant Compare At Price` = as.numeric(`Variant Compare At Price`))

listings <- listings[, -c(7, 11, 14, 17, 49, 51)]
listings <- listings[, c(1:5, 55, 6:54, 56:ncol(listings))]

colnames(listings)[6] <- "Type"
colnames(listings)[7] <- "Tags"


# saving file
write.csv(listings, "C:\\Users\\Seller_King\\Desktop\\listings.csv", row.names = FALSE, na = "")












# best buy
best_buy <- MFA_info %>%
  filter(product_group %in% c("Personal Computer", "Electronics, Office Product", "Wireless, Home Entertainment", "Baby Product", "Camera", "Electronics", "Home", "Home Entertainment", "Kitchen", "Luggage", "Major Appliances", "Musical Instruments", "Office Product", "Personal Care Appliances", "Personal Computer", "Sports", "Tools", "Toy", "Video Games", "Wireless"))

best_buy <- best_buy[, c(2, 3, 84, 86, 87, 94, 98, 99, 101, 102, 109, 81, 82)]

best_buy <- best_buy %>%
  left_join(MFA_p_q %>% select(ASIN, price), by = "ASIN")

best_buy <- best_buy %>%
  separate(Image, into = paste0("image", 1:20), sep = ";", fill = "right", extra = "drop")


