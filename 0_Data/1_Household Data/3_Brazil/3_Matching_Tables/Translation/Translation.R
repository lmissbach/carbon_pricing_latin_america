Item.Codes <- read.xlsx("../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Item_Codes_Description_Brazil.xlsx")

Translation  <- distinct(Item.Codes, Description)%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_A.xlsx")
TranslationB <- distinct(Item.Codes, Description_B)%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_B.xlsx")
TranslationC <- distinct(Item.Codes, Description_C)%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_C.xlsx")

Item.Code.A <- read.xlsx("../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_A.xlsx")
Item.Code.B <- read.xlsx("../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_B.xlsx")
Item.Code.C <- read.xlsx("../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Translation/Item_Codes_C.xlsx")

Item.Codes.1 <- Item.Codes %>%
  left_join(Item.Code.A)%>%
  left_join(Item.Code.B)%>%
  left_join(Item.Code.C)

write.xlsx(Item.Codes.1, "../0_Data/1_Household Data/3_Brazil/3_Matching_Tables/Item_Codes_Description_Brazil.xlsx")
