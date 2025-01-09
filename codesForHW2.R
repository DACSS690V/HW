
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

#see it
library(rio)
library(dplyr)
arrests=rio::import(linkMass,which = 1)
codes_data=rio::import(linkMass,which = 2)

names(codes_data)
# Load necessary library
library(readxl)
library(dplyr)


# Rename columns in the Codes sheet
codes_data <- codes_data %>%
  rename(
    `Arrest Offense by UCR Code` = `Arrest Codes`,
    `Offense Description` = `...5`
  )

# Merge the datasets
merged_data <- arrests %>%
  left_join(codes_data[, c("Arrest Offense by UCR Code", "Offense Description")],
            by = "Arrest Offense by UCR Code")

# Replace the UCR Code with the description
merged_data <- merged_data %>%
  mutate(`Arrest Offense by UCR Code` = coalesce(`Offense Description`, `Arrest Offense by UCR Code`)) %>%
  select(-`Offense Description`)

# View the resulting data
print(head(merged_data))

library(dplyr)

# Update the Race column using case_when
merged_data <- merged_data %>%
  mutate(Race = case_when(
    Race == "B" ~ "Black",
    Race == "W" ~ "White",
    Race == "H" ~ "Hispanic",
    Race == "O" ~ "Asian",
    Race == "I" ~ "American Indian",
    Race == "U" ~ "Unknown",
    Race == "J" ~ "Middle Eastern",
    Race == "N" ~ "Not Applicable",
    TRUE ~ Race  # Retain original value if no match
  ))

merged_data <- merged_data %>%
  filter(!grepl("^\\d+$", `Arrest Offense by UCR Code`))

arrests <- merged_data %>%
  filter(`Arrest Offense by UCR Code` %in% codes_data$`Offense Description`)


# see data ----------------------------------------------------------

head(arrests)
names(arrests)

# see data types ----------------------------------------------------------

str(arrests)

# contingency table ----------------------------------------------------------

(contingency=table(arrests$Race,arrests$`Arrest Offense by UCR Code`))

#the marginal percent (per column) from contingency table
library(magrittr)

#making a data frame from contingency table
contingencyDF=as.data.frame(contingency)
#renaming:
names(contingencyDF)=c("Race","UCR_Code","counts")
#marginal
contingency_mgCol=100*prop.table(contingency,margin = 2)
#adding marginal
contingencyDF$pctCol=round(as.data.frame(contingency_mgCol)[,3],1)


# Filter the data for specific races
contingencyDF <- contingencyDF %>%
  filter(Race %in% c("Asian", 
                     "Black", 
                     "Hispanic", 
                     "White"))

# Filter for 8 specific crimes
contingencyDF <- contingencyDF %>%
  filter(UCR_Code %in% c("Rape", "Robbery", "Aggravated Assault", 
                         "DUI", "Weapon Law Violations", 
                         "Stolen Property Offenses", "Kidnapping", 
                         "All Other Offenses"))


contingencyDF <- contingencyDF |>
  select(UCR_Code,Race, counts, pctCol)
# result for ggplot:
head(contingencyDF,20)

contingencyDF <- contingencyDF %>%
  group_by(Race) %>%
  mutate(UCR_Code = fct_reorder(UCR_Code, pctCol, .desc = TRUE)) %>%
  ungroup()


# deliverable 2 ----------------------------------------------------------
library(ggplot2)


# as usual for barplot (less info than base1)
base4 = ggplot(contingencyDF, aes(x = UCR_Code, y = pctCol ) ) 

#the bars
bars4  = base4 + geom_bar( stat = "identity" , width = 0.7, fill = "blue") + theme_minimal()

# bar per day time with 'facet'
bars4 = bars4 + facet_grid(~ Race) 

barsFacet = bars4 + facet_grid(~ Race)  # X
# trick
barsFacet + coord_flip()

baseRE  = ggplot(contingencyDF, 
                 aes(x = reorder(UCR_Code, pctCol), #here
                     y = pctCol ) ) + theme_minimal()

barsRE = baseRE + geom_bar( stat = "identity", width = 0.7, fill = "lightblue") 
barsREFacet = barsRE + facet_grid( ~ Race) 
barsREFacet= barsREFacet + coord_flip() 

barsREFacet + theme(axis.text.y = element_text(size=4,angle = 20)) + 
  geom_text(aes(label=pctCol),
            nudge_y = 10)

barsREFacet + theme(axis.text.y = element_text(size=7,angle = 20)) + 
  geom_text(aes(label=ifelse(pctCol>5,# condition to annotate
                             pctCol,"")),
            nudge_y = 10)

barsREFacet <- barsREFacet + 
  theme(
    strip.text = element_text(size = 5, angle = 5, face = "bold"), # Increase race label size
  )

del1_hw2 <- barsREFacet + theme(axis.text.y = element_text(size=7,angle = 20), 
                                axis.text.x = element_text(size = 5)) + 
  geom_text(aes(label = ifelse(pctCol > 5, pctCol, "")),
            nudge_y = 8, size = 1.75, fontface = "bold", vjust = 0.5) + 

  labs(title= "Percentage of Arrests by UCR Code per Race",
       subtitle = "Arrest information from January 2019 to March 2020.",
       caption = "Massachusetts Arrest Data - Field Services and Investigative Divisions",
       x="",y="Percentages")

del1_hw2


# save del1Draft ----------------------------------------------------------
saveRDS(del1_hw2, file = "del1_hw2.rds")

