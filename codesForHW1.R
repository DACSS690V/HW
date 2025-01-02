
# clean memory ------------------------------------------------------------
rm(list = ls()) # clean memory


# read in data ------------------------------------------------------------
#set working directory

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)


#getting the data TABLE from the file in the cloud:
load(file=url(link))


# see data ----------------------------------------------------------

head(eduwa)

table(eduwa$LocaleType)

cityEduwa=eduwa[eduwa$LocaleType=='City',]
table(cityEduwa$LocaleSub)

cityEduwa$LocaleSub=droplevels(cityEduwa$LocaleSub)

absoluteT=table(cityEduwa$LocaleSub)
(tableFreq=as.data.frame(absoluteT))
names(tableFreq)=c("Locale","Count")
tableFreq

#make the percentages
propT=prop.table(absoluteT)*100
tableFreq$Percent=as.vector(propT)
tableFreq

#make the locale table characters to change the names
tableFreq$Locale <- as.character(tableFreq$Locale)

tableFreq$Locale[tableFreq$Locale == "City: Small"] <- "Small Sized City"
tableFreq$Locale[tableFreq$Locale == "City: Midsize"] <- "Medium Sized City"
tableFreq$Locale[tableFreq$Locale == "City: Large"] <- "Large Sized City"

#return them to factors just in case
tableFreq$Locale <- factor(tableFreq$Locale)

#Have to make labels column to tableFreq for annotations to work
tableFreq$LABELS <- paste0(round(tableFreq$Percent, 2), '%')


# see data types ----------------------------------------------------------

str(cityEduwa)

# deliverable 1 ----------------------------------------------------------
library(ggplot2)
library(scales)

base <- ggplot(data = tableFreq, 
               aes(x = reorder(Locale, Percent), y = Percent)) +
  theme_classic()

plot1 <- base + geom_bar(fill = "blue", stat = 'identity')

titleText <- 'Which Size City Has The Most Public Schools?'
sub_titleText <- 'Washington State - 2019'
sourceText <- 'Source: US Department of Education\nFilter: Cities in Washington State'
x.AxisText <- "City Sizes"
y.AxisText <- "Percentage"

plot2 <- plot1 + labs(title = titleText,
                      subtitle = sub_titleText,
                      x = NULL, 
                      y = NULL,
                      caption = sourceText)
?labs

plot3 <- plot2 + geom_hline(yintercept = 25, 
                            linetype = "dashed", 
                            linewidth = 1, 
                            alpha = 0.5)

plot4 <- plot3 + scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                                    limits = c(0, 50),
                                    labels = unit_format(suffix = '%'))

plot5 <- plot4 + theme(plot.caption = element_text(hjust = 0),
                       plot.title = element_text(hjust = 0.5),
                       plot.subtitle = element_text(hjust = 0.5))

plot6 <- plot5 + geom_text(aes(label = LABELS),
                           vjust = -.1,
                           size = 6)

plot6

del1_hw1 <- plot6


# save RDS ----------------------------------------------------------
saveRDS(del1_hw1, file = "del1_hw1.rds")



