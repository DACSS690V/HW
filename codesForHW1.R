
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

plot3 <- plot2 + geom_hline(yintercept = 25, #where to put reference line 
                            linetype = "dashed", 
                            linewidth = 1, 
                            alpha = 0.5) #reference line

plot4 <- plot3 + scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                                    limits = c(0, 53), #max and min of the y axis
                                    labels = unit_format(suffix = '%')) #y axis information

plot5 <- plot4 + theme(axis.title.y = element_blank(),   # Remove the y-axis title
                       axis.text.y = element_blank(),    # Remove the y-axis labels
                       #axis.ticks.y = element_blank(),
                       plot.caption = element_text(hjust = 0), #caption in bottom right
                       plot.title = element_text(hjust = 0.5), #center title and subtitle
                       plot.subtitle = element_text(hjust = 0.5))

plot6 <- plot5 + geom_text(aes(label = LABELS), #LABELS column are the annotations
                           vjust = -.1,
                           size = 6) #had the annotations to the bars

plot6

del1_hw1 <- plot6


# save RDS ----------------------------------------------------------
saveRDS(del1_hw1, file = "del1_hw1.rds")



