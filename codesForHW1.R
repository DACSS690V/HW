
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

# you start from here
absoluteT=table(cityEduwa$LocaleSub)
(tableFreq=as.data.frame(absoluteT))
names(tableFreq)=c("Locale","Count")
tableFreq

propT=prop.table(absoluteT)*100
tableFreq$Percent=as.vector(propT)
tableFreq



# see data types ----------------------------------------------------------

str(cityEduwa)

# deliverable 1 ----------------------------------------------------------
library(ggplot2)
library(scales)

# Add LABELS column to tableFreq
tableFreq$LABELS <- paste0(round(tableFreq$Percent, 2), '%')

base <- ggplot(data = tableFreq, 
               aes(x = reorder(Locale, Percent), y = Percent)) +
  theme_classic()

plot1 <- base + geom_bar(fill = "blue", stat = 'identity')

titleText <- 'Percentages of Public Schools per City Size'
sub_titleText <- 'Washington State - 2019'
sourceText <- 'Source: US Department of Education'
x.AxisText <- "City Sizes"
y.AxisText <- "Percentage"

plot2 <- plot1 + labs(title = titleText,
                      x = x.AxisText, 
                      y = y.AxisText,
                      caption = sourceText)

plot3 <- plot2 + geom_hline(yintercept = 25, 
                            linetype = "dashed", 
                            linewidth = 1.5, 
                            alpha = 0.5)

plot4 <- plot3 + scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                                    limits = c(0, 50),
                                    labels = unit_format(suffix = '%'))

plot5 <- plot4 + theme(plot.caption = element_text(hjust = 0),
                       plot.title = element_text(hjust = 0.5))

# Use LABELS from the dataset
plot6 <- plot5 + geom_text(aes(label = LABELS),
                           vjust = 0, # hjust if flipping
                           size = 6) # fontface = "bold"

plot6

del1_hw1 <- plot6


# save RDS ----------------------------------------------------------
saveRDS(del1_hw1, file = "del1_hw1.rds")



