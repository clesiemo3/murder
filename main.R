library(gdata)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)


data_url <- "https://ucr.fbi.gov/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/offenses-known-to-law-enforcement/expanded-homicide/expanded_homicide_data_table_10_murder_circumstances_by_relationship_2013.xls/output.xls"
download.file(data_url, "dat.xls")
df = read.xls("dat.xls", sheet = 1, header = TRUE, skip = 3, stringsAsFactors = FALSE)

#filter blank and comment records
df <- df %>% head(nrow(df)-5) %>% select(-X, -X.1, -X.2)

# data type
df[,2:ncol(df)] <- sapply(df[,2:ncol(df)], FUN = function(x) as.integer(gsub(",", "", x)))

#remove footer notation
df$Circumstances <- gsub("[0-9]", "", df$Circumstances)

#split out summary records
df.summ <- df %>% filter(grepl("(Total|:)", Circumstances))

# category
category_id <- grep("(type)", df$Circumstances)
rn <- as.integer(row.names(df))
c1 <- rn > category_id[1] & rn < category_id[2]
c2 <- rn == category_id[2]
c3 <- rn > category_id[3]
df$category <- ""
df[c1,] <- df %>% filter(c1) %>% mutate(category = "Felony")
df[c2,] <- df %>% filter(c2) %>% mutate(category = "Suspected Felony")
df[c3,] <- df %>% filter(c3) %>% mutate(category = "Non-Felony")

df <- df %>% filter(!grepl("(Total|:)", Circumstances))
df <- df %>% arrange(-Total.murder.victims)

plot_dat <- df %>% top_n(n = 10, wt = Total.murder.victims)
lvls <- unique(plot_dat$Circumstances)
plot_dat$Circumstances <- factor(plot_dat$Circumstances, levels = rev(lvls))
ggp1 <- ggplot(plot_dat, aes(x = Circumstances, y = Total.murder.victims, fill = category)) +
	geom_bar(stat = "identity", position = "dodge") +
	labs(y = "Total Murder Victims") +
	ggtitle("Total Murder Victims by Circumstance") +
	coord_flip()

robbery <- df %>% filter(Circumstances == "Robbery")
rob.melt <- melt(robbery, id.vars = c("Circumstances","category"))
rob.melt <- rob.melt %>% filter(variable != "Total.murder.victims", value > 0)

rob.melt <- rob.melt %>% arrange(-value)
lvls <- unique(rob.melt$variable)
rob.melt$variable <- factor(rob.melt$variable, levels = rev(lvls))
ggp2 <- ggplot(rob.melt, aes(x = variable, y = value, fill = variable)) +
	geom_bar(stat = "identity") +
	labs(x = "Victim", y = "Number of Victims") +
	ggtitle("Number of Robbery Murder Victims by Relationship") +
	coord_flip()

png("MurderStats.png", width=20, height=10, units="in", res=300)
grid.arrange(ggp1, ggp2, top="FBI Murder Statistics for 2013", ncol = 2)
dev.off()
