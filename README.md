# What Do Fish Like to Eat?
Firstly, import the dataset.
```{r}
library(readr)
fish <- read_delim("C:/Users/betha/Documents/Docs/Uni/Project/Data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
```
Increase the margins to see the labels on graphs.
```{r}
par(mar=c(5,6,4,1)+.1)
```
Create a table to look at the quality of the prey length to mass conversion.

Create new set which only includes prey with most accurate length to mass conversion.
```{r}
acc <- fish[which(fish$`Prey quality of conversion to mass` > 0),]
```
Create a plot of the predator mass against the prey mass.
```{r}
plot(acc$`SI predator mass`, acc$`SI prey mass`, pch=16, cex=0.4, col="#21618c40", xlab="Predator Mass (g)", ylab="Prey Mass (g)", cex.lab=1.5)
```
Create a plot of the logged predator and prey masses, and add a linear regression line.
```{r}
plot(log10(acc$`SI predator mass`), log10(acc$`SI prey mass`), pch=16, cex=0.4,  col="#21618c40", xlab=expression("log"[10]*"(predator mass)"), ylab=expression("log"[10]*"(prey mass)"), cex.lab=1.5)
reg <- lm(log10(acc$`SI prey mass`) ~ log10(acc$`SI predator mass`))
abline(reg, col="#c0392b", lwd = 2)
```
Similar graph but including a 99.9% confidence band.
```{r}
m <- ggplot(acc, aes(log10(acc$`SI prey mass`), log10(acc$`SI predator mass`))) + geom_point(col="#21618c40", pch=20) + theme_minimal() + labs(x=expression("log"[10]*"(predator mass)"), y=expression("log"[10]*"(prey mass)")) + theme(axis.title.y=element_text(size=30), axis.title.x=element_text(size=30), text = element_text(size=20))
m + geom_smooth(method = "lm", col = "red", level = 0.999)
```
Hypothesis test on gradient equalling 1.
```{r}
b1 <- lm(log10(acc$`SI prey mass`) ~ log10(acc$`SI predator mass`), offset = log10(acc$`SI predator mass`))
summary(b1)
stargazer(b1, summary=TRUE)
```

Reduce the data again by selecting only species which have more than 30 recorded encounters.
```{r}
mostfish <- acc[acc$`Predator common name` %in% names(which(table(acc$`Predator common name`) > 29)), ]
```
