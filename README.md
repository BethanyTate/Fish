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
Get values for residuals.
```{r}
lm <- lm(log10(acc$`SI predator mass`) ~ log10(acc$`SI prey mass`))
resid <- residuals(lm)
mn <- mean(resid)
std <- sqrt(var(resid))
```
Create a histogram of the residuals with a normal curve for comparison.
```{r}
hist(resid, xlab="Residuals", ylab="Density", freq=FALSE, ylim=c(0,0.45), cex.lab=1.5)
curve(dnorm(x, mean=mn, sd=std), col="red", lwd=2, add=TRUE, yaxt="n")
```
Create Q-Q plot of residuals.
```{r}
qqnorm(residuals(lm), cex.lab= 1.7, pch=16, main=NULL)
qqline(residuals(lm), col="red", lwd=2)
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
```
Add a column to the dataset for the individual-link PPMR for each observation.
```{r}
acc$PPMR <- acc$`SI predator mass`/acc$`SI prey mass`
```
Create a plot of logged predator mass against logged individual-link PPMR.
```{r}
p <- ggplot(acc, aes(log10(acc$PPMR) , log10(acc$`SI predator mass`))) + geom_point(col="#21618c40", pch=20) + theme_minimal() + labs(x=expression("log"[10]*"(predator mass)"), y=expression("log"[10]*"(PPMR)")) + theme(axis.title.y=element_text(size=30), axis.title.x=element_text(size=30), text = element_text(size=20))
p + geom_smooth(method = "lm", col = "red", level = 0.999)
```
Select only species which have more than 30 recorded encounters for analysis on individual species.
```{r}
mostfish <- acc[acc$`Predator common name` %in% names(which(table(acc$`Predator common name`) > 29)), ]
```
Create individual plots of logged predator masses against logged prey masses for each species.
```{r}
species <- ggplot(mostfish, aes(log10(mostfish$`SI predator mass`), log10(mostfish$`SI prey mass`))) + geom_point(col="#21618c40", pch=20) + theme_minimal() + labs(x=expression("log"[10]*"(predator mass)"), y=expression("log"[10]*"(prey mass)"))
species + facet_wrap(vars(mostfish$`Predator common name`)) + theme(axis.title.y=element_text(size=25), axis.title.x=element_text(size=25))
```
Split most accurate data (acc) depending on if the interaction is piscivorous/predacious or planktivorous.
```{r}
plank <- subset(acc, acc$`Type of feeding interaction` == "planktivorous")
pisc <- subset(acc, acc$`Type of feeding interaction` == "piscivorous"| acc$`Type of feeding interaction` == "predacious"| acc$`Type of feeding interaction` == "predacious/piscivorous")
```
Create separate plots and summaries of logged predator mass against logged prey mass for predacious/piscivorous and planktivorous predators.
Predacious/piscivorous:
```{r}
plot(log10(pisc$`SI predator mass`), log10(pisc$`SI prey mass`), pch=16, cex=0.4,  col="#21618c40", xlab=expression("log"[10]*"(predator mass)"), ylab=expression("log"[10]*"(prey mass)"), cex.lab=1.5, main = "Predacious/Piscivorous")
piscreg <- lm(log10(pisc$`SI prey mass`) ~ log10(pisc$`SI predator mass`))
abline(piscreg, col="#c0392b", lwd = 2)
summary(piscreg)
```
Planktivorous:
```{r}
plot(log10(plank$`SI predator mass`), log10(plank$`SI prey mass`), pch=16, cex=0.4,  col="#21618c40", xlab=expression("log"[10]*"(predator mass)"), ylab=expression("log"[10]*"(prey mass)"), cex.lab=1.5, main = "Planktivorous")
plankreg <- lm(log10(plank$`SI prey mass`) ~ log10(plank$`SI predator mass`))
abline(plankreg, col="#c0392b", lwd = 2)
summary(plankreg)
```
Offset linear models for planktivorous and piscivorous/predacious fish to test null hypothesis of gradient = 1.
Predacious/piscivorous:
```{r}
bpisc <- lm(log10(pisc$`SI prey mass`) ~ log10(pisc$`SI predator mass`), offset = log10(pisc$`SI predator mass`))
summary(bpisc)
```
Planktivorous:
```{r}
bplank <- lm(log10(plank$`SI prey mass`) ~ log10(plank$`SI predator mass`), offset = log10(plank$`SI predator mass`))
summary(bplank)
```
Create new frames including records with each species.
```{r}
ass <- subset(acc, grepl("Atlantic sharpnose shark", acc$`Predator common name`))
bs <- subset(acc, grepl("Blacktip shark", acc$`Predator common name`))
ap <- subset(acc, grepl("Alaskan pollack", acc$`Predator common name`))
abt <- subset(acc, grepl("Atlantic bluefin tuna", acc$`Predator common name`))
ls <- subset(acc, grepl("Longfin squid", acc$`Predator common name`))
ac <- subset(acc, grepl("Atlantic cod", acc$`Predator common name`))
awf <- subset(acc, grepl("Atlantic wolf fish", acc$`Predator common name`))
swf <- subset(acc, grepl("Spotted wolf fish", acc$`Predator common name`))
bet <- subset(acc, grepl("Bigeye tuna", acc$`Predator common name`))
mfl <- subset(acc, grepl("Myctophidae fish larva", acc$`Predator common name`))
pfl <- subset(acc, grepl("Paralepididae fish larva", acc$`Predator common name`))
ff <- subset(acc, grepl("Fourspot flounder", acc$`Predator common name`))
lhs <- subset(acc, grepl("Longhorn sculpin", acc$`Predator common name`))
sr <- subset(acc, grepl("Sea raven", acc$`Predator common name`))
wp <- subset(acc, grepl("Windowpane", acc$`Predator common name`))
sh <- subset(acc, grepl("Spotted hake", acc$`Predator common name`))
svh <- subset(acc, grepl("Silver hake", acc$`Predator common name`))
rh <- subset(acc, grepl("Red hake", acc$`Predator common name`))
wf <- subset(acc, grepl("Weakfish", acc$`Predator common name`))
sf <- subset(acc, grepl("Summer flounder", acc$`Predator common name`))
lsk <- subset(acc, grepl("Little skate", acc$`Predator common name`))
bf <- subset(acc, grepl("Bluefish", acc$`Predator common name`))
ps <- subset(acc, grepl("Pink salmon", acc$`Predator common name`))
cs <- subset(acc, grepl("Chum salmon", acc$`Predator common name`))
psl <- subset(acc, grepl("Pacific sandlance", acc$`Predator common name`))
lc <- subset(acc, grepl("Lingcod", acc$`Predator common name`))
lp <- subset(acc, grepl("Longsnout prickleback", acc$`Predator common name`))
kg <- subset(acc, grepl("Kelp greenling", acc$`Predator common name`))
g <- subset(acc, grepl("Goosefish", acc$`Predator common name`))
s <- subset(acc, grepl("Saithe", acc$`Predator common name`))
ws <- subset(acc, grepl("Winter skate", acc$`Predator common name`))
sd <- subset(acc, grepl("Spurdog / spiny dogfish", acc$`Predator common name`))
sdf <- subset(acc, grepl("Smooth dogfish", acc$`Predator common name`))
w <- subset(acc, grepl("Whiting", acc$`Predator common name`))
eh <- subset(acc, grepl("European hake", acc$`Predator common name`))
mf <- subset(acc, grepl("Monkfish", acc$`Predator common name`))
mg <- subset(acc, grepl("Megrim", acc$`Predator common name`))
ba <- subset(acc, grepl("Black-bellied angler", acc$`Predator common name`))
cr <- subset(acc, grepl("Cuckoo ray", acc$`Predator common name`))
lsd <- subset(acc, grepl("Lesser spotted dogfish", acc$`Predator common name`))
jd <- subset(acc, grepl("John dory", acc$`Predator common name`))
as <- subset(acc, grepl("Antarctic Silverfish", acc$`Predator common name`))
af <- subset(acc, grepl("Antarctic fish", acc$`Predator common name`))
wh <- subset(acc, grepl("White hake", acc$`Predator common name`))
sd <- subset(acc, grepl("European sea bass", acc$`Predator common name`))
sb <- subset(acc, grepl("European sea bass", acc$`Predator common name`))
sd <- subset(acc, grepl("Spurdog / spiny dogfish", acc$`Predator common name`))
```
Create a table with the predator common names of the 47 most common predators.
```{r}
top47 <- data.frame(unique(mostfish$`Predator common name`))
```

