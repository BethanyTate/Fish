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
Call  predator and prey mass columns new easier names.
```{r}
acc$SIpredmass <- acc$`SI predator mass`
acc$SIpreymass <- acc$`SI prey mass`
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
yft <- subset(acc, grepl("Yellowfin tuna", acc$`Predator common name`))
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
top47$prednames <- top47$unique.mostfish..Predator.common.name..
top47$unique.mostfish..Predator.common.name.. <- NULL
```
Remove predator record Icefish as the predator length to mass conversion for all records is 0 and hence not accurate enough.
```{r}
top47 <- top47[-c(44),]
```
Create a column to add the gradients.
```{r}
top47$beta <- NA
```
Calculate the gradient of the linear regression models for each of the predator species and add the value to top47.
```{r}
top47$beta[1] <- coef(lm(log10(ass$SIpreymass) ~ log10(ass$SIpredmass), data = ass))[2]
top47$beta[2] <- coef(lm(log10(bs$SIpreymass) ~ log10(bs$SIpredmass), data = bs))[2]
top47$beta[3] <- coef(lm(log10(ap$SIpreymass) ~ log10(ap$SIpredmass), data = ap))[2]
top47$beta[4] <- coef(lm(log10(abt$SIpreymass) ~ log10(abt$SIpredmass), data = abt))[2]
top47$beta[5] <- coef(lm(log10(ls$SIpreymass) ~ log10(ls$SIpredmass), data = ls))[2]
top47$beta[6] <- coef(lm(log10(ac$SIpreymass) ~ log10(ac$SIpredmass), data = ac))[2]
top47$beta[7] <- coef(lm(log10(awf$SIpreymass) ~ log10(awf$SIpredmass), data = awf))[2]
top47$beta[8] <- coef(lm(log10(swf$SIpreymass) ~ log10(swf$SIpredmass), data = swf))[2]
top47$beta[9] <- coef(lm(log10(yft$SIpreymass) ~ log10(yft$SIpredmass), data = yft))[2]
top47$beta[10] <- coef(lm(log10(bet$SIpreymass) ~ log10(bet$SIpredmass), data = bet))[2]
top47$beta[11] <- coef(lm(log10(mfl$SIpreymass) ~ log10(mfl$SIpredmass), data = mfl))[2]
top47$beta[12] <- coef(lm(log10(pfl$SIpreymass) ~ log10(pfl$SIpredmass), data = pfl))[2]
top47$beta[13] <- coef(lm(log10(ff$SIpreymass) ~ log10(ff$SIpredmass), data = ff))[2]
top47$beta[14] <- coef(lm(log10(lhs$SIpreymass) ~ log10(lhs$SIpredmass), data = lhs))[2]
top47$beta[15] <- coef(lm(log10(sr$SIpreymass) ~ log10(sr$SIpredmass), data = sr))[2]
top47$beta[16] <- coef(lm(log10(wp$SIpreymass) ~ log10(wp$SIpredmass), data = wp))[2]
top47$beta[17] <- coef(lm(log10(sh$SIpreymass) ~ log10(sh$SIpredmass), data = sh))[2]
top47$beta[18] <- coef(lm(log10(svh$SIpreymass) ~ log10(svh$SIpredmass), data = svh))[2]
top47$beta[19] <- coef(lm(log10(rh$SIpreymass) ~ log10(rh$SIpredmass), data = rh))[2]
top47$beta[20] <- coef(lm(log10(wf$SIpreymass) ~ log10(wf$SIpredmass), data = wf))[2]
top47$beta[21] <- coef(lm(log10(sf$SIpreymass) ~ log10(sf$SIpredmass), data = sf))[2]
top47$beta[22] <- coef(lm(log10(lsk$SIpreymass) ~ log10(lsk$SIpredmass), data = lsk))[2]
top47$beta[23] <- coef(lm(log10(bf$SIpreymass) ~ log10(bf$SIpredmass), data = bf))[2]
top47$beta[24] <- coef(lm(log10(ps$SIpreymass) ~ log10(ps$SIpredmass), data = ps))[2]
top47$beta[25] <- coef(lm(log10(cs$SIpreymass) ~ log10(cs$SIpredmass), data = cs))[2]
top47$beta[26] <- coef(lm(log10(psl$SIpreymass) ~ log10(psl$SIpredmass), data = psl))[2]
top47$beta[27] <- coef(lm(log10(lc$SIpreymass) ~ log10(lc$SIpredmass), data = lc))[2]
top47$beta[28] <- coef(lm(log10(lp$SIpreymass) ~ log10(lp$SIpredmass), data = lp))[2]
top47$beta[29] <- coef(lm(log10(kg$SIpreymass) ~ log10(kg$SIpredmass), data = kg))[2]
top47$beta[30] <- coef(lm(log10(g$SIpreymass) ~ log10(g$SIpredmass), data = g))[2]
top47$beta[31] <- coef(lm(log10(s$SIpreymass) ~ log10(s$SIpredmass), data = s))[2]
top47$beta[32] <- coef(lm(log10(ws$SIpreymass) ~ log10(ws$SIpredmass), data = ws))[2]
top47$beta[33] <- coef(lm(log10(sd$SIpreymass) ~ log10(sd$SIpredmass), data = sd))[2]
top47$beta[34] <- coef(lm(log10(sdf$SIpreymass) ~ log10(sdf$SIpredmass), data = sdf))[2]
top47$beta[35] <- coef(lm(log10(w$SIpreymass) ~ log10(w$SIpredmass), data = w))[2]
top47$beta[36] <- coef(lm(log10(eh$SIpreymass) ~ log10(eh$SIpredmass), data = eh))[2]
top47$beta[37] <- coef(lm(log10(mf$SIpreymass) ~ log10(mf$SIpredmass), data = mf))[2]
top47$beta[38] <- coef(lm(log10(mg$SIpreymass) ~ log10(mg$SIpredmass), data = mg))[2]
top47$beta[39] <- coef(lm(log10(ba$SIpreymass) ~ log10(ba$SIpredmass), data = ba))[2]
top47$beta[40] <- coef(lm(log10(cr$SIpreymass) ~ log10(cr$SIpredmass), data = cr))[2]
top47$beta[41] <- coef(lm(log10(lsd$SIpreymass) ~ log10(lsd$SIpredmass), data = lsd))[2]
top47$beta[42] <- coef(lm(log10(jd$SIpreymass) ~ log10(jd$SIpredmass), data = jd))[2]
top47$beta[43] <- coef(lm(log10(as$SIpreymass) ~ log10(as$SIpredmass), data = as))[2]
top47$beta[44] <- coef(lm(log10(af$SIpreymass) ~ log10(af$SIpredmass), data = af))[2]
top47$beta[45] <- coef(lm(log10(wh$SIpreymass) ~ log10(wh$SIpredmass), data = wh))[2]
top47$beta[46] <- coef(lm(log10(sb$SIpreymass) ~ log10(sb$SIpredmass), data = sb))[2]
```
Add a column for the species PPMR.
```{r}
top47$PPMR <- NA
```
Input the species PPMRs.
```{r}
top47$PPMR[1] <- sum(ass$SIpredmass)/sum(ass$SIpreymass)
top47$PPMR[2] <- sum(bs$SIpredmass)/sum(bs$SIpreymass)
top47$PPMR[3] <- sum(ap$SIpredmass)/sum(ap$SIpreymass)
top47$PPMR[4] <- sum(abt$SIpredmass)/sum(abt$SIpreymass)
top47$PPMR[5] <- sum(ls$SIpredmass)/sum(ls$SIpreymass)
top47$PPMR[6] <- sum(ac$SIpredmass)/sum(ac$SIpreymass)
top47$PPMR[7] <- sum(awf$SIpredmass)/sum(awf$SIpreymass)
top47$PPMR[8] <- sum(swf$SIpredmass)/sum(swf$SIpreymass)
top47$PPMR[9] <- sum(yft$SIpredmass)/sum(yft$SIpreymass)
top47$PPMR[10] <- sum(bet$SIpredmass)/sum(bet$SIpreymass)
top47$PPMR[11] <- sum(mfl$SIpredmass)/sum(mfl$SIpreymass)
top47$PPMR[12] <- sum(pfl$SIpredmass)/sum(pfl$SIpreymass)
top47$PPMR[13] <- sum(ff$SIpredmass)/sum(ff$SIpreymass)
top47$PPMR[14] <- sum(lhs$SIpredmass)/sum(lhs$SIpreymass)
top47$PPMR[15] <- sum(sr$SIpredmass)/sum(sr$SIpreymass)
top47$PPMR[16] <- sum(wp$SIpredmass)/sum(wp$SIpreymass)
top47$PPMR[17] <- sum(sh$SIpredmass)/sum(sh$SIpreymass)
top47$PPMR[18] <- sum(svh$SIpredmass)/sum(svh$SIpreymass)
top47$PPMR[19] <- sum(rh$SIpredmass)/sum(rh$SIpreymass)
top47$PPMR[20] <- sum(wf$SIpredmass)/sum(wf$SIpreymass)
top47$PPMR[21] <- sum(sf$SIpredmass)/sum(sf$SIpreymass)
top47$PPMR[22] <- sum(lsk$SIpredmass)/sum(lsk$SIpreymass)
top47$PPMR[23] <- sum(bf$SIpredmass)/sum(bf$SIpreymass)
top47$PPMR[24] <- sum(ps$SIpredmass)/sum(ps$SIpreymass)
top47$PPMR[25] <- sum(cs$SIpredmass)/sum(cs$SIpreymass)
top47$PPMR[26] <- sum(psl$SIpredmass)/sum(psl$SIpreymass)
top47$PPMR[27] <- sum(lc$SIpredmass)/sum(lc$SIpreymass)
top47$PPMR[28] <- sum(lp$SIpredmass)/sum(lp$SIpreymass)
top47$PPMR[29] <- sum(kg$SIpredmass)/sum(kg$SIpreymass)
top47$PPMR[30] <- sum(g$SIpredmass)/sum(g$SIpreymass)
top47$PPMR[31] <- sum(s$SIpredmass)/sum(s$SIpreymass)
top47$PPMR[32] <- sum(ws$SIpredmass)/sum(ws$SIpreymass)
top47$PPMR[33] <- sum(sd$SIpredmass)/sum(sd$SIpreymass)
top47$PPMR[34] <- sum(sdf$SIpredmass)/sum(sdf$SIpreymass)
top47$PPMR[35] <- sum(w$SIpredmass)/sum(w$SIpreymass)
top47$PPMR[36] <- sum(eh$SIpredmass)/sum(eh$SIpreymass)
top47$PPMR[37] <- sum(mf$SIpredmass)/sum(mf$SIpreymass)
top47$PPMR[38] <- sum(mg$SIpredmass)/sum(mg$SIpreymass)
top47$PPMR[39] <- sum(ba$SIpredmass)/sum(ba$SIpreymass)
top47$PPMR[40] <- sum(cr$SIpredmass)/sum(cr$SIpreymass)
top47$PPMR[41] <- sum(lsd$SIpredmass)/sum(lsd$SIpreymass)
top47$PPMR[42] <- sum(jd$SIpredmass)/sum(jd$SIpreymass)
top47$PPMR[43] <- sum(as$SIpredmass)/sum(as$SIpreymass)
top47$PPMR[44] <- sum(af$SIpredmass)/sum(af$SIpreymass)
top47$PPMR[45] <- sum(wh$SIpredmass)/sum(wh$SIpreymass)
top47$PPMR[46] <- sum(sb$SIpredmass)/sum(sb$SIpreymass)
```

