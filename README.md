# What Do Fish Like to Eat?
Firstly, import the dataset.
```{r}
fish <- read_delim("C:/Users/betha/Documents/Docs/Uni/Project/Data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
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

Reduce the data again by selecting only species which have more than 30 recorded encounters.
```{r}
mostfish <- acc[acc$`Predator common name` %in% names(which(table(acc$`Predator common name`) > 29)), ]
```
