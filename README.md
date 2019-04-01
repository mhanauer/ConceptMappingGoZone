---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Try developing a concept mapping with average separat for feas and import separating the two
```{r}
library(ggplot2)
feas = rnorm(58)
import = rnorm(58)
dat = data.frame(feas, import)
```
Need to identify those above average in both quads
```{r}
dat$go_items = ifelse(dat$feas > mean(dat$feas) & dat$import > mean(dat$import), 1, 0)
dat$go_items = factor(dat$go_items)
mean_feas = data.frame(mean(dat$feas))
colnames(mean_feas) = "mean_feas"
mean_import = mean(dat$import)
```
Develop a plot with two lines at average of each for x and y axis
```{r}
plot1 = ggplot(dat, aes(x =  feas, y = import))+
  geom_point( aes(color = go_items)) +
  geom_vline(data = mean_feas, aes(xintercept = mean_feas)) +
  geom_hline(yintercept = mean_import)
  
plot1

```

