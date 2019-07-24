---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
setwd("S:/Indiana Research & Evaluation/CCPE/Concept Mapping/Data")
library(readr)
FISCH_Concept_Mapping_Data = read_csv("S:/Indiana Research & Evaluation/CCPE/Concept Mapping/Data/FISCH Concept Mapping Data.csv")
attach(FISCH_Concept_Mapping_Data)
head(FISCH_Concept_Mapping_Data)


#Create Data Frames for each item of interest
library(ggplot2)
library(ggrepel)
library(ggpubr)
feas = data.frame(c1_1f, c1_7f, c1_9f, c1_12f, c1_15f, c1_16f, c1_17f, c1_22f, c1_24f,	c1_26f,	c1_27f,	c1_32f,	c1_35f,	c2_2f,c2_25f,	c2_28f,	c2_29f,	c2_33f,	c2_34f,	c3_3f,	c3_19f,	c3_41f,	c3_45f,	c3_47f,	c3_49f,	c3_54f,	c4_4f,	c4_5f,	c4_6f,	c4_37f,	c4_46f,	c4_48f,	c4_51f,	c5_8f,	c5_18f,	c5_20f,	c5_30f,	c5_36f,	c5_39f,	c5_40f,	c5_42f,	c6_10f,	c6_21f,	c6_23f,	c6_31f,	c6_38f,	c6_43f,	c6_44f,	c6_50f,	c6_55f,	c7_11f,	c7_13f,	c7_14f,	c7_57f,	c8_52f,	c8_53f,	c8_56f,	c8_58f)
import = data.frame(c1_1i, c1_7i, c1_9i, c1_12i, c1_15i, c1_16i, c1_17i, c1_22i, c1_24i,	c1_26i,	c1_27i,	c1_32i,	c1_35i,	c2_2i,c2_25i,	c2_28i,	c2_29i,	c2_33i,	c2_34i,	c3_3i,	c3_19i,	c3_41i,	c3_45i,	c3_47i,	c3_49i,	c3_54i,	c4_4i,	c4_5i,	c4_6i,	c4_37i,	c4_46i,	c4_48i,	c4_51i,	c5_8i,	c5_18i,	c5_20i,	c5_30i,	c5_36i,	c5_39i,	c5_40i,	c5_42i,	c6_10i,	c6_21i,	c6_23i,	c6_31i,	c6_38i,	c6_43i,	c6_44i,	c6_50i,	c6_55i,	c7_11i,	c7_13i,	c7_14i,	c7_57i,	c8_52i,	c8_53i,	c8_56i,	c8_58i)
relevant = data.frame(c1_1, c1_7, c1_9, c1_12, c1_15, c1_16, c1_17, c1_22, c1_24,	c1_26,	c1_27,	c1_32,	c1_35,	c2_2,c2_25,	c2_28,	c2_29,	c2_33,	c2_34,	c3_3,	c3_19,	c3_41,	c3_45,	c3_47,	c3_49,	c3_54,	c4_4,	c4_5,	c4_6,	c4_37,	c4_46,	c4_48,	c4_51,	c5_8,	c5_18,	c5_20,	c5_30,	c5_36,	c5_39,	c5_40,	c5_42,	c6_10,	c6_21,	c6_23,	c6_31,	c6_38,	c6_43,	c6_44,	c6_50,	c6_55,	c7_11,	c7_13,	c7_14,	c7_57,	c8_52,	c8_53,	c8_56,	c8_58)

#Calculate Total Scores for each item
feasability = colMeans(feas, na.rm = TRUE)
importance = colMeans(import, na.rm = TRUE)
relevance = colMeans(relevant, na.rm = TRUE)

#Create Data Frames for Go Zones of interest
GoZone2 = data.frame(relevance, importance)
GoZone = data.frame(feasability, importance)

#Categorize B
GoZone$go_items = ifelse(GoZone$feasability > mean(GoZone$feasability) & GoZone$importance > mean(GoZone$importance), 1, 0)
GoZone$go_items = factor(GoZone$go_items)
mean_feas = data.frame(mean(GoZone$feasability))
colnames(mean_feas) = "mean_feas"
mean_import = mean(GoZone$importance)

plot1 = ggplot(GoZone, aes(x =  feasability, y = importance))+
  geom_point(position = "jitter", aes(color = go_items)) +
  geom_vline(data = mean_feas, aes(xintercept = mean_feas)) +
  geom_hline(yintercept = mean_import)

plot1

GoZone2$go_items2 = ifelse(GoZone2$relevance > mean(GoZone2$relevance) & GoZone2$importance > mean(GoZone2$importance), 1, 0)
GoZone2$go_items2 = factor(GoZone2$go_items2)
mean_relevant2 = data.frame(mean(GoZone2$relevance))
colnames(mean_relevant2) = "mean_relevant2"
mean_import2 = mean(GoZone2$importance)

rownames(GoZone2) = c("1","7","9","12","15","16","17","22","24","26","27","32","35","2","25","28","29","33","34","3","19","41","45","47","49","54","4","5","6","37","46","48","51","8","18","20","30","36","39","40","42","10","21","23","31","38","43","44","50","55","11","13","14","57","52","53","56","58")

plot2 = ggplot(GoZone2, aes(x =  relevance, y = importance))+
  geom_point(aes(color = go_items2, shape = go_items2), size = 3) + #can reinsert position = "jitter" here
  geom_vline(data = mean_relevant2, aes(xintercept = mean_relevant2)) +
  geom_hline(yintercept = mean_import2) +
  ggtitle("Average Statement Score for Relevance to Program and Importance to Program Success") +
  labs(color = "Statement Meets Cutoff", shape = "Statement Meets Cutoff", x = "Relevance", y = "Importance") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "darkturquoise")) +
  scale_shape_manual(labels = c("No", "Yes"), values = c(15,16)) +
  geom_text_repel(label = row.names(GoZone2), point.padding = 1) +
  theme_pubr()
            
plot2
