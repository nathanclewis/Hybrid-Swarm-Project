## Setup -----

setwd("~/documents/Mandeville_Lab/Hybrid_Swarm/R/Data/")

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
theme_set(theme_pubr())

## Load Data -----

# Main dataset
df_hybridswarm <- read_csv("hybridswarm - Cleaned.csv")
view(df_hybridswarm)

# Author Wordcloud Datasheet
df_authors <- read_csv("hybridswarm - AuthorFrequency.csv")

## Basic Variable Plots -----

#Shows how many papers used the term 'hybrid swarm' each year
ggplot(df_hybridswarm, aes(Year)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


#Shows how many papers used the term 'hybrid swarm' to describe each type of organism
ggplot(data = subset(df_hybridswarm, !is.na(OrganismType)), aes(OrganismType)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


#Shows how many papers used the term 'hybrid swarm' to describe each genus
ggplot(data = subset(df_hybridswarm, !is.na(Genus)), aes(Genus)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


#Shows the (lack of a) relationship between Nloci and Nind. I thought there might be a negative relationship
ggplot(data = subset(df_hybridswarm, !is.na(Nind), !is.na(Nloci)), aes(Nind,Nloci)) +
  geom_point()
cor.test(df_hybridswarm$Nloci,df_hybridswarm$Nind)

#Shows how SNPs have become more commonplace in recent years. Bar labels are proving difficult
ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="fill", stat="count")

ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="stack", stat="count") 
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


#Shows how Microsats have become less commonplace in recent years. Bar labels are proving difficult
ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="fill", stat="count")
#geom_text(aes(y = 1, label = Percentage), vjust = (1.5), colour = "white")

ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="stack", stat="count")


#Shows the seemingly random frequency of phenotypic studies over time. Bar labels are proving difficult
ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")

ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")

#Shows how many papers used genetic and/or phenotypic evidence each year
ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")

ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")

#Shows how many papers do and do not have accessible data each year
ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable, x=Year)) + 
  geom_bar(position="dodge") +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)

ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable,x=Year)) + 
  geom_bar(position="fill", stat="count")

## Basics by Taxon -----

# Disturbance
df_disturbance <- df_hybridswarm[!is.na(df_hybridswarm$Disturbance),]
ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=OrganismType, fill=Disturbance, colour=Disturbance)) + 
  geom_bar(position="dodge") +
  labs(y = "Number of Publications", x = "Organism Type") +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)

## Author frequency -----

#More simplistic model
wordcloud(words = df_authors$Author, freq = df_authors$Frequency, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#Newer package with more options
wordcloud2(data = df_authors, size=0.5, color='random-dark')
