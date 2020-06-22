#Position Analysis

library(ballr)
library(ggplot2)
library(waffle)
library(RColorBrewer)
library(gridExtra)

playersAdv <- NBAPerGameAdvStatistics(season = 2020) # adv stats for a given season

#Manipulate the data:

adv <- playersAdv
table(adv$pos)
adv[adv=="C-PF"] <- "C"
adv[adv=="F"] <- "PF"
adv[adv=="G"] <- "SG"
adv[adv=="PF-SF"] <- "PF"
adv[adv=="SF-PF"] <- "SF"
adv[adv=="SF-SG"] <- "SF"
position_order <- c("PG","SG","SF","PF","C")
adv$pos <- factor( as.character(adv$pos), levels=position_order )



# Waffle chart
table(adv$pos)
test<- c("PG"=105, "SG"=159, "SF"=118, "PF"=131, "C"=103)
Waf <- waffle(test/10, rows = 5, colors = brewer.pal(5, "Accent"), 
              title= "Players Position Waffle Chart", flip=T)
Waf


#Density plots of vorp
vorpPG <- subset(adv, pos=="PG", select=c("pos","vorp"))
vorpSG <- subset(adv, pos=="SG", select=c("pos","vorp"))
vorpSF <- subset(adv, pos=="SF", select=c("pos","vorp"))
vorpPF <- subset(adv, pos=="PF", select=c("pos","vorp"))
vorpC <- subset(adv, pos=="C", select=c("pos","vorp"))

dPG <- ggplot(vorpPG, aes(x=vorp)) +
       geom_density(fill=brewer.pal(5, "Accent")[1])+
       theme_minimal()+
       labs(title="VORP Density Plot : Point Guards", x="VORP")

dSG <- ggplot(vorpSG, aes(x=vorp)) +
       geom_density(fill=brewer.pal(5, "Accent")[2])+
       theme_minimal()+
       labs(title="VORP Density Plot : Shooting Guards", x="VORP")

dSF <- ggplot(vorpSF, aes(x=vorp)) +
       geom_density(fill=brewer.pal(5, "Accent")[3])+
       theme_minimal()+
       labs(title="VORP Density Plot : Small Forwards", x="VORP")

dPF <- ggplot(vorpPF, aes(x=vorp)) +
       geom_density(fill=brewer.pal(5, "Accent")[4])+
      theme_minimal()+
      labs(title="VORP Density Plot : Power Forwards", x="VORP")

dC <- ggplot(vorpC, aes(x=vorp)) +
      geom_density(fill=brewer.pal(5, "Accent")[5])+
      theme_minimal()+
      labs(title="VORP Density Plot : Centers", x="VORP")

Dens <-grid.arrange(dPG,dSG,dSF,dPF,dC, ncol=3)



# Winshares boxplot:
Box <- ggplot(adv, aes(x=pos, y=ws, fill=pos)) + 
  geom_boxplot() +
  theme_minimal()+
  labs(title="Win Shares Boxplot per Position",x="Player Position", y = "Win Shares (WS)", 
       caption="Twitter: filippos_pol", fill = "Player Position")+
  scale_fill_brewer(palette="Accent")
Box


  





