# Script to produce Figure 5 in the main text
# Please note that some post editing of the plot was then carried out in
# Adobe illustrator to ensure the spacing and legends were legible

# Author: Hannah Buckland
# Date: 14/07/2021

# Written for the manuscript "Modelling the transport and deposition of ash
# following a Magnitude 7 eruption: the distal Mazama tephra"
# Submitted to Bull. Volc

# Read in csv files of grain size distributions
GSD_M16 <- read.csv("data/GSD_M16.csv")
GSD_M14 <- read.csv("data/GSD_M14.csv")
GSD_B21 <- read.csv("data/GSD_B21.csv")


# Mutate the data frames into long format separating out the aggregates and 
# individual particles for each GSD 

GSD_M16_long <- GSD_M16 %>%
  dplyr::select(c(phi_full,ind_mass_phi,agg_mass_phi)) %>%
  filter(!is.na(phi_full)) %>%
  pivot_longer(cols=!phi_full)

GSD_M16_agg <- GSD_M16 %>%
  dplyr::select(!c(phi_full,ind_mass_phi,agg_mass_phi)) %>%
  pivot_longer(cols=!agg_phi)

GSD_B21_long100C <- GSD_B21 %>%
  dplyr::select(c(phi_full,ind_mass_phi_100_C,agg_mass_phi_100_C)) %>%
  filter(!is.na(phi_full)) %>%
  pivot_longer(cols=!phi_full)

GSD_B21_agg100C <- GSD_B21 %>%
  dplyr::select(c(agg_phi,agg_massdis_100_C)) %>%
  filter(!is.na(agg_phi)) %>%
  pivot_longer(cols=!agg_phi)

GSD_B21_long100F <- GSD_B21 %>%
  dplyr::select(c(phi_full,ind_mass_phi_100_F,agg_mass_phi_100_F)) %>%
  filter(!is.na(phi_full)) %>%
  pivot_longer(cols=!phi_full)

GSD_B21_agg100F <- GSD_B21 %>%
  dplyr::select(c(agg_phi,agg_massdis_100_F)) %>%
  filter(!is.na(agg_phi)) %>%
  pivot_longer(cols=!agg_phi)


GSD_M16plt <- ggplot(GSD_M16_long) + 
  geom_col(aes(x=phi_full-0.5,y=value,fill=name)) +
  scale_x_reverse(limits= c(10,-2),breaks = seq(-2,10,by=2)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("GSD_M16") +
  scale_fill_manual(values = c("#E34234","grey20")) +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        text = element_text(size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"),
        panel.spacing = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=12),
        axis.text.y = element_text(colour = "black",size=12),
        axis.title.x = element_text(vjust = -0.5),
        legend.position = "none",
        legend.text.align = 0,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

agg_M16_plt <- ggplot(GSD_M16_agg) +
  geom_col(aes(x=agg_phi-0.05,y=value),fill = "#E34234") +
  facet_wrap(~name,ncol=1,nrow=2) +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0.01,0.01), 
                     limits=c(0,0.4),
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = c(0,0.1,0.2,0.3,0.4)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("Aggregates") +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(size = 10,hjust=0.5),
        text = element_text(size = 9),
        plot.margin = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=8),
        axis.text.y = element_text(colour = "black",size=8),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GSD_M14plt <- ggplot(GSD_M14) + 
  geom_col(aes(x=phi_full-0.5,y=ind_mass_phi),fill ="grey20") +
  scale_x_reverse(limits= c(10,-2),breaks = seq(-2,10,by=2)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("GSD_M14") +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        text = element_text(size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"),
        panel.spacing = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=12),
        axis.text.y = element_text(colour = "black",size=12),
        axis.title.x = element_text(vjust = -0.5),
        legend.position = "none",
        legend.text.align = 0,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GSD_B21_Cplt <- ggplot() + 
  geom_col(data = GSD_B21,
           aes(x=phi_full-0.5,y=no_agg_mass_C),fill = "grey75") +
  geom_col(data = GSD_B21_long100C,
           aes(x=phi_full-0.5,y=value,fill=name),width = 0.5) +
  scale_x_reverse(limits= c(10,-2),breaks = seq(-2,10,by=2)) +
  scale_y_continuous(limits=c(0,1),
                     expand = c(0.01,0.01),
                     breaks = seq(0,1,by=0.2)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("GSD_B21 Bimodal") +
  scale_fill_manual(values = c("#E34234","grey20")) +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        text = element_text(size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"),
        panel.spacing = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=12),
        axis.text.y = element_text(colour = "black",size=12),
        axis.title.x = element_text(vjust = -0.5),
        legend.position = "none",
        legend.text.align = 0,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

agg_B21_Cplt <- ggplot(GSD_B21_agg100C) +
  geom_col(aes(x=agg_phi-0.05,y=value),fill = "#E34234") +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0.01,0.01), 
                     limits=c(0,0.4),
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = c(0,0.1,0.2,0.3,0.4)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("Aggregates") +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(size = 10,hjust=0.5),
        text = element_text(size = 9),
        plot.margin = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=8),
        axis.text.y = element_text(colour = "black",size=8),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

GSD_B21_Fplt <- ggplot() + 
  geom_col(data = GSD_B21,
           aes(x=phi_full-0.5,y=no_agg_mass_F),fill = "grey75") +
  geom_col(data = GSD_B21_long100F,
           aes(x=phi_full-0.5,y=value,fill=name),width = 0.5) +
  scale_x_reverse(limits= c(10,-2),breaks = seq(-2,10,by=2)) +
  scale_y_continuous(limits=c(0,1),
                     expand = c(0.01,0.01),
                     breaks = seq(0,1,by=0.2)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("GSD_B21 Unimodal") +
  scale_fill_manual(values = c("#E34234","grey20")) +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        text = element_text(size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5,unit="cm"),
        panel.spacing = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=12),
        axis.text.y = element_text(colour = "black",size=12),
        axis.title.x = element_text(vjust = -0.5),
        legend.position = "none",
        legend.text.align = 0,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

agg_B21_Fplt <- ggplot(GSD_B21_agg100F) +
  geom_col(aes(x=agg_phi-0.05,y=value),fill = "#E34234") +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0.01,0.01), 
                     limits=c(0,0.4),
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = c(0,0.1,0.2,0.3,0.4)) +
  xlab("Grain size (phi)") +
  ylab("Mass Fraction") +
  ggtitle("Aggregates") +
  theme(aspect.ratio = 1,
        panel.background= element_rect(colour="black",fill = NA),
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(size = 10,hjust=0.5),
        text = element_text(size = 9),
        plot.margin = margin(1,1,1,1,unit="cm"),
        axis.text.x = element_text(colour = "black",size=8),
        axis.text.y = element_text(colour = "black",size=8),
        axis.title.x = element_text(vjust = -0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


GSD_M14plt <- GSD_M14plt 
GSD_M16plts <- GSD_M16plt + inset_element(agg_M16_plt, left =-0.35, bottom =0.4, top=1.09,right=0.85) 

GSD_B21_Cplts <- GSD_B21_Cplt + inset_element(agg_B21_Cplt, left =-0.35, bottom =0.4, top=1.09,right=0.85) 
GSD_B21_Fplts <- GSD_B21_Fplt + inset_element(agg_B21_Fplt, left =-0.35, bottom =0.4, top=1.09,right=0.85) 

out <-GSD_M16plts + GSD_M14plt + GSD_B21_Cplts + GSD_B21_Fplts +
  plot_layout(nrow=2, ncol=2) +
  theme(plot.tag = element_text(size = 18, hjust = 0, vjust = 0))

ggsave("plots/Fig05_GSD_model.pdf",out,width=174,height=200, units = "mm")
