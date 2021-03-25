#Loading appropriate packages

library(tidyverse)
library(ggplot2)
library(janitor)
library(patchwork)
library(RColorBrewer)

#set working directory

setwd("C://Users//wjdavis//Documents//R_working_dir//NA_DM//evolution")

#Reading in the file with downy mildew species, removing the empty column,
#cleaning up the column names, reordering the columns, and filling in the
#column "mycorrhizae" based on mycorrhizas.info/nmplants.html

DM_tidy <-read.delim("DM_Mycorrhiza_210325-1.txt", header = TRUE) %>%
clean_names() %>%
subset(, select= -c(hosts, tree, deciduous_evergreen, x, x_1)) %>%
relocate(type_host_species_name, .before=host_family) %>%
mutate(mycorrhizae = case_when(host_family %in% c("Brassicaceae",
								"Cleomaceae",
								"Limnanthaceae",
								"Azioaceae",
								"Caryophyllaceae",
								"Orobanchaceae",
								"Polygonaceae",
								"Papaveraceae",
								"Amaranthaceae") ~ "nonmycorrhizal",
								TRUE ~ "mycorrhizal"))

write.table(DM_tidy, "DM_tidy_table.txt", sep="\t")

#Creating subsets of the data by genus

DM_tidy_PPH <-subset(DM_tidy,
	genus %in% c("Hyaloperonospora",
			"Peronospora",
			"Plasmopara"))

write.table(DM_tidy_PPH, "DM_tidy_Pero_Plasmo_Hyalo.txt", sep="\t")

DM_tidy_others <-subset(DM_tidy, !genus=="Hyaloperonospora" & !genus=="Peronospora" & !genus=="Plasmopara")

write.table(DM_tidy_others, "DM_tidy_others.txt", sep="\t")

#Tables

DM_tidy_mycorrhizae_table <-table(DM_tidy$genus, DM_tidy$mycorrhizae)

capture.output(table(DM_tidy$genus, DM_tidy$mycorrhizae), file="DM_tidy_mycorrhizae_table.txt")

DM_tidy_photosynthesis_table <-table(DM_tidy$genus, DM_tidy$photosynthesis)

capture.output(table(DM_tidy$genus, DM_tidy$photosynthesis), file="DM_tidy_photosynthesis_table.txt")

#Graphs

mycorr_PPH_plot <-ggplot(DM_tidy_PPH) +
				aes(x=genus, fill=mycorrhizae) +
				geom_bar() +
				scale_fill_manual(name= NULL,
							values= c("#a6cee3", "#33a02c"),
							labels=c("Mycorrhizal Host",
								"Non-mycorrhizal Host")) +
				theme(axis.title.x = element_blank())+
				labs(y="Number of Species") +
				theme(axis.title.y=element_text(size=12)) +
				theme(axis.text.y=element_text(size=10)) +
				theme(axis.text.x=element_text(vjust=0.5,
									size=10,
									face="italic")) +
				geom_text(aes(label=..count..), stat="count",
						vjust=-0.6, color="black")
 
photo_PPH_plot <-ggplot(DM_tidy_PPH) +
				aes(x=genus, fill=photosynthesis) +
				geom_bar() +
				scale_fill_manual(name= NULL,
							values= c("#b2df8a", "black", "#1f78b4"),
							labels= c("C3 Host",
								"C3/CAM Host",
								"C4 Host")) +
				theme(axis.title.x = element_blank())+
				labs(y="Number of Species") +
				theme(axis.title.y=element_text(size=12)) +
				theme(axis.text.y=element_text(size=10)) +
				theme(axis.text.x=element_text(vjust=0.5,
									size=10,
									face="italic")) +
				geom_text(aes(label=..count..), stat="count",
						vjust=-0.4, color="black")


mycorr_others_plot <-ggplot(DM_tidy_others) +
				aes(x=genus, fill=mycorrhizae) +
				geom_bar() +
				scale_fill_manual(name= NULL,
							values= c("#a6cee3", "#33a02c"),
							labels=c("Mycorrhizal Host",
								"Non-mycorrhizal Host")) +
				labs(x= "Genera", y="Number of Species") +
				theme(axis.title.y=element_text(size=12)) +
				theme(axis.title.x=element_text(size=12)) +
				theme(axis.text.y=element_text(size=10)) +
				theme(axis.text.x=element_text(angle=45,
									vjust=1,
									hjust=1,
									size=12,
									face="italic")) +
				geom_text(aes(label=..count..), stat="count",
						vjust=-0.4, color="black") +
				scale_y_continuous(expand=expansion(mult=c(0, .1)))



photo_others_plot <-ggplot(DM_tidy_others) +
				aes(x=genus, fill=photosynthesis) +
				geom_bar() +
				scale_fill_manual(name= NULL,
							values= c("#b2df8a", "#1f78b4"),
							labels= c("C3 Host", "C4 Host")) +
				labs(x= "Genera", y="Number of Species") +
				theme(axis.title.y=element_text(size=12)) +
				theme(axis.title.x=element_text(size=12)) +
				theme(axis.text.y=element_text(size=10)) +
				theme(axis.text.x=element_text(angle=45,
									vjust=1,
									hjust=1,
									size=12,
									face= c("bold.italic",
									"italic",
									"italic",
									"italic",
									"bold.italic",
									"bold.italic",
									"italic",
									"italic",
									"italic",
									"bold.italic",
									"italic",
									"bold.italic",
									"italic",
									"italic",
									"bold.italic",
									"bold.italic",
									"bold.italic"))) +
				geom_text(aes(label=..count..), stat="count",
						vjust=-0.15, color="black") +
				scale_y_continuous(expand=expansion(mult=c(0, .1)))




#can change the sizing of the output, see the R cookbook bookmark for how
pdf("mycorrhizae_plots_stacked.pdf")
mycorr_PPH_plot /mycorr_others_plot
dev.off()

pdf("photosynthesis_plots_stacked_bold.pdf")
photo_PPH_plot / photo_others_plot
dev.off()

