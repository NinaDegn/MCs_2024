setwd("~/Documents/Uni/Speciale/")
library(data.table)
library(readxl)
library(dplyr)
library(phytools)
library(ape)
library(geiger)
library(nlme)
library(ggplot2)
library(picante)
library(writexl)

#load tree data
tree<- read.nexus("~/Documents/Uni/Speciale/Data/correct_names_dated_phylo")

#load trait data
traitdata<-read_excel("~/Documents/Speciale2.0/Data/Specialedata.xlsx")

write_xlsx(traitdata,"specialedata_max.xlsx")


maxdata<-read_excel("~/Documents/Speciale2.0/Data/specialedata_max.xlsx")
bio1<-(maxdata$Bio1_max)

phylosignal(bio1, phy, reps = 999, checkdata=TRUE)

#Extraxt optimumvalue from each climavariable and create three new columns 
Bio1_Max<- traitdata[,c("Bio1_110","Bio1_128","Bio1_146","Bio1_164","Bio1_182","Bio1_200","Bio1_128","Bio1_236","Bio1_254","Bio1_272","Bio1_290")]
traitdata$Bio1_max <- colnames(Bio1_Max)[max.col(Bio1_Max, "first")]

Bio2_Max<- traitdata[,c("Bio2_43","Bio2_57","Bio2_71","Bio2_85","Bio2_99","Bio2_114","Bio2_128","Bio2_142","Bio2_156","Bio2_170","Bio2_184")]
traitdata$Bio2_max <- colnames(Bio2_Max)[max.col(Bio2_Max, "first")]

Bio3_Max<- traitdata[,c("Bio3_49","Bio3_52","Bio3_55","Bio3_57","Bio3_60","Bio3_63","Bio3_65","Bio3_68","Bio3_70","Bio3_73","Bio3_76")]
traitdata$Bio3_max <- colnames(Bio3_Max)[max.col(Bio3_Max, "first")]

Bio4_Max<- traitdata[,c("Bio4_666","Bio4_939","Bio4_1211","Bio4_1484","Bio4_1756","Bio4_2029","Bio4_2302","Bio4_2574","Bio4_2847","Bio4_3119","Bio4_3392")]
traitdata$Bio4_max <- colnames(Bio4_Max)[max.col(Bio4_Max, "first")]

Bio5_Max<- traitdata[,c("Bio5_188","Bio5_208","Bio5_227","Bio5_247","Bio5_266","Bio5_286","Bio6_305","Bio5_324","Bio5_344","Bio5_363","Bio5_383")]
traitdata$Bio5_max <- colnames(Bio5_Max)[max.col(Bio5_Max, "first")]

Bio6_Max<- traitdata[,c("Bio6_13","Bio6_34","Bio6_55","Bio6_77","Bio6_98","Bio6_119","Bio6_140","Bio6_161","Bio6_183","Bio6_204","Bio6_225")]
traitdata$Bio6_max <- colnames(Bio6_Max)[max.col(Bio6_Max, "first")]

Bio7_Max<- traitdata[,c("Bio7_88","Bio7_107","Bio7_125","Bio7_143","Bio7_161","Bio7_180","Bio7_198","Bio7_216","Bio7_234","Bio7_252","Bio7_271")]
traitdata$Bio7_max <- colnames(Bio7_Max)[max.col(Bio7_Max, "first")]

Bio8_Max<- traitdata[,c("Bio8_129","Bio8_146","Bio8_164","Bio8_181","Bio8_198","Bio8_215","Bio8_232","Bio8_249","Bio8_266","Bio8_284","Bio8_301")]
traitdata$Bio8_max <- colnames(Bio8_Max)[max.col(Bio8_Max, "first")]

Bio9_Max<- traitdata[,c("Bio9_82","Bio9_101","Bio9_120","Bio9_139","Bio9_158","Bio9_177","Bio9_195","Bio9_214","Bio9_233","Bio9_252","Bio9_271")]
traitdata$Bio9_max <- colnames(Bio9_Max)[max.col(Bio9_Max, "first")]

Bio10_Max<- traitdata[,c("Bio10_129","Bio10_146","Bio10_164","Bio10_181","Bio10_199","Bio10_217","Bio10_234","Bio10_252","Bio10_269","Bio10_287","Bio10_304")]
traitdata$Bio10_max <- colnames(Bio10_Max)[max.col(Bio10_Max, "first")]

Bio11_Max<- traitdata[,c("Bio11_82","Bio11_101","Bio11_119","Bio11_138","Bio11_157","Bio11_176","Bio11_195","Bio11_214","Bio11_233","Bio11_251","Bio11_270")]
traitdata$Bio11_max <- colnames(Bio11_Max)[max.col(Bio11_Max, "first")]

Bio12_Max<- traitdata[,c("Bio12_46","Bio12_398","Bio12_750","Bio12_1102","Bio12_1454","Bio12_1806","Bio12_2158","Bio12_2510","Bio12_2862","Bio12_3214","Bio12_3566")]
traitdata$Bio12_max <- colnames(Bio12_Max)[max.col(Bio12_Max, "first")]

Bio13_Max<- traitdata[,c("Bio13_33","Bio13_85","Bio13_138","Bio13_190","Bio13_243","Bio13_295","Bio13_348","Bio13_401","Bio13_453","Bio13_506","Bio13_558")]
traitdata$Bio13_max <- colnames(Bio13_Max)[max.col(Bio13_Max, "first")]

Bio14_Max<- traitdata[,c("Bio14_33","Bio14_85","Bio14_138","Bio14_190","Bio14_243","Bio14_295","Bio14_348","Bio14_401","Bio14_453","Bio14_506","Bio14_558")]
traitdata$Bio14_max <- colnames(Bio14_Max)[max.col(Bio14_Max, "first")]

Bio15_Max<- traitdata[,c("Bio15_26","Bio15_39","Bio15_51","Bio15_64","Bio15_76","Bio15_89","Bio15_102","Bio15_114","Bio15_127","Bio15_139","Bio15_152")]
traitdata$Bio15_max <- colnames(Bio15_Max)[max.col(Bio15_Max, "first")]

Bio16_Max<- traitdata[,c("Bio16_94","Bio16_229","Bio16_364","Bio16_499","Bio16_634","Bio16_769","Bio16_905","Bio16_1040","Bio16_1175","Bio16_1310","Bio16_1445")]
traitdata$Bio16_max <- colnames(Bio16_Max)[max.col(Bio16_Max, "first")]

Bio17_Max<- traitdata[,c("Bio17_-38","Bio17_8","Bio17_56","Bio17_104","Bio17_151","BIo17_199","Bio17_247","Bio17_294","Bio17_342","Bio17_390","Bio17_437")]
traitdata$Bio17_max <- colnames(Bio17_Max)[max.col(Bio17_Max, "first")]

Bio18_Max<- traitdata[,c("Bio18_100","Bio18_228","Bio18_356","Bio18_483","Bio18_611","Bio18_739","Bio18_867","Bio18_995","Bio18_1122","Bio18_1250","Bio18_1378")]
traitdata$Bio18_max <- colnames(Bio18_Max)[max.col(Bio18_Max, "first")]

Bio19_Max<- traitdata[,c("Bio19_-70","Bio19_15","Bio19_100","Bio19_185","Bio19_270","Bio19_365","Bio19_441","Bio19_526","Bio19_611","Bio19_696","Bio19_782")]
traitdata$Bio19_max <- colnames(Bio19_Max)[max.col(Bio19_Max, "first")]

Slope_Max<- traitdata[,c("Slope_-243","Slope_49","Slope_343","Slope_636","Slope_930","Slope_1223","Slope_1517","Slope_1811","Slope_2104","Slope_2398","Slope_2691")]
traitdata$Slope_max <- colnames(Slope_Max)[max.col(Slope_Max, "first")]

Elevation_Max<- traitdata[,c("Elevation_-251","Elevation_50","Elevation_353","Elevation_656","Elevation_958","Elevation_1261","Elevation_1564","Elevation_1866","Elevation_2169","Elevation_2472","Elevation_2774")]
traitdata$Elevation_max <- colnames(Elevation_Max)[max.col(Elevation_Max, "first")]

Soil_Max<- traitdata[,c("Soil_0","Soil_1","Soil_2","Soil_4","Soil_5","Soil_6","Soil_8","Soil_9","Soil_11","Soil_12","Soil_14")]
traitdata$Soil_max <- colnames(Soil_Max)[max.col(Soil_Max, "first")]

Landcover_Max<- traitdata[,c("Landcover_-21","Landcover_4","Landcover_29","Landcover_54","Landcover_79","Landcover_105","Landcover_130","Landcover_155","Landcover_180","Landcover_205","Landcover_231")]
traitdata$Landcover_max <- colnames(Landcover_Max)[max.col(Landcover_Max, "first")]

Latitude_Max<- traitdata[,c("Latitude_-26.18","Latitude_-24.58","Latitude_-22.98","Latitude_-21.38","Latitude_-19.78","Latitude_-18.18","Latitude_-16.58","Latitude_-14.98","Latitude_-13.38","Latitude_-11.78","Latitude_-10.18")]
traitdata$Latitude_max <- colnames(Latitude_Max)[max.col(Latitude_Max, "first")]

Longitude_Max<- traitdata[,c("Longitude_42.55","Longitude_43.41","Longitude_44.26","Longitude_45.12","Longitude_45.97","Longitude_46.83","Longitude_74.68","Longitude_48.54","Longitude_49.39","Longitude_50.25","Longitude_51.10")]
traitdata$Longitude_max <- colnames(Longitude_Max)[max.col(Longitude_Max, "first")]

#Remove tips of species that are not used in the analyses from the tree
tip<-traitdata$Phylo_names
phy<-tree
phy<-keep.tip(phy,tip)
plot(phy,cex = 0.4)



Climavariabels1 <- traitdata[,"Bio1_max"]
matrix1<- as.matrix(Climavariabels1)
rownames(matrix1)<-traitdata$Phylo_names
rownames1<-rownames(matrix1)
print(rownames1)

categories <- unique(traitdata$Bio1_max)
categories

colors1<-setNames(c("blue","red","green","orange","white","yellow","purple"),c("Bio1_290","Bio1_110","Bio1_236","Bio1_272","Bio1_164","Bio1_200","Bio1_254"))
colors1
dotTree(phy,matrix1,colors=colors1, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix1), fill = unique(colors1))

# Set the desired width and height for the plot
plot_width <- 10  # Adjust this value as needed
plot_height <- 50  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_bio1.pdf", width = plot_width, height = plot_height)
dotTree(phy,matrix1,colors=colors1, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix1), fill = unique(colors1))
dev.off()


Climavariabels2 <- traitdata[,"Bio2_max"]
matrix2<- as.matrix(Climavariabels2)
rownames(matrix2)<-traitdata$Phylo_names
rownames2<-rownames(matrix2)
print(rownames2)

categories <- unique(traitdata$Bio2_max)
categories

colors2<-setNames(c("blue","red","green","orange","white","yellow"),c("Bio2_43","Bio2_85","Bio2_99","Bio2_71","Bio2_57","Bio2_184"))
colors2
dotTree(phy,matrix2,colors=colors2, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix2), fill = unique(colors2))

# Set the desired width and height for the plot
plot_width <- 10  # Adjust this value as needed
plot_height <- 50  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_bio2.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix2,colors=colors2, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix2), fill = unique(colors2))
dev.off()

Climavariabels3 <- traitdata[,"Bio3_max"]
matrix3<- as.matrix(Climavariabels3)
rownames(matrix3)<-traitdata$Phylo_names
rownames3<-rownames(matrix3)
print(rownames3)

categories <- unique(traitdata$Bio3_max)
categories

colors3<-setNames(c("blue","red"),c("Bio3_49","Bio3_76"))
colors3
dotTree(phy,matrix3,colors=colors3, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix3), fill = unique(colors3))

# Set the desired width and height for the plot
plot_width <- 10  # Adjust this value as needed
plot_height <- 50  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_bio3.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix3,colors=colors3, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix3), fill = unique(colors3))
dev.off()

Climavariabels4 <- traitdata[,"Bio4_max"]
matrix4<- as.matrix(Climavariabels4)
rownames(matrix4)<-traitdata$Phylo_names
rownames4<-rownames(matrix4)
print(rownames4)

categories <- unique(traitdata$Bio4_max)
categories

colors4<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink"),c("Bio4_666","Bio4_3392","Bio4_2302","Bio4_2029","Bio4_2574","Bio4_2847","Bio4_1484","Bio4_1756"))
colors4
dotTree(phy,matrix4,colors=colors4, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix4), fill = unique(colors4))

# Set the desired width and height for the plot
plot_width <- 10  # Adjust this value as needed
plot_height <- 50  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_bio4.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix4,colors=colors4, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix4), fill = unique(colors4))
dev.off()

Climavariabels5 <- traitdata[,"Bio5_max"]
matrix5<- as.matrix(Climavariabels5)
rownames(matrix5)<-traitdata$Phylo_names
rownames5<-rownames(matrix5)
print(rownames5)

categories <- unique(traitdata$Bio5_max)
categories

colors5<-setNames(c("blue","red","green","orange","white","yellow"),c("Bio5_188","Bio5_383","Bio5_266","Bio5_247","Bio5_227","Bio5_286"))
colors5
dotTree(phy,matrix5,colors=colors5, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix5), fill = unique(colors5))

# Set the desired width and height for the plot
plot_width <- 10  # Adjust this value as needed
plot_height <- 50  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_bio5.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix5,colors=colors5, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix5), fill = unique(colors5))
dev.off()

Climavariabels6 <- traitdata[,"Bio6_max"]
matrix6<- as.matrix(Climavariabels6)
rownames(matrix6)<-traitdata$Phylo_names
rownames6<-rownames(matrix6)
print(rownames6)

categories <- unique(traitdata$Bio6_max)
categories

colors6<-setNames(c("blue","red","green","orange","white"),c("Bio6_225","Bio6_13","Bio6_140","Bio6_55","Bio6_34"))
colors6
dotTree(phy,matrix6,colors=colors6, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix6), fill = unique(colors6))


# Save the plot with the specified width and height
pdf("Max_bio6.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix6,colors=colors6, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix6), fill = unique(colors6))
dev.off()

Climavariabels7 <- traitdata[,"Bio7_max"]
matrix7<- as.matrix(Climavariabels7)
rownames(matrix7)<-traitdata$Phylo_names
rownames7<-rownames(matrix7)
print(rownames7)

categories <- unique(traitdata$Bio7_max)
categories

colors7<-setNames(c("blue","red","green","orange"),c( "Bio7_88","Bio7_161","Bio7_143","Bio7_271"))
colors7
dotTree(phy,matrix7,colors=colors7, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix7), fill = unique(colors7))


# Save the plot with the specified width and height
pdf("Max_bio7.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix7,colors=colors7, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix7), fill = unique(colors7))
dev.off()

Climavariabels8 <- traitdata[,"Bio8_max"]
matrix8<- as.matrix(Climavariabels8)
rownames(matrix8)<-traitdata$Phylo_names
rownames8<-rownames(matrix8)
print(rownames8)

categories <- unique(traitdata$Bio8_max)
categories

colors8<-setNames(c("blue","red","green","orange","white","yellow","purple"),c("Bio8_301","Bio8_129","Bio8_181","Bio8_164","Bio8_284","Bio8_198","Bio8_232"))
colors8
dotTree(phy,matrix8,colors=colors8, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix8), fill = unique(colors8))


# Save the plot with the specified width and height
pdf("Max_bio8.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix8,colors=colors8, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix8), fill = unique(colors8))
dev.off()

Climavariabels9 <- traitdata[,"Bio9_max"]
matrix9<- as.matrix(Climavariabels9)
rownames(matrix9)<-traitdata$Phylo_names
rownames9<-rownames(matrix9)
print(rownames9)

categories <- unique(traitdata$Bio9_max)
categories

colors9<-setNames(c("blue","red","green","orange"),c("Bio9_271","Bio9_82","Bio9_139","Bio9_120"))
colors9
dotTree(phy,matrix9,colors=colors9, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix9), fill = unique(colors9))


# Save the plot with the specified width and height
pdf("Max_bio9.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix9,colors=colors9, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix9), fill = unique(colors9))
dev.off()

Climavariabels10 <- traitdata[,"Bio10_max"]
matrix10<- as.matrix(Climavariabels10)
rownames(matrix10)<-traitdata$Phylo_names
rownames10<-rownames(matrix10)
print(rownames10)

categories <- unique(traitdata$Bio10_max)
categories

colors10<-setNames(c("blue","red","green","orange","white","yellow"),c("Bio10_304","Bio10_129","Bio10_181","Bio10_164","Bio10_199","Bio10_234"))
colors10
dotTree(phy,matrix10,colors=colors10, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix10), fill = unique(colors10))

# Save the plot with the specified width and height
pdf("Max_bio10.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix10,colors=colors10, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix10), fill = unique(colors10))
dev.off()

Climavariabels11 <- traitdata[,"Bio11_max"]
matrix11<- as.matrix(Climavariabels11)
rownames(matrix11)<-traitdata$Phylo_names
rownames11<-rownames(matrix11)
print(rownames11)

categories <- unique(traitdata$Bio11_max)
categories

colors11<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink"),c("Bio11_270","Bio11_82","Bio11_195","Bio11_214","Bio11_138","Bio11_119","Bio11_176","Bio11_157"))
colors11
dotTree(phy,matrix11,colors=colors11, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix11), fill = unique(colors11))

# Save the plot with the specified width and height
pdf("Max_bio11.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix11,colors=colors11, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix11), fill = unique(colors11))
dev.off()

Climavariabels12 <- traitdata[,"Bio12_max"]
matrix12<- as.matrix(Climavariabels12)
rownames(matrix12)<-traitdata$Phylo_names
rownames12<-rownames(matrix12)
print(rownames12)

categories <- unique(traitdata$Bio12_max)
categories

colors12<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink","lightblue"),c("Bio12_3214","Bio12_3566","Bio12_1454","Bio12_1806","Bio12_2158","Bio12_398","Bio12_46","Bio12_2862","Bio12_2510"))
colors12
dotTree(phy,matrix12,colors=colors12, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix12), fill = unique(colors12))

# Save the plot with the specified width and height
pdf("Max_bio12.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix12,colors=colors12, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix12), fill = unique(colors12))
dev.off()

Climavariabels13 <- traitdata[,"Bio13_max"]
matrix13<- as.matrix(Climavariabels13)
rownames(matrix13)<-traitdata$Phylo_names
rownames13<-rownames(matrix13)
print(rownames13)

categories <- unique(traitdata$Bio13_max)
categories

colors13<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink","lightblue"),c("Bio13_558","Bio13_33","Bio13_243","Bio13_295","Bio13_453","Bio13_348","Bio13_401","Bio13_138","Bio13_506"))
colors13
dotTree(phy,matrix13,colors=colors13, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix13), fill = unique(colors13))

# Save the plot with the specified width and height
pdf("Max_bio13.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix13,colors=colors13, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix13), fill = unique(colors13))
dev.off()

Climavariabels14 <- traitdata[,"Bio14_max"]
matrix14<- as.matrix(Climavariabels14)
rownames(matrix14)<-traitdata$Phylo_names
rownames14<-rownames(matrix14)
print(rownames14)

categories <- unique(traitdata$Bio14_max)
categories

colors14<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink","lightblue"),c("Bio14_558","Bio14_33","Bio14_243","Bio14_295","Bio14_453","Bio14_348","Bio14_401","Bio14_138","Bio14_506"))
colors14
dotTree(phy,matrix14,colors=colors14, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix14), fill = unique(colors14))

# Save the plot with the specified width and height
pdf("Max_bio14.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix14,colors=colors14, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix14), fill = unique(colors14))
dev.off()

Climavariabels15 <- traitdata[,"Bio15_max"]
matrix15<- as.matrix(Climavariabels15)
rownames(matrix15)<-traitdata$Phylo_names
rownames15<-rownames(matrix15)
print(rownames15)

categories <- unique(traitdata$Bio15_max)
categories

colors15<-setNames(c("blue","red","green","orange","white","yellow"),c("Bio15_51","Bio15_26","Bio15_152","Bio15_64","Bio15_76","Bio15_39" ))
colors15
dotTree(phy,matrix15,colors=colors15, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix15), fill = unique(colors15))

# Save the plot with the specified width and height
pdf("Max_bio15.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix15,colors=colors15, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix15), fill = unique(colors15))
dev.off()

Climavariabels16 <- traitdata[,"Bio16_max"]
matrix16<- as.matrix(Climavariabels16)
rownames(matrix16)<-traitdata$Phylo_names
rownames16<-rownames(matrix16)
print(rownames16)

categories <- unique(traitdata$Bio16_max)
categories

colors16<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink"),c("Bio16_1445","Bio16_94","Bio16_634","Bio16_1040","Bio16_905","Bio16_769","Bio16_1310","Bio16_229" ))
colors16
dotTree(phy,matrix16,colors=colors16, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix16), fill = unique(colors16))

# Save the plot with the specified width and height
pdf("Max_bio16.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix16,colors=colors16, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix16), fill = unique(colors16))
dev.off()

Climavariabels17 <- traitdata[,"Bio17_max"]
matrix17<- as.matrix(Climavariabels17)
rownames(matrix17)<-traitdata$Phylo_names
rownames17<-rownames(matrix17)
print(rownames17)

categories <- unique(traitdata$Bio17_max)
categories

colors17<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink","lightblue"),c("Bio17_342","Bio17_437","Bio17_-38","Bio17_104","BIo17_199","Bio17_151","Bio17_294","Bio17_247","Bio17_56"))
colors17
dotTree(phy,matrix17,colors=colors17, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix17), fill = unique(colors17))

# Save the plot with the specified width and height
pdf("Max_bio17.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix17,colors=colors17, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix17), fill = unique(colors17))
dev.off()

Climavariabels18 <- traitdata[,"Bio18_max"]
matrix18<- as.matrix(Climavariabels18)
rownames(matrix18)<-traitdata$Phylo_names
rownames18<-rownames(matrix1)
print(rownames18)

categories <- unique(traitdata$Bio18_max)
categories

colors18<-setNames(c("blue","red","green","orange"),c("Bio18_1378","Bio18_100","Bio18_739","Bio18_1122"))
colors18
dotTree(phy,matrix18,colors=colors18, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix18), fill = unique(colors18))

# Save the plot with the specified width and height
pdf("Max_bio18.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix18,colors=colors18, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix18), fill = unique(colors18))
dev.off()

Climavariabels19 <- traitdata[,"Bio19_max"]
matrix19<- as.matrix(Climavariabels19)
rownames(matrix19)<-traitdata$Phylo_names
rownames19<-rownames(matrix19)
print(rownames19)

categories <- unique(traitdata$Bio19_max)
categories

colors19<-setNames(c("blue","red","green","orange","white","yellow","purple","hotpink","lightblue","lightgreen","grey","pink"),c("Bio19_611","Bio19_782","Bio19_-70","Bio19_100","Bio19_185","Bio19_270","Bio19_526","Bio19_441","Bio19_696","Bio19_365","NA","Bio19_15"))
colors19
dotTree(phy,matrix19,colors=colors19, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrix19), fill = unique(colors19))

# Save the plot with the specified width and height
pdf("Max_bio19.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix19,colors=colors19, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrix19), fill = unique(colors19))
dev.off()

Elevation <- traitdata[,"Longitude_max"]
matrixElevation<- as.matrix(Elevation)
rownames(matrixElevation)<-traitdata$Phylo_names
rownamesElevation<-rownames(matrixElevation)
print(rownamesElevation)

categories <- unique(traitdata$Longitude_max)
categories

colors19<-setNames(c("blue","red","green"),c( "Longitude_51.10","Longitude_42.55","Longitude_74.68"))
colors19
dotTree(phy,matrixElevation,colors=colors19, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrixElevation), fill = unique(colors19))

# Save the plot with the specified width and height
pdf("Max_Longitude.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrixElevation,colors=colors19, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)
legend("topleft", legend = unique(matrixElevation), fill = unique(colors19))
dev.off()



variabels <- trait[,c("Bio1","Bio12", "Bio15")]
matrix2<- as.matrix(variabels)


#Remove tips of species that are not used in the analyses from the tree
tip<-traitdata$Phylo_names
phy<-tree
phy<-keep.tip(phy,tip)
plot(phy,cex = 0.4)



Climavariabels <- traitdata[,c("Bio1_max","Bio12_max","Bio15_max")]
matrixx<- as.matrix(Climavariabels)
rownames(matrixx)<-traitdata$Phylo_names
rownames<-rownames(matrixx)
print(rownames)


colorsx<-setNames(c("blue","red","green","orange","white","yellow","purple","blue","red","green","orange","white","yellow","purple","hotpink","lightblue","blue","red","green","orange","white","yellow"),c("Bio1_290","Bio1_110","Bio1_236","Bio1_272","Bio1_164","Bio1_200","Bio1_254","Bio12_3214","Bio12_3566","Bio12_1454","Bio12_1806","Bio12_2158","Bio12_398","Bio12_46","Bio12_2862","Bio12_2510","Bio15_51","Bio15_26","Bio15_152","Bio15_64","Bio15_76","Bio15_39"))
colorsx
dotTree(phy,matrixx,colors=colorsx, labels=TRUE, data.type="discrete", fsize=0.2, legend = FALSE)
legend("topleft", legend = unique(matrixx), fill = unique(colorsx))
# Set the desired width and height for the plot
plot_width <- 15  # Adjust this value as needed
plot_height <- 25  # Adjust this value as needed

# Save the plot with the specified width and height
pdf("Max_climate.pdf", width = plot_width, height = plot_height,onefile = FALSE)
dotTree(phy,matrix,colors=colors, labels=TRUE, data.type="discrete", fsize=1, legend = FALSE)

dev.off()
