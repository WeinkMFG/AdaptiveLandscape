#Show adaptive landscape concept
#Further reading:
#	-Waddington, C. H. (1974) A Catastrophe Theory of Evolution. Annals of the New York Academy of Sciences...
#		231 (1): 32-41. doi:10.1111/j.1749-6632.1974.tb20551.x
#Author: Manuel Weinkauf (Manuel.Weinkauf@gmx.de)
#Version: 1.0
#Date: 13 June 2020

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.#
#To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/.                   #
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#**************************************************************************************
#Loading functions
#source("XXX.r")
#"%notin%"<-Negate("%in%")

#Load packages
library(tidyr)
library(tibble)
library(readr)
library(dplyr)
library(forcats)
library(rgl)

##############################################################################################################################
# Prepare data ###############################################################################################################
##############################################################################################################################

#Design surface data
x<-y<-seq(from=-3, to=3, by=0.1)
zf<-function (x, y) {3*exp(-(y+1)**2-x**2)*(x-1)**2-((exp(-(x+1)**2-y**2))/3)+exp(-x**2-y**2)*(10*x**3-2*x+10*y**5)}
z<-outer(x, y, zf)

##############################################################################################################################
# Plot adaptive landscapes ###################################################################################################
##############################################################################################################################

############################
###                      ###
### EVOLUTIONARY PROCESS ###
###                      ###
############################

setwd("C:/R_Data/AdaptiveLandscape")
#Species radius
r=0.3
#Species colours
ColCode<-c("darkred", "darkblue", "darkorange")

#########################
### Initial situation ###
#########################

#Calculate paths
Path.length<-20
EvoLines<-list()
EvoLines$Aux<-matrix(c(seq(from=-0.4, to=0, length.out=Path.length), seq(from=-0.6, to=1.5, length.out=Path.length)), Path.length, 2)
EvoLines$Aux<-cbind(EvoLines$Aux, (-((1955*EvoLines$Aux[,1]**2)/4)-((761*EvoLines$Aux[,1])/4)+(3/2))+0.1)
EvoLines$Aux<-EvoLines$Aux[-(1:2),]
EvoLines$Aux<-EvoLines$Aux[-((nrow(EvoLines$Aux)-2):nrow(EvoLines$Aux)),]

#Plot
open3d(windowRect=c(100,100,4900,4900))
bg3d("white")
material3d(col="black")
persp3d(x, y, z, col="lemonchiffon", aspect=c(1, 1, 0.3), shininess=20, polygon_offset=1, xlab="", ylab="", zlab="", axes=FALSE)
persp3d(x, y, z, front="lines", back="lines", lit=FALSE, add=TRUE, col="black")
rgl.spheres(x=2, y=1.1, z=zf(2, 1.1)+3.5*r, radius=r, col=ColCode[1])
rgl.spheres(x=-2.2, y=-1.1, z=zf(-2.2, -1.1)+3.5*r, radius=r, col=ColCode[2])
rgl.spheres(x=2.4, y=-2.1, z=zf(2.4, -2.1)+3.5*r, radius=r, col=ColCode[3])
rgl.spheres(EvoLines$Aux, radius=r*0.2, col=ColCode[2], alpha=0)
view3d(theta=0, phi=-70, fov=60)
rgl.snapshot("AdaptiveInitial.png")
rgl.close()

############################
### First selection turn ###
############################

#Calculate flood
z.flood<-matrix(2, length(x), length(y))

#Calculate paths
Path.length<-20
EvoLines<-list()
EvoLines$Red<-matrix(c(seq(from=2, to=0.5, length.out=Path.length), seq(from=1.1, to=1.2, length.out=Path.length)), Path.length, 2)
EvoLines$Red<-cbind(EvoLines$Red, zf(EvoLines$Red[,1], EvoLines$Red[,2])+0.1)
EvoLines$Blue<-matrix(c(seq(from=-2.2, to=-0.4, length.out=Path.length), seq(from=-1.1, to=-0.6, length.out=Path.length)), Path.length, 2)
EvoLines$Blue<-cbind(EvoLines$Blue, zf(EvoLines$Blue[,1], EvoLines$Blue[,2])+0.1)
EvoLines$Orange<-matrix(c(seq(from=2.4, to=1.6, length.out=Path.length), seq(from=-2.1, to=-1, length.out=Path.length)), Path.length, 2)
EvoLines$Orange<-cbind(EvoLines$Orange, zf(EvoLines$Orange[,1], EvoLines$Orange[,2])+0.1)
EvoLines$Aux<-matrix(c(seq(from=-0.4, to=0, length.out=Path.length), seq(from=-0.6, to=1.5, length.out=Path.length)), Path.length, 2)
EvoLines$Aux<-cbind(EvoLines$Aux, (-((1955*EvoLines$Aux[,1]**2)/4)-((761*EvoLines$Aux[,1])/4)+(3/2))+0.1)
EvoLines$Aux<-EvoLines$Aux[-(1:2),]
EvoLines$Aux<-EvoLines$Aux[-((nrow(EvoLines$Aux)-2):nrow(EvoLines$Aux)),]

#Plot
open3d(windowRect=c(100,100,4900,4900))
bg3d("white")
material3d(col="black")
persp3d(x, y, z, col="lemonchiffon", aspect=c(1, 1, 0.3), shininess=20, polygon_offset=1, xlab="", ylab="", zlab="", axes=FALSE)
persp3d(x, y, z, front="lines", back="lines", lit=FALSE, add=TRUE, col="black")
rgl.spheres(x=0.5, y=1.2, z=zf(0.5, 1.2)+3.5*r, radius=r, col=ColCode[1])
rgl.spheres(EvoLines$Red, radius=r*0.2, col=ColCode[1])
rgl.spheres(x=-0.4, y=-0.6, z=zf(-0.5, -0.5)+3.5*r, radius=r, col=ColCode[2])
rgl.spheres(EvoLines$Blue, radius=r*0.2, col=ColCode[2])
rgl.spheres(x=1.6, y=-1, z=zf(1.6, -1)+3.5*r, radius=r, col=ColCode[3])
rgl.spheres(EvoLines$Orange, radius=r*0.2, col=ColCode[3])
persp3d(x, y, z.flood, col="cornflowerblue", aspect=c(1, 1, 0.3), shininess=20, add=TRUE, alpha=0.5)
rgl.spheres(EvoLines$Aux, radius=r*0.2, col=ColCode[2], alpha=0)
view3d(theta=0, phi=-70, fov=60)
rgl.snapshot("AdaptiveFirstSelection.png")
rgl.close()

#############################
### Second selection turn ###
#############################

#Calculate flood
z.flood<-matrix(6, length(x), length(y))

#Calculate paths
Path.length<-20
EvoLines<-list()
EvoLines$Red<-matrix(c(seq(from=0.4, to=0, length.out=Path.length), seq(from=1.2, to=1.5, length.out=Path.length)), Path.length, 2)
EvoLines$Red<-cbind(EvoLines$Red, zf(EvoLines$Red[,1], EvoLines$Red[,2])+0.1)
EvoLines$Aux<-matrix(c(seq(from=-0.4, to=0, length.out=Path.length), seq(from=-0.6, to=1.5, length.out=Path.length)), Path.length, 2)
EvoLines$Aux<-cbind(EvoLines$Aux, (-((1955*EvoLines$Aux[,1]**2)/4)-((761*EvoLines$Aux[,1])/4)+(3/2))+0.1)
EvoLines$Aux<-EvoLines$Aux[-(1:2),]
EvoLines$Aux<-EvoLines$Aux[-((nrow(EvoLines$Aux)-2):nrow(EvoLines$Aux)),]

#Plot
open3d(windowRect=c(100,100,4900,4900))
bg3d("white")
material3d(col="black")
persp3d(x, y, z, col="lemonchiffon", aspect=c(1, 1, 0.3), shininess=20, polygon_offset=1, xlab="", ylab="", zlab="", axes=FALSE)
persp3d(x, y, z, front="lines", back="lines", lit=FALSE, add=TRUE, col="black")
rgl.spheres(x=0, y=1.5, z=zf(0, 1.5)+3.5*r, radius=r, col=ColCode[1])
rgl.spheres(EvoLines$Red, radius=r*0.2, col=ColCode[1])
rgl.spheres(x=-0.4, y=-0.6, z=zf(-0.5, -0.5)+3.5*r, radius=r, col=ColCode[2])
rgl.spheres(x=1.6, y=-1, z=zf(1.6, -1)+3.5*r, radius=r, col=ColCode[3])
persp3d(x, y, z.flood, col="cornflowerblue", aspect=c(1, 1, 0.3), shininess=20, add=TRUE, alpha=0.5)
rgl.spheres(EvoLines$Aux, radius=r*0.2, col=ColCode[2], alpha=0)
view3d(theta=0, phi=-70, fov=60)
rgl.snapshot("AdaptiveSecondSelection.png")
rgl.close()

####################
### Evolvability ###
####################

#Calculate flood
z.flood<-matrix(2, length(x), length(y))

#Calculate paths
Path.length<-20
EvoLines<-list()
EvoLines$Blue<-matrix(c(seq(from=-0.4, to=0, length.out=Path.length), seq(from=-0.6, to=1.5, length.out=Path.length)), Path.length, 2)
EvoLines$Blue<-cbind(EvoLines$Blue, (-((1955*EvoLines$Blue[,1]**2)/4)-((761*EvoLines$Blue[,1])/4)+(3/2))+0.1)
EvoLines$Blue<-EvoLines$Blue[-(1:2),]
EvoLines$Blue<-EvoLines$Blue[-((nrow(EvoLines$Blue)-2):nrow(EvoLines$Blue)),]

#Plot
open3d(windowRect=c(100,100,4900,4900))
bg3d("white")
material3d(col="black")
persp3d(x, y, z, col="lemonchiffon", aspect=c(1, 1, 0.3), shininess=20, polygon_offset=1, xlab="", ylab="", zlab="", axes=FALSE)
persp3d(x, y, z, front="lines", back="lines", lit=FALSE, add=TRUE, col="black")
rgl.spheres(x=-0.4, y=-0.6, z=zf(-0.5, -0.5)+3.5*r, radius=r, col=ColCode[2], alpha=0.2)
rgl.spheres(x=0, y=1.5, z=zf(0, 1.5)+3.5*r, radius=r, col=ColCode[2])
rgl.spheres(EvoLines$Blue, radius=r*0.2, col=ColCode[2])
persp3d(x, y, z.flood, col="cornflowerblue", aspect=c(1, 1, 0.3), shininess=20, add=TRUE, alpha=0.5)
view3d(theta=0, phi=-70, fov=60)
rgl.snapshot("AdaptiveEvolvability.png")
rgl.close()

################
#Version history
#1.0	Finished program
################