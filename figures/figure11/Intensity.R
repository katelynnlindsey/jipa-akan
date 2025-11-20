#Intensity visualisation in R v3.2.1

#To begin, open each tract in Praat (Boersma and Weenink 2013), select the 'Sound' files, then click the 'To Intensity...' button in the Object's window
#Save each Intensity file separately as a text file: Main menu --> 'Save' --> 'Save as text file...'. Save these files in the same working directory you will use for R.

#Loading intensity text files. Be sure to change address to the correct working directory.
nasal=read.table("%DIRECTORY%/INTENSITYFILENAME_ch1.Intensity", header=T, sep="\t", encoding="UTF-8")
oral=read.table("%DIRECTORY%/INTENSITYFILENAME_ch2.Intensity", header=T, sep="\t", encoding="UTF-8")

#For Windows users using characters other than standard ASCII you may have to change the encoding from UTF-16 to UTF-8.
#In EditPad Lite (Goyvaets, 2015) (free download) click 'Covert' in the main menu --> Text Encoding --> Under 'New Encoding' choose 'Unicode, UTF-8' --> OK. Then save.
textgrid=read.csv("%DIRECTORY%/TEXTGRIDNAME.TextGrid", header=F, encoding="UTF-8")

#Renaming the first column of the 'nasal' and 'oral' objects
colnames(nasal)[1]="Nasal"
colnames(oral)[1]="Oral"

#Subsetting the 'nasal' and 'oral' objects to get the intensity data.
N=subset(nasal, grepl("z\\s\\[.*\\s=\\s\\d.",nasal$Nasal))
O=subset(oral, grepl("z\\s\\[.*\\s=\\s\\d.",oral$Oral))

#Combining 'N' and 'O' objects
NO=cbind(N,O)

#Removing non-numeric data from the 'NO' object. 
NO1=as.data.frame(apply(NO,2,function(i)gsub('\\s+z\\s\\[.*\\s=\\s', '',i)))
NO1=as.data.frame(apply(NO1,2,function(i)gsub('\\s', '',i)))

#Renaming columns of NO1 to 'Nasal' and 'Oral' objects
colnames(NO1)[1]="Nasal"
colnames(NO1)[2]="Oral"

#Converting the NO1 data into numeric values
NO2=c()
NO2$Nasal=as.numeric(levels(NO1$Nasal)[NO1$Nasal])
NO2$Oral=as.numeric(levels(NO1$Oral)[NO1$Oral])

#Creating a new column with the length of NO2 listed numerically from 1...n
NO2$Frame=(1:nrow(NO1))

#Ratio:
NO2$Ratio=NO2$Nasal/(NO2$Nasal+NO2$Oral)
#Percentage:
NO2$Percentage=NO2$Ratio*100

#TextGrid manipulation begins here.

##Subsetting the textGrid file to get the min time, max time, and text.
TGMin=subset(textgrid, grepl("            xmin = (|-)(\\d+\\.\\d+|\\d)", textgrid$V1))
TGMax=subset(textgrid, grepl("            xmax = (|-)(\\d+\\.\\d+|\\d)", textgrid$V1))
TGTxt=subset(textgrid, grepl('            text = ', textgrid$V1))

#Combining textGrid subsets
TG=cbind(TGMin, TGMax, TGTxt)

#Creating  an object with the time step between each frame.
dx=subset(oral, grepl("dx\\s=\\s\\d.", oral$Oral))
dx=as.data.frame(apply(dx,2,(function(i)gsub('dx = ', '', i))))
colnames(dx)[1]="dx"
dx=as.data.frame(apply(dx,2,function(i)gsub('\\s', '',i)))
colnames(dx)[1]="dx"
dx=as.vector(dx$dx)[1]
dx=as.numeric(dx)

#Removing the spaces 'xmin', 'xmax', 'text' and '=' signs
TG1=as.data.frame(apply(TG,2,function(i)gsub('(            xmin = |            xmax = |            text = )', '',i)))
TG1=as.data.frame(apply(TG1,2,function(i)gsub('\\s', '',i)))

#Renaming the columns
colnames(TG1)[1]="Min"
colnames(TG1)[2]="Max"
colnames(TG1)[3]="Segment"

#Converting the factor data to numeric
TG1$MinNum=as.numeric(levels(TG1$Min)[TG1$Min])
TG1$MaxNum=as.numeric(levels(TG1$Max)[TG1$Max])

#Calculating the average of the minimum and maximum values
TG1$Average=(TG1$MaxNum+TG1$MinNum)/2
TG1$MinFrame=round(TG1$MinNum/dx)
TG1$MaxFrame=round(TG1$MaxNum/dx)
TG1$AvgFrame=round((TG1$MaxFrame+TG1$MinFrame)/2)

#Adding in the annotation boundary markers with a "|".
v=as.data.frame(TG1$AvgFrame)
w=as.data.frame(TG1$MaxFrame)
x=as.data.frame(TG1$Segment)
y=data.frame(a=1:nrow(x),b="|\n|")
colnames(w)[1]="x"
colnames(v)[1]="y"
colnames(x)[1]="z"

#Gathering the frame data that matches the textGrid's boundaries with correct frames from the 'Sound' file. 
SegmentFrame=as.data.frame(append(v$y,w$x))
colnames(SegmentFrame)[1]="Frame"
Segment=as.data.frame(append(as.vector(x$z),as.vector(y$b)))
colnames(Segment)[1]="Segment"

#Gathering the frame data that matches the textGrid's annotations with correct frames from the 'Sound' file. 
Text=as.data.frame(cbind(as.vector(SegmentFrame$Frame), as.vector(Segment$Segment)))
colnames(Text)[1]="Frame"
colnames(Text)[2]="Segment"

#Dividing the frame values by 1000 to convert the data to milliseconds.
Text$FrameV=as.vector(Text$Frame)
Text$dec=(as.numeric(Text$FrameV)/1000)
Order=Text[order(as.numeric(Text$dec)),]

#Transforming N1 into a data frame.
NO2=as.data.frame(NO2)

#Populates the correct rows with the correct segments
b=0
z=c()
for (i in 1:NROW(NO2)){
b=b+1
i=NO2$Frame[b]
a=match(NO2$Frame==i, Order$FrameV==i)
num=max(a)
z[b]=as.vector(Order$Segment[num])}
NO2$SegmentsAligned=z

#Getting the maximum and minimum values from the textGrid (Order) object.
l=min(Order$dec*1000)
m=max(Order$dec*1000)

#Creating an object for the annotation data.
graphdata=subset(NO2, Frame<=m)
graphdata=subset(graphdata, Frame>=l)

#Graphing
#NOTE: If you are using Rstudio, the axes may appear distorted until you click the 'zoom' button. When saving, we suggest 1600w X 1000h.

#Diving the plot window into one row and one column.
par(mfrow=c(1,1))

#Graph
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
plot(NO2$Nasal,type="l", ylim=c(min(NO2$Nasal)-2,max(NO2$Nasal)+2), col="red", xlab="", ylab="", xaxt='n', frame.plot=TRUE)
lines(NO2$Oral, type="l", lty=2, col="blue")
mtext(NO2$SegmentsAligned,side=1, cex=1.4, at=1:nrow(graphdata), line=0)
mtext(side = 2, text = "Decibels (dB)", line = 3, cex=2)
mtext(side = 1, text = "Frames", line = 2, cex=2)

#Ratio
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 4, 0, 0)) 
plot(NO2$Ratio,type="l", ylim=c(0.4, 0.6), col="purple", xlab="", ylab="", xaxt='n', frame.plot=TRUE)
mtext(NO2$SegmentsAligned,side=1, cex=1.4, at=1:nrow(graphdata), line=0)
mtext(side = 2, text = "Nasal to Oral Ratio", line = 3, cex=2)
mtext(side = 1, text = "Frames", line = 2, cex=2)
