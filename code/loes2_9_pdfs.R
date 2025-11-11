# Sämtliche Plots als pdf abspeichern
# Achtung: 
# Die pdf's werden ins aktuelle Working directory gespeichert.
# Dieses wird durch getwd() ausgegeben
# Ändern des aktuellen Working directory mit setwd()

pdf("loes2_9_vor_hist.pdf")
par(cex=1.5)
hist(r.bmw_vor,main="breaks=14 (Standard)")
dev.off()

pdf("loes2_9_vor_hist2.pdf")
par(cex=1.5)
hist(r.bmw_vor, breaks=10,main="breaks=10") 
dev.off()

pdf("loes2_9_vor_hist3.pdf")
par(cex=1.5)
hist(r.bmw_vor, breaks=20,main="breaks=20") 
dev.off()

pdf("loes2_9_nach_hist.pdf")
par(cex=1.5)
hist(r.bmw_nach,main="breaks=16 (Standard)")     
dev.off()

pdf("loes2_9_nach_hist2.pdf")
par(cex=1.5)
hist(r.bmw_nach, breaks=10,main="breaks=10") 
dev.off()

pdf("loes2_9_nach_hist3.pdf")
par(cex=1.5)
hnach3<-hist(r.bmw_nach, breaks=20,main="breaks=20")  
dev.off()

pdf("loes2_9_vor_hist_vergl.pdf")
par(cex=1.5)
hist(r.bmw_vor,xlim=c(-0.15,0.12))
dev.off()

pdf("loes2_9_nach_hist_vergl.pdf")
par(cex=1.5)
hist(r.bmw_nach,xlim=c(-0.15,0.12))
dev.off()

pdf("loes2_9_vor_box.pdf")
par(cex=1.5)
boxplot(r.bmw_vor,main="vorher",ylim=c(-0.15,0.12))    
dev.off()

pdf("loes2_9_nach_box.pdf")
par(cex=1.5)
boxplot(r.bmw_nach,main="nachher",ylim=c(-0.15,0.12)) 
dev.off()










