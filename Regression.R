library(tree)
tree.model<-tree(log(Deaths)~Active.,data=check2)
plot(tree.model)
text(tree.model,cex=.75)