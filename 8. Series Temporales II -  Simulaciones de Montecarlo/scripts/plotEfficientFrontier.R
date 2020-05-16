plotEfficientFrontier <- function (eff, header = "Efficient Frontier\nand Optimal Portfolio"){
  
  library(ggplot2) # Used to graph efficient frontier
  library(reshape2) # Used to melt the data
  # graph efficient frontier
  # Start with color scheme
  ealred <- "#7D110C"
  ealtan <- "#CDC4B6"
  eallighttan <- "#F7F6F0"
  ealdark <- "#423C30"
  
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=5) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=2),"\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=2),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe, digits=2), sep=""),
           hjust=-0.2, vjust=0.5) +
  ggtitle(header) +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))
}