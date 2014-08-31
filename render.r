require(ggplot2)

args <- commandArgs(TRUE)

fs <- args[1]

dumb <- read.csv(paste('dumb-', fs, '.csv', sep=''))
nice <- read.csv(paste('nice-', fs, '.csv', sep=''))
smart <- read.csv(paste('smart-', fs, '.csv', sep=''))

png(paste('average-', fs, '.png', sep=''), width=720, height=480)
ggplot(dumb, aes(dumb$Floors)) +
    geom_line(aes(y=dumb$AvgWait, colour='Dumb')) +
    geom_line(aes(y=nice$AvgWait, colour='Nice')) +
    geom_line(aes(y=smart$AvgWait, colour='Smart')) +
    ylab("Average Wait Time") +
    xlab("Number of Floors") +
    scale_color_manual("",
                       breaks=c('Dumb', 'Nice', 'Smart'),
                       values=c('red', 'green', 'blue'))

dev.off()
