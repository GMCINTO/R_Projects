# Glen McIntosh
# Project  - Spotify
# 2-15-24




spotify <- read.csv("project1_spotify.csv")

summary(spotify)

streams <- spotify$streams
bpm <- spotify$bpm
mode <- spotify$mode

count_mode <- table(mode)

print(paste("Average of streams:", mean(streams)))
print(paste("Minimum number of streams:" , min(streams)))
print(paste("Median number of streams:", median(streams)))
print(paste("Maximum number of streams:", max(streams)))
print(paste("Standard deviation of streams:", sd(streams)))

print(paste("Average beat per minute:", mean(bpm)))
print(paste("Minimum beats per minute:" , min(bpm)))
print(paste("Median beats per minute:", median(bpm)))
print(paste("Maximum beats per minute:", max(bpm)))
print(paste("Standard deviation of beats per minute:", sd(bpm)))

streams_boxplot <- boxplot(streams, col = c("lightgreen"), main = "Number of streams boxplot", xlab = "2023 Streams", ylab = "Number of Streams")

modes_boxplot <- barplot(count_mode, col = c("darkblue", "darkred"), main = "Song Modes", xlab = "Mode", ylab = "Frequency")

bpm_scatterplot <- plot(bpm, streams, xlab = "Beats per Minute", ylab = "Number of Streams", main = "Beats per Minute vs Number of Streams")

mode_comp_boxplot <- boxplot(bpm ~ mode, data = spotify, col = c("blue", "pink"), main = "Beats Per Minute by Track Key", xlab = "Song Mode", ylab = "Beats Per Minute")

