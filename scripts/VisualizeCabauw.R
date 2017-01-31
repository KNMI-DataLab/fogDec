library(data.table)
library(ggplot2)
library(ggvis)

processedImages <- "/nobackup/users/roth/processedImages/Cabauw"
files <- list.files(processedImages, full.names = TRUE)

tmp <- rbindlist(lapply(files, readRDS))
tmp[, id := 1 : .N]


ggplot(tmp, aes(meanEdge, meanHue)) + geom_point()

returnFilePath <- function(x) {
  if(is.null(x)) return(NULL)
  tmp[id == x$id]
  paste0(tmp[id == x$id, filePath])
}

tmp %>% ggvis(~log(meanEdge), ~log(smoothness), key := ~id) %>%
  layer_points() %>%
  add_tooltip(html = returnFilePath, on = "click")

# lower values seem interesting
# peak in smoothness is not obvious
# higher edge values maybe influenced by snow...

tmp[dateTime < "2017-01-20"] %>% ggvis(~log(meanEdge), ~changePoint, key := ~id) %>%
  layer_points() %>%
  add_tooltip(html = returnFilePath, on = "click")

# camera alignment is changing
