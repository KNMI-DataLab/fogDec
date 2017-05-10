library(data.table)

library(DBI)
library(jsonlite)

dbConfig <- fromJSON("config.json")

images <- readRDS("~/Desktop/Daniel/Training/ImageDescription2.rds")

con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "FOGDB",
                 host = dbConfig[["host"]], port = 9418,
                 user = dbConfig[["user"]], password = dbConfig[["pw"]])

dbGetQuery(con, "SELECT table_name FROM INFORMATION_SCHEMA.tables WHERE table_schema = ANY (current_schemas(false));")

tmp <- as.data.table(dbGetQuery(con, "SELECT * FROM image_features;"))

dbDisconnect(con)

imageSummary <- merge(images, tmp)

fn <- function(x) {
  (x - mean(x))/sd(x)
}

imageSummary[, mean_edge := fn(mean_edge), by = camera_id.x]
imageSummary[, change_point := fn(change_point), by = camera_id.x]
imageSummary[, smoothness := fn(smoothness), by = camera_id.x]
imageSummary[, fractal_dim := fn(fractal_dim), by = camera_id.x]
imageSummary[, mean_hue := fn(mean_hue), by = camera_id.x]
imageSummary[, mean_saturation := fn(mean_saturation), by = camera_id.x]
imageSummary[, mean_brightness := fn(mean_brightness), by = camera_id.x]
imageSummary[, mean_transmission := fn(mean_transmission), by = camera_id.x]


library(rpart)
library(rattle)
library(MLmetrics)
library(ROCR)

imageSummary[, foggy := mor_visibility < 250]
fogTree1 <- rpart(foggy ~ mean_edge + change_point + fractal_dim + mean_hue + mean_saturation + mean_brightness + mean_transmission, imageSummary , control = rpart.control(cp = 0.019))

fancyRpartPlot(fogTree1, sub="")

pred <- predict(fogTree1, imageSummary, )
predClas <- ifelse(pred > 0.3, 1, 0)

truth <- as.numeric(imageSummary[, foggy])
table(imageSummary[, foggy], predClas)

Precision(truth, predClas)
Accuracy(truth, predClas)
Sensitivity(truth, predClas)
Specificity(truth, predClas)
AUC(pred, truth)
ConfusionMatrix(predClas, truth)

perf <- performance(prediction(pred, truth), measure = "prec", x.measure = "rec")

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# library(ROCR)
# data(ROCR.simple)
pred2 <- prediction(pred, truth)
perf <- performance(pred2,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred2, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred2, "sens", "spec")
plot(perf1)


fogTree2 <- rpart(vis_class ~ mean_edge + change_point + fractal_dim + mean_hue + mean_saturation + mean_brightness + mean_transmission, imageSummary , control = rpart.control(cp = 0.019))

fancyRpartPlot(fogTree2, sub="")

mlm <- lm(log(mor_visibility) ~ mean_edge + change_point + fractal_dim + mean_hue + mean_saturation + mean_brightness + mean_transmission, imageSummary) 

library(glmnet)
imageSummary2 <- na.omit(imageSummary)
glm <- glmnet(as.matrix(imageSummary2[, .(mean_edge, change_point, fractal_dim, mean_hue, mean_saturation, mean_brightness, mean_transmission)]), log(imageSummary2[, mor_visibility])) 

glm2 <- glmnet(as.matrix(imageSummary2[, .(mean_edge, fractal_dim, mean_hue, mean_brightness, mean_transmission)]), log(imageSummary2[, mor_visibility]), alpha = 0.5)

pred <- predict(glm2, as.matrix(imageSummary2[, .(mean_edge, fractal_dim, mean_hue, mean_brightness, mean_transmission)]), s=0.15)
