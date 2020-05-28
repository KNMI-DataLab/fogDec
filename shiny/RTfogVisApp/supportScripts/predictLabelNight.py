import re
import sys
import json
import os
import time
import datetime
import pandas as pd
from pandas.io.json import  json_normalize
from keras.preprocessing import image
from keras.models import load_model
import numpy as np


image_height = 64
image_width = 64




model_path = "/external/models/best-MODEL_NIGHT.h5"

localTempSavedLocation = sys.argv[1]

img = image.img_to_array(image.load_img(localTempSavedLocation, target_size=(image_height, image_width))) / 255.

img2 = np.expand_dims(img, axis=0)

print("starting prediction for "+ localTempSavedLocation)
#logger.info("starting prediction for %s ", localTempSavedLocation)

#logger.info("loading model and making prediction")

model = load_model(model_path)
Y_pred = model.predict(img2)
print(Y_pred)

#Keras sorts the labels (names of folders in the train directory) by alphabetical order. If you have a list of labels called labels, the predicted label name will be: predicted_label = sorted(labels)[y_classes] –


target_names = np.array(['Cannot Say', 'Fog', 'No Fog'])



y_pred = np.argmax(Y_pred, axis=1)

prediction = target_names[y_pred]

print(Y_pred)

print("finished prediction for "+ localTempSavedLocation)
#logger.info("finished prediction for %s", localTempSavedLocation)

fogClassDict = {"No Fog":0, "Fog":1, "Cannot Say":2}

fogClass = fogClassDict[prediction[0]]



predTRUE  = Y_pred[0,1]
predFALSE = Y_pred[0,2]

print("image fog class: "+ str(fogClass))

df2 = pd.DataFrame({"fileLocation":[localTempSavedLocation], "originalPath":[localTempSavedLocation], "fogClass":[fogClass], "predTrue":[predTRUE], "predFalse":[predFALSE], "model_id": [model_path]})

#cameraTarget = cameraTarget.reset_index(drop=True)

#final = pd.concat([cameraTarget, df2], axis=1)

#final =

#finalJSON = df2.to_json(orient="records")
finalDICT = df2.to_dict('records')
#with open('/tmp/predicitonLabel.json', 'w') as outfile:
#    json.dump(finalJSON, outfile)
with open('/tmp/predicitonLabel.json', 'w') as outfile:
    json.dump(finalDICT, outfile)






