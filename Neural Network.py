### Neural Network 
### Language: Python
### Author: Luisa Ripoll Alberola

pip install seaborn

### Import libraries

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder, StandardScaler
import tensorflow as tf
from keras import Sequential, models, losses
from keras.layers import Dense, Input, Dropout, BatchNormalization
from keras.optimizers import Adam
from keras.losses import Loss
from keras.callbacks import EarlyStopping, ModelCheckpoint
from keras.models import Model
from keras.initializers import HeNormal
import matplotlib.pyplot as plt
import seaborn as sns

### Preprocessing

# Read dataset
dataset6 = pd.read_csv("dataset6.csv")

# Ensure correct data types
dataset6['Users'] = dataset6['Users'].astype(int)
dataset6['New_users'] = dataset6['New_users'].astype(int)
dataset6['Avg_session'] = dataset6['Avg_session'].astype(int)
dataset6['Sessions'] = dataset6['Sessions'].astype(int)
dataset6['Year'] = dataset6['Year'].astype(int)
dataset6['Month'] = dataset6['Month'].astype(int)

# Convert columns 10 to 44 into integers
dataset6.iloc[:, 9:44] = dataset6.iloc[:, 9:44].astype(int)
# dataset6['Topics'] = dataset6['Topics'].astype('category')
dataset6['author_quartiles'] = dataset6['author_quartiles'].astype('category')
# dataset6['Author_gender'] = dataset6['Author_gender'].astype('category')

# Poisson regression doesn't work well with NaN values.
# Renaming NA levels to "Other"
dataset6['Section'] = dataset6['Section'].astype(str)
dataset6['Topics'] = dataset6['Topics'].astype(str)
dataset6['Author_gender'] = dataset6['Author_gender'].astype(str)

# Converting NA levels into "Other"
dataset6['Section'].fillna('Other', inplace=True)
dataset6['Topics'].fillna('Other', inplace=True)
dataset6['Author_gender'].fillna('Other', inplace=True)

print(dataset6.isna().sum().sum())  # Check for any NaN values

# Converting columns to categorical data type
dataset6['Section'] = dataset6['Section'].astype('category')
dataset6['Topics'] = dataset6['Topics'].astype('category')
dataset6['Author_gender'] = dataset6['Author_gender'].astype('category')

# Check for missing values
print(dataset6.isna().sum().sum())

# Dummy variable creation
dummy_data = pd.get_dummies(dataset6, drop_first=True)
print(dummy_data.columns)

# Define predictors and response
X = dummy_data.drop('Users', axis=1).values
y = dummy_data['Users'].values

# Split into train and test data partitions
train_data, test_data, train_labels, test_labels = train_test_split(X, y, test_size=0.2, random_state=123)

# Ensure data is scaled properly
scaler = StandardScaler()
train_data_scaled = scaler.fit_transform(train_data)
test_data_scaled = scaler.transform(test_data)

### Custom negative binomial loss function

class NegativeBinomialLoss(Loss):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def call(self, y_true, y_pred):
        # Assuming y_pred has two outputs: mean and variance
        mean = y_pred[:, 0]
        var = y_pred[:, 1]

        # Avoid division by zero and negative values
        mean = tf.maximum(mean, 1e-10)
        var = tf.maximum(var, mean + 1e-10)

        # Calculate the negative binomial log-likelihood
        log_r = tf.math.log(mean / var)
        log1p_r = tf.math.log(1 - (mean / var))

        # Define dispersion parameter
        sustr = var - mean
        sustr = tf.maximum(sustr, 1e-10)
        r = mean*mean / sustr

        # Ensure y_true is cast to float32
        y_true = tf.cast(y_true, tf.float32)

        ll = tf.math.lgamma(y_true + r) - tf.math.lgamma(y_true + 1) - tf.math.lgamma(r) + r * log_r + y_true * log1p_r
        return -tf.reduce_mean(ll)

### Neural Network: 1st attempt 
### Using negative binomial loss function 

input = Input(shape=(train_data.shape[1],))
hidden1 = Dense(64, activation='relu')(input)
output = Dense(2, activation='softplus')(hidden1)

model1 = models.Model(inputs=input, outputs=[output])

model1.summary()

callbacks = [EarlyStopping(
        # "no longer improving" being defined as "no better than 1e-2 less"
        min_delta=1e-2,
        verbose=1)]

model1.compile(optimizer = Adam(learning_rate = 0.0001), 
              loss = NegativeBinomialLoss(),
              metrics=['mse'])

history = model1.fit(train_data, train_labels, epochs = 150, batch_size = 20, validation_split = 0.2)

# Evaluate the model
test_loss = model1.evaluate(test_data, test_labels)
print(f"Test loss: {test_loss}")

# Plot training and validation loss
plt.plot(history.history['loss'], label='Train Loss')
plt.plot(history.history['val_loss'], label='Val Loss')
plt.legend()
plt.show()

# Get predictions for the first five instances in the test data
predictions = model1.predict(test_data[:5])

# Print predictions and compare them with the real values
for i in range(5):
    print(f"Prediction: {predictions[i]}, Real Value: {test_labels[i]}")

### Neural network: 2nd attempt
### Using MSE loss function

input = Input(shape=(train_data.shape[1],))
hidden1 = Dense(64, activation='relu')(input)

# Output layer with softplus activation to ensure positive values
output = Dense(1, activation='softplus')(hidden1)

model2 = models.Model(inputs=input, outputs=[output])

model2.summary()

callbacks = [EarlyStopping(
        # "no longer improving" being defined as "no better than 1e-2 less"
        min_delta=1e-2,
        verbose=1)]

model2.compile(optimizer = Adam(learning_rate = 0.0001), 
              # loss = NegativeBinomialLoss(),
              loss = losses.MeanSquaredError())
              # metrics = ['NegativeBinomialLoss'])
              # metrics=[negative_binomial_metric])

history = model2.fit(train_data, train_labels, epochs = 150, batch_size = 20, validation_split = 0.2)

# Evaluate the model
test_loss = model2.evaluate(test_data, test_labels)
print(f"Test loss: {test_loss}")

# Plot training and validation loss
plt.plot(history.history['loss'], label='Train Loss')
plt.plot(history.history['val_loss'], label='Val Loss')
plt.legend()
plt.show()

# Get predictions for the first fifteen instances in the test data
predictions = model2.predict(test_data[:15])

# Print predictions and compare them with the real values
for i in range(15):
    print(f"Prediction: {predictions[i]}, Real Value: {test_labels[i]}")

### Density plots of the predictions of test data

predictions = model2.predict(test_data)

plt.figure(figsize=(10, 5))

# Plot density of predictions
sns.kdeplot(predictions, label='Predictions', color='blue', fill=False, alpha=0.5)

# Plot density of real values
sns.kdeplot(test_labels, label='Real Values', color='red', fill=False, alpha=0.5)

# Adding titles and labels
plt.title('Density Plot of Predictions vs Real Values')
plt.xlabel('Values')
plt.ylabel('Density')
plt.legend()

# Show the plot
plt.show()

### Plot comparing predicted vs real values

plt.figure(figsize=(8, 8))
plt.scatter(test_labels, predictions, color='blue', label='Predictions vs Real Labels')
plt.plot([min(test_labels), max(test_labels)], [min(predictions), max(predictions)], color='red', linestyle='--', label='y=x')
plt.xlabel('Real Labels')
plt.ylabel('Predictions')
plt.title('Model Predictions vs Real Labels')
plt.legend()
plt.grid(True)
plt.show()

### Neural Network with dropout: 3rd attempt

# Input layer
input = Input(shape=(train_data.shape[1],))

# First hidden layer with He initialization, batch normalization, and dropout
hidden1 = Dense(64, activation='relu', kernel_initializer=HeNormal())(input)
hidden1 = BatchNormalization()(hidden1)
hidden1 = Dropout(0.6)(hidden1)

# Second hidden layer with He initialization, batch normalization, and dropout
hidden2 = Dense(64, activation='relu', kernel_initializer=HeNormal())(hidden1)
hidden2 = BatchNormalization()(hidden2)
hidden2 = Dropout(0.6)(hidden2)

# Third hidden layer with He initialization, batch normalization, and dropout
hidden3 = Dense(32, activation='relu', kernel_initializer=HeNormal())(hidden2)
hidden3 = BatchNormalization()(hidden3)
hidden3 = Dropout(0.6)(hidden3)

# Output layer
output = Dense(2, activation='linear')(hidden2)

# Define the model
model3 = Model(inputs=input, outputs=output)

# Print the model summary
model3.summary()

callbacks = [EarlyStopping(
    monitor="val_loss",
    patience=15,
    verbose=1)]

model3.compile(optimizer=Adam(learning_rate=0.0001), 
               # loss = NegativeBinomialLoss(), metrics=['mse'])
               loss = losses.MeanSquaredError())

history = model3.fit(train_data, train_labels, epochs=250, batch_size=128, validation_split=0.2, callbacks=callbacks)

# Evaluate the model
test_loss = model3.evaluate(test_data, test_labels)
print(f"Test loss: {test_loss}")

# Plot training and validation loss
# Poor convergence
plt.plot(history.history['loss'], label='Train Loss')
plt.plot(history.history['val_loss'], label='Val Loss')
plt.legend()
plt.show()