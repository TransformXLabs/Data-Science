def tensorflow_lstm(X, y, look_back = 12, epochs = 10, batch_size = 80, units_1 = 64, activation_1 = "tanh", units_2 = 12, activation_2 = "relu", optimizer = "rmsprop", loss = "mae", seed = 5):
    
    tf.random.set_seed(seed=seed)
    
    model = Sequential()

    model.add(Input(shape=(look_back, 1)))
    model.add(LSTM(units_1, activation=activation_1, return_sequences=True))
    model.add(LSTM(units_2, activation=activation_2, return_sequences=False))
    
    model.add(Dense(1))
    
    model.add(Activation(activation='linear'))
    
    model.compile(optimizer=optimizer, loss=loss)
    
    # Fit Model
    model.fit(X, y, epochs=epochs, batch_size=batch_size, verbose=1, shuffle=False)
    
    model.save("models/app_model.h5", overwrite=True)
    
    
    
    
