def predict_lstm(new_data):
    
    model = load_model("models/app_model.h5")
    
    predictions_2d = model.predict(new_data)
    
    return predictions_2d.flatten()
