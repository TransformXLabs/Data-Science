def print_model_summary():
    
    model = load_model("models/app_model.h5")
    
    print(model.summary())
