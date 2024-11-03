def prepare_data(data, column, look_back = 12):
    
    timeseries = np.asarray(data[column])
    timeseries = np.atleast_2d(timeseries)

    if timeseries.shape[0] == 1:
        timeseries = timeseries.T
        
    X = np.atleast_3d(
        np.array(
            [timeseries[start:start + look_back] for start in range(0, timeseries.shape[0] - look_back)]
        )
    )
    
    y = timeseries[look_back:]
    
    return X, y


