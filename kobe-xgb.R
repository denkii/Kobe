# Kaggle Kobe shot selection XGBoost classifier

# Set seed for reproducibility
set.seed(123)

# Set up grid for parameter tuning
xgb_grid = expand.grid(
	nrounds = 1000,
	eta = c(0.01, 0.001, 0.0001),
	max_depth = c(2, 4, 6, 8, 10),
	gamma = 0,
	colsample_bytree = 0.6,
	min_child_weight = 1
)

# Pack training control parameters
xgb_trcontrol = trainControl(
	method="cv",
	number = 10,
	verboseIter = F,
	returnData = F,
	returnResamp = 'none',
	summaryFunction = multiClassSummary,
	classProbs = T,
	allowParallel = T
)

# Train model for each parameter combination in grid
xgb_train = train(
	x = X,
	y = y,
	trControl = xgb_trcontrol,
	tuneGrid = xgb_grid,
	method = "xgbTree",
	metric = "logLoss",
	maximize = F
)

# Remove shot_made_flag from data
train_xgb = train
test_xgb = test
train_xgb$shot_made_flag = NULL
test_xgb$shot_made_flag = NULL

# Set test df as matrix
test_xgb = data.matrix(test,rownames.force=NA)

# Predict and output to csv
shot_made_flag = predict(xgb_train, test_xgb, type="prob")[,2]
output = data.frame(shot_id, shot_made_flag)
write.csv(output, "kobe-xgb.csv", row.names=F)
