##          REALTIME WEB SERVICE EXAMPLE                ##

# For R Server 9.0, load mrsdeploy package on R Server     
library(mrsdeploy)

##########################################################
#   Create/Test Logistic Regression Model with rxLogit   #
##########################################################

# Create logistic regression model 
# using rxLogit modeling function from RevoScaleR package
# and the Rpart `kyphosis` dataset available to all R users
kyphosisModel <- rxLogit(Kyphosis ~ Age, data=kyphosis)

# Test the model locally
testData <- data.frame(Kyphosis=c("absent"), Age=c(71), Number=c(3), Start=c(5))
rxPredict(kyphosisModel, data = testData)  # Kyphosis_Pred: 0.1941938

##########################################################
#            Log into Microsoft R Server                 #
##########################################################

# Use `remoteLogin` to authenticate with R Server using 
# the local admin account. Use session = false so no 
# remote R session started
# REMEMBER: replace with the login info for your organization
remoteLogin("http://localhost:12800", 
            username = "admin", 
            password = "Pa$$w0rd",
            session = FALSE)

##########################################################
#    Publish Kyphosis Model as a Realtime Service        #
##########################################################

# Generate a unique serviceName for demos 
# and assign to variable serviceName
serviceName <- paste0("kyphosis", round(as.numeric(Sys.time()), 0))

# Publish as service using publishService() function. 
# Use the variable name for the service and version `v1.0`
# Assign service to the variable `realtimeApi`.
realtimeApi <- publishService(
     serviceType = "Realtime",
     name = serviceName,
     code = NULL,
     model = kyphosisModel,
     v = "v1.0",
     alias = "kyphosisService"
)

##########################################################
#           Consume Realtime Service in R                #
##########################################################

# Print capabilities that define the service holdings: service 
# name, version, descriptions, inputs, outputs, and the 
# name of the function to be consumed
print(realtimeApi$capabilities())

# Consume service by calling function contained in this service
realtimeResult <- realtimeApi$kyphosisService(testData)

# Print response output
print(realtimeResult$outputParameters) # 0.1941938   

##########################################################
#         Get Service-specific Swagger File in R         #
##########################################################

# During this authenticated session, download the  
# Swagger-based JSON file that defines this service
rtSwagger <- realtimeApi$swagger()
cat(rtSwagger, file = "realtimeSwagger.json", append = FALSE)

# Share Swagger-based JSON with those who need to consume it
