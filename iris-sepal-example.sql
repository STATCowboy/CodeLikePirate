-- sp_configure 'external scripts enabled', 1
-- GO

-- RECONFIGURE

-- RESTART SQL Server

use IntroToR
go

if exists(select * from sys.objects where name = 'get_iris_dataset' and type = 'P')
drop proc get_iris_dataset;
GO

CREATE PROC get_iris_dataset 
AS 
BEGIN 
EXEC sp_execute_external_script 
@language = N'R' 
, @script = N'iris_data <- iris;' 
, @input_data_1 = N'' 
, @output_data_1_name = N'iris_data' 
WITH RESULT SETS (("Sepal.Length" float not null, "Sepal.Width" float not null,"Petal.Length" float not null, "Petal.Width" float not null, "Species" varchar(100))); 
END;
GO



if exists(select * from sys.tables where name = 'iris_data')
drop table iris_data;
GO

create table iris_data 
( 
"Sepal.Length" float not null, 
"Sepal.Width" float not null, 
"Petal.Length" float not null, 
"Petal.Width" float not null, 
"Species" varchar(100) 
);
GO 
INSERT INTO iris_data Exec get_iris_dataset 
GO



if exists(select * from sys.objects where name = 'generate_iris_model' and type = 'P')
drop proc generate_iris_model;
GO

CREATE PROC generate_iris_model 
AS 
BEGIN 
EXEC sp_execute_external_script 
@language = N'R' 
, @script = N' library(e1071); 
irismodel <-naiveBayes(iris_data[,1:4], iris_data[,5]); 
trained_model <- data.frame(payload = as.raw(serialize(irismodel, connection=NULL)));' 
, @input_data_1 = N'select "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" from iris_data' 
, @input_data_1_name = N'iris_data' 
, @output_data_1_name = N'trained_model' 
WITH RESULT SETS ((model varbinary(max))); 
END;
GO



if exists(select * from sys.tables where name = 'model')
drop table model;
GO

CREATE TABLE model( 
[model] [varbinary](max) NULL 
) 
GO 
insert into model exec generate_iris_model 
GO



declare @model varbinary(MAX) select @model=model from model 

EXEC sp_execute_external_script 
@language = N'R' 
, @script = N' library(e1071) 
model <-unserialize(as.raw(iris_model)) 
pred<-predict(model, iris_data) 
result<-cbind(iris_data, pred)' 
, @input_data_1 = N'select "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" from iris_data' 
, @input_data_1_name = N'iris_data' 
, @output_data_1_name = N'result' 
, @params = N'@iris_model varbinary(MAX)' 
, @iris_model= @model
WITH RESULT SETS (("Sepal.Length" float not null, "Sepal.Width" float not null,
                   "Petal.Length" float not null, "Petal.Width" float not null, "Species" varchar(100), 
				   "SpeciesPredicted" varchar(100)))
GO


/*
library(e1071); 
iris_data <- iris; 
irismodel <-naiveBayes(iris_data[,1:4], iris_data[,5]); 
result<-predict(irismodel, iris_data[,1:4]) 
cbind(iris_data, result)
*/
