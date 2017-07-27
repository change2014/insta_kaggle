## sequence learning prediction 
## HMM 
## own sequence prediction algo 


setwd("C:\\Users\\ss13522\\Documents\\kaggle_insta\\")


data = read.csv("order_products__prior.csv")
data2 = read.csv("order_products__train.csv")
order = read.csv("orders.csv")


submission = read.csv("sample_submission.csv")


### roll up order vs product 

productDetails = read.csv("products.csv")
orderVsProduct = read.csv("aisles.csv")

data = rbind(data,data2)



unqiueOrders = unique(data$order_id)

product = character(length(unqiueOrders)) 

for(i in 5:length(unqiueOrders)){
  ind = which(data$order_id==unqiueOrders[i])
  temp = data[ind,]
 # data = data[-ind,]
  product[i] = paste(temp$product_id[order(temp$add_to_cart_order,decreasing=F)],collapse = ";")
print(i)  
  
}


data$add_to_cart_order
data$
data = data[order(data$add_to_cart_order),]
## 

data = data[,c(1,2)]

df5 <- reshape(data, idvar="order_id", timevar="product_id", direction="wide")
View(df5) # QC
table(df3$Bucket, df3$Spec.Grp)  # QC


uniqueorders = unique(data$order_id)

orderVsProduct = data.frame(matrix(nrow=length(uniqueorders),ncol=2))
names(orderVsProduct) = c("order_id","product_id")
orderVsProduct$order_id = as.character(orderVsProduct$order_id)
orderVsProduct$order_id = uniqueorders
orderVsProduct$product_id = ""
for(i in 1:dim(data)[1]){
  
 x = as.character(data$order_id[i])
 y = orderVsProduct[x,2]
 
 orderVsProduct$product_id[x,2] = paste(data$product_id[i],y,sep=";")
 
print(i)   
}





