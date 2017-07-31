## sequence learning prediction 
## HMM 
## own sequence prediction algo 

## change at 8:05 pm 


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

for(i in 1:length(unqiueOrders)){
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



## remove all the users/orders which are not in train or val 



userUnique = unique(order$user_id[which(order$eval_set%in%c('train','test'))])


## all order id corresponding to these users 

orderID = unique(order$order_id[which(order$user_id%in%userUnique)])
206209

###

testUser = unique(order$user_id[which(order$eval_set%in%'test')])
testOrder = unique(order$order_id[which(order$eval_set%in%'test')])

## for each user calculate average order size k
## top k products 


## average order size 

order_prior = order[which(order$eval_set%in%'prior'),]


testPrevOrder = order_prior$order_id[which(order_prior$user_id%in%testUser)]


data = data[which(data$order_id%in%testPrevOrder)]




### use data.table 


data = fread('order_products__prior.csv')

data = data[order_id%in%testPrevOrder,]

unqiueOrders = unique(data$order_id)
product = character(length(unqiueOrders)) 
Length  = numeric(length(unqiueOrders))
for(i in 1:length(unqiueOrders)){
   x = data[order_id==unqiueOrders[i],2]
  product[i] = paste(x,collapse=";")
  Length[i]  = dim(x)[1]
   print(i)  
  
}





OrderVsProduct = data.frame(cbind(unqiueOrders,product,Length))

OrdervsUser = order[,c(1,2)]

OrdervsUser = OrdervsUser[which(OrdervsUser$user_id%in%testUser),]

OrdervsUser = data.table(OrdervsUser)
OrderVsProduct = data.table(OrderVsProduct)
userProduct = character(length(testUser))

for(i in 1:length(testUser)){
   user = testUser[i]
  orders = data.frame(OrdervsUser[user_id==user,1])[,1]
  products = paste(as.character(data.frame(OrderVsProduct[unqiueOrders%in%orders,2])[,1]),collapse = ",")
  products = gsub("c","",products)
  products = gsub("\\(","",products)
  products = gsub("\\)","",products)
  products = gsub(" ","",products)
  meanlength = mean(as.numeric(as.character(data.frame(OrderVsProduct[unqiueOrders%in%orders,3])[,1])))
  userProduct[i]= paste(topK(products,",",floor(meanlength)),collapse=" ")
  print(i)
  
}

topK <- function(orderz,sep,k){
  
  orderz = strsplit(orderz,split = sep)[[1]]
  tbl = table(orderz)
  ind = order(tbl,decreasing=T)
  return(names(tbl)[ind[1:k]])
  
}

userID_product = data.frame(cbind(testUser,userProduct))

names(userID_product) = c("user_id",'products')

userID_product$user_id = as.character(userID_product$user_id)
OrdervsUser$user_id = as.character(OrdervsUser$user_id)

userID_product_order = userID_product %>% 
  left_join(OrdervsUser,by="user_id")

submission$order_id = as.character(submission$order_id)
OrdervsUser$order_id = as.character(OrdervsUser$order_id)

submission_user = submission %>%
  left_join(OrdervsUser,by="order_id")


submission_user = submission_user %>%
  left_join(userID_product,by="user_id")


submission_user = submission_user[,-2]


write.csv(submission_user,file="submission_basic.csv",row.names = F)


############## 

# product life time analysis 

# "aisles.csv"                
# "departments.csv"           
#"order_products__prior.csv" 
#"order_products__train.csv"
# "orders.csv"                
#"products.csv"              
#"sample_submission.csv"  

order2 = order[,-c(3,5,6)]

names(OrderVsProduct)[1] = 'order_id'

OrderVsProduct$order_id = as.character(OrderVsProduct$order_id)
order2$order_id         = as.character(order2$order_id)


orderVsProductVstime = OrderVsProduct %>% 
  left_join(order2,by='order_id')


##
uniqueUser = unique(orderVsProductVstime$user_id)

orderVsProductVstime = orderVsProductVstime[order(orderVsProductVstime$order_number,decreasing=F),]
orderVsProductVstime = data.table(orderVsProductVstime)


OrderVsProduct$product = gsub(" ","",OrderVsProduct$product)
OrderVsProduct$product = gsub("c\\(","",OrderVsProduct$product)
OrderVsProduct$product = gsub("\\)","",OrderVsProduct$product)

orderVsProductVstime

orderVsProductVstime$product = gsub(" ","",orderVsProductVstime$product)
orderVsProductVstime$product = gsub("c\\(","",orderVsProductVstime$product)
orderVsProductVstime$product = gsub("\\)","",orderVsProductVstime$product)




allProducts = unique(strsplit(paste(OrderVsProduct$product,collapse  = ","),split = ",")[[1]])

lifetime = character(length(allProducts)) 


productVslifeTime = data.frame(cbind(allProducts,lifetime))

names(productVslifeTime) = c("Product","LifeTime")

for(i in 1:length(uniqueUser)){
 temp = data.frame(orderVsProductVstime[user_id==uniqueUser[i],])  
 productPresent2 = unique(strsplit(paste(temp$product,collapse  = ","),split = ",")[[1]])
 productPresent = paste('\\b',productPresent2,'\\b',sep="")
 for(j in 1:length(productPresent)){
  ind =  which(regexpr(productPresent[j],temp$product)>0)
  if(length(ind)>1){
    tim = NULL
    for(k in 2:length(ind)){
      timsum = sum(temp$days_since_prior_order[ind[k]:ind[k-1]+1])
      tim = c(tim,timsum)
      tim = NULL
      for(k in 2:length(ind)){
        timsum = sum(temp$days_since_prior_order[ind[k]:ind[k-1]+1])
        tim = c(tim,timsum)
        }
    }
  inx = which(allProducts==productPresent2[j])  
    
  x = lifetime[inx]
  lifetime[inx] = paste(x,paste(tim,collapse =";"),sep=";")  
  }
 }
 print(i)
 
}


productVslifeTime = data.frame(cbind(allProducts,lifetime))

names(productVslifeTime) = c("Product","LifeTime")


write.csv(productVslifeTime,file="productVsLifeTime.csv",row.names=F)































