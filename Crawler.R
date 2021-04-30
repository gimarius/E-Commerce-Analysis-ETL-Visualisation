zz <- file("error_log.text", open="wt")
sink(zz, type="message")

library("rvest")
library("tidyverse")


data_url <- read.csv('URL.csv', header=TRUE, sep=";")
data_url <- as.data.frame(data_url)

data_csv <- read.csv('Output_data_test.csv', header=TRUE, sep=',')
data_csv <- as.data.frame(data_csv)
data_csv[is.na(data_csv)] <- 0 


print("Start Microspot")

counter = 1
x_m <- rep(NA,ncol(data_url)-1)


for (i in data_url[1:1,2:ncol(data_url)]){
  price_01 <- i %>%
    read_html() %>%
    html_node(xpath = '//*[@id="container-productdetailPrice"]')%>%
    html_text()
  price_01 <- gsub("CHF", '', price_01)
  price_01 <- gsub("'", '', price_01)
  price_01 <- gsub(".-", '', price_01)
  if (price_01 == 0) {
    price_03 <- i %>%
      read_html() %>%
      html_node(xpath = '//*[@id="container-0979c190-2fe6-11eb-adad-b1e91df74438-price"]')%>%
      html_text()
  }
  print(price_01)
  
  x_m[counter] <- price_01
  counter = counter + 1
  Sys.sleep(1)
}





data_microspot <- data.frame(t(x_m))
names(data_microspot)[1:ncol(data_microspot)] <-names(data_url)[2:ncol(data_url)]
time <- as.character(Sys.time())
vendor <- "Microspot"
data_microspot <- data.frame(Timeframe = rep(time, nrow(data_microspot)), data_microspot[,])
data_microspot <- data.frame(Vendor = rep(vendor, nrow(data_microspot)), data_microspot[,])
difference <- ncol(data_microspot)-ncol(data_csv)

if (difference > 0){
  for (i in 1:difference){
    data_csv <- data_csv %>%
      add_column(Empty_Col = NA)}
}


names(data_csv)<-names(data_microspot)
data_csv <- rbind(data_csv,data_microspot)




print("Start digitec")
counter = 1
x_d <- rep(NA,ncol(data_url)-1)

for (i in data_url[2:2,2:ncol(data_url)]){
  price_02 <- i %>%
    read_html() %>%
    html_node(xpath = '/html/body/div/div/div[2]/div[1]/main/div/div[2]/div/div[2]/div/div[1]/strong')%>%
    html_text()
  price_02 <- gsub("CHF", '', price_02)
  price_02 <- gsub(".-", '', price_02)
  price_02 <- gsub(" ", '', price_02)
  x_d[counter] <- price_02
  counter = counter + 1
  print(price_02)
  Sys.sleep(2)
}


data_digitec <- data.frame(t(x_d))
time <- as.character(Sys.time())
vendor <- "digitec"
data_digitec <- data.frame(Timeframe = rep(time, nrow(data_digitec)), data_digitec[,])
data_digitec <- data.frame(Vendor = rep(vendor, nrow(data_digitec)), data_digitec[,])
names(data_digitec)<- names(data_csv)
data_csv <- rbind(data_csv,data_digitec)





print("Start interdiscount")

counter = 1
x_i <- rep(NA,ncol(data_url)-1)

for (i in data_url[3:3,2:ncol(data_url)]){
  price_03 <- i %>%
    read_html() %>%
    html_node("._3H04_H") %>%
    html_text()
  price_03 <- gsub("CHF", '', price_03)
  price_03 <- gsub("'", '', price_03)
  price_03 <- gsub(".-", '0', price_03)
  x_i[counter] <- price_03
  counter = counter + 1
  print(price_03)
  Sys.sleep(2)
}

data_interdiscount <- data.frame(t(x_i))
time <- as.character(Sys.time())
vendor <- "interdiscount"
data_interdiscount <- data.frame(Timeframe = rep(time, nrow(data_interdiscount)), data_interdiscount[,])
data_interdiscount <- data.frame(Vendor = rep(vendor, nrow(data_interdiscount)), data_interdiscount[,])
names(data_interdiscount)<- names(data_csv)
data_csv <- rbind(data_csv,data_interdiscount)




print("Start brack")


counter = 1
x_b <- rep(NA,ncol(data_url)-1)
for (i in data_url[4:4,2:ncol(data_url)]){
  price_03 <- i %>%
    read_html() %>%
    html_node(xpath = '/html/body/article/div[2]/div/div/div[2]/form/div[1]/div[4]/div/div[4]/p/span/em')%>%
    html_text()
  if (is.na(price_03) == TRUE) {
    price_03 <- i %>%
      read_html() %>%
      html_node(xpath = '//*[@id="AddToCartForm"]/div[1]/div[3]/div/div[5]/p/span/em')%>%
      html_text()
  }
  if (is.na(price_03) == TRUE) {
    price_03 <- i %>%
      read_html() %>%
      html_node(xpath = '//*[@id="AddToCartForm"]/div[1]/div[4]/div/div[5]/p/span/em')%>%
      html_text()
  }
  if (is.na(price_03) == TRUE) {
    price_03 <- i %>%
      read_html() %>%
      html_node(xpath = '//*[@id="AddToCartForm"]/div[1]/div[4]/div/div[5]/p/span/em')%>%
      html_text()
  }
  print(price_03)
  price_03 <- gsub("CHF", '', price_03)
  price_03 <- gsub("'", '', price_03)
  price_03 <- gsub(".-", '0', price_03)
  x_b[counter] <- price_03
  counter = counter + 1
  Sys.sleep(2)
}


data_brack <- data.frame(t(x_b))
time <- as.character(Sys.time())
vendor <- "brack"
data_brack <- data.frame(Timeframe = rep(time, nrow(data_brack)), data_brack[,])
data_brack <- data.frame(Vendor = rep(vendor, nrow(data_brack)), data_brack[,])
names(data_brack)<- names(data_csv)

data_csv <- rbind(data_csv,data_brack)


write.csv(data_csv,'Output_data_test.csv', row.names =FALSE)

print("all done")
message(getwd())
for(i in 1:100) {
  cat(".")
}
message("\nBye.")

sink(type="message") 
close(zz)