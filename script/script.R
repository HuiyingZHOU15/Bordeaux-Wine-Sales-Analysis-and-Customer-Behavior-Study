
##1,数据读取和准备
library(tidyverse)
library(lubridate)

customers <- read.csv("E:/projet/wine/data/customers.csv")
products <- read.csv("E:/projet/wine/data/products.csv")
transactions <- read.csv("E:/projet/wine/data/transactions.csv")

transactions$purchase_date <- as.Date(transactions$purchase_date)

data <- transactions %>%
  left_join(customers, by="customer_id") %>%
  left_join(products, by="product_id")

##2,数据探索EDA
#销售金额分布
p<-ggplot(data, aes(x=order_amount)) +
  geom_histogram(bins=40, fill="steelblue") +
  labs(title="订单金额分布")
ggsave("E:/projet/wine/output/figures/订单金额分布.png", p)

#购买频次分布
purchase_freq <- data %>%
  group_by(customer_id) %>%
  summarise(freq=n())

p<-ggplot(purchase_freq, aes(x=freq)) +
  geom_histogram(bins=30, fill="orange")
ggsave("E:/projet/wine/output/figures/购买频率.png", p)

#产品类别销量
product_sales <- data %>%
  group_by(Name) %>%
  summarise(sales=sum(quantity)) %>%
  arrange(desc(sales)) %>%
  head(10)

p<-ggplot(product_sales,
       aes(x=reorder(Name,sales), y=sales)) +
  geom_bar(stat="identity") +
  coord_flip()
ggsave("E:/projet/wine/output/figures/产品类别销量.png", p)

#月度销售趋势
monthly_sales <- data %>%
  mutate(month=floor_date(purchase_date,"month")) %>%
  group_by(month) %>%
  summarise(sales=sum(order_amount))

p<-ggplot(monthly_sales,aes(x=month,y=sales))+
  geom_line()
ggsave("E:/projet/wine/output/figures/月度销售趋势.png", p)
##3,异常值识别
p<-ggplot(data,aes(y=order_amount))+
  geom_boxplot()

data %>%
  group_by(customer_id) %>%
  summarise(total=sum(order_amount)) %>%
  arrange(desc(total)) %>%
  head(10)
ggsave("E:/projet/wine/output/figures/异常值识别.png", p)
#异常值分析
data %>%
  filter(order_amount > 1000) %>%
  select(customer_id, order_amount, quantity) %>%
  head(20)
#识别vip客户
vip_customers <- data %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(order_amount)) %>%
  arrange(desc(total_spent))
head(vip_customers,10)
#大客户买的什么酒水
data %>%
  filter(order_amount > 1000) %>%
  group_by(Name) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
data %>%
  filter(order_amount > 1000) %>%
  group_by(segment) %>%
  summarise(n=n())

##4,统计检验
#1,正态性检验
shapiro.test(sample(data$order_amount,500))
#2,t检验
#不同客户消费差异
#premium vs Regular
t.test(order_amount ~ segment,
       data=data %>% filter(segment %in% c("Regular","Premium")))
#3.卡方检验
#不同客户群体在产品选择上存在显著差异
table_data <- table(data$segment, data$Name)


chisq.test(table_data)
#5,A/B Test 实验分析
#1,客单价差异
t.test(order_amount ~ experiment_group,
       data=data)
p<-ggplot(data, aes(x=experiment_group, y=order_amount)) +
  geom_boxplot(fill="skyblue") +
  labs(title="A/B Test: Order Amount Comparison",
       x="Experiment Group",
       y="Order Amount")
ggsave("E:/projet/wine/output/figures/客单价差异.png", p)
#2,转化率差异
data$converted <- ifelse(data$quantity>0,1,0)
conversion <- data %>%
  group_by(experiment_group) %>%
  summarise(conv=sum(converted),
            total=n())

prop.test(conversion$conv, conversion$total)
##6,结果解读
#1,价格敏感度
ggplot(data,aes(x=Price,y=order_amount))+
  geom_point()
#2,产品类型偏好
data %>%
  group_by(segment,Name) %>%
  summarise(n=n())
