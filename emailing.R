library(mailR)

## send results folder via e-mail
# files <- list.files(results.folder, recursive=TRUE)

body = "I have finished running the codes, and you can go to check them if you want.\n\n\nBest Wishes!"  
recipients <- c("*******@mail2.sysu.edu.cn")   
sender = "********@qq.com"  
title = "Simulation Message"   

send.mail(from = sender,  to = recipients,  subject = title,  body = body,  
          encoding = "utf-8", smtp = list(host.name = "smtp.qq.com", 
                                          port = 465, user.name = sender, passwd = "******", ssl = TRUE), 
          authenticate = TRUE, send = TRUE)
