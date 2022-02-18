library(openxlsx)

wd <- 'D:/'
datawd <- paste0(wd, 'data/')
resultwd <- paste0(wd, 'output/')
picwd <- paste0(resultwd, 'pic/')
filename <- 'results.xlsx'
setwd(resultwd)
data=diag(10)

# 设置格式：createStyle、mergeCells
wb <- createWorkbook()
  tmp_sheetname <-"sheet"
  addWorksheet(wb, sheetName = tmp_sheetname, tabColour = 'yellow')    
  writeData(wb, sheet = tmp_sheetname, x = "注：")
  writeData(wb, sheet = tmp_sheetname, x = data, startRow = 2)
  setColWidths(wb, tmp_sheetname, cols = 1:ncol(data), widths = 'auto')
saveWorkbook(wb, filename, overwrite = T)
openXL(wb)
save.image(paste0(resultwd, week, '.RData'))
