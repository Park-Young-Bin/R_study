# DataEditR

# 엑셀 파일을 다루듯 데이터 프레임을 직접 수정 가능
# 출처: https://dillonhammill.github.io/DataEditR/?fbclid=IwAR3R6kxQve565pTx_-srN2CiALkuWnj6fz7JDSK2YXWFB3PfQXvD1Ez7HnE

# Installation
install.packages("DataEditR")
library(devtools)
install_github("DillonHammill/DataEditR")
devtools::install_github("DillonHammill/rhandsontable")

# Load required packages
library(DataEditR)

# Save output to R object & csv file(편집 작업 후 save selection to file 클릭)
mtcars_new <- data_edit(mtcars,
                        save_as = "mtcars_new.csv")
