library(readxl)
library(writexl)
library(openxlsx)
library(mirt)
library(stringr)



# -----------------------------------------------------------------
# get data and transform to binary

penalaran_matematika <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-merge.xlsx")
data_dim <- dim(penalaran_matematika)
penalaran_matematika <- penalaran_matematika[1: (data_dim[1]-1), ]
mapel <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-merge.xlsx", col_names = FALSE, range = paste0("Data!A", data_dim[1]+1, ":", int2col(data_dim[2]), data_dim[1]+1))
# print(penalaran_matematika)
# print(colnames(penalaran_matematika)[2:86])

# print(length(penalaran_matematika[1:n_students+1,1]))
# print(penalaran_matematika[1,1])
# print(penalaran_matematika[2,1])
# print(penalaran_matematika[1:n_students+1,1])
# print(if (penalaran_matematika[1,1] == penalaran_matematika[2,1]) 1 else 0)

n_students = nrow(penalaran_matematika)
n_questions = ncol(penalaran_matematika) - 1
# print(n_students)
# print(n_questions)

binary_data <- data.matrix(penalaran_matematika[1:n_students, 2:(n_questions+1)])

# print(binary_data)
# print(length(binary_data[1:n_students,1])
# binary_data[1,1] = 1
# print(binary_data[,1])

itemstats(binary_data)



# -----------------------------------------------------------------
# get mapel array

mapel_arr <- list()
mapel_arr_question <- list()

for (i in 2:(data_dim[2])) {
  if ( ! mapel[[i]] %in% mapel_arr ) {
    mapel_arr = append(mapel_arr, mapel[[i]])
    mapel_arr_question = append(mapel_arr_question, i-1)
  } else {
    idx = which(mapel_arr == mapel[[i]])
    mapel_arr_question[[idx]] = append(mapel_arr_question[[idx]], i-1)
  }
}



# -----------------------------------------------------------------
# calculate the Factor Analysis

n_factors = 2
binary_fa = factanal(binary_data, factors = n_factors)

while (binary_fa[['PVAL']] < 0.05) {
  n_factors = n_factors + 1
  binary_fa = factanal(binary_data, factors = n_factors)
}

binary_fa_abs = apply( binary_fa$loadings, 1, abs)
factor_element_list <- vector("list", length = n_factors)

for (i in 1:n_questions) {
  pos_max <- which.max(binary_fa_abs[, i])
  print(paste0(i, ' : ', pos_max))
  if ( is.null(factor_element_list[[pos_max]]) ) {
    factor_element_list[[pos_max]] <- paste0(i)
  } else {
    factor_element_list[[pos_max]] <- paste0(factor_element_list[[pos_max]], ",", i)
  }
}

string_factor <- ''

for (i in 1:length(factor_element_list)) {
  if ( !is.null(factor_element_list[[i]]) ) {
    if ( nchar(string_factor) == 0 ) {
      string_factor <- paste0('F', i, ' = ', factor_element_list[[i]])
    } else {
      string_factor <- paste0(string_factor, '\nF', i, ' = ', factor_element_list[[i]])
    }
  }
}



# -----------------------------------------------------------------
# calculate the IRT

unimodel <- 'F1=1-30\nF2=31-50\nF3=51-70\nF4=71-85'
k <- str_count(unimodel, "=")

fitMirt <- mirt(data = binary_data, 
               model = mirt.model(unimodel),
               itemtype = "3PL", 
               method = 'MHRM',
               technical = list(NCYCLES=1000),
               verbose = TRUE)

paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
round(paramsMirt$items, 4)

# options(max.print = .Machine$integer.max)
# options(max.print = 2828)

irt_2pl_1 <- function(theta, a, b){
  p <- 1 / (1 + exp(-1.702 * a * (theta - b)))
  p
}

irt_2pl_2 <- function(theta, a, b){
  p <- 1 / (1 + exp(-1.702 * (a*theta + b)))
  p
}

irt_3pl_1 <- function(theta, a, b, c){
  p <- c + (1 - c) / (1 + exp(-1.702 * a * (theta - b)))
  p
}

irt_3pl_2 <- function(theta, a, b, c){
  p <- c + (1 - c) / (1 + exp(-1.702 * (a*theta + b)))
  p
}

irt_4pl_1 <- function(theta, a, b, c, d){
  p <- c + ((d - c) * ((exp(1.702*a*(theta - b)) / (1 + exp(-1.702*(a * (theta - b)))))))
  p
}

irt_4pl_2 <- function(theta, a, b, c, d){
  p <- c + ((d - c) * ((exp(1.702*(a*theta + b)) / (1 + exp(-1.702*(a*theta + b))))))
  p
}

theta_est <- fscores(fitMirt, method = "ML")
pij <- matrix(NA, n_students, n_questions)
itemtype <- "3PL"

for (i in 1:n_students) {
  for (j in 1:n_questions) {
    if (itemtype == "3PL") {
      if (paramsMirt$items[j,3] > 0) {
        pij[i, j] <- irt_3pl_1(theta_est[i],
                           paramsMirt$items[j,1],
                           paramsMirt$items[j,2],
                           paramsMirt$items[j,3])
      } else {
        pij[i, j] <- irt_3pl_2(theta_est[i],
                           paramsMirt$items[j,5],
                           paramsMirt$items[j,6],
                           paramsMirt$items[j,3])
      }
    } else if (itemtype == "4PL") {
      if (paramsMirt$items[j,3] > 0) {
        pij[i, j] <- irt_4pl_1(theta_est[i],
                             paramsMirt$items[j,1],
                             paramsMirt$items[j,2],
                             paramsMirt$items[j,3],
                             paramsMirt$items[j,4])
      } else {
        pij[i, j] <- irt_4pl_2(theta_est[i],
                             paramsMirt$items[j,5],
                             paramsMirt$items[j,6],
                             paramsMirt$items[j,3],
                             paramsMirt$items[j,4])
      }
    } else if (itemtype == "2PL") {
      if (paramsMirt$items[j,3] == 0) {
        pij[i, j] <- irt_2pl_1(theta_est[i],
                             paramsMirt$items[j,1],
                             paramsMirt$items[j,2])
      } else {
        pij[i, j] <- irt_2pl_2(theta_est[i],
                             paramsMirt$items[j,5],
                             paramsMirt$items[j,6])
      }
    }
  }
}

# alternative

# unimodel_split <- strsplit(unimodel, split = "\n")[[1]]

# factor_criteria <- vector("list", length = length(unimodel_split))

# for (i in 1:length(unimodel_split)) {
#   k <- str_count(unimodel_split[[i]], ",")
# }

p_dan_ceeb <- matrix(0, n_students, 2*k)
mean_p <- vector("list", k)
stdev_p <- vector("list", k)

for (i in 1:n_students) {
  for (j in 1:n_questions) {
    z <- which(mapel_arr == mapel[[j+1]])
    
    if (itemtype == "3PL") {
      pij[i, j] <- irt_3pl_1(
        theta_est[i, z],
        paramsMirt$items[j, z],
        paramsMirt$items[j, k + 1],
        paramsMirt$items[j, k + 2]
      )
    } else if (itemtype == "4PL") {
      pij[i, j] <- irt_4pl_1(
        theta_est[i, z],
        paramsMirt$items[j, z],
        paramsMirt$items[j, k + 1],
        paramsMirt$items[j, k + 2],
        paramsMirt$items[j, k + 3]
      )
    } else if (itemtype == "2PL") {
      pij[i, j] <- irt_2pl_1(
        theta_est[i, z],
        paramsMirt$items[j, z],
        paramsMirt$items[j, k + 1]
      )
    }
    
    p_dan_ceeb[i, z] <- p_dan_ceeb[i, z] + pij[i, j]
  }
}

for (i in 1:k) {
  p_dan_ceeb[,i] <- p_dan_ceeb[,i]/length(mapel_arr_question[[i]])
  mean_p[[i]] <- mean(p_dan_ceeb[,i])
  stdev_p[[i]] <- sd(p_dan_ceeb[,i])
  p_dan_ceeb[,i+k] <- 500 + 100 * ( (p_dan_ceeb[,i] - mean_p[[i]])/stdev_p[[i]] )
}



# -----------------------------------------------------------------
# output

n_koef <- 0
colname2 <- c()
  
if (itemtype == '2PL' ) {
  n_koef <- 2
  colname2 <- c('MATA UJI', 'a', 'b', 'daya beda', 'taraf sukar', 'k')
} else if (itemtype == '3PL' ) {
  n_koef <- 3
  colname2 <- c('MATA UJI', 'a', 'b', 'g', 'daya beda', 'taraf sukar', 'tebakan semu', 'k')
} else if (itemtype == '4PL' ) {
  n_koef <- 3
  colname2 <- c('MATA UJI', 'a', 'b', 'g', 'u', 'daya beda', 'taraf sukar', 'tebakan semu', 'k')
}

output_data <- data.frame(penalaran_matematika[1:n_students,1], pij, p_dan_ceeb)
colnames(output_data) <- c(colnames(penalaran_matematika), paste0('p - ', mapel_arr), paste0('ceeb - ', mapel_arr))
n_param_irt <- dim(paramsMirt$items)[2]

output_data_2 <- data.frame(paste0('KPU', 1:n_questions), matrix(NA, nrow = n_questions, ncol = n_koef+3), matrix(0.2, nrow = n_questions, ncol = 1))
colnames(output_data_2) <- colname2

for (i in 1:n_questions) {
  z <- which(mapel_arr == mapel[[i+1]])
  
  for (j in 1:n_koef) {
    if (j == 1) {
      output_data_2[i, j+1] <- paramsMirt$items[i, z]
      
      if (paramsMirt$items[i, z] < 0.31) {
        output_data_2[i, j+1+n_koef] <- 'Perlu Direvisi'
      } else if (paramsMirt$items[i, z] <= 0.66) {
        output_data_2[i, j+1+n_koef] <- 'Jelek'
      } else if (paramsMirt$items[i, z] <= 1.34) {
        output_data_2[i, j+1+n_koef] <- 'Cukup'
      } else if (paramsMirt$items[i, z] <= 1.69) {
        output_data_2[i, j+1+n_koef] <- 'Baik'
      } else {
        output_data_2[i, j+1+n_koef] <- 'Sangat Baik'
      }
    } else {
      output_data_2[i, j+1] <- paramsMirt$items[i, k+j-1]
      
      if (j == 2) {
        if (paramsMirt$items[i, k+j-1] < -2) {
          output_data_2[i, j+1+n_koef] <- 'Sangat Mudah'
        } else if (paramsMirt$items[i, k+j-1] <= -1) {
          output_data_2[i, j+1+n_koef] <- 'Mudah'
        } else if (paramsMirt$items[i, k+j-1] <= 1) {
          output_data_2[i, j+1+n_koef] <- 'Sedang'
        } else if (paramsMirt$items[i, k+j-1] <= 2) {
          output_data_2[i, j+1+n_koef] <- 'Sulit'
        } else {
          output_data_2[i, j+1+n_koef] <- 'Sangat Sulit'
        }
      }
      
      if (j == 3) {
        if (paramsMirt$items[i, k+j-1] < 0.2) {
          output_data_2[i, j+1+n_koef] <- 'Baik'
        } else {
          output_data_2[i, j+1+n_koef] <- 'Tidak Baik'
        }
      }
    }
    
  }
}


write_xlsx(list(skor_kpu = output_data, irt_kpu = output_data_2), paste0("output-21-25-maret-to-02183.xlsx"))



