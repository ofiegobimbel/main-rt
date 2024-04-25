library(readxl)
library(writexl)
library(openxlsx)
library(mirt)
library(stringr)


# -----------------------------------------------------------------
# get data and transform

penalaran_matematika <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-minus1.xlsx")
data_dim <- dim(penalaran_matematika)
penalaran_matematika <- penalaran_matematika[1: (data_dim[1]-3),]
nama_koloms <- colnames(penalaran_matematika)[4:data_dim[2]]
mapel <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-minus1.xlsx", col_names = FALSE, range = paste0("Data!D", data_dim[1]-1, ":", int2col(data_dim[2]), data_dim[1]-1))
tipe_jawaban <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-minus1.xlsx", col_names = FALSE, range = paste0("Data!D", data_dim[1], ":", int2col(data_dim[2]), data_dim[1]))
tipe_soal <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-minus1.xlsx", col_names = FALSE, range = paste0("Data!D", data_dim[1]+1, ":", int2col(data_dim[2]), data_dim[1]+1))
# print(penalaran_matematika)
# print(colnames(penalaran_matematika)[2:86])

# print(length(penalaran_matematika[1:n_students+1,1]))
# print(penalaran_matematika[1,1])
# print(penalaran_matematika[2,1])
# print(penalaran_matematika[1:n_students+1,1])
# print(if (penalaran_matematika[1,1] == penalaran_matematika[2,1]) 1 else 0)

n_students = nrow(penalaran_matematika)
n_questions = ncol(tipe_jawaban)
# print(n_students)
# print(n_questions)


# -----------------------------------------------------------------
# get mapel array

mapel_arr <- list()
mapel_arr_question <- list()
poly_arr < c()

for (i in 4:(data_dim[2]-1)) {
  if ( ! mapel[[i]] %in% mapel_arr ) {
    mapel_arr = append(mapel_arr, mapel[[i]])
    mapel_arr_question = append(mapel_arr_question, i-1)
  } else {
    idx = which(mapel_arr == mapel[[i]])
    mapel_arr_question[[idx]] = append(mapel_arr_question[[idx]], i-1)
  }
}


# -----------------------------------------------------------------
# transform data to binary

binary_data <- data.matrix(penalaran_matematika[1:n_students, 4:(n_questions+3)])
# print(binary_data)
# print(length(binary_data[1:n_students,1])
# binary_data[1,1] = 1
# print(binary_data[,1])

itemstats(binary_data)

# -----------------------------------------------------------------
# split binary data to each question type to check if all type is empty

for (i in 1:n_students) {
  batasBawah <- 0
  
  for (j in 1:max(tipe_soal)) {
    answers <- binary_data[i, which(tipe_soal == j)]
    nEmptyAnswer <- length(which(answers == -1))
    
    if (nEmptyAnswer == length(which(tipe_soal == j))) {
      binary_data[i, which(tipe_soal == j)] = NA
    } else if (nEmptyAnswer != 0) {
      for (k in (batasBawah+which(answers == -1))) {
        if (tipe_jawaban[k] == 1) {
          binary_data[i, k] = 0
        } else {
          binary_data[i, k] = 1
        }
      }
    }
    
    batasBawah <- batasBawah + length(answers)
  }
}


# -----------------------------------------------------------------
# split binary data to dicho and poly (answer type)

data_dicho <- binary_data[,which(tipe_jawaban == 1)]
data_poly <- binary_data[,which(tipe_jawaban == 2)]



# -----------------------------------------------------------------
# get model string

unimodel_str <- ""
multimodel_str <- ""
idx_uni <- 1
idx_multi <- 1
idx_params <- c()
count_uni <- 0
count_multi <- 0
count_uni2 <- 0
count_multi2 <- 0

for (i in 1:n_questions) {
  if ( i %in% which(tipe_jawaban == 1) ) {
    count_uni <- count_uni + 1
    count_uni2 <- count_uni2 + 1
    idx_params <- append(idx_params, idx_uni)
    
    if ( i == n_questions ) {
      if ( count_uni2 == 1 ) {
        unimodel_str <- paste0(unimodel_str, "F", idx_uni, "=", count_uni)
      } else {
        unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
      }
      if ( count_multi2 == 1 ) {
        multimodel_str <- paste0(multimodel_str, "G", idx_multi, "=", count_multi)
      } else {
        multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
      }
    } else {
      if ( tipe_soal[[i]] == tipe_soal[[i+1]] ) {
        if ( !grepl(paste0("F",idx_uni), unimodel_str) ) {
          unimodel_str <- paste0(unimodel_str, "F", idx_uni, "=", count_uni)
        }
      } else {
        if (count_uni2 > 1) {
          unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
          idx_uni <- idx_uni + 1
          count_uni2 <- 0
        } else if (count_uni2 == 1) {
          idx_uni <- idx_uni + 1
          count_uni2 <- 0
        }
        if (count_multi2 > 1) {
          multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
          idx_multi <- idx_multi + 1
          count_multi2 <- 0
        } else if (count_multi2 == 1) {
          idx_multi <- idx_multi + 1
          count_multi2 <- 0
        }
      }
    }
  } else {
    count_multi <- count_multi + 1
    count_multi2 <- count_multi2 + 1
    idx_params <- append(idx_params, idx_multi)
    
    if ( i == n_questions ) {
      if ( count_uni2 == 1 ) {
        unimodel_str <- paste0(unimodel_str, "F", idx_uni, "=", count_uni)
      } else {
        unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
      }
      if ( count_multi2 == 1 ) {
        multimodel_str <- paste0(multimodel_str, "G", idx_multi, "=", count_multi)
      } else {
        multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
      }
    } else {
      if ( tipe_soal[[i]] == tipe_soal[[i+1]] ) {
        if ( !grepl(paste0("G",idx_multi), multimodel_str) ) {
          multimodel_str <- paste0(multimodel_str, "G", idx_multi, "=", count_multi)
        }
        
      } else {
        if (count_uni2 > 1) {
          unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
          idx_uni <- idx_uni + 1
          count_uni2 <- 0
        } else if (count_uni2 == 1) {
          idx_uni <- idx_uni + 1
          count_uni2 <- 0
        }
        if (count_multi2 > 1) {
          multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
          idx_multi <- idx_multi + 1
          count_multi2 <- 0
        } else if (count_multi2 == 1) {
          idx_multi <- idx_multi + 1
          count_multi2 <- 0
        }
      }
    }
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

# use parallel computing
# if(interactive()) mirtCluster()

# dichotomous model
unimodel <- unimodel_str
k <- str_count(unimodel, "=")

fitMirt <- mirt(data = data_dicho, 
               model = mirt.model(unimodel),
               itemtype = "3PL", 
               method = 'MHRM',
               technical = list(NCYCLES=100, MAXQUAD=100000),
               verbose = TRUE)

paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
theta_est <- fscores(fitMirt, method = "ML")
round(paramsMirt$items, 4)

# polytomous model
multimodel <- multimodel_str
k2 <- str_count(multimodel, "=")

fitMirtPoly <- mirt(data = data_poly, 
                model = mirt.model(multimodel),
                itemtype = "graded",
                technical = list(NCYCLES=150, MAXQUAD=100000),
                verbose = TRUE)

paramsMirtPoly <- coef(fitMirtPoly, IRTpars = TRUE, simplify = TRUE)
theta_est_poly <- fscores(fitMirtPoly)
round(paramsMirtPoly$items, 4)

# options(max.print = .Machine$integer.max)
# options(max.print = 2828)

# define probability function
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

irt_graded_2pl <- function(k, K, theta, a, b) {
  jumlah_exp <- 0
  for (j in 1:(K-1)) {
    jumlah_exp <- jumlah_exp + exp(a[j] * (theta - b))
  }
  hasil <- exp(a[k-1] * (theta - b)) / (1 + jumlah_exp)
  hasil
}

irt_graded <- function(k, K, theta, a, b){
  x <- 1
  if (k != 1) {
    x <- irt_graded_2pl(k, K, theta, a, b)
  }
  y <- 0
  if (k != K) {
    y <- irt_graded_2pl(k+1, K, theta, a, b)
  }
  p <- x - y
  p
}

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

p_dan_ceeb <- matrix(0, n_students, 2*(tipe_soal[[n_questions]]))
mean_p <- vector("list", tipe_soal[[n_questions]])
stdev_p <- vector("list", tipe_soal[[n_questions]])

for (i in 1:n_students) {
  for (j in 1:n_questions) {
    # z <- which(mapel_arr == mapel[[j]])
    pij[i, j] <- 0
    
    if (!is.na(binary_data[i, nama_koloms[j]])) {
    
      if ( j %in% which(tipe_jawaban == 1) ) {
        
        if (itemtype == "3PL") {
          pij[i, j] <- irt_3pl_1(
            theta_est[[i, idx_params[j]]],
            paramsMirt$items[nama_koloms[j], idx_params[j]],
            paramsMirt$items[nama_koloms[j], k + 1],
            paramsMirt$items[nama_koloms[j], k + 2]
          )
        } else if (itemtype == "4PL") {
          pij[i, j] <- irt_4pl_1(
            theta_est[[i, idx_params[j]]],
            paramsMirt$items[nama_koloms[j], idx_params[j]],
            paramsMirt$items[nama_koloms[j], k + 1],
            paramsMirt$items[nama_koloms[j], k + 2],
            paramsMirt$items[nama_koloms[j], k + 3]
          )
        } else if (itemtype == "2PL") {
          pij[i, j] <- irt_2pl_1(
            theta_est[[i, idx_params[j]]],
            paramsMirt$items[nama_koloms[j], idx_params[j]],
            paramsMirt$items[nama_koloms[j], k + 1]
          )
        }
      } else {
        # poly
        unikArray <- sort(unique(binary_data[, nama_koloms[j]]))
          
        pij[i, j] <- irt_graded(
          which(unikArray == binary_data[i, nama_koloms[j]]),
          length(unikArray),
          theta_est_poly[[i, idx_params[j]]],
          paramsMirtPoly$items[nama_koloms[j], (k2+1):ncol(paramsMirtPoly$items)],
          paramsMirtPoly$items[nama_koloms[j], idx_params[j]]
        )
      }
      
      p_dan_ceeb[i, tipe_soal[[j]]] <- p_dan_ceeb[i, tipe_soal[[j]]] + pij[i, j]
    } else {
      p_dan_ceeb[i, tipe_soal[[j]]] <- 0
    }
  }
}

for (i in 1:tipe_soal[[n_questions]]) {
  p_dan_ceeb[,i] <- p_dan_ceeb[,i]/length(which(tipe_soal == i))
  
  mean_p[[i]] <- mean(p_dan_ceeb[which(p_dan_ceeb[,i] != 0),i])
  stdev_p[[i]] <- sd(p_dan_ceeb[which(p_dan_ceeb[,i] != 0),i])
  p_dan_ceeb[,i+tipe_soal[[n_questions]]] <- 500 + 100 * ( (p_dan_ceeb[,i] - mean_p[[i]])/stdev_p[[i]] )
  p_dan_ceeb[which(p_dan_ceeb[,i] == 0),i+tipe_soal[[n_questions]]] <- 0
}



# -----------------------------------------------------------------
# output

n_koef <- 0
n_koef2 <- 2
colname2 <- c()
colname3 <- c('MATA UJI-poly', 'a', 'b', 'daya beda', 'taraf sukar', 'k')
  
if (itemtype == '2PL' ) {
  n_koef <- 2
  colname2 <- c('MATA UJI-dicho', 'a', 'b', 'daya beda', 'taraf sukar', 'k')
} else if (itemtype == '3PL' ) {
  n_koef <- 3
  colname2 <- c('MATA UJI-dicho', 'a', 'b', 'g', 'daya beda', 'taraf sukar', 'tebakan semu', 'k')
} else if (itemtype == '4PL' ) {
  n_koef <- 3
  colname2 <- c('MATA UJI-dicho', 'a', 'b', 'g', 'u', 'daya beda', 'taraf sukar', 'tebakan semu', 'k')
}

output_data <- data.frame(penalaran_matematika[1:n_students,1], pij, p_dan_ceeb)
colnames(output_data) <- c(colnames(penalaran_matematika[c(1,4:data_dim[2])]), paste0('p - ', unique(c(mapel))), paste0('ceeb - ', unique(c(mapel))))
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


write_xlsx(list(skor_kpu = output_data), paste0("output-new1-to-73-nullcheck.xlsx"))



