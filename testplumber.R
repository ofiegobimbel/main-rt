library(readxl)
library(writexl)
library(openxlsx)
library(jsonlite)
library(readr)
library(mirt)
library(stringr)

#* Log some information about the incoming request
#* @filter logger
function(req) {
  # print("LOGGER")
  # print(req)
  # cat(
  #   as.character(Sys.time()), "-",
  #   req$REQUEST_METHOD, req$PATH_INFO, "-",
  #   req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  # )
  plumber::forward()
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
  list(msg = paste0("The message is: '", msg, "'"), msg2 = paste0("The message2 is: '", msg, "'"))
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}

#* Return the sum of two numbers through request body
#* @param a
#* @param b
#* @post /sumbod
function(req, res, a, b) {
  print(a)
  print(b)
  print(req$body)
  print(req$body$a$value)
  print(req$bodyRaw)
  # print(req$postBody$a)
  # print(req$bodyRaw)
  # print(req$body$a)
  # print(req$body$a$value)
  # print(req$body$b$value)
  # as.numeric(req$body$a$value) + as.numeric(req$body$b$value)
  list(summation = as.numeric(req$body$a$value))
}

#* Return the IRT
#* @post /data
function(req, res) {
  # ----------------------------------------------------------------------------
  # get excel file and store it in folder
  filename1 <- paste0(
    req$body$f$filename, sprintf("%04d", sample(9999, 1, TRUE)),
    sample(LETTERS, 1, TRUE), "-",
    format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
    ".xlsx"
  )
  content <- req$body$f$value
  write_file(content, filename1)


  # ----------------------------------------------------------------------------
  # get another form data
  # model_param <- gsub("\\\\n", "\n", rawToChar(req$body$model$value))
  # itemtype_param <- rawToChar(req$body$itemtype$value)
  # method_param <- rawToChar(req$body$method$value)
  # maxiter_param <- as.integer(rawToChar(req$body$maxiter$value))

  # print(paste0("modelParam: ", model_param))
  # print(paste0("itemtypeParam: ", itemtype_param))
  # print(paste0("methodParam: ", method_param))
  # print(paste0("maxiterParam: ", maxiter_param))


  # ----------------------------------------------------------------------------
  # get data and transform to binary
  penalaran_matematika <- read_excel(filename1)
  data_dim <- dim(penalaran_matematika)
  penalaran_matematika <- penalaran_matematika[1:(data_dim[1] - 3), ]
  nama_koloms <- colnames(penalaran_matematika)[4:data_dim[2]]
  mapel <- read_excel(filename1, col_names = FALSE, range = paste0("Data!D", data_dim[1] - 1, ":", int2col(data_dim[2]), data_dim[1] - 1))
  tipe_jawaban <- read_excel(filename1, col_names = FALSE, range = paste0("Data!D", data_dim[1], ":", int2col(data_dim[2]), data_dim[1]))
  tipe_soal <- read_excel("D:/Normalisasi Data Rstudio/hasil-jawaban-tobk-minus1.xlsx", col_names = FALSE, range = paste0("Data!D", data_dim[1] + 1, ":", int2col(data_dim[2]), data_dim[1] + 1))

  n_students <- nrow(penalaran_matematika)
  n_questions <- ncol(tipe_jawaban)
  binary_data <- data.matrix(
    penalaran_matematika[1:n_students, 2:(n_questions + 1)]
  )


  # -----------------------------------------------------------------
  # transform data to binary
  binary_data <- data.matrix(penalaran_matematika[1:n_students, 4:(n_questions + 3)])


  # -----------------------------------------------------------------
  # split binary data to each question type to check if all type is empty
  for (i in 1:n_students) {
    batasBawah <- 0

    for (j in 1:max(tipe_soal)) {
      answers <- binary_data[i, which(tipe_soal == j)]
      nEmptyAnswer <- length(which(answers == -1))

      if (nEmptyAnswer == length(which(tipe_soal == j))) {
        binary_data[i, which(tipe_soal == j)] <- NA
      } else if (nEmptyAnswer != 0) {
        for (k in (batasBawah + which(answers == -1))) {
          if (tipe_jawaban[k] == 1) {
            binary_data[i, k] <- 0
          } else {
            binary_data[i, k] <- 1
          }
        }
      }

      batasBawah <- batasBawah + length(answers)
    }
  }


  # -----------------------------------------------------------------
  # split binary data to dicho and poly (answer type)
  data_dicho <- binary_data[, which(tipe_jawaban == 1)]
  data_poly <- binary_data[, which(tipe_jawaban == 2)]


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
    if (i %in% which(tipe_jawaban == 1)) {
      count_uni <- count_uni + 1
      count_uni2 <- count_uni2 + 1
      idx_params <- append(idx_params, idx_uni)

      if (i == n_questions) {
        if (count_uni2 == 1) {
          unimodel_str <- paste0(unimodel_str, "F", idx_uni, "=", count_uni)
        } else {
          unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
        }
        if (count_multi2 == 1) {
          multimodel_str <- paste0(multimodel_str, "G", idx_multi, "=", count_multi)
        } else {
          multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
        }
      } else {
        if (tipe_soal[[i]] == tipe_soal[[i + 1]]) {
          if (!grepl(paste0("F", idx_uni), unimodel_str)) {
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

      if (i == n_questions) {
        if (count_uni2 == 1) {
          unimodel_str <- paste0(unimodel_str, "F", idx_uni, "=", count_uni)
        } else {
          unimodel_str <- paste0(unimodel_str, "-", count_uni, "\n")
        }
        if (count_multi2 == 1) {
          multimodel_str <- paste0(multimodel_str, "G", idx_multi, "=", count_multi)
        } else {
          multimodel_str <- paste0(multimodel_str, "-", count_multi, "\n")
        }
      } else {
        if (tipe_soal[[i]] == tipe_soal[[i + 1]]) {
          if (!grepl(paste0("G", idx_multi), multimodel_str)) {
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


  # ----------------------------------------------------------------------------
  # calculate the IRT
  # dichotomous model
  unimodel <- unimodel_str
  k <- str_count(unimodel, "=")

  fitMirt <- mirt(
    data = data_dicho,
    model = mirt.model(unimodel),
    itemtype = "3PL",
    method = "MHRM",
    technical = list(NCYCLES = 100, MAXQUAD = 100000),
    verbose = TRUE
  )
  # technical = list(NCYCLES = as.integer(maxiter_param))

  paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
  theta_est <- fscores(fitMirt, method = "ML")

  # polytomous model
  multimodel <- multimodel_str
  k2 <- str_count(multimodel, "=")

  fitMirtPoly <- mirt(
    data = data_poly,
    model = mirt.model(multimodel),
    itemtype = "graded",
    technical = list(NCYCLES = 150, MAXQUAD = 100000),
    verbose = TRUE
  )

  paramsMirtPoly <- coef(fitMirtPoly, IRTpars = TRUE, simplify = TRUE)
  theta_est_poly <- fscores(fitMirtPoly)

  # ----------------------------------------------------------------------------
  # define probability function for 2PL, 3PL, 4PL, Graded
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

  # ----------------------------------------------------------------------------
  # calculate the probability matrix between students and questions
  pij <- matrix(NA, n_students, n_questions)
  itemtype <- "3PL"
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

  # ----------------------------------------------------------------------------
  # generate the output of xls
  # n_koef <- 0
  # colname2 <- c()

  # if (itemtype_param == "2PL") {
  #   n_koef <- 2
  #   colname2 <- c("MATA UJI", "a", "b", "daya beda", "taraf sukar", "k")
  # } else if (itemtype_param == "3PL") {
  #   n_koef <- 3
  #   colname2 <- c(
  #     "MATA UJI", "a", "b", "g", "daya beda", "taraf sukar",
  #     "tebakan semu", "k"
  #   )
  # } else if (itemtype_param == "4PL") {
  #   n_koef <- 3
  #   colname2 <- c(
  #     "MATA UJI", "a", "b", "g", "u", "daya beda",
  #     "taraf sukar", "tebakan semu", "k"
  #   )
  # }

  output_data <- data.frame(penalaran_matematika[1:n_students,1], pij, p_dan_ceeb)
  colnames(output_data) <- c(colnames(penalaran_matematika[c(1,4:data_dim[2])]), paste0('p - ', unique(c(mapel))), paste0('ceeb - ', unique(c(mapel))))

  # n_param_irt <- dim(paramsMirt$items)[2]

  # output_data_2 <- data.frame(
  #   paste0("KPU", 1:n_questions),
  #   matrix(NA, nrow = n_questions, ncol = n_koef + 3),
  #   matrix(0.2, nrow = n_questions, ncol = 1)
  # )
  # colnames(output_data_2) <- colname2

  # for (i in 1:n_questions) {
  #   z <- which(mapel_arr == mapel[[i + 1]])

  #   for (j in 1:n_koef) {
  #     if (j == 1) {
  #       output_data_2[i, j + 1] <- paramsMirt$items[i, z]

  #       if (paramsMirt$items[i, z] < 0.31) {
  #         output_data_2[i, j + 1 + n_koef] <- "Perlu Direvisi"
  #       } else if (paramsMirt$items[i, z] <= 0.66) {
  #         output_data_2[i, j + 1 + n_koef] <- "Jelek"
  #       } else if (paramsMirt$items[i, z] <= 1.34) {
  #         output_data_2[i, j + 1 + n_koef] <- "Cukup"
  #       } else if (paramsMirt$items[i, z] <= 1.69) {
  #         output_data_2[i, j + 1 + n_koef] <- "Baik"
  #       } else {
  #         output_data_2[i, j + 1 + n_koef] <- "Sangat Baik"
  #       }
  #     } else {
  #       output_data_2[i, j + 1] <- paramsMirt$items[i, k + j - 1]

  #       if (j == 2) {
  #         if (paramsMirt$items[i, k + j - 1] < -2) {
  #           output_data_2[i, j + 1 + n_koef] <- "Sangat Mudah"
  #         } else if (paramsMirt$items[i, k + j - 1] <= -1) {
  #           output_data_2[i, j + 1 + n_koef] <- "Mudah"
  #         } else if (paramsMirt$items[i, k + j - 1] <= 1) {
  #           output_data_2[i, j + 1 + n_koef] <- "Sedang"
  #         } else if (paramsMirt$items[i, k + j - 1] <= 2) {
  #           output_data_2[i, j + 1 + n_koef] <- "Sulit"
  #         } else {
  #           output_data_2[i, j + 1 + n_koef] <- "Sangat Sulit"
  #         }
  #       }

  #       if (j == 3) {
  #         if (paramsMirt$items[i, k + j - 1] < 0.2) {
  #           output_data_2[i, j + 1 + n_koef] <- "Baik"
  #         } else {
  #           output_data_2[i, j + 1 + n_koef] <- "Tidak Baik"
  #         }
  #       }
  #     }
  #   }
  # }

  write_xlsx(
    list(skor = output_data),
    # list(skor_kpu = output_data, irt_kpu = output_data_2),
    paste0("output-", filename1)
  )


  # ----------------------------------------------------------------------------
  # success response
  res$status <- 201 # created
  return(list(
    msg = paste0("The message is: '", "'"),
    msg2 = paste0("The message2 is: '", "'")
  ))
}
