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
  print("LOGGER")
  print(req)
  cat(
    as.character(Sys.time()), "-",
    req$REQUEST_METHOD, req$PATH_INFO, "-",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n"
  )
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
  model_param <- gsub("\\\\n", "\n", rawToChar(req$body$model$value))
  itemtype_param <- rawToChar(req$body$itemtype$value)
  method_param <- rawToChar(req$body$method$value)
  maxiter_param <- as.integer(rawToChar(req$body$maxiter$value))

  print(paste0("modelParam: ", model_param))
  print(paste0("itemtypeParam: ", itemtype_param))
  print(paste0("methodParam: ", method_param))
  print(paste0("maxiterParam: ", maxiter_param))

  # ----------------------------------------------------------------------------
  # get data and transform to binary
  penalaran_matematika <- read_excel(filename1)
  data_dim <- dim(penalaran_matematika)
  penalaran_matematika <- penalaran_matematika[1:(data_dim[1] - 1), ]
  mapel <- read_excel(filename1,
    col_names = FALSE,
    range = paste0(
      "Data!A", data_dim[1] + 1, ":",
      int2col(data_dim[2]), data_dim[1] + 1
    )
  )

  n_students <- nrow(penalaran_matematika)
  n_questions <- ncol(penalaran_matematika) - 1
  binary_data <- data.matrix(
    penalaran_matematika[1:n_students, 2:(n_questions + 1)]
  )

  # ----------------------------------------------------------------------------
  # get mapel array
  mapel_arr <- list()
  mapel_arr_question <- list()

  for (i in 2:(data_dim[2])) {
    if (!mapel[[i]] %in% mapel_arr) {
      mapel_arr <- append(mapel_arr, mapel[[i]])
      mapel_arr_question <- append(mapel_arr_question, i - 1)
    } else {
      idx <- which(mapel_arr == mapel[[i]])
      mapel_arr_question[[idx]] <- append(mapel_arr_question[[idx]], i - 1)
    }
  }

  # ----------------------------------------------------------------------------
  # calculate the IRT
  k <- str_count(model_param, "=")

  fitMirt <- mirt(
    data = binary_data,
    model = mirt.model(model_param),
    itemtype = itemtype_param,
    method = method_param,
    technical = list(NCYCLES = as.integer(maxiter_param)),
    verbose = TRUE
  )
  paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
  theta_est <- fscores(fitMirt, method = "ML")

  # ----------------------------------------------------------------------------
  # define probability function for 2PL, 3PL, 4PL
  irt_2pl_1 <- function(theta, a, b) {
    p <- 1 / (1 + exp(-1.702 * a * (theta - b)))
    p
  }
  irt_2pl_2 <- function(theta, a, b) {
    p <- 1 / (1 + exp(-1.702 * (a * theta + b)))
    p
  }
  irt_3pl_1 <- function(theta, a, b, c) {
    p <- c + (1 - c) / (1 + exp(-1.702 * a * (theta - b)))
    p
  }
  irt_3pl_2 <- function(theta, a, b, c) {
    p <- c + (1 - c) / (1 + exp(-1.702 * (a * theta + b)))
    p
  }
  irt_4pl_1 <- function(theta, a, b, c, d) {
    p <- c + ((d - c) * ((exp(1.702 * a * (theta - b)) / (1 + exp(-1.702 * (a * (theta - b)))))))
    p
  }
  irt_4pl_2 <- function(theta, a, b, c, d) {
    p <- c + ((d - c) * ((exp(1.702 * (a * theta + b)) / (1 + exp(-1.702 * (a * theta + b))))))
    p
  }

  # ----------------------------------------------------------------------------
  # calculate the probability matrix between students and questions
  pij <- matrix(NA, n_students, n_questions)
  p_dan_ceeb <- matrix(0, n_students, 2 * k)
  mean_p <- vector("list", k)
  stdev_p <- vector("list", k)

  for (i in 1:n_students) {
    for (j in 1:n_questions) {
      z <- which(mapel_arr == mapel[[j + 1]])

      if (itemtype_param == "3PL") {
        pij[i, j] <- irt_3pl_1(
          theta_est[i, z],
          paramsMirt$items[j, z],
          paramsMirt$items[j, k + 1],
          paramsMirt$items[j, k + 2]
        )
      } else if (itemtype_param == "4PL") {
        pij[i, j] <- irt_4pl_1(
          theta_est[i, z],
          paramsMirt$items[j, z],
          paramsMirt$items[j, k + 1],
          paramsMirt$items[j, k + 2],
          paramsMirt$items[j, k + 3]
        )
      } else if (itemtype_param == "2PL") {
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
    p_dan_ceeb[, i] <- p_dan_ceeb[, i] / length(mapel_arr_question[[i]])
    mean_p[[i]] <- mean(p_dan_ceeb[, i])
    stdev_p[[i]] <- sd(p_dan_ceeb[, i])
    p_dan_ceeb[, i + k] <- 500 + 100 * ((p_dan_ceeb[, i] - mean_p[[i]]) / stdev_p[[i]])
  }

  # ----------------------------------------------------------------------------
  # generate the output of xls
  n_koef <- 0
  colname2 <- c()

  if (itemtype_param == "2PL") {
    n_koef <- 2
    colname2 <- c("MATA UJI", "a", "b", "daya beda", "taraf sukar", "k")
  } else if (itemtype_param == "3PL") {
    n_koef <- 3
    colname2 <- c(
      "MATA UJI", "a", "b", "g", "daya beda", "taraf sukar",
      "tebakan semu", "k"
    )
  } else if (itemtype_param == "4PL") {
    n_koef <- 3
    colname2 <- c(
      "MATA UJI", "a", "b", "g", "u", "daya beda",
      "taraf sukar", "tebakan semu", "k"
    )
  }

  output_data <- data.frame(
    penalaran_matematika[1:n_students, 1], pij,
    p_dan_ceeb
  )
  colnames(output_data) <- c(
    colnames(penalaran_matematika),
    paste0("p - ", mapel_arr), paste0("ceeb - ", mapel_arr)
  )
  n_param_irt <- dim(paramsMirt$items)[2]

  output_data_2 <- data.frame(
    paste0("KPU", 1:n_questions),
    matrix(NA, nrow = n_questions, ncol = n_koef + 3),
    matrix(0.2, nrow = n_questions, ncol = 1)
  )
  colnames(output_data_2) <- colname2

  for (i in 1:n_questions) {
    z <- which(mapel_arr == mapel[[i + 1]])

    for (j in 1:n_koef) {
      if (j == 1) {
        output_data_2[i, j + 1] <- paramsMirt$items[i, z]

        if (paramsMirt$items[i, z] < 0.31) {
          output_data_2[i, j + 1 + n_koef] <- "Perlu Direvisi"
        } else if (paramsMirt$items[i, z] <= 0.66) {
          output_data_2[i, j + 1 + n_koef] <- "Jelek"
        } else if (paramsMirt$items[i, z] <= 1.34) {
          output_data_2[i, j + 1 + n_koef] <- "Cukup"
        } else if (paramsMirt$items[i, z] <= 1.69) {
          output_data_2[i, j + 1 + n_koef] <- "Baik"
        } else {
          output_data_2[i, j + 1 + n_koef] <- "Sangat Baik"
        }
      } else {
        output_data_2[i, j + 1] <- paramsMirt$items[i, k + j - 1]

        if (j == 2) {
          if (paramsMirt$items[i, k + j - 1] < -2) {
            output_data_2[i, j + 1 + n_koef] <- "Sangat Mudah"
          } else if (paramsMirt$items[i, k + j - 1] <= -1) {
            output_data_2[i, j + 1 + n_koef] <- "Mudah"
          } else if (paramsMirt$items[i, k + j - 1] <= 1) {
            output_data_2[i, j + 1 + n_koef] <- "Sedang"
          } else if (paramsMirt$items[i, k + j - 1] <= 2) {
            output_data_2[i, j + 1 + n_koef] <- "Sulit"
          } else {
            output_data_2[i, j + 1 + n_koef] <- "Sangat Sulit"
          }
        }

        if (j == 3) {
          if (paramsMirt$items[i, k + j - 1] < 0.2) {
            output_data_2[i, j + 1 + n_koef] <- "Baik"
          } else {
            output_data_2[i, j + 1 + n_koef] <- "Tidak Baik"
          }
        }
      }
    }
  }

  write_xlsx(
    list(skor_kpu = output_data, irt_kpu = output_data_2),
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
