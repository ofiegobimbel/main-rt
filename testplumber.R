library(readxl)
library(writexl)
library(jsonlite)
library(readr)
library(mirt)

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
function(req, res, model, itemtype, method, maxiter) {
  filename1 <- paste0(req$body$f$filename, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE), "-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx")
  content <- req$body$f$value
  write_file(content, filename1)

  print(paste0("Model: ", model, " - itemtype: ", itemtype, " - method: ", method, " - maxiter: ", maxiter))

  penalaran_matematika <- read_excel(filename1)
  penalaran_matematika[is.na(penalaran_matematika)] <- "-"

  n_students <- nrow(penalaran_matematika) - 1
  n_questions <- ncol(penalaran_matematika) - 1

  binary_data <- matrix(NA, n_students, n_questions)
  colnames(binary_data) <- colnames(penalaran_matematika)[2:(n_questions + 1)]

  for (x in 1:n_students) {
    for (y in 1:n_questions) {
      binary_data[x, y] <- if ((penalaran_matematika[1, y + 1] == penalaran_matematika[x + 1, y + 1])[1]) 1 else 0
    }
  }

  # calculate the IRT

  fitMirt <- mirt(
    data = binary_data,
    model = model,
    itemtype = itemtype,
    method = method,
    technical = list(NCYCLES = as.numeric(maxiter)),
    verbose = TRUE
  )

  paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
  # round(paramsMirt$items, 4)

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

  theta_est <- fscores(fitMirt, method = "ML")
  pij <- matrix(NA, n_students, n_questions)

  for (i in 1:n_subjects) {
    for (j in 1:n_items) {
      if (itemtype == "3PL") {
        if (paramsMirt$items[j, 3] > 0) {
          pij[i, j] <- irt_3pl_1(
            theta_est[i],
            paramsMirt$items[j, 1],
            paramsMirt$items[j, 2],
            paramsMirt$items[j, 3]
          )
        } else {
          pij[i, j] <- irt_3pl_2(
            theta_est[i],
            paramsMirt$items[j, 5],
            paramsMirt$items[j, 6],
            paramsMirt$items[j, 3]
          )
        }
      } else if (itemtype == "4PL") {
        if (paramsMirt$items[j, 3] > 0) {
          pij[i, j] <- irt_4pl_1(
            theta_est[i],
            paramsMirt$items[j, 1],
            paramsMirt$items[j, 2],
            paramsMirt$items[j, 3],
            paramsMirt$items[j, 4]
          )
        } else {
          pij[i, j] <- irt_4pl_2(
            theta_est[i],
            paramsMirt$items[j, 5],
            paramsMirt$items[j, 6],
            paramsMirt$items[j, 3],
            paramsMirt$items[j, 4]
          )
        }
      } else if (itemtype == "2PL") {
        if (paramsMirt$items[j, 3] == 0) {
          pij[i, j] <- irt_2pl_1(
            theta_est[i],
            paramsMirt$items[j, 1],
            paramsMirt$items[j, 2]
          )
        } else {
          pij[i, j] <- irt_2pl_2(
            theta_est[i],
            paramsMirt$items[j, 5],
            paramsMirt$items[j, 6]
          )
        }
      }
    }
  }

  # output
  output_data <- data.frame(penalaran_matematika[1:n_students + 1, 1], pij)
  colnames(output_data) <- colnames(penalaran_matematika)

  for (i in 1:6) {
    output_data[nrow(output_data) + 1, ] <- NA
  }

  output_data[nrow(output_data) - 5, 1] <- "a"
  output_data[nrow(output_data) - 4, 1] <- "b"
  output_data[nrow(output_data) - 3, 1] <- "g"
  output_data[nrow(output_data) - 2, 1] <- "u"
  output_data[nrow(output_data) - 1, 1] <- "a1"
  output_data[nrow(output_data), 1] <- "d"

  for (j in 1:n_questions) {
    output_data[nrow(output_data) - 5, j + 1] <- paramsMirt[["items"]][j, 1]
    output_data[nrow(output_data) - 4, j + 1] <- paramsMirt[["items"]][j, 2]
    output_data[nrow(output_data) - 3, j + 1] <- paramsMirt[["items"]][j, 3]
    output_data[nrow(output_data) - 2, j + 1] <- paramsMirt[["items"]][j, 4]
    output_data[nrow(output_data) - 1, j + 1] <- paramsMirt[["items"]][j, 5]
    output_data[nrow(output_data), j + 1] <- paramsMirt[["items"]][j, 6]
  }

  write_xlsx(output_data, paste0("output-", filename1))

  res$status <- 201 # created
  return(list(msg = paste0("The message is: '", "'"), msg2 = paste0("The message2 is: '", "'")))
}
