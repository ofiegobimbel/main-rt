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
#* @post /sumbod
function(req, res) {
  print(req$body)
  print(req$body$a)
  print(req$body$a$value)
  print(req$body$b$value)
  as.numeric(req$body$a$value) + as.numeric(req$body$b$value)
}

library(readxl)
library(writexl)
library(jsonlite)
library(readr)
library(mirt)

#* Return the IRT
#* @post /data
function(req, res) {
  filename1 <- paste0(req$body$f$filename, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE), "-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx")
  content <- req$body$f$value
  write_file(content, filename1)

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

  unimodel <- "F1 = 1-20"
  fitMirt <- mirt(
    data = binary_data,
    model = unimodel,
    itemtype = "3PL",
    method = "MHRM",
    technical = list(NCYCLES = 2000),
    verbose = TRUE
  )

  paramsMirt <- coef(fitMirt, IRTpars = TRUE, simplify = TRUE)
  round(paramsMirt$items, 4)

  fit4PL <- mirt(
    data = binary_data,
    model = unimodel,
    itemtype = "4PL",
    method = "QMCEM",
    technical = list(NCYCLES = 100),
    verbose = TRUE
  )

  params4PL <- coef(fit4PL,
    IRTpars = TRUE,
    simplify = TRUE
  )

  fit2PL <- mirt(
    data = binary_data,
    model = unimodel,
    itemtype = "2PL",
    method = "MHRM",
    technical = list(NCYCLES = 100),
    verbose = TRUE
  )

  params2PL <- coef(fit2PL,
    IRTpars = TRUE,
    simplify = TRUE
  )

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

  write_xlsx(dframe, paste0("output-", filename1))

  res$status <- 201
  list(msg = paste0("The message is: '", "'"), msg2 = paste0("The message2 is: '", "'"))
}
