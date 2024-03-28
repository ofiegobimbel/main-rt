import rpy2
import rpy2.robjects as robjects

# import rpy2.ipython.html
# rpy2.ipython.html.init_printing()

from rpy2.robjects.packages import importr, data

utils = importr('utils')
base = importr('base')

# utils.chooseCRANmirror(graphics=False, ind=0)
# utils.getCRANmirrors(all=True)
utils.install_packages('lme4', type="binary", lib="D:/Normalisasi Data Rstudio/mylibrary", repos = "https://cloud.r-project.org")
utils.install_packages('stats', type="binary", lib="D:/Normalisasi Data Rstudio/mylibrary", repos = "https://cloud.r-project.org")
utils.install_packages('ggplot2', type="binary", lib="D:/Normalisasi Data Rstudio/mylibrary", repos = "https://cloud.r-project.org")
utils.install_packages('lazyeval', type="binary", lib="D:/Normalisasi Data Rstudio/mylibrary", repos = "https://cloud.r-project.org")
utils.install_packages('mirt', type="binary", lib="D:/Normalisasi Data Rstudio/mylibrary", repos = "https://cloud.r-project.org")

# stats = importr('stats')
# lme4 = importr('lme4')
# import rpy2.robjects.lib.ggplot2 as ggplot2

# # from rpy2.ipython.ggplot import image_png

# sleepstudy = data(lme4).fetch('sleepstudy')['sleepstudy']
# print(sleepstudy)

# gp = ggplot2.ggplot(sleepstudy)

# p1 = (gp +
#       ggplot2.aes_string(x = 'Days', y = 'Reaction') +
#       ggplot2.geom_point(color = '#648FFF', alpha = 0.7) +
#       ggplot2.geom_smooth(method = 'lm', color = 'black') +
#       ggplot2.theme_minimal())

# # redisplay(image_png(p1))

# p2 = (p1 +
#       ggplot2.facet_wrap(robjects.Formula('. ~ Subject'), **{'ncol':6}))

# # redisplay(image_png(p2))

# lm1 = stats.lm('Reaction ~ Days', data = sleepstudy)
# print(stats.coef(lm1))
# print(stats.confint(lm1))

# fm1 = lme4.lmer('Reaction ~ Days + (Days | Subject)', data = sleepstudy)
# print(base.summary(fm1))
# print()

import pandas as pd
import math

my_data = pd.read_csv("data-sampel.csv", sep=";", low_memory=False, index_col=False)

mirt = importr('mirt', lib_loc="D:/Normalisasi Data Rstudio/mylibrary")

# penalaran_matematika <- read_excel("D:/Normalisasi Data Rstudio/data-sampel.xlsx", sheet = "Sheet3")
# PM <- penalaran_matematika[,2:86]

fit3PL = mirt(data = my_data, 
               model = 'F1 = 1-20',
               itemtype = "3PL", 
               method = 'MHRM',
               verbose = True)

params3PL = mirt.coef(fit3PL, IRTpars = True, simplify = True)
print(params3PL)
# round(params3PL$items, 4)

# options(max.print = .Machine$integer.max)
# options(max.print = 2828)

def irt_p(theta, a, b, c):
  return c + (1 - c) / (1 + math.exp(-1.702 * a * (theta - b)))

theta_est = mirt.fscores(fit3PL, method = "ML")
n_subjects = 2828
n_items = 85

pij = pd.DataFrame(columns=range(n_items), index=range(n_subjects))

# for i in range(n_subjects):
#   for j in range(n_items):
#     if (params3PL$items[j,3] > 0) {
#       pij[i, j] <- irt_p(theta_est[i],
#                          params3PL$items[j,1],
#                          params3PL$items[j,2],
#                          params3PL$items[j,3])
#     } else {
#       pij[i, j] <- irt_p(theta_est[i],
#                          params3PL$items[j,5],
#                          params3PL$items[j,6],
#                          params3PL$items[j,3])
#     }

# print(pij)