#' Perveived Fairness of rules for allocating seats in trains and parking spaces
#' @name fairness
#' @docType data
#' @keywords dataset
#' @description  a pseudo-panel of 401 individuals from 2003 
#' @format a tibble containing:
#' - id: the individual index
#' - answer: a factor with levels 0 (very unfair), 1 (essentially unfair), 2 (essentially fair) and 3 (very fair)
#' - good: one of \code{'tgv'} (French fast train) and \code{'Parking'}
#' - rule: the allocation rule, a factor with levels \code{'peak'}, \code{'admin'}, \code{'lottery'}, \code{'addsupply'}, \code{'queuing'}, \code{'moral'} and \code{'compensation'}
#' - driving: does the individual has the driving license ?
#' - education: does the individual has a diploma ?
#' - recurring: does the allocation problem is reccuring ?
#' @source provided by the authors
#' @references
#' \insertRef{RAUX:SOUC:CROIS:09}{pglm}
#' @importFrom Rdpack reprompt
NULL

#' Health Insurance and Doctor Visits
#' @name health_ins
#' @docType data
#' @keywords dataset
#' @description  a cross-section of 5908 individuals from 1974 to 1982 
#' @format a tibble containing:
#' - id: the individual index
#' - year: the year
#' - mdu: number of outpatient visits to an MD
#' - opu: number of outpation visits to all providers
#' - coins: coinsurance rate (0, 25, 50 or 100 percent)
#' - idp: if individual deductible plan: 1, otherwise 0
#' - lpi: log of the max of 1 and annual participation incentive payment
#' - fmde: if idp = 1: 0 otherwise ln of the max of 1 and MDE / (0.01 coins)
#' - income: family income
#' - size: family size
#' - age: the age
#' - sex: a factor with level \code{'male'} and \code{'female'}
#' - child: a factor with levels \code{'no'} and \code{'yes'}
#' - race: a factor with levels \code{'white'} and \code{'black'}
#' - health: self-rated health, a factor with levels `poor`, `fair`, `good` and `verygood`
#' - educ: education of the household head in years
#' - physlim: if the person has a physical limitation: 1
#' - disease: index of chronic diseases
#' @source \url{http://cameron.econ.ucdavis.edu/musbook/mus.html}
#' @references
#' \insertRef{MANN:NEWH:KEEL:LEIB:87}{pglm}
#' 
#' \insertRef{DEB:TRIV:02}{pglm}
#' @importFrom Rdpack reprompt
NULL

#' Hedonic Prices of Census Tracts in the Boston Area
#' @name hedonic
#' @docType data
#' @keywords dataset
#' @description  a cross-section of 506 census tracts 
#' @format a tibble containing:
#' - mv: median value of owner--occupied homes
#' - crim: crime rate
#' - zn: proportion of 25,000 square feet residential lots
#' - indus: proportion of no--retail business acres
#' - chas: is the tract bounds the Charles River?
#' - nox: annual average nitrogen oxide concentration in parts per hundred million
#' - rm: average number of rooms
#' - age: proportion of owner units built prior to 1940
#' - dis: weighted distances to five employment centers in the Boston area
#' - rad: index of accessibility to radial highways
#' - tax: full value property tax rate ($/$10,000)
#' - ptratio: pupil/teacher ratio
#' - blacks: proportion of blacks in the population
#' - lstat: proportion of population that is lower status
#' - townid: town identifier
#' @source Online complements to Baltagi (2013): \url{https://bcs.wiley.com/he-bcs/Books?action=resource&bcsId=4338&itemId=1118672321&resourceId=13452}
#' @references
#' \insertRef{BALT:01}{pglm}
#' 
#' \insertRef{BALT:13}{pglm}
#' 
#' \insertRef{BESL:KUH:WELS:80}{pglm}
#' 
#' \insertRef{HARR:RUBI:78}{pglm}
#' @importFrom Rdpack reprompt
NULL

#' Dynamic Relation Between Patents and R&D
#' @name patents_rd
#' @docType data
#' @keywords dataset
#' @description  yearly observations of 346 production units 
#' @format a tibble containing:
#' - cusip: Compustat's identifying number for the firm
#' - year: year
#' - ardssic: a two-digit code for the applied R
#' - scisect: is the firm in the scientific sector ?
#' - capital72: book value of capital in 1972
#' - sumpat: the sum of patents applied for between 1972-1979
#' - rd: R and D spending during the year (in 1972 dollars)
#' - patents: the number of patents applied for during the year that were eventually granted
#' @source \url{http://cameron.econ.ucdavis.edu/racd/racddata.html}, chapter 9.
#' @references
#' \insertRef{HALL:GRIL:HAUS:86}{pglm}
#' @importFrom Rdpack reprompt
NULL

#' Unionism and wage rate determination
#' @name union_wage
#' @docType data
#' @keywords dataset
#' @description  yearly observations of 545 individuals from 1980 to 1987 
#' @format a tibble containing:
#' - id: the individual index
#' - year: the year
#' - exper: the experience, computed as age - 6 - schooling
#' - health: does the individual has health disability ?
#' - hours: the number of hours worked
#' - married: is the individual married ?
#' - rural: does the individual lives in a rural area ?
#' - school: years of schooling
#' - union: does the wage is set by collective bargaining
#' - wage: hourly wage in US dollars
#' - sector: one of agricultural, mining, construction, trade, transportation, finance,  businessrepair, personalservice, entertainment, manufacturing, pro.rel.service, pub.admin
#' - occ: one of proftech, manoffpro, sales, clerical, craftfor, operative, laborfarm, farmlabor, service
#' - com: one of black, hisp and other
#' - region: the region, one of NorthEast, NothernCentral, South and other
#' @source Journal of Applied Econometrics Data Archive : \url{http://qed.econ.queensu.ca/jae/}
#' @references
#' \insertRef{VELL:VERB:98}{pglm}
#' @importFrom Rdpack reprompt
NULL

