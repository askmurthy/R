getRequirementPerLL <- function (x) x["REQUIREMENT"]/(x["LOGIC_LEVELS"]+1)

paths = read.csv ("critPaths.csv", colClasses = "character")
setupPaths = subset (paths, PATH_TYPE == "SETUP")
setupPaths$REQUIREMENT = as.numeric(setupPaths$REQUIREMENT)
setupPaths$LOGIC_LEVELS = as.numeric(setupPaths$LOGIC_LEVELS)
setupPaths$PATH_DELAY = as.numeric(setupPaths$PATH_DELAY)
setupPaths$LOGIC_DELAY = as.numeric(setupPaths$LOGIC_DELAY)
setupPaths$NET_DELAY = as.numeric(setupPaths$NET_DELAY)
setupPaths$SKEW = as.numeric(setupPaths$SKEW)
setupPaths$WNS = as.numeric(setupPaths$WNS)
setupPaths$HIGH_FANOUT = as.numeric(setupPaths$HIGH_FANOUT)
setupPaths$CUMULATIVE_FANOUT = as.numeric(setupPaths$CUMULATIVE_FANOUT)
setupPaths$INITWNS = as.numeric(setupPaths$INITWNS)
setupPaths$PLACEWNS = as.numeric(setupPaths$PLACEWNS)
setupPaths$PREROUTEWNS = as.numeric(setupPaths$PREROUTEWNS)
setupPaths$ROUTEWNS = as.numeric(setupPaths$ROUTEWNS)

setupPaths["ReqPerLL"] = getRequirementPerLL(setupPaths)
cor(setupPaths["WNS"], setupPaths["ReqPerLL"], use="pairwise.complete.obs")

sortedData = setupPaths[order(setupPaths$PLACEWNS, setupPaths$INITWNS), ]











