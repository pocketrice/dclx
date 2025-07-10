import pandas as pd
import matplotlib.pyplot as plt
plt.rcParams["figure.figsize"] = [7.00, 3.50]
plt.rcParams["figure.autolayout"] = True
columns = ["time", "hr"]

dat = pd.read_csv("../data/HR_001b.csv", usecols=columns)
print("Contents:", dat)

plt.plot(dat.time, dat.hr)
plt.show()
