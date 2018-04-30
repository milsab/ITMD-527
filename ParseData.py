#Milad Sabouri

import csv
import itertools

with open(r"Data\bitc_temp.csv") as readFile, open(r"Data\bitc_modified.csv", "w") as writeFile:
    readCSV1 = csv.reader(readFile, delimiter=',')
    readCSV2 = csv.reader(readFile, delimiter=',')

    i = 0
    previousRow = [];
    for a, b in zip(readCSV1, readCSV1):
        i = i + 1
        print("THe A List: ")
        print( a)
        print("THe B List: ")
        print( b)
        data = [b[0], b[5], a[1], a[2], a[3], a[4], a[6], a[7], a[8]];
        if i > 10:
            break
        try:
            writeFile.write(','.join(data) + '\n')
        except:
            print("Error to write data")

try:
    readFile.close()
    writeFile.close()
    print("All Data Successfully wrote to the file!")
except:
    print("error to final wrote")