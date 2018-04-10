import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt

'''/*================================================================
==================    METADATA OF THE FILE      =====================
===================================================================*/
/**
 * @author  Rosas Hernandez Oscar Andres
 * @author  Alan Enrique Ontiveros Salazar
 * @author  Laura Andrea Morales 
 * @version 0.1
 * @team    CompilandoConocimiento
 * @date    2/04/2018
 * @run     "python3.6 Make.py"
 * @require numpy, matplotlib
 */
'''


'''=======================================================
==================    DATA    ============================
======================================================='''

DataSize = [
    100, 1000
] 

Algorithms = [
    "LinealSearch", 
    "ParalellLinealSearch", 
    "BinarySearch", 
    "ParalellBinarySearch", 
    "SearchWithBST"
]

Cases = [
    322486, 14700764, 3128036, 6337399, 61396, 10393545, 
    2147445644, 1295390003, 450057883, 187645041, 1980098116,
    152503, 5000, 1493283650, 214826, 1843349527, 1360839354,
    2109248666 , 214747085, 0
]



'''=======================================================
============      COMPILE THE PROGRAM     ================
======================================================='''

CasesFile = "Inputs/RealCases.txt"
CasesSize = 20

ProgramName = "TestSearchAlgorithms"
Input       = "Inputs/Input10MillionSorted.txt"
Flags       = "-std=c11 -pthread Time.c"

os.system("reset")
os.system(F"gcc {Flags} {ProgramName}.c -o {ProgramName}")




'''=======================================================
============      RUN THE PROGRAM         ================
======================================================='''
for NumAlgorithm in range(0, len(Algorithms)):

    AlgorithmName = Algorithms[NumAlgorithm]

    for n in DataSize:

        OutputPlace = f"Outputs/Out-{AlgorithmName}-N={n}"

        print(f"*********** Running Algorithm {AlgorithmName} for n = {n} ****************")

        OutputProgram = subprocess.check_output(
            f'./{ProgramName} {n} {NumAlgorithm} {CasesSize} {CasesFile} {OutputPlace} < {Input}',
            shell = True,
            universal_newlines = True)

        RealTimeAverage = 0
        UserTimeAverage = 0
        SysTimeAverage  = 0

        print(f"*********** Each Number ****************")
        for Line in OutputProgram.splitlines(False):
            Data = [float(i) for i in Line.split()]
            RealTime = Data[0]
            UserTime = Data[1]
            SysTime = Data[2]

            RealTimeAverage += RealTime
            UserTimeAverage += UserTime
            SysTimeAverage  += SysTime

            CPUWall = (UserTime + SysTime) / RealTime;

            print(f"Real:     {RealTime}s")
            print(f"User:     {UserTime}s")
            print(f"Sys:      {SysTime}s")
            print(f"CPU/Wall: {CPUWall * 100}%")
            print("")

        print(f"*********** Average Time ****************")
        RealTimeAverage /= len(Cases)
        UserTimeAverage /= len(Cases)
        SysTimeAverage  /= len(Cases)
        CPUWallAverage  = (UserTimeAverage + SysTimeAverage) / RealTimeAverage;

        print(f"Real:     {RealTimeAverage}s")
        print(f"User:     {UserTimeAverage}s")
        print(f"Sys:      {SysTimeAverage}s")
        print(f"CPU/Wall: {CPUWallAverage * 100}%")
        print("")



