import sys
import os

filename = sys.argv[1]
name, extension = os.path.splitext(filename)

flags = " ".join([
    "-std=c++17", "-lpthread", "-O2", "-Wall", "-Wextra", "-Wshadow",  "-Wpedantic",
    ])

compile = f"g++ {filename} {flags} -o {name}.out"
os.system(compile)
