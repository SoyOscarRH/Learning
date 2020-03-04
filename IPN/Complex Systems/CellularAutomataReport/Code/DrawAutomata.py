from PIL import Image
import os
import random
import plotly.io as pio
from math import sqrt

n = iterations = 500


def graph(histogram, average, variance, path):
    global n

    desviation = sqrt(variance)

    trace1 = {"type": "bar", "y": histogram, "opacity": 1,
              "name": "Histogram", "marker": {"color": "rgb(158,202,225)"}, }

    trace2 = {"type": "scatter", "y": [average, average], "x": [0, len(histogram) - 1], "opacity": 0.5,
              "name": "Average", "marker": {"color": "#FC7A7A"}, }

    trace3 = {"type": "scatter", "y": [average + desviation, average + desviation], "x": [0, len(histogram) - 1], "opacity": 0.5,
              "name": "sqrt derivation +", "marker": {"color": "#009999"}, }

    trace4 = {"type": "scatter", "y": [average - desviation, average - desviation], "x": [0, len(histogram) - 1], "opacity": 0.5,
              "name": "sqrt derivation -", "marker": {"color": "#009999"}, }

    fig = { "data": [trace1, trace2, trace3, trace4],  "layout": {"title": {"text": "Ones"}} }

    pio.write_image(fig, path)


def get_rules(rules_id):
    def rule(s, i):
        limit = len(s) - 1
        n1 = limit if i == 0 else i - 1
        n2 = 0 if i == limit else i + 1

        id = (s[n1] << 2) + (s[i] << 1) + (s[n2] << 0)
        return (rules_id >> id) & 1

    return rule


def print_evolution(steps, s, rules_id, pixels, path):
    global n
    histogram = []

    rules = get_rules(rules_id)
    current, temporal = list(s), list(s)
    limit = len(s)

    one, zero = (255, 255, 255), (40, 44, 52)

    for step in range(steps):
        histogram.append(sum(current))

        for i in range(limit):
            temporal[i] = rules(current, i)
        for j, val in enumerate(temporal):
            pixels[j, step] = one if val else zero

        current = list(temporal)

    average = sum(histogram) / n

    variance = 0
    for val in histogram:
        variance += (val - average) * (val - average)

    variance = variance / n

    graph(histogram, average, variance, path)


rules = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 22, 23, 24, 25, 26, 27, 28, 29,
         30, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 51, 54, 56, 57, 58, 60, 62, 72, 73,
         74, 76, 77, 78, 90, 94, 104, 105, 106, 108, 110, 122, 126, 128, 130, 132, 134, 136, 138, 140, 142,
         146, 150, 152, 154, 156, 160, 162, 164, 168, 170, 172, 178, 184, 200, 204, 232]

inits = []
random.seed(10)


def oneMiddle():
    s = [0] * n
    s[n // 2] = 1
    return s


def getPercentage(percentage):
    s = [0] * n
    for i in range(len(s)):
        s[i] = 1 if percentage > random.random() else 0

    return s


inits = [oneMiddle(), getPercentage(0.08), getPercentage(.5), getPercentage(.87)]

for rule in [28, 199, 70, 157]:
    print(rule)
    for i, init in enumerate(inits):
        letter = chr(ord("a") + i)
        img = Image.new('RGB', (500, 500), "black")
        pixels = img.load()


        diagram = f"../Images/{rule}/{letter}.png"
        path = f"../Images/{rule}/dia-{letter}.png"

        print_evolution(iterations, init, rule, pixels, path)
        img.save(diagram)
