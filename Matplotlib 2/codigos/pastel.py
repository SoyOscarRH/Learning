import matplotlib.pyplot as plt


labels = 'America', 'Europa', 'Asia', 'Oceania', 'Africa', 'Antartida'
sizes = [42.55, 10.18, 44.58, 8.52, 30.37, 14]

explode = (0.3, 0, 0, 0, 0, 0)

f, ax1 = plt.subplots()

ax1.pie(sizes, explode=explode, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)

ax1.axis('equal')

plt.title("Porcentaje de extension territorial")
plt.show()