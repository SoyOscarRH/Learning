import sys

filename = sys.argv[1]
reverse = len(sys.argv) > 2

with open(filename, 'r') as file:
    filedata = file.read()

if not reverse:
    filedata = filedata.replace('ε', 'epsilon')
    filedata = filedata.replace('∅', 'empty')
else:
    filedata = filedata.replace('epsilon', 'ε')
    filedata = filedata.replace('empty', '∅')


with open(filename, 'w') as file:
    file.write(filedata)
