from pexpect import pxssh
import json

username = "admin"
password = "firulais"

Routers = {}


def get_info_from_ip(ip):
    child = pxssh.pxssh()
    print("\nLogging in...")
    child.login(ip, username, password, auto_prompt_reset=False)
    print("Logged in")

    name = child.before[-3: -1].decode("UTF-8")

    if Routers.get(name, "NEW") != "NEW": return

    child.sendline("enable")
    child.expect("Password:")

    child.sendline("12345678")
    child.expect(f"{name}#")

    child.sendline("show cdp neighbors")
    child.expect(f"{name}#")

    data = child.before
    data = data.decode("UTF-8")
    data = data.split("\n")
    data = data[5: len(data) - 1]
    connections = {}

    for line in data:
        line = line.split()
        con_line = line[0].split(".")[0]
        interface = line[2]
        connections[interface] = con_line

    print(name)
    print(connections)

    child.sendline("show ip route")
    child.expect(f"{name}#")

    data = child.before

    data = data.decode("UTF-8")
    data = data.split("\r\n")
    data = [x.split() for x in data]

    real_data = []
    seen = []
    for index, line in enumerate(data):
        if len(line) == 0 or line[0] != "is": continue
        
        interface = line[-1][-3:]
        real_ip = data[index -1][-1]

        if "NO" == connections.get(interface, "NO") or interface in seen:
            continue

        seen.append(interface)
        real_data.append({"enlace": connections.get(interface), "salto": real_ip})
    
    Routers[name] = real_data
    print(Routers)

    for element in real_data:
        print(element)
        if Routers.get(element["enlace"], "NEW") != "NEW": continue
        get_info_from_ip(element["salto"])

get_info_from_ip("148.204.56.1")

print(Routers)

with open("network.txt", "w") as text_file:
    text_file.write(json.dumps(Routers))