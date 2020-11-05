from pexpect import pxssh, spawn
import json
import netifaces
from ipaddress import IPv4Address
from threading import Lock


username, password = "admin", "firulais"
algorithm, cipher = "diffie-hellman-group1-sha1", "aes256-cbc"
options = {"KexAlgorithms": algorithm, "Ciphers": cipher}

routers = {}
lock = Lock()


def crawling_from(ip):
    print("\nLogging into", ip)

    child = pxssh.pxssh(options=options)
    child.login(ip, username, password, auto_prompt_reset=False)

    name = child.before[-2:].decode("utf-8")
    print(f"[{name}] logged into")

    child.sendline("show cdp neighbors")
    child.expect(f"{name}#")

    data = child.before.decode("utf-8").split("\n")[5:-1]
    connections = {}

    for line in data:
        info = line.split()
        connection_name, interface = info[0][:2], info[2]
        connections[interface] = connection_name

    print(f"[{name}] neigbours found: {connections}")

    child.sendline("show ip interface brief")
    child.expect(f"{name}#")

    data = child.before.decode("utf-8").split("\r\n")
    data = [line for line in data if 'Fast' in line]

    terminals = {}
    for line in data:
        info = line.split()
        interface, network_ip = info[0][-3:], info[1]
        if interface not in connections:
            terminal_ip = IPv4Address(int(IPv4Address(network_ip)) + 9)
            terminals[interface] = str(terminal_ip)

    print(f"[{name}] terminals found: {terminals}")

    routers[name] = {"terminals": terminals, "neigbours": connections}
    child.sendline("show ip route connected")
    child.expect(f"{name}#")

    next_jumps = []
    data = child.before.decode("utf-8").split("\r\n")
    data = [line for line in data if line != "" and line[0] == "C"]
    for line in data:
        info = line.split()
        network_ip, interface = info[1], info[-1][-3:]
        if interface in connections:
            print(network_ip)
            next_ip = IPv4Address(int(IPv4Address(network_ip)) + 1)
            connection_name = connections[interface]
            next_jumps.append((connection_name, str(next_ip)))

    print(f"[{name}] next jumps: {next_jumps}")
    for connection_name, jump_ip in next_jumps:
        with lock:
            if connection_name not in routers:
                routers[connection_name] = "seen"
                print(f"searching {connection_name} in {jump_ip}")


gateways = netifaces.gateways()
default_gateway = gateways['default'][netifaces.AF_INET][0]
ip = default_gateway
crawling_from(ip)

print(routers)
