import json
import netifaces
import os

from pexpect import pxssh, spawn
from ipaddress import IPv4Address
from graphviz import Digraph
from threading import Lock, Thread

username, password = "admin", "firulais"
algorithm, cipher = "diffie-hellman-group1-sha1", "aes256-cbc"
options = {"KexAlgorithms": algorithm, "Ciphers": cipher}

routers = {}
lock = Lock()

try:
    os.system("rm /home/soyoscarrh/.ssh/known_hosts")
except:
    pass


def crawling_from(ip):
    print("\nLogging into", ip)

    child = pxssh.pxssh(options=options)
    child.login(ip, username, password, auto_prompt_reset=False)

    name = child.before[-2:].decode("utf-8")
    if name == "":
        return

    print(f"[{name}] logged into")

    with lock:
        if name in routers and routers[name] != "seen":
            print(f"[{name}] data already here, logging out")
            return

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
        not_a_router = interface not in connections
        switch_connection = network_ip[0] != "8"
        if (not_a_router or switch_connection) and network_ip != "unassigned":
            network_ip = network_ip[:-1] + "0"
            terminal_ip = str(IPv4Address(int(IPv4Address(network_ip)) + 10))
            if terminal_ip[0] != "8":
                terminals[interface] = terminal_ip

    print(f"[{name}] terminals found: {terminals}")

    with lock:
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
            next_ip1 = str(IPv4Address(int(IPv4Address(network_ip)) + 1))
            next_ip2 = str(IPv4Address(int(IPv4Address(network_ip)) + 2))
            connection_name = connections[interface]
            next_jumps.append((connection_name, next_ip1, next_ip2))

    child.sendline("conf t")
    child.sendline("username pirata priv 15 password pirata")
    child.sendline("end")

    print(f"[{name}] next jumps: {next_jumps}")
    for connection_name, next_ip1, next_ip2 in next_jumps:
        with lock:
            if connection_name in routers:
                continue
            routers[connection_name] = "seen"

        print(f"[{name}] searching {connection_name}")
        crawling_from(next_ip1)
        crawling_from(next_ip2)


gateways = netifaces.gateways()
default_gateway = gateways['default'][netifaces.AF_INET][0]
ip = default_gateway
crawling_from(ip)

dot = Digraph(comment='Topology of network', format='png')
for router in routers:
    if routers[router] == "seen":
        continue

    print("\n", router)
    print("\t neigbours:", routers[router]["neigbours"])
    print("\t terminals:", routers[router]["terminals"])
    dot.node(router, router)

    for interface in routers[router]["neigbours"]:
        jump_name = routers[router]["neigbours"][interface]
        dot.edge(router, jump_name)

    for interface in routers[router]["terminals"]:
        ip = routers[router]["terminals"][interface]
        dot.node(ip, ip, shape='plaintext')
        dot.edge(router, ip)
        dot.edge(ip, router)

dot.render('./net')
