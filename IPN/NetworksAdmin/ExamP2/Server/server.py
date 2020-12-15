import netifaces
import os

from fastapi import FastAPI
from fastapi.responses import FileResponse
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy import create_engine
from typing import Dict, Any

from pexpect import pxssh, spawn
from ipaddress import IPv4Address
from graphviz import Digraph

username, password = "admin", "admin"
algorithm, cipher = "diffie-hellman-group1-sha1", "aes256-cbc"
options = {"KexAlgorithms": algorithm, "Ciphers": cipher}

app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_credentials=True,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"]
)

engine = create_engine('mysql://hugo:Oscarj2#@localhost/network')


@app.get("/getRoutes")
def read_root():
    with engine.connect() as connection:
        response = connection.execute('SELECT * FROM router')
        router = [row[0] for row in response]

        return router


@app.post("/user/{router}")
def edit_router(router: str, data: Dict[Any, Any]):
    old, name, user_password = data["old"], data["name"], data["password"]

    with engine.connect() as connection:
        query1 = f"DELETE FROM user WHERE username = '{old}' AND router_name = '{router}'"
        connection.execute(query1)

        if name != "":
            query2 = f"INSERT INTO user (username, password, router_name) VALUES ('{name}', '{user_password}', '{router}')"

            connection.execute(query2)

        query3 = f"SELECT ip_id FROM router WHERE name = '{router}'"
        result = tuple(connection.execute(query3))[0]

        ip_id = result[0]

        child = pxssh.pxssh(options=options)
        child.login(ip_id, username, password, auto_prompt_reset=False)

        print(f"[{router}] logged into")

        child.sendline("configure terminal")
        child.expect("#")
        child.sendline(f"no username {old}")
        child.expect("#")

        if name != "":
            child.sendline(f"username {name} priv 15 password {user_password}")
            child.expect(f"#")
            print(
                f"[{router}] new user with: username {name} priv 15 password {user_password}")


@app.get("/router/{router}/{interface}")
def get_interface(router: str, interface: str):
    with engine.connect() as connection:
        name = f"{interface[0]}/{interface[1]}"
        query = f"SELECT * FROM interface WHERE router_name = '{router}' AND name = '{name}'"
        info = tuple(connection.execute(query))[0]

        return info


@app.post("/router/editRouter/{router}")
def edit_router(router: str, data: Dict[Any, Any]):
    with engine.connect() as connection:
        name, ip_id, model, version = data["name"], data["ip_id"], data["model"], data["version"]
        where = f"WHERE name = '{router}'"
        values = f"name = '{name}', ip_id = '{ip_id}', model = '{model}', version = '{version}'"
        query = f"UPDATE router SET {values} {where}"
        connection.execute(query)


@app.get("/router/{router}")
def read_router(router: str):
    with engine.connect() as connection:
        query1 = f"SELECT * FROM router WHERE name = '{router}'"
        info1 = tuple(connection.execute(query1))[0]

        query2 = f"SELECT name, ip_id FROM interface WHERE router_name = '{router}'"
        info2 = tuple(connection.execute(query2))

        query3 = f"SELECT username, router_name, password FROM user WHERE router_name = '{router}'"
        info3 = tuple(connection.execute(query3))

        return {"info": info1, "interfaces": info2, "users": info3}


@app.get("/topology")
def get_topology():
    return FileResponse(f"./net.png")


@app.get("/getTopo")
def create_topology():
    routers = {}
    with engine.connect() as connection:
        connection.execute('DELETE FROM router')
        connection.execute('DELETE FROM interface')
        connection.execute('DELETE FROM user')

    print('deleting tables')

    try:
        os.system("rm /home/oscar/.ssh/known_hosts")
    except:
        pass

    def crawling_from(ip: str) -> None:
        print("\nLogging into", ip)

        child = pxssh.pxssh(options=options)
        child.login(ip, username, password, auto_prompt_reset=False)

        name = child.before[-2:].decode("utf-8")
        if name == "":
            return

        print(f"[{name}] logged into")

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
        interfaces = {}
        for line in data:
            info = line.split()
            interface, network_ip = info[0][-3:], info[1]
            if network_ip != "unassigned":
                interfaces[interface] = network_ip
            not_a_router = interface not in connections
            switch_connection = network_ip[0] != "8"
            if (not_a_router or switch_connection) and network_ip != "unassigned":
                network_ip = network_ip[:-1] + "0"
                terminal_ip = str(IPv4Address(
                    int(IPv4Address(network_ip)) + 10))
                if terminal_ip[0] != "8":
                    terminals[interface] = terminal_ip

        print(f"[{name}] interfaces found: {interfaces}")
        print(f"[{name}] terminals found: {terminals}")

        ip_id = str(IPv4Address(max([int(IPv4Address(interface))
                                     for interface in interfaces.values()])))

        with engine.connect() as connection:
            model, version = "Cisco IOS 3600", "Version 12.4(25d)"
            query = f"insert into router (name, ip_id, model, version) values ('{name}', '{ip_id}', '{model}', '{version}')"
            connection.execute(query)

        with engine.connect() as connection:
            for interface_name in interfaces:
                ip_id = interfaces[interface_name]
                mask = "255.255.255.0" if ip_id[0] == "1" else "255.255.255.252"
                connected_router_name = connections[interface_name] if interface_name in connections else "terminal"

                insert_into = "insert into interface (name, router_name, ip_id, mask, connected_router_name)"
                query = f"{insert_into} values ('{interface_name}', '{name}', '{ip_id}', '{mask}', '{connected_router_name}')"

                connection.execute(query)

        routers[name] = {"terminals": terminals, "neigbours": connections,
                         "interfaces": interfaces, "ip_id": ip_id}
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

        print(f"[{name}] next jumps: {next_jumps}")
        for connection_name, next_ip1, next_ip2 in next_jumps:
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
        ip_id = routers[router]["ip_id"]
        dot.node(router, f"{router}: {ip_id}")

        for interface in routers[router]["neigbours"]:
            jump_name = routers[router]["neigbours"][interface]
            ip_interface = routers[router]["interfaces"][interface]
            dot.edge(router, jump_name, f"{interface} {ip_interface}")

        for interface in routers[router]["terminals"]:
            ip = routers[router]["terminals"][interface]
            dot.node(ip, ip, shape='plaintext')
            dot.edge(router, ip)
            dot.edge(ip, router)

    dot.render('./net')

    return FileResponse(f"./net.png")
