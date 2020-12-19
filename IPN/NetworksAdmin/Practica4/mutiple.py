from typing import Optional, Tuple, Any
import netifaces
import os

from pexpect import pxssh
from ipaddress import IPv4Address

username, password = "admin", "admin"
default_gateway = netifaces.gateways()['default'][netifaces.AF_INET][0]


def clean_ssh_host_file() -> None:
    try:
        os.system("rm /home/oscar/.ssh/known_hosts")
    except:
        pass


def get_connection(ip: str, next_level_ip: Optional[str]) -> Tuple[str, Any]:
    print("\nTrying to log into", ip)
    child = pxssh.pxssh()
    child.login(ip, username, password, auto_prompt_reset=False)

    router_name = child.before[-2:].decode("utf-8")
    print(f"[{router_name}] logged into")

    if next_level_ip:
        print("Now trying to log into", next_level_ip)
        child.sendline(f"ssh -l {username} {next_level_ip}")
        child.sendline(password)
        child.expect("#")
        router_name = child.before[-2:].decode("utf-8")
        print(f"[{router_name}] logged into")

    return router_name, child


seen_networks, static = set(), None


def get_network_from_ip(ip: str) -> str:
    return ip[:-3] + "0"


def get_wildcard_from_ip(ip: str) -> str:
    return "0." * 3 + "255"


def get_mask_from_ip(ip: str) -> str:
    return "255." * 3 + "0"


def get_connection_info(router_name: str, child):
    # get neighbors
    child.sendline("show cdp neighbors")
    child.expect("#")

    raw_data_neigbours = child.before.decode("utf-8").split("\n")[5:-1]
    neigbours = {}

    for line in raw_data_neigbours:
        info = line.split()
        name, interface = info[0][:2], info[2]
        neigbours[interface] = name

    print(f"[{router_name}] neigbours found: {neigbours}")

    # get ip data
    child.sendline("show ip interface brief")
    child.expect("#")

    raw_interfaces_text = child.before.decode("utf-8").split("\r\n")
    raw_interfaces = [line for line in raw_interfaces_text if 'Fast' in line]

    connection_info, terminals = {}, {}
    for line in raw_interfaces:
        info = line.split()
        interface, interface_ip = info[0][-3:], info[1]

        if interface in neigbours:
            other_side = int(IPv4Address(interface_ip)) - 1
            info = {"interface": interface, "ip": str(IPv4Address(other_side))}
            connection_info[neigbours[interface]] = info

            network_ip = get_network_from_ip(interface_ip)
            seen_networks.add(str(IPv4Address(network_ip)))

        elif interface_ip != "unassigned":
            info = {"interface": interface, "ip": interface_ip}
            name = "PC" + router_name[1:]
            terminals[interface] = name
            connection_info[name] = info

            network_ip = get_network_from_ip(interface_ip)
            seen_networks.add(str(IPv4Address(network_ip)))

    print(f"[{router_name}] terminals found: {terminals}")
    print(f"[{router_name}] final info: {connection_info}")

    return connection_info, neigbours


def visit(real_ip: str, mode: str) -> None:
    global seen_networks, static

    router_name, child = get_connection(default_gateway, real_ip)
    connection_info, neigbours = get_connection_info(router_name, child)

    neigbour = list(neigbours.values())[0]
    neighbor_ip = get_network_from_ip(connection_info[neigbour]["ip"])

    network_ips = connection_info.values()
    network_ips = [get_network_from_ip(x["ip"]) for x in network_ips]
    seen_networks = seen_networks.union(network_ips)

    commands = []

    if mode == "OSPF":
        commands += ["conf t", "router ospf 1", "redistribute connected subnets",
                     f"network {neighbor_ip} {get_wildcard_from_ip(neighbor_ip)} area 0", "end"]

    elif mode == "RIP":
        commands += ["conf t", "router rip", "version 2", "redistribute connected",
                     "no auto-summary", f"network {neighbor_ip}", "end"]

    elif mode == "static":
        static = get_network_from_ip(connection_info["PC1"]["ip"])
        interface = connection_info["R4"]["interface"]

        commands += ["conf t"]
        def not_neigbour(network): return network not in network_ips
        static_networks = list(filter(not_neigbour, seen_networks))
        mask = get_mask_from_ip(static_networks[0])
        commands += [f"ip route {network} {mask} f{interface}" for network in static_networks]
        commands += ["end"]

    else:
        interface = connection_info["R1"]["interface"]
        commands_static = [
            "conf t", f"ip route {static} {get_mask_from_ip(static)} f{interface}", "end"]

        core_rip = "network "
        core_rip += get_network_from_ip(connection_info["R2"]["ip"])
        commands_rip = ["conf t", "router rip", "version 2", "no auto-summary",
                        "redistribute connected", "redistribute static", "redistribute ospf 1 metric 1", core_rip, "end"]

        network_ip_ospf = get_network_from_ip(connection_info["R3"]["ip"])
        wildcard = get_wildcard_from_ip(network_ip_ospf)
        core_ospf = f"network {network_ip_ospf} {wildcard} area 0"
        commands_ospf = ["conf t", "router ospf 1", "redistribute connected subnets",
                         "redistribute static subnets", "redistribute rip subnets", core_ospf, "end"]

        commands += commands_static + commands_rip + commands_ospf

    for command in commands:
        print(command)
        child.sendline(command)
        child.expect("#")


clean_ssh_host_file()

router_name, child = get_connection(default_gateway, None)
connection_info, neigbours = get_connection_info(router_name, child)

visit(connection_info["R3"]["ip"], "OSPF")
visit(connection_info["R2"]["ip"], "RIP")
visit(connection_info["R1"]["ip"], "static")
visit(None, "all")
