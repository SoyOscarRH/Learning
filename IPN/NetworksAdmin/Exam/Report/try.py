from pexpect import pxssh, spawn
import json
import netifaces

gateways = netifaces.gateways()
default_gateway = gateways['default'][netifaces.AF_INET][0]

username = "admin"
password = "firulais"

routers = {}

ssh_options = {"KexAlgorithms": "diffie-hellman-group1-sha1",
               "Ciphers": "aes256-cbc"}

ip = default_gateway

print("\nLogging into", ip)

child = pxssh.pxssh(options=ssh_options)
child.login(ip, username, password, auto_prompt_reset=False)

name = child.before[-2:].decode("UTF-8")
print("Logged into", name)

child.sendline("enable")
child.expect(f"{name}#")

print(child.before)
print(child.after)

child.sendline("show cdp neighbors")
child.expect(f"{name}#")

print(child.before)
print(child.after)

