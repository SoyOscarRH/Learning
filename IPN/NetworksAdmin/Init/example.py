import pexpect

devices = {"iosv-1": {"promt": "iosv-1#", "ip": "172.16.1.20"},
           "iosv-2": {"promt": "iosv-2#", "ip": "172.16.1.21"}
           }

user = password = "cisco"

for device in devices:
    child = pexpect.spawn("telnet", device["ip"])

    child.expect("Username")
    child.sendline(user)

    child.expect("Password")
    child.sendline(password)

    child.expect(device["promt"])
    child.sendline("show version | iV")

    child.expect(device["promt"])
    child.expect(child.before)
    child.sendline("exit")
