import pexpect

devices = [('R1', '192.168.122.65'), ('R2', '192.168.122.130'),
           ('R3', '192.168.122.138'), ('R4', '192.168.122.134')]

password, secret = "1234", "12345678"

for name, ip in devices:
    print("\nentering into ", name)
    child = pexpect.spawn('telnet ' + ip)

    child.expect('Password:')
    child.sendline(password)
    print("1")

    child.expect(name)
    child.sendline("enable")
    print("2")

    child.expect('Password:')
    child.sendline(secret)
    print("3")

    child.expect(name)
    child.sendline("configure terminal")
    print("5")

    child.expect(name)
    child.sendline("hostname " + name)
    print("6")

    child.expect(name)
    child.sendline(f"ip domain-name {name}.LOCAL")
    print("7")

    child.expect(name)
    child.sendline(f"crypto key generate rsa")
    print("8")

    child.expect(":")
    child.sendline("yes")
    print("9")

    child.expect(":")
    child.sendline("2048")
    print("9.5")

    child.expect(name)
    child.sendline("ip ssh time-out 10")
    print("10")

    child.expect(name)
    child.sendline(f"ip ssh authentication-retries 3")
    print("11")

    child.expect(name)
    child.sendline(f"ip ssh version 2")
    print("12")

    child.expect(name)
    child.sendline(f"line vty 0 4 ")
    print("13")

    child.expect(name)
    child.sendline("transport input ssh telnet")
    print("14")

    child.expect(name)
    child.sendline("login local")
    print("15")

    child.expect(name)
    child.sendline("exit")
    print("16")

    child.expect(name)
    child.sendline("exit")
    print("17")

    child.expect(name)
    child.sendline('exit')
    print("18")
