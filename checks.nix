''
  machine.wait_for_x()
  machine.wait_for_file("/home/florian/.Xauthority")
  # this is somehow needed for machine.wait_for_window
  machine.succeed("xauth merge /home/florian/.Xauthority")
  machine.send_key("meta_l-ret")
  machine.wait_for_window("florian.*nixosThinkpad")
  machine.screenshot("terminalBeforeLock")
  machine.send_key("meta_l-u")
  machine.sleep(3)
  machine.send_chars("touch /tmp/locked\n",0.2)
  machine.screenshot("locked")
  machine.sleep(1)
  machine.fail("stat /tmp/locked")
  # import time
  # this part is flaky
  # machine.send_key("esc")
  # time.sleep(0.2)
  # machine.send_key("ret")
  # time.sleep(0.2)
  # machine.send_chars("password",0.2)
  # time.sleep(0.2)
  # machine.send_key("ret")
  # time.sleep(1)
  # machine.send_chars("touch /tmp/locked\n",0.2)
  # machine.wait_for_file("/tmp/locked")
  # machine.screenshot("terminalAfterLock")
''
