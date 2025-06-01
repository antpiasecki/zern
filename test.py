import os
import sys
import time

os.system("cargo build --release")

for x in os.listdir("examples/"):
    print(
        "\033[93mBuilding %s...\033[0m" % x,
        end="",
        flush=True,
    )
    t = time.time()
    if os.system("./target/release/zern examples/" + x) != 0:
        sys.exit(1)
    print(" %sms" % round((time.time() - t) * 1000, 2))

    t = time.time()
    if os.system("./out") != 0:
        sys.exit(1)
    print("\033[93mRunning %s...\033[0m %sms" % (x, round((time.time() - t) * 1000, 2)))
