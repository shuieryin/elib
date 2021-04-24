import os
import re
import sys
import fileinput


def replace_str_in_file(filepath, match_str, new_str):
    with fileinput.FileInput(filepath, inplace=True) as file:
        for cur_line in file:
            print(cur_line.replace(match_str, new_str), end="")


for filename in os.listdir("src"):
    app_name_matches = re.search(r"^(\w+)\.app\.src$", filename)
    if app_name_matches:
        app_name = app_name_matches.group(1)
        version_number = None
        major_number = None
        minor_number = None
        patch_number = None
        with open("src/" + filename, "r") as app_file:
            while not version_number:
                line = app_file.readline()
                if not line:
                    break
                line = line.strip()
                vsn_matches = re.search(r"{vsn,\s?\"((\d+)\.(\d+)\.(\d+))\"}", line)
                if vsn_matches:
                    version_number = vsn_matches.group(1)
                    major_number = int(vsn_matches.group(2))
                    minor_number = int(vsn_matches.group(3))
                    patch_number = int(vsn_matches.group(4))
                    break

        if not version_number:
            print("version number in `{vsn, xxx}` not found.")
            sys.exit(1)

        print("Current version:", version_number)

        first_input = True
        mode = None
        mode_matches = None
        while not mode_matches:
            if first_input:
                first_input = False
            else:
                sys.stdout.write("\033[F" * 5)
            print("1. Major\n2. Minor\n3. Patch\n4. Exit")
            sys.stdout.write("\033[K")
            mode = input("Please select: ").strip()
            mode_matches = re.search(r"^[1-4]$", mode)

        if mode == "1":
            major_number += 1
            minor_number = 0
            patch_number = 0
        elif mode == "2":
            minor_number += 1
            patch_number = 0
        elif mode == "3":
            patch_number += 1
        elif mode == "4":
            print("Bye.")
            sys.exit()

        updated_version_number = "%d.%d.%d" % (major_number, minor_number, patch_number)
        replace_str_in_file("src/" + filename, "{vsn, \"%s\"}" % version_number, "{vsn, \"%s\"}" %
                            updated_version_number)
        replace_str_in_file("rebar.config", "{%s, \"%s\"}" % (app_name, version_number), "{%s, \"%s\"}" %
                            (app_name, updated_version_number))

        commit_message = "Release [%s]" % updated_version_number
        print(commit_message)

        os.system("git commit -am \"%s\" && git tag \"%s\" && git push origin master && git push --tags" %
                  (commit_message, updated_version_number))

        break
