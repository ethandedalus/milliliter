#!/usr/bin/env python3

import sys
import os
import subprocess
from argparse import ArgumentParser
from tempfile import NamedTemporaryFile

assert sys.version_info >= (3, 10)

parser = ArgumentParser()
parser.add_argument("file")
parser.add_argument("-l", "--lex", action="store_true")
parser.add_argument("-p", "--parse", action="store_true")
parser.add_argument("-t", "--tacky", action="store_true")
parser.add_argument("-c", "--codegen", action="store_true")
args = parser.parse_args()

script_dir = os.path.dirname(os.path.realpath(__file__))
compiler = os.path.join(script_dir, "bin", "milliliter")


file = args.file
flags = []
if args.lex:
    flags.append("--lex")
if args.parse:
    flags.append("--parse")
if args.tacky:
    flags.append("--tacky")
if args.codegen:
    flags.append("--codegen")

outdir = os.path.dirname(os.path.realpath(file))
outname = os.path.splitext(os.path.basename(file))[0]

with (
    NamedTemporaryFile(suffix=".i", delete=False) as pre,
    NamedTemporaryFile(suffix=".s", delete=False) as asm,
):
    pre_path = pre.name
    asm_path = asm.name

    try:
        subprocess.run(["gcc", "-E", "-P", file, "-o", pre_path], check=True)
        cmd = [compiler, pre_path] + flags
        cmd += ["--out", asm_path]

        result = subprocess.run(cmd, stdout=open(asm_path, "w"))
        if result.returncode != 0:
            sys.exit(result.returncode)

        if not args.codegen and not args.lex and not args.parse and not args.tacky:
            subprocess.run(
                ["gcc", asm_path, "-o", os.path.join(outdir, outname)],
                check=True,
            )
        else:
            with open(asm_path) as f:
                sys.stdout.write(f.read())
    finally:
        os.unlink(pre_path)
        os.unlink(asm_path)
