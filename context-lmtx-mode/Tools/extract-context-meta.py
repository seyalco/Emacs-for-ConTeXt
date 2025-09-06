#!/usr/bin/env python3
import xml.etree.ElementTree as ET
import json
import os
import sys

ELISP_FILE = "context-lmtx-commands.el"

def wrap_with_delimiters(text, delimiters):
    if delimiters == "braces":
        return f"{{{text}}}"
    elif delimiters == "brackets":
        return f"[{text}]"
    elif delimiters == "parentheses":
        return f"({text})"
    elif delimiters == "angle":
        return f"<{text}>"
    elif delimiters == "none":
        return text
    # پیش‌فرض
    return f"[{text}]"

def parse_arguments(arg_tag, NS):
    if arg_tag is None:
        return "", ""
    summary_parts = []
    details_parts = []
    param_index = 1

    for child in arg_tag:
        tag = child.tag.split("}")[-1]
        optional = child.attrib.get("optional", "no") == "yes"
        delimiters = child.attrib.get("delimiters", "").strip()
        opt_marker = " <OPT>" if optional else ""

        if tag == "keywords":
            const = child.find("cd:constant", NS)
            ctype = const.attrib.get("type", "") if const is not None else ""
            display_name = ctype.upper() or "NAME"

            # --- فرم کوتاه ---
            if delimiters:
                arg_form = wrap_with_delimiters(display_name, delimiters)
            else:
                arg_form = wrap_with_delimiters(display_name, "brackets")
            summary_parts.append(arg_form)

            # --- توضیح کامل ---
            details_parts.append(f"{param_index}{opt_marker} {display_name}")

        elif tag == "assignments":
            parameters = child.findall("cd:parameter", NS)

            # --- فرم کوتاه (ساده) ---
            if delimiters:
                arg_form = wrap_with_delimiters("..,..=..,..", delimiters)
            else:
                arg_form = wrap_with_delimiters("..,..=..,..", "brackets")
            summary_parts.append(arg_form)

            # --- توضیح کامل (پیشرفته در یک خط) ---
            if parameters:
                param_specs = []
                for p in parameters:
                    pname = p.attrib.get("name", "")
                    types = []
                    for c in p.findall("cd:constant", NS):
                        t = c.attrib.get("type", "").upper()
                        if c.attrib.get("default", "no") == "yes":
                            t += " (default)"
                        types.append(t)
                    for inh in p.findall("cd:inherit", NS):
                        inh_name = inh.attrib.get("name", "")
                        types.append(f"inherits: \\{inh_name}")
                    type_str = " | ".join(types) if types else "ANY"
                    param_specs.append(f"{pname}={type_str}")
                details_parts.append(f"{param_index}{opt_marker} " + ", ".join(param_specs))
            else:
                inherits = child.find("cd:inherit", NS)
                inherit_disp = f"inherits: \\{inherits.attrib.get('name', '')}" if inherits is not None else "assignments"
                details_parts.append(f"{param_index}{opt_marker} {inherit_disp}")

        param_index += 1

    return " ".join(summary_parts), "\n".join(details_parts)

def main():
    if len(sys.argv) > 1:
        XML_FILE = sys.argv[1]
    else:
        XML_FILE = "context-en.xml"

    if not os.path.exists(XML_FILE):
        raise FileNotFoundError(f"Cannot find {XML_FILE}")

    tree = ET.parse(XML_FILE)
    root = tree.getroot()

    # تشخیص namespace به صورت پویا
    if root.tag.startswith("{"):
        ns_uri = root.tag.split("}")[0][1:]
        NS = {"cd": ns_uri}
    else:
        NS = {}

    commands = []
    for cmd in root.findall(".//cd:command", NS):
        name = cmd.attrib.get("name", "").strip()
        category = cmd.attrib.get("category", "").strip()
        file_name = cmd.attrib.get("file", "").strip()
        keywords = cmd.attrib.get("keywords", "").strip()
        level = cmd.attrib.get("level", "").strip()
        ctype = cmd.attrib.get("type", "").strip()

        if not name:
            continue

        command_str = "\\" + name
        annotation = category if category else "ConTeXt"

        meta_parts = []
        if category: meta_parts.append(f"Category: {category}")
        if file_name: meta_parts.append(f"File: {file_name}")
        if level: meta_parts.append(f"Level: {level}")
        if keywords: meta_parts.append(f"Keywords: {keywords}")
        if ctype: meta_parts.append(f"Type: {ctype}")

        form_line, args_list = parse_arguments(cmd.find("cd:arguments", NS), NS)
        if form_line:
            meta_parts.append(f"Args: {form_line}")
        if args_list:
            meta_parts.append(args_list)

        meta_str = "\n".join(meta_parts)
        commands.append((command_str, annotation, meta_str))

    # نوشتن خروجی Elisp
    with open(ELISP_FILE, "w", encoding="utf-8") as f:
        f.write(";;; context-lmtx-commands.el --- Auto-generated -*- lexical-binding: t; -*-\n\n")
        f.write("(defvar context-lmtx-command-list\n")
        f.write("  '(\n")
        for cmd_name, annotation, meta in sorted(commands):
            f.write(f"    ({json.dumps(cmd_name, ensure_ascii=False)} "
                    f"{json.dumps(annotation, ensure_ascii=False)} "
                    f"{json.dumps(meta, ensure_ascii=False)})\n")
        f.write("  )\n")
        f.write("  \"List of ConTeXt LMTX commands with annotation and meta.\")\n\n")
        f.write("(provide 'context-lmtx-commands)\n")
        f.write(";;; context-lmtx-commands.el ends here\n")

    print(f"[+] Generated {ELISP_FILE} with {len(commands)} commands.")

if __name__ == "__main__":
    main()
