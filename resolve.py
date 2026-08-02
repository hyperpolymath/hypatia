import os
import re

def resolve_file(filepath):
    with open(filepath, "r") as f:
        content = f.read()

    # Find conflict blocks
    conflict_pattern = re.compile(r"<<<<<<< HEAD\n(.*?)\n=======\n(.*?)\n>>>>>>> origin/main\n", re.DOTALL)
    
    def replacer(match):
        head = match.group(1)
        main = match.group(2)
        
        # Check if head has timeout-minutes
        timeout_match = re.search(r"^[ \t]*timeout-minutes: \d+", head, re.MULTILINE)
        if timeout_match and "timeout-minutes" not in main:
            # Append it to main
            return main + "\n" + timeout_match.group(0)
        return main
        
    resolved = conflict_pattern.sub(replacer, content)
    with open(filepath, "w") as f:
        f.write(resolved)

for file in os.popen("git diff --name-only --diff-filter=U").read().splitlines():
    resolve_file(file)
