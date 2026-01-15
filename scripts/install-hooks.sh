#!/bin/bash
#
# Install git hooks for the terminus project
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Create the pre-commit hook
cat > "$PROJECT_ROOT/.git/hooks/pre-commit" << 'EOF'
#!/bin/bash
#
# Pre-commit hook to prevent increases in .subscribe/.sample usage
# These FRP escape hatches should be reduced over time, not increased.
#

BASELINE_FILE=".frp-baseline"

# Count current .subscribe and .sample usages in widget code only (not tests)
current_count=$(grep -rn '\.subscribe\|\.sample' --include='*.lean' ./Terminus/Reactive 2>/dev/null | wc -l | tr -d ' ')

# Read baseline
if [ ! -f "$BASELINE_FILE" ]; then
    echo "Warning: No .frp-baseline file found. Creating one with current count: $current_count"
    echo "$current_count" > "$BASELINE_FILE"
    git add "$BASELINE_FILE"
    exit 0
fi

baseline=$(cat "$BASELINE_FILE" | tr -d ' \n')

if [ "$current_count" -gt "$baseline" ]; then
    echo "============================================================"
    echo "ERROR: FRP lint check failed!"
    echo "============================================================"
    echo ""
    echo "  .subscribe/.sample count increased: $baseline → $current_count"
    echo ""
    echo "  Direct use of .subscribe and .sample is discouraged."
    echo "  Use declarative FRP combinators instead."
    echo ""
    echo "  To find the new usages:"
    echo "    grep -rn '\\.subscribe\\|\\.sample' --include='*.lean' ./Terminus/Reactive"
    echo ""
    echo "  If you've removed usages elsewhere, update .frp-baseline:"
    echo "    echo $current_count > .frp-baseline"
    echo ""
    echo "============================================================"
    exit 1
fi

if [ "$current_count" -lt "$baseline" ]; then
    echo "FRP lint: Nice! Reduced .subscribe/.sample usage: $baseline → $current_count"
    echo "Updating .frp-baseline..."
    echo "$current_count" > "$BASELINE_FILE"
    git add "$BASELINE_FILE"
fi

exit 0
EOF

chmod +x "$PROJECT_ROOT/.git/hooks/pre-commit"

echo "Git hooks installed successfully!"
echo "The pre-commit hook will prevent increases in .subscribe/.sample usage."
