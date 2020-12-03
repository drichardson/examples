#!/bin/bash

# Documentation: https://www.gnu.org/software/bash/manual/bash.html#Here-Documents

MYVARIABLE=123

echo "Unquoted here doc:"
cat << EOF
    My Variable is: $MYVARIABLE
EOF

echo "Quoted here doc:"
cat << 'EOF'
    My Variable is: $MYVARIABLE
EOF

echo "Unquoted, with indentation removed. Using TABs:"
cat <<- EOF
	My Variable is: $MYVARIABLE
EOF

echo "Unquoted, with indentation removed. Using spaces."
cat <<- EOF
    My Variable is: $MYVARIABLE
EOF

