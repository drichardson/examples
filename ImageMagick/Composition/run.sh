mate #!/bin/bash

if [ ! -f generateCommands.rb ]; then
	echo "You must run this command from the folder that contains generateCommands.rb"
	exit 1
fi

rm -rf results
mkdir results || exit 1
ruby generateCommands.rb | bash || exit 1

echo Success