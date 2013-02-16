#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-12.
#  Copyright (c) 2007. All rights reserved.

require 'sqlite'

db = SQLite::Database.new('sample.db', 0644)