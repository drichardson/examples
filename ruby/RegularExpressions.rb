#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-11.
#  Copyright (c) 2007. All rights reserved.

require "Address.rb"
require "PersonName.rb"

def printAddressParts(address)
  a = Address.new address
  
  puts "Extended: #{a.extended}"
  puts "Street: #{a.street}"
  puts "City: #{a.locality}\nState: #{a.region}\nPostal: #{a.postalCode}"
  puts "Country: #{a.country}"
  puts
end

def printPersonParts(name)
  n = PersonName.new(name)
  puts "Prefixes: #{n.prefixes.join(':')}"
  puts "First: #{n.given}"
  puts "Additionals: #{n.additionals.join(':')}"
  puts "Last: #{n.last}"
  puts "Suffixes: #{n.suffixes.join(':')}"
end

printPersonParts "Mr. Douglas Ryan Richardson Esq."
printPersonParts "Mr. Douglas Ryan Richardson"
printPersonParts "Mr. Douglas Richardson"
printPersonParts "Douglas Ryan Richardson Esq."
printPersonParts "Douglas Ryan Richardson"
printPersonParts "Douglas Richardson"
printPersonParts "Mr. Douglas Richardson"
printPersonParts "Douglas Richardson Esq."
printPersonParts "Mr. Richardson Esq."
printPersonParts "Mr. Richardson"
printPersonParts "Richardson Esq."
printPersonParts "Mr. Douglas Ryan Richardson Esq."
exit 1

printAddressParts "Mailbox 253\n8717 Research Dr.\nIrvine, CA 92832\nUSA"
printAddressParts "Mailbox 253\n8717 Research Dr.\nIrvine, CA 92832"
printAddressParts "8717 Research Dr.\nIrvine, CA 92832\nUSA"
printAddressParts "8717 Research Dr.\nIrvine, CA 92832"
printAddressParts "Irvine, CA 92832"
printAddressParts "8717 Research Dr."
printAddressParts "Irvine, CA"
printAddressParts "Irvine, CA\nUSA"

printAddressParts "Peoria, AZ 85382"
printAddressParts "San Fransisco, CA 90842"
printAddressParts "San Luis Obispo, CA 90032"
printAddressParts "New York City, New York 10032"
printAddressParts "New York City, New York"
printAddressParts "Washington, D.C. 85382-1423" # DC should go in the state part, according to google maps.
printAddressParts "St. Louis, MI 82532"
printAddressParts "San-Bernadino, CA"
printAddressParts "Washington, D.C."
printAddressParts "Huntington Beach, California"
