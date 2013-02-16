#!/usr/bin/ruby

puts "{"

while line = gets
  #puts line
  elements = line.split(" ")
  for word in elements[1..-1]
    #puts "word: " + word
    puts "0x" + word[0..1] + ", 0x" + word[2..3] + ", 0x" + word[4..5] + ", 0x" + word[6..7] + ","
  end
end

puts "}"