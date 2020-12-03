#!/usr/bin/ruby
# Usage: ./make_license_key.sh <name or email address>
# echo -n "$1" | openssl dgst -sha1 -binary | openssl rsautl -sign -inkey private.pem | openssl enc -base64

require 'openssl'

message = "This is some cool text."

private_key = OpenSSL::PKey::RSA.new(File.read("./private.pem"))
cipher_text = private_key.private_encrypt(message)

public_key = OpenSSL::PKey::RSA.new(File.read("./public.pem"))
plain_text = public_key.public_decrypt(cipher_text)

puts "Message: #{message}"
puts "Cipher Text: #{cipher_text}"
puts "Plain Text: #{plain_text}"