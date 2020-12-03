require 'openssl'
require 'digest/sha1'
c = OpenSSL::Cipher::Cipher.new("aes-256-cbc")
c.encrypt
# your pass is what is used to encrypt/decrypt
c.key = key = Digest::SHA1.hexdigest("yourpass").unpack('a2'*32).map{|x| x.hex}.pack('c'*32)
c.iv = iv = c.random_iv
e = c.update("crypt this")
e << c.final
puts "encrypted: #{e}\n"
c = OpenSSL::Cipher::Cipher.new("aes-256-cbc")
c.decrypt
c.key = key
c.iv = iv
d = c.update(e)
d << c.final
puts "decrypted: #{d}\n"